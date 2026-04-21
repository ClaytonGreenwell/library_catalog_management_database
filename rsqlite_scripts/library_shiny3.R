# -------------------------
# Libraries
# -------------------------
library(DBI)
library(RSQLite)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(DT)
library(forecast)
library(ggplot2)
library(plotly)
library(maps)
library(leaflet)
library(readxl)  # still here only if you use it elsewhere; safe to remove if not needed

# -------------------------
# Database connection
# -------------------------

# Assumes library.db already exists and contains:
#   - books
#   - patron
#   - transaction_log
db <- dbConnect(RSQLite::SQLite(), "library.db")

# Read tables from SQLite
books <- dbReadTable(db, "books")
patron <- dbReadTable(db, "patron")
transaction_log <- dbReadTable(db, "transaction_log")

# Convert date columns from SQLite (stored as text) to Date
transaction_log$Checkout_Date <- as.Date(transaction_log$Checkout_Date)
transaction_log$Return_Date   <- as.Date(transaction_log$Return_Date)
transaction_log$Due_Date      <- as.Date(transaction_log$Due_Date)

# -------------------------
# Helper queries / values
# -------------------------

# Patron dropdown list
patron_list <- dbGetQuery(
  db,
  "SELECT DISTINCT 
     Last_Name || ', ' || First_Name AS Full_Name 
   FROM patron 
   ORDER BY Last_Name"
)

# Default patron
default_patron <- if ("Greenwell, Clayton" %in% patron_list$Full_Name) {
  "Greenwell, Clayton"
} else {
  patron_list$Full_Name[[1]]
}

# Date bounds for transaction log
date_bounds <- dbGetQuery(
  db,
  "SELECT 
     MIN(Checkout_Date) AS min_dt, 
     MAX(Checkout_Date) AS max_dt 
   FROM transaction_log"
)

min_date <- as.Date(date_bounds$min_dt[[1]])
max_date <- as.Date(date_bounds$max_dt[[1]])

# Important join query
important_query <- "
  SELECT
    t.Transaction_id,
    p.Patron_id,
    p.First_Name,
    p.Last_Name,
    p.Phone_Number,
    p.Email_Address,
    b.[Etext Number],
    b.Title,
    b.Authors,
    b.Bookshelves,
    t.Checkout_Date,
    t.Due_Date,
    t.Return_Date
  FROM 
    transaction_log t
  LEFT JOIN patron p ON t.Patron_id = p.Patron_id
  LEFT JOIN books b ON t.book_id = b.[Etext Number]
"

# -------------------------
# UI
# -------------------------

ui <- tagList(
  useShinyalert(),
  navbarPage(
    title = "Community Library Catalog App",
    windowTitle = "Community Library Catalog Management Application",
    position = "fixed-top",
    collapsible = TRUE,
    theme = shinytheme("cosmo"),
    
    # 1. About tab
    tabPanel(
      title = "About",
      fluidPage(
        br(), br(),
        h3("Community Library Catalog Management Application"),
        p("An app that helps librarians and patrons checkout books from the library."),
        tags$ul(
          tags$li("Book Checkout: A patron drop-down filter, a searchable book availability table, and a save-to-database action button."),
          tags$li("Patrons: A searchable table displaying all patrons and their corresponding information."),
          tags$li("Transaction Log: A searchable table displaying all book checkout transactions.")
        )
      )
    ),
    
    # 2. Book Checkout tab
    tabPanel(
      title = "Book Checkout",
      fluidPage(
        br(), br(),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "checkout_patron",
              label = "Select Patron:",
              choices = patron_list$Full_Name,
              selected = default_patron
            ),
            actionButton(
              "save_transaction", 
              "Finalize Checkout", 
              class = "btn-primary", 
              width = "100%"
            )
          ),
          mainPanel(
            DTOutput("book_availability_table")
          )
        )
      )
    ),
    
    # 3. Patrons tab
    tabPanel(
      title = "Patrons",
      fluidPage(
        br(), br(),
        DTOutput("patron_table")
      )
    ),
    
    # 4. Transaction Log tab
    tabPanel(
      title = "Transaction Log",
      fluidPage(
        br(), br(),
        DTOutput("transaction_log_table")
      )
    )
  )
)

# -------------------------
# Server
# -------------------------

server <- function(input, output, session) {
  
  # Reactive: books table (simple version; same behavior as Excel-era app)
  books_available <- reactive({
    dbGetQuery(
      db,
      "SELECT 
         [Etext Number],
         Title,
         Authors,
         Bookshelves
       FROM books"
    )
  })
  
  # Book availability table
  output$book_availability_table <- renderDT({
    datatable(
      books_available(),
      selection = "single",
      options = list(pageLength = 10)
    )
  })
  
  # Patrons table
  output$patron_table <- renderDT({
    datatable(
      patron,
      options = list(pageLength = 10)
    )
  })
  
  # Transaction log table (using the important join query)
  output$transaction_log_table <- renderDT({
    joined <- dbGetQuery(db, important_query)
    datatable(
      joined,
      options = list(pageLength = 10)
    )
  })
  
  # -------------------------
  # Checkout logic
  # -------------------------

  observeEvent(input$save_transaction, {
    # Ensure a book is selected
    sel <- input$book_availability_table_rows_selected
    if (length(sel) != 1) {
      shinyalert(
        title = "No book selected",
        text = "Please select a book from the table before finalizing checkout.",
        type = "warning"
      )
      return(NULL)
    }
    
    # Get selected book row
    books_df <- books_available()
    selected_book <- books_df[sel, ]
    
    # Parse patron name "Last_Name, First_Name"
    patron_name <- input$checkout_patron
    name_parts <- strsplit(patron_name, ",\\s*")[[1]]
    last_name  <- name_parts[1]
    first_name <- ifelse(length(name_parts) > 1, name_parts[2], "")
    
    # Look up Patron_id
    patron_row <- dbGetQuery(
      db,
      "SELECT Patron_id 
       FROM patron 
       WHERE Last_Name = ? AND First_Name = ?",
      params = list(last_name, first_name)
    )
    
    if (nrow(patron_row) == 0) {
      shinyalert(
        title = "Patron not found",
        text = "Could not find the selected patron in the database.",
        type = "error"
      )
      return(NULL)
    }
    
    patron_id <- patron_row$Patron_id[[1]]
    
    # Basic checkout dates (adjust if your Excel version used different logic)
    checkout_date <- Sys.Date()
    due_date      <- checkout_date + 14  # example: 2-week loan
    return_date   <- NA                  # not yet returned
    
    # Insert new transaction row
    dbExecute(
      db,
      "INSERT INTO transaction_log 
         (Patron_id, book_id, Checkout_Date, Due_Date, Return_Date)
       VALUES (?, ?, ?, ?, ?)",
      params = list(
        patron_id,
        selected_book[["Etext Number"]],
        as.character(checkout_date),
        as.character(due_date),
        NA
      )
    )
    
    shinyalert(
      title = "Checkout saved",
      text = paste0(
        "Book '", selected_book$Title, 
        "' has been checked out to ", patron_name, "."
      ),
      type = "success"
    )
    
    # Refresh transaction log table
    output$transaction_log_table <- renderDT({
      joined <- dbGetQuery(db, important_query)
      datatable(
        joined,
        options = list(pageLength = 10)
      )
    })
  })
  
  # Disconnect when session ends (optional but nice)
  session$onSessionEnded(function() {
    dbDisconnect(db)
  })
}

# -------------------------
# Run the app
# -------------------------

shinyApp(ui = ui, server = server)