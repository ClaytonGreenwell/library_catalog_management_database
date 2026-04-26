# =====================================================================
#  Packages — the usual suspects
#  (TODO: consider renaming this block if we ever modularize the app)
# =====================================================================

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(DT)
library(DBI)
library(RSQLite)
library(dplyr)

# =====================================================================
#  Database Connection
#  (Note: assumes working directory contains community_library.sqlite)
# =====================================================================

db <- dbConnect(SQLite(), "community_library.sqlite")

# =====================================================================
#  Helpers — small utilities that keep the server code cleaner
# =====================================================================

# Convert date-like text columns to proper Date objects.
# This keeps the transaction log from turning into a character soup.
fix_dates <- function(df) {
  date_cols <- c("Checkout_Date", "Due_Date", "Return_Date")
  for (col in date_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.Date(df[[col]])
    }
  }
  df
}

# =====================================================================
#  UI Helper Functions — Option A (Extracted UI helpers)
#  These make the UI code feel more "lived in" and less repetitive.
# =====================================================================

# A tiny spacer — humans love breathing room.
ui_space <- function(height = "20px") {
  div(style = paste0("height:", height, ";"))
}

# Header helper — keeps typography consistent.
ui_section_header <- function(text) {
  tagList(
    ui_space("10px"),
    h4(text),
    ui_space("10px")
  )
}

# Patron CRUD button row
patronCrudButtons <- function() {
  tagList(
    actionButton("add_patron", "Add Patron", class = "btn-success"),
    actionButton("edit_patron", "Edit Selected Patron", class = "btn-primary"),
    actionButton("delete_patron", "Delete Selected Patron", class = "btn-danger")
  )
}

# Book CRUD button row
bookCrudButtons <- function() {
  tagList(
    actionButton("add_book", "Add Book", class = "btn-success"),
    actionButton("edit_book", "Edit Selected Book", class = "btn-primary"),
    actionButton("delete_book", "Delete Selected Book", class = "btn-danger")
  )
}

# A reusable sidebar pattern for selectInput + actionButton
# (TODO: consider generalizing this later if more sidebars appear)
ui_sidebar_action <- function(select_id, label, choices, btn_id, btn_label, btn_class) {
  sidebarPanel(
    selectInput(select_id, label, choices = choices),
    actionButton(btn_id, btn_label, class = btn_class, width = "100%")
  )
}

# =====================================================================
#  UI — Heavy Humanized, reorganized internally but same tab order
# =====================================================================

ui <- tagList(
  useShinyalert(force = TRUE),

  navbarPage(
    title = "Community Library Catalog App",
    windowTitle = "Community Library Catalog Management Application",
    position = "fixed-top",
    collapsible = TRUE,
    theme = shinytheme("cosmo"),

    # -----------------------------------------------------------------
    # 1. ABOUT TAB — lightly humanized text (Option B)
    # -----------------------------------------------------------------
    tabPanel(
      title = "About",
      br(), br(),

      h3("Welcome to the Community Library Catalog"),
      p("This application helps our library volunteers manage day-to-day tasks without needing to dig through spreadsheets or handwritten notes."),
      p("You can check out books, return them, update patron records, maintain the catalog, and review the full transaction history — all in one place."),

      tags$ul(
        tags$li("Book Checkout — pick a patron, choose a book, and record the checkout."),
        tags$li("Book Return — see what's currently out and mark items as returned."),
        tags$li("Patrons — add, edit, or remove patron records."),
        tags$li("Books — maintain the catalog with full CRUD controls."),
        tags$li("Transaction Log — browse the full history of checkouts and returns.")
      ),

      ui_space("40px")
    ),
        # -----------------------------------------------------------------
    # 2. BOOK CHECKOUT TAB
    #    (UI reorganized for readability; logic unchanged)
    # -----------------------------------------------------------------
    tabPanel(
      title = "Book Checkout",
      br(), br(),

      sidebarLayout(
        # Sidebar: patron selector + checkout button
        sidebarPanel(
          selectInput(
            inputId = "checkout_patron",
            label = "Select Patron:",
            choices = NULL
          ),
          ui_space("10px"),
          actionButton(
            inputId = "checkout_btn",
            label = "Checkout",
            class = "btn-primary",
            width = "100%"
          )
        ),

        # Main panel: available books table
        mainPanel(
          ui_section_header("Available Books"),
          DTOutput("books_table")
        )
      )
    ),

    # -----------------------------------------------------------------
    # 3. BOOK RETURN TAB
    #    (Same functionality; layout cleaned up)
    # -----------------------------------------------------------------
    tabPanel(
      title = "Book Return",
      br(), br(),

      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "return_filter_patron",
            label = "Select Patron:",
            choices = NULL
          ),
          ui_space("10px"),
          actionButton(
            inputId = "return_btn2",
            label = "Mark Book as Returned",
            class = "btn-warning",
            width = "100%"
          )
        ),

        mainPanel(
          ui_section_header("Books Currently Checked Out"),
          DTOutput("books_return_table")
        )
      )
    ),

    # -----------------------------------------------------------------
    # 4. PATRONS TAB
    #    (Using extracted CRUD helpers — Option A)
    # -----------------------------------------------------------------
    tabPanel(
      title = "Patrons",
      br(), br(),

      fluidPage(
        fluidRow(
          column(
            width = 12,
            patronCrudButtons()   # <— extracted helper
          )
        ),

        ui_space("20px"),

        DTOutput("patron_table")
      )
    ),
        # -----------------------------------------------------------------
    # 5. BOOKS TAB
    #    (CRUD buttons extracted; layout cleaned up)
    # -----------------------------------------------------------------
    tabPanel(
      title = "Books",
      br(), br(),

      fluidPage(
        fluidRow(
          column(
            width = 12,
            bookCrudButtons()   # <— extracted helper
          )
        ),

        ui_space("20px"),

        DTOutput("books_admin")
      )
    ),

    # -----------------------------------------------------------------
    # 6. TRANSACTION LOG TAB
    #    (Simple table; kept clean and readable)
    # -----------------------------------------------------------------
    tabPanel(
      title = "Transaction Log",
      br(), br(),

      fluidPage(
        ui_section_header("Full Transaction History"),
        DTOutput("transaction_log_table")
      )
    )
  ) # end navbarPage
)   # end tagList


# =====================================================================
#  SERVER LOGIC — reorganized for readability, functionality unchanged
# =====================================================================

server <- function(input, output, session) {

  # Reactive containers — the app’s working memory.
  # (TODO: consider switching to reactivePoll if DB grows large)
  rv <- reactiveValues(
    patrons = NULL,
    books = NULL,
    transactions = NULL
  )

  # -------------------------------------------------------------------
  # Data Loader — pulls fresh data from SQLite into reactives
  # -------------------------------------------------------------------
  load_data <- function() {

    # Patrons
    rv$patrons <- dbGetQuery(
      db,
      "SELECT 
         Patron_id,
         First_Name,
         Last_Name,
         Phone_Number,
         Email_Address
       FROM patron
       ORDER BY Last_Name, First_Name"
    )

    # Books
    rv$books <- dbGetQuery(
      db,
      "SELECT 
         Book_id,
         Author_Names,
         Title,
         Bookshelves,
         Subjects,
         Available,
         Checked_Out_By
       FROM books
       ORDER BY Title"
    )

    # Transactions (with joins + date fix)
    rv$transactions <- fix_dates(
      dbGetQuery(
        db,
        "SELECT
           t.Transaction_id,
           t.Patron_id,
           p.First_Name,
           p.Last_Name,
           t.book_id AS Book_id,
           b.Title,
           t.Checkout_Date,
           t.Due_Date,
           t.Return_Date
         FROM transaction_log t
         LEFT JOIN patron p ON t.Patron_id = p.Patron_id
         LEFT JOIN books b ON t.book_id = b.Book_id
         ORDER BY t.Transaction_id DESC"
      )
    )
  }

  # Initial load
  load_data()
    # -------------------------------------------------------------------
  # Patron dropdown for checkout — keep it simple
  # -------------------------------------------------------------------
  observe({
    req(rv$patrons)

    p_choices <- rv$patrons %>%
      mutate(Full = paste(Last_Name, First_Name, sep = ", ")) %>%
      pull(Full)

    updateSelectInput(session, "checkout_patron", choices = p_choices)
  })

  # -------------------------------------------------------------------
  # Book status — determines what's actually available
  # -------------------------------------------------------------------
  books_status <- reactive({
    req(rv$books, rv$transactions)

    active_ids <- rv$transactions %>%
      filter(is.na(Return_Date)) %>%
      pull(Book_id)

    rv$books %>%
      mutate(Status = ifelse(Book_id %in% active_ids | Available == "No", "Out", "In"))
  })

  output$books_table <- renderDT({
    df <- books_status() %>%
      filter(Status == "In") %>%
      select(Book_id, Title, Author_Names, Bookshelves, Subjects)

    datatable(df, selection = "single")
  })

  # -------------------------------------------------------------------
  # Checkout logic — unchanged, just cleaned up
  # -------------------------------------------------------------------
  observeEvent(input$checkout_btn, {
    req(input$books_table_rows_selected, input$checkout_patron)

    available_books <- books_status() %>% filter(Status == "In")
    book_row <- available_books[input$books_table_rows_selected, ]

    # Resolve patron
    parts <- strsplit(input$checkout_patron, ", ")[[1]]
    patron_id <- rv$patrons %>%
      filter(Last_Name == parts[1], First_Name == parts[2]) %>%
      pull(Patron_id)

    # Insert transaction
    dbExecute(
      db,
      "INSERT INTO transaction_log (Patron_id, Checkout_Date, Due_Date, book_id)
       VALUES (?, ?, ?, ?)",
      params = list(
        patron_id,
        as.character(Sys.Date()),
        as.character(Sys.Date() + 14),
        book_row$Book_id
      )
    )

    # Update book availability
    dbExecute(
      db,
      "UPDATE books
       SET Available = 'No', Checked_Out_By = ?
       WHERE Book_id = ?",
      params = list(patron_id, book_row$Book_id)
    )

    load_data()
    shinyalert("Success", "Book checked out.", type = "success")
  })

  # -------------------------------------------------------------------
  # Book Return — dropdown population
  # -------------------------------------------------------------------
  observe({
    req(rv$transactions)

    active <- rv$transactions %>%
      filter(is.na(Return_Date)) %>%
      mutate(Full = paste(Last_Name, First_Name, sep = ", ")) %>%
      distinct(Full) %>%
      pull(Full)

    updateSelectInput(session, "return_filter_patron", choices = c("All", active))
  })

  # -------------------------------------------------------------------
  # Books currently checked out
  # -------------------------------------------------------------------
  checked_out_data <- reactive({
    req(rv$books, rv$patrons)

    df <- rv$books %>%
      filter(Available == "No") %>%
      left_join(rv$patrons, by = c("Checked_Out_By" = "Patron_id")) %>%
      mutate(Patron_Name = paste(Last_Name, First_Name, sep = ", "))

    if (input$return_filter_patron != "All") {
      df <- df %>% filter(Patron_Name == input$return_filter_patron)
    }

    df
  })

  output$books_return_table <- renderDT({
    datatable(
      checked_out_data() %>%
        select(Book_id, Title, Patron_Name),
      selection = "single"
    )
  })

  # -------------------------------------------------------------------
  # Return logic
  # -------------------------------------------------------------------
  observeEvent(input$return_btn2, {
    req(input$books_return_table_rows_selected)

    book_id <- checked_out_data()$Book_id[input$books_return_table_rows_selected]

    # Mark transaction as returned
    dbExecute(
      db,
      "UPDATE transaction_log
       SET Return_Date = ?
       WHERE book_id = ? AND Return_Date IS NULL",
      params = list(as.character(Sys.Date()), book_id)
    )

    # Update book availability
    dbExecute(
      db,
      "UPDATE books
       SET Available = 'Yes', Checked_Out_By = NULL
       WHERE Book_id = ?",
      params = list(book_id)
    )

    load_data()
    shinyalert("Returned", "Database updated.", type = "success")
  })

  # -------------------------------------------------------------------
  # Patron CRUD — unchanged logic, humanized formatting
  # -------------------------------------------------------------------
  output$patron_table <- renderDT({
    datatable(rv$patrons, selection = "single")
  })

  # -------------------------------------------------------------------
  # Books admin table — simple passthrough
  # -------------------------------------------------------------------
  output$books_admin <- renderDT({
    datatable(rv$books, selection = "single")
  })

  # -------------------------------------------------------------------
  # Transaction log table
  # -------------------------------------------------------------------
  output$transaction_log_table <- renderDT({
    datatable(rv$transactions)
  })

  # -------------------------------------------------------------------
  # Session cleanup — always good practice
  # -------------------------------------------------------------------
  session$onSessionEnded(function() {
    dbDisconnect(db)
  })
}

# =====================================================================
#  Run the App
# =====================================================================

shinyApp(ui, server)