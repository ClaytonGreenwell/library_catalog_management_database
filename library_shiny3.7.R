# Loading the packages

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(DT)
library(DBI)
library(RSQLite)
library(dplyr)

# Connecting to the database

db <- dbConnect(SQLite(), "community_library.sqlite")

# Date data type fix

fix_dates <- function(df) {
  date_cols <- c("Checkout_Date", "Due_Date", "Return_Date")
  for (col in date_cols) {
    if (col %in% names(df)) {
      df[[col]] <- as.Date(df[[col]])
    }
  }
  df
}

# User interface

ui <- tagList(
  useShinyalert(force = TRUE),
  navbarPage(
    title = "Community Library Catalog App",
    windowTitle = "Community Library Catalog Management Application",
    position = "fixed-top",
    collapsible = TRUE,
    theme = shinytheme("cosmo"),
    
    #  About tab
    
    tabPanel(
      title = "About",
      br(), 
      br(),
      h3("Community Library Catalog Management Application"),
      p("A full CRUD library system backed by a SQLite database."),
      tags$ul(
        tags$li("Book Checkout: Select a patron, choose an available book, and record a checkout."),
        tags$li("Book Return: View and return books currently checked out."),
        tags$li("Patrons: Add, edit, and delete patrons."),
        tags$li("Books: Add, edit, and delete books."),
        tags$li("Transaction Log: View all checkout and return activity.")
      )
    ),
    
    # Book Checkout tab
    
    tabPanel(
      title = "Book Checkout",
      br(), 
      br(),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "checkout_patron", 
            label = "Select Patron:", 
            choices = NULL
          ),
          actionButton(
            inputId = "checkout_btn", 
            label = "Checkout", 
            class = "btn-primary", 
            width = "100%"
          )
        ),
        mainPanel(
          h4("Available Books"),
          DTOutput("books_table")
        )
      )
    ),
    
    # Book Return tab
    
    tabPanel(
      title = "Book Return",
      br(), 
      br(),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "return_filter_patron", 
            label = "Select Patron:", 
            choices = NULL
          ),
          actionButton(
            inputId = "return_btn2", 
            label = "Mark Book as Returned", 
            class = "btn-warning", 
            width = "100%"
          )
        ),
        mainPanel(
          h4("Books Currently Checked Out"),
          DTOutput("books_return_table")
        )
      )
    ),
    
    # Patrons tab
    
    tabPanel(
      "Patrons",
      br(), 
      br(),
      fluidPage(
        fluidRow(
          column(
            12,
            actionButton(
              inputId = "add_patron", 
              label = "Add Patron", 
              class = "btn-success"
            ),
            actionButton(
              inputId = "edit_patron", 
              label = "Edit Selected Patron", 
              class = "btn-primary"
            ),
            actionButton(
              inputId = "delete_patron", 
              label = "Delete Selected Patron", 
              class = "btn-danger"
            )
          )
        ),
        br(),
        DTOutput("patron_table")
      )
    ),
    
    # Books tab
    
    tabPanel(
      title = "Books",
      br(), 
      br(),
      fluidPage(
        fluidRow(
          column(
            12,
            actionButton(
              inputId = "add_book", 
              label = "Add Book", 
              class = "btn-success"
            ),
            actionButton(
              inputId = "edit_book", 
              label = "Edit Selected Book", 
              class = "btn-primary"
            ),
            actionButton(
              inputId = "delete_book", 
              label = "Delete Selected Book", 
              class = "btn-danger"
            )
          )
        ),
        br(),
        DTOutput("books_admin")
      )
    ),
    
    # Transaction Log tab
    
    tabPanel(
      title = "Transaction Log",
      br(), 
      br(),
      DTOutput("transaction_log_table")
    )
  )
)

# Server Logic

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    patrons = NULL,
    books = NULL,
    transactions = NULL
  )
  
  # Load all data from DB into reactives
  
  load_data <- function() {
    rv$patrons <- dbGetQuery(
      db,
      "SELECT 
         Patron_id, 
         First_Name, 
         Last_Name, 
         Phone_Number, 
         Email_Address
       FROM 
         patron
       ORDER BY 
         Last_Name, 
         First_Name"
    )
    
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
       FROM 
         books
       ORDER BY 
         Title"
    )
    
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
         FROM 
           transaction_log t
         LEFT JOIN 
           patron p ON t.Patron_id = p.Patron_id
         LEFT JOIN 
           books b ON t.book_id = b.Book_id
         ORDER BY 
           t.Transaction_id DESC"
      )
    )
  }
  
  load_data()
  
  observe({
    req(rv$patrons)
    p_choices <- rv$patrons %>% mutate(Full = paste(Last_Name, First_Name, sep = ", ")) %>% pull(Full)
    updateSelectInput(session, "checkout_patron", choices = p_choices)
  })
  
  # Book status
  
  books_status <- reactive({
    req(rv$books, rv$transactions)
    active_ids <- rv$transactions %>% filter(is.na(Return_Date)) %>% pull(Book_id)
    rv$books %>%
      mutate(Status = ifelse(Book_id %in% active_ids | Available == "No", "Out", "In"))
  })
  
  output$books_table <- renderDT({
    df <- books_status() %>% filter(Status == "In") %>% 
      select(
        Book_id, 
        Title, 
        Author_Names,
        Bookshelves,
        Subjects
      )
    datatable(df, selection = "single")
  })
  
  # Checkout Action
  
  observeEvent(input$checkout_btn, {
    req(input$books_table_rows_selected, input$checkout_patron)
    
    available_books <- books_status() %>% filter(Status == "In")
    book_row <- available_books[input$books_table_rows_selected, ]
    
    # Resolve Patron
    parts <- strsplit(input$checkout_patron, ", ")[[1]]
    patron_id <- rv$patrons %>% 
      filter(Last_Name == parts[1], First_Name == parts[2]) %>% 
      pull(Patron_id)
    
    dbExecute(db, 
              "INSERT INTO 
                 transaction_log (
                   Patron_id, 
                   Checkout_Date, 
                   Due_Date, 
                   book_id
                 ) 
               VALUES 
                 (?, ?, ?, ?)",
              params = list(patron_id, as.character(Sys.Date()), as.character(Sys.Date() + 14), book_row$Book_id))
    
    dbExecute(db, 
              "UPDATE 
                 books 
               SET 
                 Available = 'No', 
                 Checked_Out_By = ? 
               WHERE 
                 Book_id = ?",
              params = list(patron_id, book_row$Book_id))
    
    load_data()
    shinyalert("Success", "Book checked out.", type = "success")
  })
  
  # Book Return Tab Logic
  
  observe({
    req(rv$transactions)
    active <- rv$transactions %>% 
      filter(is.na(Return_Date)) %>% 
      mutate(Full = paste(Last_Name, First_Name, sep = ", ")) %>%
      distinct(Full) %>% pull(Full)
    updateSelectInput(session, "return_filter_patron", choices = c("All", active))
  })
  
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
    datatable(checked_out_data() %>% 
                select(
                  Book_id, 
                  Title, 
                  Patron_Name
                ), 
              selection = "single")
  })
  
  observeEvent(input$return_btn2, {
    req(input$books_return_table_rows_selected)
    book_id <- checked_out_data()$Book_id[input$books_return_table_rows_selected]
    
    dbExecute(db, 
              "UPDATE 
                 transaction_log 
               SET 
                 Return_Date = ? 
               WHERE 
                 book_id = ? AND Return_Date IS NULL",
              params = list(as.character(Sys.Date()), book_id))
    dbExecute(db, 
              "UPDATE 
                 books 
               SET 
                 Available = 'Yes', 
                 Checked_Out_By = NULL 
               WHERE Book_id = ?",
              params = list(book_id))
    
    load_data()
    shinyalert("Returned", "Database updated.", type = "success")
  })
  
  # CRUD
  
  output$patron_table <- renderDT({
    datatable(rv$patrons, selection = "single")
  })
  
  output$books_admin <- renderDT({
    datatable(rv$books, selection = "single")
  })
  
  output$transaction_log_table <- renderDT({
    datatable(rv$transactions)
  })
  
  # Disconnect session
  
  session$onSessionEnded(function() { dbDisconnect(db) })
}

# Run app

shinyApp(ui, server)