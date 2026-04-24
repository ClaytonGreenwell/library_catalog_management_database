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

# Helper: convert date-like text columns to Date

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
# 
# The UI is a navbarPage with 6 tabs:
# 1. About (landing page)
# 2. Book Checkout
# 3. Book Return
# 4. Patrons
# 5. Books (table of all books for deletions, additions, edits)
# 6. Transaction Log

ui <- tagList(
  useShinyalert(force = TRUE),
  navbarPage(
    title = "Community Library Catalog App",
    windowTitle = "Community Library Catalog Management Application",
    position = "fixed-top",
    collapsible = TRUE,
    theme = shinytheme("cosmo"),
    
    # 1. About tab: simple descriptive landing page explaining what the app 
    # contains.
    
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
    
    # 2. Book Checkout tab: A patron drop-down filter, a searchable book 
    # availability table, a save-to-database action button (Checkout).
    
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
            DTOutput("books_table"),
          )
        )
      ),
      
      # 3. Book Return tab: A patron drop-down filter, a searchable books 
      # currently check out table, a return-item action button.
      
      tabPanel(
        title = "Book Return",
        br(), 
        br(),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "return_filter_patron", 
              label = "Select Patron:", choices = NULL),
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
      
      # 4. Patrons tab: A searchable table displaying all patrons and their 
      # corresponding information.
      
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
      
      # 5. Books tab: A searchable table of books with Add, Delete, and 
      # Edit action buttons.
      
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
      
      # 6. Transaction Log tab: A searchable table displaying all book checkout 
      # transactions.
      
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
  
  # Patron dropdown for checkout
  
  observe({
    req(rv$patrons)
    patrons <- rv$patrons
    patrons$Full_Name <- paste(patrons$Last_Name, patrons$First_Name, sep = ", ")
    updateSelectInput(session, "checkout_patron", choices = patrons$Full_Name)
  })
  
  # Books table (availability)
  
  books_with_status <- reactive({
    req(rv$books, rv$transactions)
    active <- rv$transactions %>% filter(is.na(Return_Date))
    active_book_ids <- active$Book_id
    
    rv$books %>%
      mutate(
        ActiveByLog = ifelse(Book_id %in% active_book_ids, "Yes", "No"),
        Effective_Available = ifelse(
          Available == "Yes" & ActiveByLog == "No",
          "Yes",
          "No"
        )
      )
  })
  
  output$books_table <- renderDT({
    df <- books_with_status() %>%
      filter(Effective_Available == "Yes") %>%
      select(Book_id, 
             Title, 
             Author_Names, 
             Bookshelves, 
             Subjects
      )
    datatable(df, selection = "single", options = list(pageLength = 10))
  })
  
  # Active checkouts table
  
  active_transactions_df <- reactive({
    req(rv$transactions)
    rv$transactions %>% filter(is.na(Return_Date))
  })
  
  output$active_transactions <- renderDT({
    datatable(active_transactions_df(), selection = "single", options = list(pageLength = 10))
  })
  
  # Checkout logic
  
  observeEvent(input$checkout_btn, {
    req(rv$patrons, rv$books, rv$transactions)
    
    sel <- input$books_table_rows_selected
    if (length(sel) != 1) {
      shinyalert("No Book Selected", "Please select a book to check out.", type = "warning")
      return()
    }
    
    available_df <- books_with_status() %>%
      filter(Effective_Available == "Yes") %>%
      select(Book_id, 
             Title, 
             Author_Names, 
             Bookshelves, 
             Subjects
      )
    book_row <- available_df[sel, ]
    book_id <- book_row$Book_id
    
    book_record <- rv$books %>% filter(Book_id == book_id)
    if (nrow(book_record) != 1) {
      shinyalert("Error", "Book record not found.", type = "error")
      return()
    }
    
    if (book_record$Available != "Yes") {
      shinyalert("Unavailable", "This book is marked as unavailable.", type = "error")
      return()
    }
    
    if (book_id %in% (rv$transactions %>% filter(is.na(Return_Date)) %>% pull(Book_id))) {
      shinyalert("Unavailable", "This book is already checked out.", type = "error")
      return()
    }
    
    patron_name <- input$checkout_patron
    if (is.null(patron_name) || patron_name == "") {
      shinyalert("No Patron Selected", "Please select a patron.", type = "warning")
      return()
    }
    
    parts <- strsplit(patron_name, ",\\s*")[[1]]
    last_name <- parts[1]
    first_name <- ifelse(length(parts) > 1, parts[2], "")
    
    patron_row <- rv$patrons %>% filter(Last_Name == last_name, First_Name == first_name)
    
    if (nrow(patron_row) != 1) {
      shinyalert("Patron Not Found", "Could not find the selected patron.", type = "error")
      return()
    }
    
    patron_id <- patron_row$Patron_id
    
    checkout_date <- Sys.Date()
    due_date <- checkout_date + 14
    
    dbExecute(
      db,
      "INSERT INTO 
       transaction_log (
         Patron_id, 
         Checkout_Date, 
         Return_Date, 
         Due_Date, 
         book_id
       )
     VALUES 
       (?, ?, ?, ?, ?)",
      params = list(
        patron_id,
        as.character(checkout_date),
        NA,
        as.character(due_date),
        book_id
      )
    )
    
    dbExecute(
      db,
      "UPDATE 
       books
     SET 
       Available = 'No', 
       Checked_Out_By = ?
     WHERE 
       Book_id = ?",
      params = list(patron_id, book_id)
    )
    
    load_data()
    
    shinyalert(
      "Checkout Recorded",
      paste0("Book '", book_row$Title, "' has been checked out to ", patron_name, "."),
      type = "success"
    )
  })
  
  # Book Return tab logic
  #
  # Patrons with active checkouts only
  
  observe({
    req(rv$transactions, rv$patrons)
    
    active_patrons <- rv$transactions %>%
      filter(is.na(Return_Date)) %>%
      distinct(Patron_id) %>%
      inner_join(rv$patrons, by = "Patron_id") %>%
      mutate(Full_Name = paste(Last_Name, First_Name, sep = ", "))
    
    updateSelectInput(
      session,
      "return_filter_patron",
      choices = c("All", active_patrons$Full_Name),
      selected = "All"
    )
  })
  
  # Books currently checked out
  
  checked_out_books <- reactive({
    req(rv$books, rv$patrons)
    
    df <- rv$books %>%
      filter(Available == "No") %>%
      left_join(
        rv$patrons %>% mutate(Patron_Name = paste(First_Name, Last_Name)),
        by = c("Checked_Out_By" = "Patron_id")
      ) %>%
      left_join(
        rv$transactions %>% filter(is.na(Return_Date)) %>% select(Book_id, Due_Date),
        by = "Book_id"
      )
    
    if (!is.null(input$return_filter_patron) && input$return_filter_patron != "All") {
      df <- df %>% filter(Patron_Name == input$return_filter_patron)
    }
    
    df
  })
  
  output$books_return_table <- renderDT({
    df <- checked_out_books() %>%
      select(
        Book_id, 
        Title, 
        Author_Names, 
        Bookshelves, 
        Subjects, 
        Patron_Name, 
        Due_Date
      )
    
    datatable(df, selection = "single", options = list(pageLength = 10))
  })
  
  # Return button logic (Book Return tab)
  
  observeEvent(input$return_btn2, {
    req(rv$transactions, rv$books)
    
    sel <- input$books_return_table_rows_selected
    if (length(sel) != 1) {
      shinyalert("No Book Selected", "Please select a book to mark as returned.", type = "warning")
      return()
    }
    
    df <- checked_out_books()
    
    book_row <- df[sel, ]
    
    book_id <- book_row$Book_id
    
    active_trans <- rv$transactions %>%
      filter(Book_id == book_id, is.na(Return_Date))
    
    if (nrow(active_trans) != 1) {
      shinyalert("Error", "Active checkout record not found.", type = "error")
      return()
    }
    
    trans_id <- active_trans$Transaction_id
    
    dbExecute(
      db,
      "UPDATE 
       transaction_log
     SET 
       Return_Date = ?
     WHERE 
       Transaction_id = ?",
      params = list(as.character(Sys.Date()), trans_id)
    )
    
    dbExecute(
      db,
      "UPDATE 
       books
     SET 
       Available = 'Yes', 
       Checked_Out_By = NULL
     WHERE 
       Book_id = ?",
      params = list(book_id)
    )
    
    load_data()
    
    shinyalert("Return Recorded", "The book has been marked as returned.", type = "success")
  })
  
  # Patrons table + CRUD
  
  output$patron_table <- renderDT({
    req(rv$patrons)
    df <- rv$patrons %>% mutate(Full_Name = paste(Last_Name, First_Name, sep = ", "))
    datatable(df, selection = "single", options = list(pageLength = 10))
  })
  
  # Add Patron
  
  observeEvent(input$add_patron, {
    showModal(modalDialog(
      title = "Add Patron",
      textInput("new_patron_first", "First Name"),
      textInput("new_patron_last", "Last Name"),
      textInput("new_patron_phone", "Phone Number"),
      textInput("new_patron_email", "Email Address"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          inputId = "save_new_patron", 
          label = "Save", 
          class = "btn-success"
        )
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$save_new_patron, {
    dbExecute(
      db,
      "INSERT INTO 
       patron (
         First_Name, 
         Last_Name, 
         Phone_Number, 
         Email_Address
       )
     VALUES 
       (?, ?, ?, ?)",
      params = list(
        input$new_patron_first,
        input$new_patron_last,
        input$new_patron_phone,
        input$new_patron_email
      )
    )
    removeModal()
    load_data()
  })
  
  # Edit Patron
  
  observeEvent(input$edit_patron, {
    req(rv$patrons)
    sel <- input$patron_table_rows_selected
    if (length(sel) != 1) {
      shinyalert("No Patron Selected", "Please select a patron to edit.", type = "warning")
      return()
    }
    
    patron_row <- rv$patrons[sel, ]
    
    showModal(modalDialog(
      title = "Edit Patron",
      textInput("edit_patron_first", "First Name", value = patron_row$First_Name),
      textInput("edit_patron_last", "Last Name", value = patron_row$Last_Name),
      textInput("edit_patron_phone", "Phone Number", value = patron_row$Phone_Number),
      textInput("edit_patron_email", "Email Address", value = patron_row$Email_Address),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          inputId = "save_edit_patron", 
          label = "Save Changes", 
          class = "btn-primary"
        )
      ),
      easyClose = TRUE
    ))
    
    observeEvent(input$save_edit_patron, {
      dbExecute(
        db,
        "UPDATE 
         patron
       SET 
         First_Name = ?, 
         Last_Name = ?, 
         Phone_Number = ?, 
         Email_Address = ?
       WHERE 
         Patron_id = ?",
        params = list(
          input$edit_patron_first,
          input$edit_patron_last,
          input$edit_patron_phone,
          input$edit_patron_email,
          patron_row$Patron_id
        )
      )
      removeModal()
      load_data()
    }, ignoreInit = TRUE, once = TRUE)
  })
  
  # Delete Patron
  
  observeEvent(input$delete_patron, {
    req(rv$patrons)
    sel <- input$patron_table_rows_selected
    if (length(sel) != 1) {
      shinyalert("No Patron Selected", "Please select a patron to delete.", type = "warning")
      return()
    }
    
    patron_row <- rv$patrons[sel, ]
    
    patron_id <- patron_row$Patron_id
    
    count_trans <- dbGetQuery(
      db,
      "SELECT 
       COUNT(*) AS n 
     FROM 
       transaction_log 
     WHERE 
       Patron_id = ?",
      params = list(patron_id)
    )$n
    
    if (count_trans > 0) {
      shinyalert(
        "Cannot Delete Patron",
        "This patron has transaction history and cannot be deleted.",
        type = "error"
      )
      return()
    }
    
    dbExecute(
      db,
      "DELETE FROM 
       patron 
     WHERE 
       Patron_id = ?",
      params = list(patron_id)
    )
    
    load_data()
    
    shinyalert("Patron Deleted", "The patron has been deleted.", type = "success")
  })
  
  # Books admin table + CRUD
  
  output$books_admin <- renderDT({
    req(rv$books, rv$patrons, rv$transactions)
    
    active_trans <- rv$transactions %>%
      filter(is.na(Return_Date)) %>%
      select(Book_id, Due_Date)
    
    df <- rv$books %>%
      left_join(
        rv$patrons %>% mutate(Patron_Name = paste(First_Name, Last_Name)),
        by = c("Checked_Out_By" = "Patron_id")
      ) %>%
      left_join(active_trans, by = "Book_id") %>%
      mutate(
        Checked_Out_By = ifelse(is.na(Patron_Name), "", Patron_Name),
        Due_Date = ifelse(is.na(Due_Date), "", as.character(Due_Date))
      ) %>%
      select(
        Book_id,
        Author_Names,
        Title,
        Bookshelves,
        Subjects,
        Available,
        Checked_Out_By,
        Due_Date
      )
    
    datatable(df, selection = "single", options = list(pageLength = 10))
  })
  
  # Add Book
  
  observeEvent(input$add_book, {
    showModal(modalDialog(
      title = "Add Book",
      textInput("new_book_title", "Title"),
      textInput("new_book_authors", "Author Names"),
      textInput("new_book_shelves", "Bookshelves"),
      textInput("new_book_subjects", "Subjects"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          inputId = "save_new_book", 
          label = "Save", 
          class = "btn-success"
        )
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$save_new_book, {
    dbExecute(
      db,
      "INSERT INTO 
       books (
         Author_Names, 
         Title, 
         Bookshelves, 
         Subjects, 
         Available, 
         Checked_Out_By
     )
     VALUES 
       (?, ?, ?, ?, 'Yes', NULL)",
      params = list(
        input$new_book_authors,
        input$new_book_title,
        input$new_book_shelves,
        input$new_book_subjects
      )
    )
    removeModal()
    load_data()
  })
  
  # Edit Book
  
  observeEvent(input$edit_book, {
    req(rv$books)
    sel <- input$books_admin_rows_selected
    if (length(sel) != 1) {
      shinyalert("No Book Selected", "Please select a book to edit.", type = "warning")
      return()
    }
    
    book_row <- rv$books[sel, ]
    
    showModal(modalDialog(
      title = "Edit Book",
      textInput("edit_book_title", "Title", value = book_row$Title),
      textInput("edit_book_authors", "Author Names", value = book_row$Author_Names),
      textInput("edit_book_shelves", "Bookshelves", value = book_row$Bookshelves),
      textInput("edit_book_subjects", "Subjects", value = book_row$Subjects),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          inputId = "save_edit_book", 
          label = "Save Changes", 
          class = "btn-primary"
        )
      ),
      easyClose = TRUE
    ))
    
    observeEvent(input$save_edit_book, {
      dbExecute(
        db,
        "UPDATE 
         books
       SET 
         Author_Names = ?, 
         Title = ?, 
         Bookshelves = ?, 
         Subjects = ?
       WHERE 
         Book_id = ?",
        params = list(
          input$edit_book_authors,
          input$edit_book_title,
          input$edit_book_shelves,
          input$edit_book_subjects,
          book_row$Book_id
        )
      )
      removeModal()
      load_data()
    }, ignoreInit = TRUE, once = TRUE)
  })
  
  # Delete Book
  
  observeEvent(input$delete_book, {
    req(rv$books)
    sel <- input$books_admin_rows_selected
    if (length(sel) != 1) {
      shinyalert("No Book Selected", "Please select a book to delete.", type = "warning")
      return()
    }
    
    book_row <- rv$books[sel, ]
    
    book_id <- book_row$Book_id
    
    if (book_row$Available != "Yes" || !is.na(book_row$Checked_Out_By)) {
      shinyalert(
        "Cannot Delete Book",
        "This book is currently checked out and cannot be deleted.",
        type = "error"
      )
      return()
    }
    
    count_trans <- dbGetQuery(
      db,
      "SELECT 
       COUNT(*) AS n 
     FROM 
       transaction_log 
     WHERE 
       book_id = ?",
      params = list(book_id)
    )$n
    
    if (count_trans > 0) {
      shinyalert(
        "Cannot Delete Book",
        "This book has transaction history and cannot be deleted.",
        type = "error"
      )
      return()
    }
    
    dbExecute(
      db,
      "DELETE FROM 
       books 
     WHERE 
       Book_id = ?",
      params = list(book_id)
    )
    
    load_data()
    
    shinyalert("Book Deleted", "The book has been deleted.", type = "success")
  })
  
  # Transaction log table
  
  output$transaction_log_table <- renderDT({
    req(rv$transactions)
    datatable(rv$transactions, options = list(pageLength = 15))
  })
  
  # Disconnect DB when session ends
  
  session$onSessionEnded(function() {
    dbDisconnect(db)
  })
}

# Run the app

shinyApp(ui = ui, server = server)