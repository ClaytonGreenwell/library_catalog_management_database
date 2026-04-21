# ============================================================
# Community Library Catalog App - Option B (Full CRUD, SQLite)
# ============================================================

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(DT)
library(DBI)
library(RSQLite)
library(dplyr)

# ------------------------------------------------------------
# Database connection
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------

ui <- tagList(
  useShinyalert(force = TRUE),
  navbarPage(
    title = "Community Library Catalog App",
    windowTitle = "Community Library Catalog Management Application",
    position = "fixed-top",
    collapsible = TRUE,
    theme = shinytheme("cosmo"),
    
    # About tab
    tabPanel(
      "About",
      br(), br(),
      h3("Community Library Catalog Management Application"),
      p("A full CRUD library system backed by a SQLite database."),
      tags$ul(
        tags$li("Book Checkout: Select a patron, choose an available book, and record a checkout."),
        tags$li("Patrons: Add, edit, and delete patrons (when they have no transaction history)."),
        tags$li("Books: Add, edit, and delete books (when not referenced or checked out)."),
        tags$li("Transaction Log: View all checkout and return activity.")
      )
    ),
    
    # Book Checkout tab
    tabPanel(
      "Book Checkout",
      br(), br(),
      sidebarLayout(
        sidebarPanel(
          selectInput("checkout_patron", "Select Patron:", choices = NULL),
          actionButton("checkout_btn", "Finalize Checkout", class = "btn-primary", width = "100%"),
          br(), br(),
          actionButton("return_btn", "Mark Selected Checkout as Returned", class = "btn-warning", width = "100%")
        ),
        mainPanel(
          h4("Available Books"),
          DTOutput("books_table"),
          br(),
          h4("Active Checkouts"),
          DTOutput("active_transactions")
        )
      )
    ),
    
    # Patrons tab
    tabPanel(
      "Patrons",
      br(), br(),
      fluidPage(
        fluidRow(
          column(
            12,
            actionButton("add_patron", "Add Patron", class = "btn-success"),
            actionButton("edit_patron", "Edit Selected Patron", class = "btn-primary"),
            actionButton("delete_patron", "Delete Selected Patron", class = "btn-danger")
          )
        ),
        br(),
        DTOutput("patron_table")
      )
    ),
    
    # Books tab
    tabPanel(
      "Books",
      br(), br(),
      fluidPage(
        fluidRow(
          column(
            12,
            actionButton("add_book", "Add Book", class = "btn-success"),
            actionButton("edit_book", "Edit Selected Book", class = "btn-primary"),
            actionButton("delete_book", "Delete Selected Book", class = "btn-danger")
          )
        ),
        br(),
        DTOutput("books_admin")
      )
    ),
    
    # Transaction Log tab
    tabPanel(
      "Transaction Log",
      br(), br(),
      DTOutput("transaction_log_table")
    )
  )
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

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
      "SELECT Patron_id, First_Name, Last_Name, Phone_Number, Email_Address
       FROM patron
       ORDER BY Last_Name, First_Name"
    )
    
    rv$books <- dbGetQuery(
      db,
      "SELECT Book_id, Author_Names, Title, Bookshelves, Subjects, Available, Checked_Out_By
       FROM books
       ORDER BY Title"
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
         FROM transaction_log t
         LEFT JOIN patron p ON t.Patron_id = p.Patron_id
         LEFT JOIN books b ON t.book_id = b.Book_id
         ORDER BY t.Transaction_id DESC"
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
  
  # -----------------------------
  # Books table (availability)
  # -----------------------------
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
      select(Book_id, Title, Author_Names, Bookshelves, Subjects)
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
  
  # -----------------------------
  # Checkout logic
  # -----------------------------
  observeEvent(input$checkout_btn, {
    req(rv$patrons, rv$books, rv$transactions)
    
    sel <- input$books_table_rows_selected
    if (length(sel) != 1) {
      shinyalert("No Book Selected", "Please select a book to check out.", type = "warning")
      return()
    }
    
    available_df <- books_with_status() %>%
      filter(Effective_Available == "Yes") %>%
      select(Book_id, Title, Author_Names, Bookshelves, Subjects)
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
      "INSERT INTO transaction_log (Patron_id, Checkout_Date, Return_Date, Due_Date, book_id)
       VALUES (?, ?, ?, ?, ?)",
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
      "UPDATE books
       SET Available = 'No', Checked_Out_By = ?
       WHERE Book_id = ?",
      params = list(patron_id, book_id)
    )
    
    load_data()
    
    shinyalert(
      "Checkout Recorded",
      paste0("Book '", book_row$Title, "' has been checked out to ", patron_name, "."),
      type = "success"
    )
  })
  
  # -----------------------------
  # Return logic
  # -----------------------------
  observeEvent(input$return_btn, {
    req(rv$transactions, rv$books)
    
    sel <- input$active_transactions_rows_selected
    if (length(sel) != 1) {
      shinyalert("No Checkout Selected", "Please select an active checkout to mark as returned.", type = "warning")
      return()
    }
    
    active_df <- active_transactions_df()
    trans_row <- active_df[sel, ]
    trans_id <- trans_row$Transaction_id
    book_id <- trans_row$Book_id
    
    dbExecute(
      db,
      "UPDATE transaction_log
       SET Return_Date = ?
       WHERE Transaction_id = ?",
      params = list(as.character(Sys.Date()), trans_id)
    )
    
    dbExecute(
      db,
      "UPDATE books
       SET Available = 'Yes', Checked_Out_By = NULL
       WHERE Book_id = ?",
      params = list(book_id)
    )
    
    load_data()
    
    shinyalert("Return Recorded", "The book has been marked as returned.", type = "success")
  })
  
  # -----------------------------
  # Patrons table + CRUD
  # -----------------------------
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
        actionButton("save_new_patron", "Save", class = "btn-success")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$save_new_patron, {
    dbExecute(
      db,
      "INSERT INTO patron (First_Name, Last_Name, Phone_Number, Email_Address)
       VALUES (?, ?, ?, ?)",
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
        actionButton("save_edit_patron", "Save Changes", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
    
    observeEvent(input$save_edit_patron, {
      dbExecute(
        db,
        "UPDATE patron
         SET First_Name = ?, Last_Name = ?, Phone_Number = ?, Email_Address = ?
         WHERE Patron_id = ?",
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
      "SELECT COUNT(*) AS n FROM transaction_log WHERE Patron_id = ?",
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
      "DELETE FROM patron WHERE Patron_id = ?",
      params = list(patron_id)
    )
    
    load_data()
    
    shinyalert("Patron Deleted", "The patron has been deleted.", type = "success")
  })
  
  # -----------------------------
  # Books admin table + CRUD
  # -----------------------------
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
        actionButton("save_new_book", "Save", class = "btn-success")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$save_new_book, {
    dbExecute(
      db,
      "INSERT INTO books (Author_Names, Title, Bookshelves, Subjects, Available, Checked_Out_By)
       VALUES (?, ?, ?, ?, 'Yes', NULL)",
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
        actionButton("save_edit_book", "Save Changes", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
    
    observeEvent(input$save_edit_book, {
      dbExecute(
        db,
        "UPDATE books
         SET Author_Names = ?, Title = ?, Bookshelves = ?, Subjects = ?
         WHERE Book_id = ?",
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
      "SELECT COUNT(*) AS n FROM transaction_log WHERE book_id = ?",
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
      "DELETE FROM books WHERE Book_id = ?",
      params = list(book_id)
    )
    
    load_data()
    
    shinyalert("Book Deleted", "The book has been deleted.", type = "success")
  })
  
  # -----------------------------
  # Transaction log table
  # -----------------------------
  output$transaction_log_table <- renderDT({
    req(rv$transactions)
    datatable(rv$transactions, options = list(pageLength = 15))
  })
  
  # Disconnect DB when session ends
  session$onSessionEnded(function() {
    dbDisconnect(db)
  })
}

# ------------------------------------------------------------
# Run the app
# ------------------------------------------------------------

shinyApp(ui = ui, server = server)