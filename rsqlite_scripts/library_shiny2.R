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
library(readxl)

# Upload spreadsheets that will become tables of database.

books <- read_excel("gutenberg_data.xlsx")
patron <- read_excel("patron.xlsx")
transaction_log <- read_excel("transaction_log.xlsx")

# Fix dates
# Possibly review

transaction_log$Checkout_Date <- as.Date(transaction_log$Checkout_Date, format="%m/%d/%Y")
transaction_log$Return_Date <- as.Date(transaction_log$Return_Date, format="%m/%d/%Y")
transaction_log$Due_Date <- as.Date(transaction_log$Due_Date, format="%m/%d/%Y")

# Create database

db <- dbConnect(RSQLite::SQLite(), "library.db")

# Write the 3 tables to the database

dbWriteTable(db, "books", books, overwrite = TRUE)
dbWriteTable(db, "patron", patron, overwrite = TRUE)
dbWriteTable(db, "transaction_log", transaction_log, overwrite = TRUE)

# Pull Patron names formatted as "Last_Name, First_Name" to power dropdown.

patron_list <- dbGetQuery(
  db,
  "SELECT DISTINCT 
     Last_Name || ', ' || First_Name AS Full_Name 
   FROM 
     patron 
   ORDER BY 
     Last_Name"
)

# This is the default Patron for when the app opens.

default_patron <- if ("Greenwell, Clayton" %in% patron_list$Full_Name) {
  "Greenwell, Clayton"
} else {
  patron_list$Full_Name[[1]]
}

# Min and Max Checkout dates.
# Possibly delete?

date_bounds <- dbGetQuery(
  db,
  "SELECT 
     MIN(Checkout_Date) AS min_dt, 
     MAX(Checkout_Date) AS max_dt 
   FROM 
     transaction_log"
)

# Convert the database date strings into Date objects for Shiny inputs.
# Possibly review

min_date <- as.Date(date_bounds$min_dt[[1]])
max_date <- as.Date(date_bounds$max_dt[[1]])

# This is an important query that joins all tables in the database

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

# ----------------
# User interface
# ----------------
# The UI is a navbarPage with four tabs:
# 1. About
# 2. Book Checkout
# 3. Patrons
# 4. Transaction Log

ui <- tagList(
  useShinyalert(),
  navbarPage(
    title = "Community Library Catalog App",
    windowTitle = "Community Library Catalog Management Application",
    position = "fixed-top",
    collapsible = TRUE,
    theme = shinytheme("cosmo"),
    
    # 1. About tab: simple descriptive text explaining what the app contains.
    tabPanel(
      title = "About",
      fluidPage(
        br(),
        br(),
        h3("Community Library Catalog Management Application"),
        p("An app that helps librarians and patrons checkout books from the library."),
        tags$ul(
          tags$li("Book Checkout: A patron drop-down filter, a searchable book availability table, and a save-to-database action button."),
          tags$li("Patrons: A searchable table displaying all patrons and their corresponding information."),
          tags$li("Transaction Log: A searchable table displaying all book checkout transactions.")
        )
      )
    ),
    
    # 2. Books Checkout tab: A patron drop-down filter, a searchable book availability table, and a save-to-database action button.
    tabPanel(
      title = "Book Checkout",
      fluidPage(
        br(),
        br(),
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
    