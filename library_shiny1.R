# Loading the packages

install.packages(c(
  "DBI",
  "RSQLite",
  "shiny",
  "shinythemes",
  "shinyWidgets",
  "shinyalert",
  "DT",
  "forecast",
  "ggplot2",
  "plotly",
  "maps",
  "leaflet",
  "readxl"
))

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

# Upload data that will become datas of spreadsheet

books <- read_excel("gutenberg_data.xlsx")
patron <- read_excel("patron.xlsx")
transaction_log <- read_excel("transaction_log.xlsx")

# Fix dates

transaction_log$Checkout_Date <- as.Date(transaction_log$Checkout_Date, format="%m/%d/%Y")
transaction_log$Return_Date <- as.Date(transaction_log$Return_Date, format="%m/%d/%Y")
transaction_log$Due_Date <- as.Date(transaction_log$Due_Date, format="%m/%d/%Y")

# Create database

db <- dbConnect(RSQLite::SQLite(), "library.db")

# Write the 3 tables to the database

dbWriteTable(db, "books", books, overwrite = TRUE)
dbWriteTable(db, "patron", patron, overwrite = TRUE)
dbWriteTable(db, "transaction_log", transaction_log, overwrite = TRUE)

# Pull Patron names formatted as "Last_Name, First_Name" to power dropdowns.

patron_list <- dbGetQuery(
  db,
  "SELECT DISTINCT Last_Name || ', ' || First_Name AS Full_Name 
   FROM patron 
   ORDER BY Last_Name"
)

# This is the default Patron for when the app opens.

default_patron <- if ("Greenwell, Clayton" %in% patron_list$Full_Name) {
  "Greenwell, Clayton"
} else {
  patron_list$Full_Name[[1]]
}

# Min and Max Checkout dates.
# These will be used to initialize the Book Checkout tab date picker.

date_bounds <- dbGetQuery(
  db,
  "SELECT MIN(Checkout_Date) AS min_dt, MAX(Checkout_Date) AS max_dt FROM transaction_log"
)

# Convert the database date strings into Date objects for Shiny inputs.

min_date <- as.Date(date_bounds$min_dt[[1]])
max_date <- as.Date(date_bounds$max_dt[[1]])

# Format a number with commas and a configurable rounding level.
# Returns "0" for NULL, empty, or missing values so the UI stays clean.

fmt_num <- function(x, digits = 0) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("0")
  }
  format(round(as.numeric(x), digits), big.mark = ",", scientific = FALSE, trim = TRUE)
}

# This is an important query that joins all tables in the database.

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
  LEFT JOIN patron p
    ON t.Patron_id = p.Patron_id
  LEFT JOIN books b
    ON t.book_id = b.[Etext Number]
")