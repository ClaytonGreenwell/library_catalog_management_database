
# Load packages

install.packages(c("DBI", "RSQLite", "tidyverse", "readxl"))

library(DBI)
library(RSQLite)
library(tidyverse)
library(readxl)

# Upload data that will become datas of spreadsheet

books <- read_excel("gutenberg_data.xlsx")
patron    <- read_excel("patron.xlsx")
transaction_log  <- read_excel("transaction_log.xlsx")

# Fix dates

transaction_log$Checkout_Date <- as.character(transaction_log$Checkout_Date)
transaction_log$Return_Date   <- as.character(transaction_log$Return_Date)
transaction_log$Due_Date      <- as.character(transaction_log$Due_Date)

# Create database

comm_library <- dbConnect(RSQLite::SQLite(), "comm_library.db")

# Write the 3 tables to the database

dbWriteTable(comm_library, "books", books, overwrite = TRUE)
dbWriteTable(comm_library, "patron", patron, overwrite = TRUE)
dbWriteTable(comm_library, "transaction_log", transaction_log, overwrite = TRUE)

# Join the tables

join <- dbGetQuery(comm_library, "
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
  FROM transaction_log t
  LEFT JOIN patron p
    ON t.Patron_id = p.Patron_id
  LEFT JOIN books b
    ON t.book_id = b.[Etext Number]
")

View(join)

# Close database connection

dbDisconnect(comm_library)
