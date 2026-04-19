
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
  "leaflet"
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

# Connecting to the database
# The app expects pineapple.db to sit next to app.R when you run shiny::runApp().

db <- dbConnect(RSQLite::SQLite(), "pineapple.db")

# Now begins a section of setting up that isn't in the demo tutorial, but will 
# get the app running correctly. Large parts here can be ignored for our 
# learning purposes all the way until we get to navbar.

# prodtype, which are distinct values of all of the product types from the 
# table prods_i in the database. prodtype as a data frame 
# is a subset of the table prods_i. Later in Shiny, we will be selecting 
# a product type with prodtype$prod_type from a dropdown list to generate data 
# tables and plots. prod_type is the field name in the table prods_i storing 
# product types.

# Pull the list of product types from the product table.
# This powers the product dropdowns across tabs.

prodtype <- dbGetQuery(
  db,
  "SELECT DISTINCT prod_type FROM prods_i ORDER BY prod_type"
)

# Choose a default product shown when the app first loads.
# Prefer Macbook if it exists; otherwise use the first available product type.
default_product <- if ("Macbook" %in% prodtype$prod_type) {
  "Macbook"
} else {
  prodtype$prod_type[[1]]
}

# Read the minimum and maximum order dates from the orders table.
# These are used to initialize the Overview tab date picker.
date_bounds <- dbGetQuery(
  db,
  "SELECT MIN(ord_dt) AS min_dt, MAX(ord_dt) AS max_dt FROM orders"
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

# Format a value as currency.
# Returns $0.00-like output even when the query returns NULL or NA.
fmt_money <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(dollar(0))
  }
  dollar(as.numeric(x), accuracy = 0.01)
}

# Create a KPI card with a title and a UI Output placeholder.
# The actual value is supplied later in the server via renderUI().
make_kpi <- function(title, value_id) {
  wellPanel(
    style = "background-color: #696969; color: #ffffff;",
    h4(title),
    uiOutput(value_id)
  )
}

# -----------------------------
# User interface definition
# -----------------------------
# The UI is a navbarPage with four tabs:
# 1. About
# 2. Overview
# 3. Store Info
# 4. Sales Prediction

ui <- tagList(
  useShinyalert(),
  navbarPage(
    title = "PineApple BI Dashboard",
    windowTitle = "PineApple Business Intelligence",
    position = "fixed-top",
    collapsible = TRUE,
    theme = shinytheme("cosmo"),
    
    # 1. About tab: simple descriptive text explaining what the app contains.
    tabPanel(
      title = "About",
      fluidPage(
        br(),
        h3("PineApple Business Intelligence"),
        p("Reconstructed from the public NYU Shanghai tutorial and component demo apps."),
        p("This version merges the public pieces into one app.R file."),
        tags$ul(
          tags$li("Overview: KPI cards, filtered daily sales chart, and transaction detail table."),
          tags$li("Store Info: state sales map, top 10 stores table, and save-to-database action."),
          tags$li("Sales Prediction: 7-day forecast for the selected product type.")
        )
      )
    ),
    
    # 2. Overview tab: top-line KPIs, daily sales chart, and filtered transaction table.
    tabPanel(
      title = "Overview",
      fluidPage(
        br(),
        fluidRow(
          column(4, make_kpi("Number of Orders", "nOrders")),
          column(4, make_kpi("Sales Revenues", "totSales")),
          column(4, make_kpi("Average Order Value (AOV)", "aov"))
        ),
        hr(),
        sidebarLayout(
          sidebarPanel(
            # Product filter used by the KPI cards, bar chart, and transaction table.
            selectInput(
              inputId = "overview_prod_type",
              label = "Product Type:",
              choices = prodtype$prod_type,
              selected = default_product
            ),
            # Date filter used by the same Overview outputs.
            dateRangeInput(
              inputId = "overview_date_range",
              label = "Date Range:",
              start = min_date,
              end = max_date,
              min = min_date,
              max = max_date
            )
          ),
          mainPanel(
            # Daily sales bar chart.
            plotOutput("playChrt", width = "100%", height = "400px"),
            br(),
            # Detailed order-level table.
            DTOutput("transactions")
          )
        )
      )
    ),
    
    # 3. Store Info tab: map + top 10 stores table + button to persist the top 10 result.
    tabPanel(
      title = "Store Info",
      fluidPage(
        br(),
        fluidRow(
          column(
            6,
            selectInput(
              inputId = "store_prod_type",
              label = "Product Type:",
              choices = prodtype$prod_type,
              selected = default_product
            )
          ),
          column(
            6,
            div(
              style = "margin-top: 25px; text-align: right;",
              actionButton("save", "Save top 10 table")
            )
          )
        ),
        br(),
        # Interactive US state sales map.
        plotlyOutput("stateMap", width = "100%", height = "600px"),
        br(),
        h4("Top 10 stores by sales"),
        # Table of top stores for the selected product.
        DTOutput("store")
      )
    ),
    
    # 4. Sales Prediction tab: forecast the next 7 days for the selected product type.
    tabPanel(
      title = "Sales Prediction",
      fluidPage(
        br(),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "pred_prod_type",
              label = "Product Type:",
              choices = prodtype$prod_type,
              selected = default_product
            )
          ),
          mainPanel(
            plotOutput("prediction", width = "100%", height = "500px")
          )
        )
      )
    )
  )
)

# -----------------------------
# Server logic
# -----------------------------

server <- function(input, output, session) {
  
  # Collect and standardize the Overview tab filter values in one place.
  # This reactive returns a list in the exact order needed by the SQL queries:
  #   1) selected product type
  #   2) selected start date
  #   3) selected end date
  
  overview_params <- reactive({
    req(input$overview_prod_type, input$overview_date_range)
    list(
      input$overview_prod_type,
      format(as.Date(input$overview_date_range[1]), "%Y-%m-%d"),
      format(as.Date(input$overview_date_range[2]), "%Y-%m-%d")
    )
  })

  # KPI 1: "Number of Orders"
  # This query sums qty for the filtered product/date range.
  # Note: this mirrors the public demo logic even though the label suggests order count.
  output$nOrders <- renderUI({
    value <- dbGetQuery(
      conn = db,
      statement = paste(
        "SELECT COALESCE(SUM(qty), 0) AS value",
        "FROM orders a",
        "LEFT JOIN order_items b ON a.ord_id = b.ord_id",
        "LEFT JOIN prods_i c ON b.prod_id = c.prod_id",
        "WHERE prod_type = ? AND ord_dt BETWEEN ? AND ?"
      ),
      params = overview_params()
    )$value[[1]]
    
    tags$h3(fmt_num(value))
  })
  
  # KPI 2: total revenue for the filtered product/date range.
  # Revenue is calculated as price * qty and summed across matching rows.
  output$totSales <- renderUI({
    value <- dbGetQuery(
      conn = db,
      statement = paste(
        "SELECT COALESCE(SUM(price * qty), 0) AS value",
        "FROM orders a",
        "LEFT JOIN order_items b ON a.ord_id = b.ord_id",
        "LEFT JOIN prods_i c ON b.prod_id = c.prod_id",
        "WHERE prod_type = ? AND ord_dt BETWEEN ? AND ?"
      ),
      params = overview_params()
    )$value[[1]]
    
    tags$h3(fmt_money(value))
  })
  
  # KPI 3: average order value-style metric for the filtered selection.
  # This follows the public demo calculation: AVG(qty * price).
  output$aov <- renderUI({
    value <- dbGetQuery(
      conn = db,
      statement = paste(
        "SELECT COALESCE(ROUND(AVG(qty * price), 2), 0) AS value",
        "FROM orders a",
        "LEFT JOIN order_items b ON a.ord_id = b.ord_id",
        "LEFT JOIN prods_i c ON b.prod_id = c.prod_id",
        "WHERE prod_type = ? AND ord_dt BETWEEN ? AND ?"
      ),
      params = overview_params()
    )$value[[1]]
    
    tags$h3(fmt_money(value))
  })
  
  # Overview detail table:
  # Show transactions matching the selected product and date range.
  # The joins bring in product and store metadata so the table is more informative.
  output$transactions <- renderDT({
    data <- dbGetQuery(
      conn = db,
      statement = paste(
        "SELECT a.ord_id, ord_dt, cust_id, prod_type, prod_grp, qty,",
        "store_name, city, state",
        "FROM orders a",
        "LEFT JOIN order_items b ON a.ord_id = b.ord_id",
        "LEFT JOIN prods_i c ON b.prod_id = c.prod_id",
        "LEFT JOIN stores d ON a.store_id = d.store_id",
        "WHERE prod_type = ? AND ord_dt BETWEEN ? AND ?",
        "ORDER BY ord_dt DESC"
      ),
      params = overview_params()
    )
    
    datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # Overview bar chart:
  # Aggregate sales by day for the selected product/date range,
  # then draw a column chart of daily sales revenue.
  output$playChrt <- renderPlot({
    d <- dbGetQuery(
      conn = db,
      statement = paste(
        "SELECT ord_dt AS day, SUM(qty) AS qty, SUM(price * qty) AS sales",
        "FROM orders a",
        "LEFT JOIN order_items b ON a.ord_id = b.ord_id",
        "LEFT JOIN prods_i c ON b.prod_id = c.prod_id",
        "WHERE prod_type = ? AND ord_dt BETWEEN ? AND ?",
        "GROUP BY day",
        "ORDER BY day ASC"
      ),
      params = overview_params()
    )
    
    ggplot(data = d, aes(x = as.Date(day), y = sales, group = 1)) +
      geom_col() +
      labs(x = "Day", y = "Sales Revenues ($)") +
      theme_minimal(base_size = 16) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      theme(legend.position = "none")
  })
  
  # Reactive data source for the Store Info table.
  # This computes the top 10 stores by sales for the selected product type.
  # Store ID '00000' is excluded to match the public example logic.
  store_sales <- reactive({
    req(input$store_prod_type)
    
    dbGetQuery(
      conn = db,
      statement = paste(
        "SELECT city, d.store_name, SUM(price * qty) AS sales",
        "FROM orders a",
        "LEFT JOIN order_items b ON a.ord_id = b.ord_id",
        "LEFT JOIN prods_i c ON b.prod_id = c.prod_id",
        "LEFT JOIN stores d ON a.store_id = d.store_id",
        "WHERE prod_type = ? AND a.store_id != '00000'",
        "GROUP BY 1, 2",
        "ORDER BY sales DESC",
        "LIMIT 10"
      ),
      params = list(input$store_prod_type)
    )
  })
  
  # Render the top 10 stores table using the reactive query above.
  output$store <- renderDT({
    datatable(
      store_sales(),
      rownames = FALSE,
      options = list(pageLength = 10, dom = "tip", scrollX = TRUE)
    )
  })
  
  # Render the interactive US state map.
  # This query aggregates total sales by state for the selected product type,
  # and plotly shades each state according to its sales value.
  output$stateMap <- renderPlotly({
    d2 <- dbGetQuery(
      conn = db,
      statement = paste(
        "SELECT state, SUM(price * qty) AS sales",
        "FROM orders a",
        "LEFT JOIN order_items b ON a.ord_id = b.ord_id",
        "LEFT JOIN prods_i c ON b.prod_id = c.prod_id",
        "LEFT JOIN stores d ON a.store_id = d.store_id",
        "WHERE prod_type = ? AND a.store_id != '00000'",
        "GROUP BY 1",
        "ORDER BY sales DESC"
      ),
      params = list(input$store_prod_type)
    )
    
    plot_geo(d2, locationmode = "USA-states", sizes = c(1, 1000)) %>%
      add_trace(
        z = ~sales,
        locations = ~state,
        color = ~sales,
        colors = "Purples"
      ) %>%
      colorbar(title = "$ USD") %>%
      layout(
        title = "Total Sales Revenues ($): State-Level",
        geo = list(
          scope = "usa",
          projection = list(type = "albers usa"),
          showlakes = TRUE,
          lakecolor = toRGB("white")
        )
      )
  })
  
  # Build a safe SQL table name for the saved top-10-stores table.
  # Any non-alphanumeric characters in the product name are replaced with underscores.
  safe_store_table_name <- reactive({
    req(input$store_prod_type)
    paste0("top10_store_", gsub("[^A-Za-z0-9]+", "_", input$store_prod_type))
  })
  
  # Save button behavior:
  # 1) drop an existing table with the same name if it already exists
  # 2) create a new table containing the top 10 stores for the selected product
  # 3) show a success popup to the user
  observeEvent(input$save, {
    table_name <- as.character(dbQuoteIdentifier(db, safe_store_table_name()))
    
    dbExecute(db, paste("DROP TABLE IF EXISTS", table_name))
    
    dbExecute(
      conn = db,
      statement = paste(
        "CREATE TABLE", table_name, "AS",
        "SELECT city, d.store_name, SUM(price * qty) AS sales",
        "FROM orders a",
        "LEFT JOIN order_items b ON a.ord_id = b.ord_id",
        "LEFT JOIN prods_i c ON b.prod_id = c.prod_id",
        "LEFT JOIN stores d ON a.store_id = d.store_id",
        "WHERE prod_type = ? AND a.store_id != '00000'",
        "GROUP BY 1, 2",
        "ORDER BY sales DESC",
        "LIMIT 10"
      ),
      params = list(input$store_prod_type)
    )
    
    shinyalert(
      title = "OK!",
      text = paste("Successfully saved to the database as", safe_store_table_name()),
      type = "success"
    )
  })
  
  # Sales Prediction plot:
  # Query daily sales for the selected product type, draw the historical line,
  # and then extend it with a 7-day forecast using geom_forecast().
  output$prediction <- renderPlot({
    req(input$pred_prod_type)
    
    dy <- dbGetQuery(
      conn = db,
      statement = paste(
        "SELECT ord_dt AS day, SUM(price * qty) AS sales",
        "FROM orders a",
        "LEFT JOIN order_items b ON a.ord_id = b.ord_id",
        "LEFT JOIN prods_i c ON b.prod_id = c.prod_id",
        "WHERE prod_type = ?",
        "GROUP BY day",
        "ORDER BY day DESC"
      ),
      params = list(input$pred_prod_type)
    )
    
    ggplot(dy, aes(x = as.Date(day), y = sales, group = 1)) +
      geom_line(color = "red", size = 1) +
      labs(x = "Day", y = "Sales") +
      theme_minimal(base_size = 16) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      theme(legend.position = "none") +
      geom_forecast(h = 7)
  })
  
  # When the app stops, close the database connection cleanly.
  onStop(function() {
    dbDisconnect(db)
  })
}

# Launch the Shiny application by pairing the UI and server objects.
shinyApp(ui, server)