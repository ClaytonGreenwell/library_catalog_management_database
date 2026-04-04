
library(DBI)
library(RSQLite)
library(shiny)
library(shinythemes)
library(shinyalert)
library(DT)
library(ggplot2)
library(plotly)
library(forecast)
library(dplyr)
library(scales)

if (!file.exists("pineapple.db")) {
  stop("Place pineapple.db in the same folder as app.R before running the app.")
}

# Connect to the database
# The public mini-apps mostly use pineapple.db, although DataTable/app.R uses apple.db.
db <- dbConnect(RSQLite::SQLite(), "pineapple.db")

prodtype <- dbGetQuery(
  db,
  "SELECT DISTINCT prod_type FROM prods_i ORDER BY prod_type"
)

if (nrow(prodtype) == 0) {
  stop("No product types were found in prods_i.")
}

default_product <- if ("Macbook" %in% prodtype$prod_type) {
  "Macbook"
} else {
  prodtype$prod_type[[1]]
}

date_bounds <- dbGetQuery(
  db,
  "SELECT MIN(ord_dt) AS min_dt, MAX(ord_dt) AS max_dt FROM orders"
)

min_date <- as.Date(date_bounds$min_dt[[1]])
max_date <- as.Date(date_bounds$max_dt[[1]])

fmt_num <- function(x, digits = 0) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return("0")
  }
  format(round(as.numeric(x), digits), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_money <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(dollar(0))
  }
  dollar(as.numeric(x), accuracy = 0.01)
}

make_kpi <- function(title, value_id) {
  wellPanel(
    style = "background-color: #696969; color: #ffffff;",
    h4(title),
    uiOutput(value_id)
  )
}

ui <- tagList(
  useShinyalert(),
  navbarPage(
    title = "PineApple BI Dashboard",
    windowTitle = "PineApple Business Intelligence",
    position = "fixed-top",
    collapsible = TRUE,
    theme = shinytheme("cosmo"),
    
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
            selectInput(
              inputId = "overview_prod_type",
              label = "Product Type:",
              choices = prodtype$prod_type,
              selected = default_product
            ),
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
            plotOutput("playChrt", width = "100%", height = "400px"),
            br(),
            DTOutput("transactions")
          )
        )
      )
    ),
    
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
        plotlyOutput("stateMap", width = "100%", height = "600px"),
        br(),
        h4("Top 10 stores by sales"),
        DTOutput("store")
      )
    ),
    
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

server <- function(input, output, session) {
  overview_params <- reactive({
    req(input$overview_prod_type, input$overview_date_range)
    list(
      input$overview_prod_type,
      format(as.Date(input$overview_date_range[1]), "%Y-%m-%d"),
      format(as.Date(input$overview_date_range[2]), "%Y-%m-%d")
    )
  })
  
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
  
  output$store <- renderDT({
    datatable(
      store_sales(),
      rownames = FALSE,
      options = list(pageLength = 10, dom = "tip", scrollX = TRUE)
    )
  })
  
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
  
  safe_store_table_name <- reactive({
    req(input$store_prod_type)
    paste0("top10_store_", gsub("[^A-Za-z0-9]+", "_", input$store_prod_type))
  })
  
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
  
  onStop(function() {
    dbDisconnect(db)
  })
}

shinyApp(ui, server)
