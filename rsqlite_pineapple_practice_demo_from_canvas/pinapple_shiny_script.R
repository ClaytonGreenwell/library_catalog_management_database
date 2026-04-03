
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

db <- dbConnect(SQLite(), 'pineapple.db')

prodtype <- dbGetQuery(db, 'SELECT distinct prod_type from prods_i')
prodtype

prodtype$prod_type

# Navigation bar

ui <- navbarPage(
  title = 'Demo for Navigation Bar',
  windowTitle = 'Navigation Bar', 
  position = 'fixed-top', 
  collapsible = TRUE, 
  theme = shinytheme('cosmo'), 
  tabPanel(title = 'About'),
  tabPanel(title = 'Overview'),
  tabPanel(title = 'Store Info'),
  tabPanel(title = 'Sales Prediction')
)

server <- function(input, output, session)
  
# Select input
  
ui <- fluidPage(
  titlePanel('Demo for Select Input'),
  inputPanel(
    selectInput(
      inputId = 'prod_type_id',
      label = 'Product Type:',
      choices = prodtype$prod_type,
      selected = 'Macbook'
    )
  )
)

server <- function(input, output, session)
  
# Date Range
  
ui <- fluidPage(
  titlePanel('Demo for Date Range'),
  inputPanel(
    dateRangeInput(
      inputId = "date_range_id",
      label = "Date Range:",
      start = "2020-01-01",
      end = "2020-01-31"
    )
  )
)

server <- function(input, output, session)
  
# Well panel
  
ui <- fluidPage(
  titlePanel('Demo for Well Panel'),
  fluidRow(
    align = 'center',
    ### well panel 1
    column(
      width = 4,
      wellPanel(
        style = 'background-color: #696969; color: #ffffff;',
        h4('Number of Orders'),
        htmlOutput(
          outputId = 'nOrders'
        )
      )
    ),
    ### well panel 2
    column(
      width = 4,
      wellPanel(
        style = 'background-color: #696969; color: #ffffff; bold = TRUE',
        h4('Sales Revenues'),
        htmlOutput(
          outputId = 'totSales'
        )
      )
    ),
    ### well panel 3
    column(
      width = 4,
      wellPanel(
        style = 'background-color: #696969; color: #ffffff;',
        h4('Average Order Value (AOV)'),
        htmlOutput(
          outputId = 'aov'
        )
      )
    )
  ),
  hr(),
  sidebarLayout(
    sidebarPanel(
      # style = 'background-color: #ffa700; color: #ffffff;',
      selectInput(
        inputId = 'prod_type',
        label = 'Product Type:',
        choices = prodtype$prod_type,
        selected = 'Macbook'),
    ),
    mainPanel(
    )
  )
)

# Rendering Texts

server <- function(input, output, session){
  # render the summary metrics shown at the top 
  output$nOrders <- renderText(
    paste0('<h4>', 
           dbGetQuery(
             conn = db, 
             statement = 
               'SELECT sum(qty) 
                   FROM order_items a
                  INNER JOIN  
                   (SELECT * FROM prods_i WHERE prod_type = ?) b 
                  ON a.prod_id = b.prod_id',
             params = input$prod_type
           ), 
           '</h4>'))
  
  output$totSales <- renderText(
    paste0('<h4>', 
           dbGetQuery(
             conn = db, 
             statement = 
               'SELECT sum(price*qty) 
                  FROM order_items a
                INNER JOIN 
                 (SELECT * from prods_i WHERE prod_type = ?) b 
                ON a.prod_id = b.prod_id',
             params = input$prod_type
           ), 
           '</h4>'))
  
  output$aov <- renderText(
    paste0('<h4>', 
           dbGetQuery(
             conn = db, 
             statement = 
               'SELECT round(avg(qty*price),2)
                   FROM order_items a
                INNER JOIN 
                 (SELECT * from prods_i WHERE prod_type = ?) b 
                ON a.prod_id = b.prod_id',
             params = input$prod_type
           ), 
           '</h4>')
  )
}

# Data Table

ui <- fluidPage(
  titlePanel('Demo for Data Table'),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = 'prod_type_id',
        label = 'Product Type:',
        choices = prodtype$prod_type,
        selected = 'Macbook'
      ),
      br(),
    ),
    mainPanel(
      dataTableOutput(
        outputId = 'transactions'
      )
    )
  )
)

# Rendering a data table

server <- function(input, output, session) {
  output$transactions <- renderDataTable(
    data <- dbGetQuery(
      conn = db,
      statement = 
        'SELECT a.ord_id,
                  ord_dt,
                  cust_id,
                  prod_type,
                  prod_grp,
                  qty,
                  store_name,
                  city,
                  state
            FROM orders a
              LEFT JOIN order_items b on a.ord_id = b.ord_id 
              LEFT JOIN prods_i c on b.prod_id = c.prod_id
              LEFT JOIN stores d on a.store_id = d.store_id
            WHERE prod_type = ?
            ORDER BY ord_dt DESC', 
      params = input$prod_type
    )
  )
}

# Plot

ui <- fluidPage(
  titlePanel('Demo for Plot'),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = 'prod_type',
        label = 'Product Type:',
        choices = prodtype$prod_type,
        selected = 'Macbook'
      ),
      br(),
      dateRangeInput(
        inputId = "date_range",
        label = "Date Range:",
        start = "2020-01-01",
        end = "2020-01-31"
      )
    ),
    mainPanel(
      plotOutput(
        outputId = 'playChrt'
      )
    )
  )
)

# Rendering a plot

server <- function(input, output, session) {
  output$playChrt <- renderPlot(
    {
      d <- dbGetQuery(
        conn = db,
        statement = 
          'SELECT ord_dt as day,
                  sum(qty) as qty,
                  sum(price*qty) as sales
            FROM orders a
               LEFT JOIN order_items b on a.ord_id = b.ord_id
               LEFT JOIN prods_i c on b.prod_id = c.prod_id
            WHERE prod_type = ? and (day BETWEEN ? AND ?)
            GROUP BY day
            ORDER BY day DESC',
        params = list(input$prod_type,
                      format(input$date_range[1], format = "%Y-%m-%d"),
                      format(input$date_range[2], format = "%Y-%m-%d"))
      )
      ggplot(data = d, aes(x = as.Date(day), y = sales)) +
        geom_col(size = 1) +
        labs(x = 'Day', y = 'Sales Revenues ($)') + 
        theme_minimal(base_size = 16) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        theme(legend.position = 'none')
    }
  )
}

# Geographic map

ui <- fluidPage(
  titlePanel('Demo for Map'),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = 'prod_type_id',
        label = 'Product Type:',
        choices = prodtype$prod_type,
        selected = 'Macbook'
      ),
      br(),
    ),
    mainPanel(
      plotlyOutput(
        outputId = 'stateMap', width='100%', height='600px' )
    )
  )
)

# Rendering a plotly graph

server <- function(input, output, session) {
  output$stateMap <- renderPlotly(
    {
      d2 <- dbGetQuery(
        conn = db,
        statement = 
          'SELECT state,
                  sum(price*qty) as sales
             FROM orders a
                LEFT JOIN order_items b ON a.ord_id = b.ord_id
                LEFT JOIN prods_i c ON b.prod_id = c.prod_id
                LEFT JOIN stores d ON a.store_id = d.store_id
            WHERE prod_type = ?
              AND a.store_id != "00000"
            GROUP BY 1
            ORDER BY sales DESC',
        params = input$prod_type
      )
      plot_geo(d2, locationmode = 'USA-states',sizes = c(1, 1000)) %>%
        add_trace(z = ~sales, locations = ~state,
                  color = ~sales, colors = 'Purples') %>%
        colorbar(title = "$ USD") %>%
        layout(title = 'Total Sales Revenues ($): State-Level',
               geo = list(
                 scope = 'usa',
                 projection = list(type = 'albers usa'),
                 showlakes = TRUE,
                 lakecolor = toRGB('white')
               )
        )
    } 
  )
}

# Prediction plot

ui <- fluidPage(
  titlePanel('Demo for Prediction'),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = 'prod_type',
        label = 'Product Type:',
        choices = prodtype$prod_type,
        selected = 'Macbook'
      ),
      br(),
    ),
    mainPanel(
      plotOutput(
        outputId = 'prediction', width='100%', height='500px' )
    )
  )
)

server <- function(input, output, session) {
  output$prediction <- renderPlot(
    {
      dy <- dbGetQuery(
        conn = db,
        statement = 
          'SELECT ord_dt as day,
                  sum(price*qty) as sales
            from orders a
               left join order_items b on a.ord_id = b.ord_id
               left join prods_i c on b.prod_id = c.prod_id
            WHERE prod_type = ?
            GROUP BY day
            ORDER BY day DESC',
        params = input$prod_type
      )
      ggplot(dy, aes(x = as.Date(day), y = sales, group = 1)) +
        geom_line(color='red',size = 1) +
        labs(x = 'Day', y = 'Sales') +
        theme_minimal(base_size = 16) +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
        theme(legend.position = 'none')+
        geom_forecast(h=7)
    }
  )
}

# Action Button

ui <- fluidPage(
  titlePanel('Demo for Create Table'),
  column(6,
         selectInput(
           inputId = 'prod_type',
           label = 'Product Type:',
           choices = prodtype$prod_type,
           selected = 'Macbook'
         )
  ),
  column(6,
         useShinyalert(),  
         actionButton(inputId = "save", 
                      label = "Save"),
         align = 'right'
  ),
  hr(),
  dataTableOutput(outputId = 'store')
)

# Pop-up message

server <- function(input, output, session){
  output$store <- renderDataTable(
    data <- dbGetQuery(
      conn = db,
      statement = 
        'SELECT city, d.store_name,
                     sum(price*qty) as sales
               FROM orders a
                  left join order_items b on a.ord_id = b.ord_id
                  left join prods_i c on b.prod_id = c.prod_id
                  left join stores d on a.store_id = d.store_id
               WHERE prod_type = ? and a.store_id != "00000"
               GROUP BY 1,2
               ORDER BY sales DESC
               LIMIT 10', 
      params = input$prod_type
    )
  )
  
  observeEvent(input$save, {
    dbGetQuery(
      conn = db,
      statement = paste0(
        'DROP TABLE IF EXISTS top10_store_',input$prod_type)
    )
  }
  )
  
  observeEvent(input$save, {
    dbGetQuery(
      conn = db,
      statement = paste0(
        'CREATE TABLE top10_store_',input$prod_type,' AS
               SELECT city, d.store_name,
                     sum(price*qty) as sales
               FROM orders a
                  left join order_items b on a.ord_id = b.ord_id
                  left join prods_i c on b.prod_id = c.prod_id
                  left join stores d on a.store_id = d.store_id
               WHERE prod_type = ? and a.store_id != "00000"
               GROUP BY 1,2
               ORDER BY sales DESC
               LIMIT 10'),
      params = input$prod_type
    )
  }
  )
  
  observeEvent(input$save, {
    shinyalert(title = "OK!", 
               text = "Successfully saved to the database.", 
               type = "success")
  })
  
}

# Disconnecting from the database and executing the app

onStop(
  function()
  {
    dbDisconnect(db)
  }
)

shinyApp(ui, server)
