library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(scales)
library(readr)
library(stringr)
library(tidyr)
library(ggsci)
library(forecast)
library(timetk)

#Loads data ####
butter <- read_csv("Datamart-Export_DY_WK100-Butter Prices and Sales_20190407_042246.csv")
cheese_barrel <- read_csv("Datamart-Export_DY_WK100-500 Pound Barrel Cheddar Cheese Prices, Sales, and Moisture Content_20190407_042246.csv")
cheese_block <- read_csv("Datamart-Export_DY_WK100-40 Pound Block Cheddar Cheese Prices and Sales_20190407_042246.csv")
dry_milk <- read_csv("Datamart-Export_DY_WK100-Nonfat Dry Milk Prices and Sales_20190407_042246.csv")
dry_whey <- read_csv("Datamart-Export_DY_WK100-Dry Whey Prices and Sales_20190407_042246.csv")

#Formats data ####
cheese_barrel %>%
  select(-4, -7) %>%
  rename("Weighted Prices" = "Weighted Price adjusted to 38% moisture") -> cheese_barrel
dairy_products <- c("butter", "cheese_barrel", "cheese_block", "dry_milk", "dry_whey")
dairy_tables <- list(butter, cheese_barrel, cheese_block, dry_milk, dry_whey)
i <- c()
for(i in 1:5) {
  dairy_tables[[i]] %>%
    mutate(product = dairy_products[i]) -> dairy_tables[[i]]
}
dairy_tables %>%
  lapply(data.frame) %>%
  bind_rows() -> dairy_data
rm(butter, cheese_barrel, cheese_block, dairy_tables, dry_milk, dry_whey, i)

dairy_data %>%
  colnames() %>%
  tolower() %>%
  str_replace_all("\\.", "_") -> colnames(dairy_data)

dairy_data %>%
  select(-week_ending_date, -report_date) %>%
  rename("sale_date" = "date", "weighted_price" = "weighted_prices") %>%
  mutate(sale_date = as_date(sale_date, format = "%m/%d/%Y", tz = "ETC")) %>%
  distinct() %>%
  filter(sales != 0) -> dairy_data

#Creates ts object and table used for comparison ####
dairy_data %>%
  mutate(sale_year = year(sale_date), sale_month = month(sale_date)) %>%
  group_by(sale_year, sale_month, product) %>%
  summarize(avg_weighted_price = weighted.mean(weighted_price, sales, na.rm = TRUE),
            avg_sales = mean(sales, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sale_date = as_date(paste(sale_month, "01", sale_year, sep = "/"), 
                             format = "%m/%d/%Y", tz = "ETC")) %>%
  mutate_at(c("avg_sales"), as.integer) -> avg_dairy_data

avg_dairy_data %>%
  select(sale_date, avg_weighted_price, product) %>%
  spread(key = product, value = avg_weighted_price) -> avg_dairy_data_spread

avg_dairy_data_spread %>%
  pull(sale_date) %>%
  max() -> max_sale_date

avg_dairy_data_spread %>%
  pull(sale_date) %>%
  min() -> min_sale_date

avg_dairy_data_spread[, -1] %>%
  ts(start = c(year(min_sale_date), month(min_sale_date)), end = c(year(max_sale_date), 
                                                                   month(max_sale_date)),
     frequency = 12) -> avg_dairy_ts

tk_tbl(avg_dairy_ts, rename_index = "sale_date") %>%
  mutate(sale_date = zoo::as.Date(sale_date, frac = 0)) %>%
  gather(key = product, value = price, -sale_date) %>%
  mutate(value_type = "actual") -> avg_dairy_tbl

pred_table <- data.frame("sale_date" = date("1/1/1"), "product" = NA, "price" = NA, "value_type" = NA)

n <- ncol(avg_dairy_ts)
i <- c()

  for(i in 1:n) {
    
    train <- window(avg_dairy_ts[, i], end = c(year(max_sale_date - months(4)), 
                                               month(max_sale_date - months(4))))
    test <- window(avg_dairy_ts[, i], start = c(year(max_sale_date - months(3)), 
                                                month(max_sale_date - months(3))))
    
    fit_arima <- auto.arima(train)
    fc1 <- forecast(fit_arima, h = 4, level = c(95))
    
    date(max_sale_date) -> pred_table[i, 1]
    dairy_products[i] -> pred_table[i, 2]
    as.vector(fc1$mean)[4] -> pred_table[i, 3]
    "pred" -> pred_table[i, 4]
    
  }

#Custom color pallette ####
my_pal <- pal_rickandmorty()(7)

#Application ####
header <- dashboardHeader(
  title = "Dairy Data Analysis",
  titleWidth = 450
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dairy Products", tabName = "dairy_prod", icon = icon("chart-line")),
    menuItem("Price Forecasts", tabName = "fore", icon = icon("chart-line")),
    selectizeInput("prod", "Product Selection",
                   choices = c("All", "Butter" = "butter", "Cheese Barrel" = "cheese_barrel",
                               "Cheese Block" = "cheese_block", "Dry Milk" = "dry_milk",
                               "Dry Whey" = "dry_whey"))
  )
)

dairy_data %>%
  pull(product) %>%
  unique()

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dairy_prod",
            fluidRow(selectizeInput("metric", "Metric Selection",
                                    choices = c("Sales" = "sales", "Weighted Price" = "weighted_price"))),
            fluidRow(
              box(plotlyOutput("line_gr", height = 750), width = 12)
            )
      ),
    tabItem(tabName = "fore",
            fluidRow(
              numericInput("h_cast", "Forecast Training Window - n",
                           value = 2, min = 1, max = 12,
                           step = 1),
              actionButton("f_cast", "Forecast"),
              box(plotlyOutput("fore_gr", height = 750), width = 12)
            )
    )
    )
  )

ui <- dashboardPage(
  skin = "yellow",
  header,
  sidebar,
  body
)

server <- function(input, output, session) {
  
  pred_filter <- eventReactive(input$f_cast, {
    
    req(input$h_cast)
    
    pred_table <- data.frame("sale_date" = date("1/1/1"), "product" = NA, "price" = NA, "value_type" = NA)
    
    n <- ncol(avg_dairy_ts)
    i <- c()
    
    for(i in 1:n) {
      
      train <- window(avg_dairy_ts[, i], end = c(year(max_sale_date - months(input$h_cast)), 
                                                 month(max_sale_date - months(input$h_cast))))
      test <- window(avg_dairy_ts[, i], start = c(year(max_sale_date - months(input$h_cast - 1)), 
                                                  month(max_sale_date - months(input$h_cast - 1))))
      
      fit_arima <- auto.arima(train)
      fc1 <- forecast(fit_arima, h = input$h_cast, level = c(95))
      
      date(max_sale_date) -> pred_table[i, 1]
      dairy_products[i] -> pred_table[i, 2]
      as.vector(fc1$mean)[input$h_cast] -> pred_table[i, 3]
      "pred" -> pred_table[i, 4]
      
    }
    
    avg_dairy_tbl %>%
      bind_rows(pred_table) %>%
      filter(product == input$prod)
    
  })
  
  product_filter <- reactive({
    
    req(input$prod)
    
    dairy_data %>%
      filter(product == input$prod)
    
  })
  
  output$line_gr <- renderPlotly({
    
   req(input$prod)
    
    line_plot <- function(dat, metr) {
      
      dat %>%
        ggplot(aes(sale_date, {{ metr }}, col = product, group = 1,
                   text = paste("Product:", product, "<br>",
                                "Sale Date:", sale_date, "<br>",
                                "Metric:", {{ metr }}))) + geom_line() +
        scale_y_continuous(labels = comma) +
        labs(x = "Date", y = paste0(tools::toTitleCase(str_replace_all(input$metric, "_", " "))),
             color = "Product", title = paste(tools::toTitleCase(str_replace_all(input$prod, "_", " ")), 
                                              tools::toTitleCase(str_replace_all(input$metric, "_", " ")), "Trends")) + 
        scale_color_npg() + theme(plot.title = element_text(hjust = 0.5))
      
      ggplotly(tooltip = "text")
    }
    
    if(input$prod == "All") {
      
    line_plot(dairy_data, !!sym(input$metric))
      
    } else {
    
    line_plot(product_filter(), !!sym(input$metric))
      
    }
    
  })
  
  output$fore_gr <- renderPlotly({
    
    if(input$prod == "All") {
      
      ggplot(data = data.frame("Sale Date" = NA, "Price" = NA),
             aes("Sale Date", Price)) + geom_point() +
        labs(title = "Select Individual Dairy Product") +
        theme(plot.title = element_text(hjust = 0.5))

    } else {
    
    pred_filter() %>%
      filter(value_type == "actual") %>%
      ggplot(aes(sale_date, price)) + geom_line(col = my_pal[3]) +
      labs(title = "Actual vs. Predicted Price", x = "Sale Date", y = "Price") +
      scale_color_discrete(name = "") + 
        geom_point(data = pred_filter() %>% 
                     filter(value_type == "pred"),
                   aes(sale_date, price, col = "Forecast"), size = 3) +
        theme(plot.title = element_text(hjust = 0.5))
      
    }
    
  })
  
}

shinyApp(ui, server)


