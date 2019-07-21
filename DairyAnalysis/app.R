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
  rename("sale_date" = "date") %>%
  mutate(sale_date = as_date(sale_date, format = "%m/%d/%Y", tz = "ETC")) %>%
  distinct() %>%
  filter(sales != 0) -> dairy_data

rm(dairy_products)

#Custom color pallette
my_pal <- pal_rickandmorty()(7)

show_col(my_pal)

header <- dashboardHeader(
  title = "Dairy Data Analysis",
  titleWidth = 450
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dairy Products", tabName = "dairy_prod", icon = icon("chart-line")),
    selectizeInput("prod", "Product Selection",
                   choices = c("All", "Butter" = "butter", "Cheese Barrel" = "cheese_barrel",
                               "Cheese Block" = "cheese_block", "Dry Milk" = "dry_milk",
                               "Dry Whey" = "dry_whey"),
                   selected = ""),
    selectizeInput("metric", "Metric Selection",
                   choices = c("Sales" = "sales", "Weighted Prices" = "weighted_prices"))
  )
)

dairy_data %>%
  pull(product) %>%
  unique()

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dairy_prod",
            fluidRow(
              box(plotlyOutput("line_gr", height = 750), width = 12)
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
  
  product_filter <- reactive({
    
    req(input$prod)
    
    dairy_data %>%
      filter(product == input$prod)
    
  })
  
  output$line_gr <- renderPlotly({
    
   req(input$prod)
    
    line_plot <- function(dat, metr) {
      
      dat %>%
        ggplot(aes(sale_date, {{ metr }}, col = product, group = product,
                   text = paste("Product:", product, "<br>",
                                "Sale Date:", sale_date, "<br>",
                                "Metric:", {{ metr }}))) + geom_line() +
        scale_y_continuous(labels = comma) +
        labs(x = "Date", y = paste0(tools::toTitleCase(str_replace_all(input$metric, "_", " "))),
             color = "Product") + 
        scale_color_npg()
      
      ggplotly(tooltip = "text")
    }
    
    if(input$prod == "All") {
      
    line_plot(dairy_data, !!sym(input$metric))
      
    } else {
    
    line_plot(product_filter(), !!sym(input$metric))
    }
    
  })
  
}

shinyApp(ui, server)

