library(readxl)
library(lubridate)
library(forecast)
library(plotly)
library(tidyverse)

#Loads data ####
butter <- read_csv("Raw Data/Datamart-Export_DY_WK100-Butter Prices and Sales_20190407_042246.csv")
cheese_barrel <- read_csv("Raw Data/Datamart-Export_DY_WK100-500 Pound Barrel Cheddar Cheese Prices, Sales, and Moisture Content_20190407_042246.csv")
cheese_block <- read_csv("Raw Data/Datamart-Export_DY_WK100-40 Pound Block Cheddar Cheese Prices and Sales_20190407_042246.csv")
dry_milk <- read_csv("Raw Data/Datamart-Export_DY_WK100-Nonfat Dry Milk Prices and Sales_20190407_042246.csv")
dry_whey <- read_csv("Raw Data/Datamart-Export_DY_WK100-Dry Whey Prices and Sales_20190407_042246.csv")

#Formats data ####

#Removes moisture, and un adjusted price from cheese barrel table
cheese_barrel %>%
  select(-4, -7) %>%
  rename("Weighted Prices" = "Weighted Price adjusted to 38% moisture") -> cheese_barrel

#Creates vector of dairy product names to be used as values when the tables are combined
dairy_products <- c("butter", "cheese_barrel", "cheese_block", "dry_milk", "dry_whey")

#Combines the dairy product tables into a list that can be used to add a product name all at once
dairy_tables <- list(butter, cheese_barrel, cheese_block, dry_milk, dry_whey)

#Loops over each table to add the product name
i <- c()
for(i in 1:5) {
  
  dairy_tables[[i]] %>%
    mutate(product = dairy_products[i]) -> dairy_tables[[i]]
  
}

#Extracts each table from the list and binds them by rows
dairy_tables %>%
  lapply(data.frame) %>%
  bind_rows() -> dairy_data

#Can remove unnecessary objects
rm(butter, cheese_barrel, cheese_block, dairy_tables, dry_milk, dry_whey, i)

#Updates the column headers
dairy_data %>%
  colnames() %>%
  tolower() %>%
  str_replace_all("\\.", "_") -> colnames(dairy_data)

#Examine the data structure to determine which variable classes need to be updated or removed
dairy_data %>%
  head(10)

dairy_data %>%
  str()

#The data appears to contain duplicate entries due to different week ending and report dates
#I will remove the other date columns then filter to only keep distinct rows

#Removes the week ending and reporting date columns. Updates the sale date column to class date
dairy_data %>%
  select(-week_ending_date, -report_date) %>%
  rename("sale_date" = "date") %>%
  mutate(sale_date = as_date(sale_date, format = "%m/%d/%Y", tz = "ETC")) %>%
  distinct() -> dairy_data

#Analysis ####

#Look at the data summary to see the characteristics of the data
dairy_data %>%
  summary()

#Visualize the price movements over time by product
dairy_data %>%
  ggplot(aes(sale_date, weighted_prices, col = factor(product))) + geom_line()

#Want to forecast monthly changes. Will take sales weighted monthly averages to determine the price per month
dairy_data %>%
  mutate(sale_year = year(sale_date), sale_month = month(sale_date)) %>%
  group_by(sale_year, sale_month, product) %>%
  summarize(avg_weighted_price = weighted.mean(weighted_prices, sales, na.rm = TRUE),
         avg_sales = mean(sales, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(sale_date = as_date(paste(sale_month, "01", sale_year, sep = "/"), format = "%m/%d/%Y", tz = "ETC")) %>%
  mutate_at(c("avg_sales"), as.integer) -> avg_dairy_data
  
avg_dairy_data %>%
  ggplot(aes(sale_date, avg_weighted_price, col = factor(product))) + geom_line()

#Forecasting ####

#Spreads the avg dairy data table to be converted into a ts object for forecasting
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
  ts(start = c(year(min_sale_date), month(min_sale_date)), end = c(year(max_sale_date), month(max_sale_date)),
     frequency = 12) -> avg_dairy_ts

#Removes unnecessary onjects
rm(avg_dairy_data, avg_dairy_data_spread, dairy_data)

#Looping through models to determine the best model per product

n_forecast <- 5
n <- ncol(avg_dairy_ts)
model_results <- data.frame("product" = NA, "ARIMA_RMSE" = NA, "ETS_RMSE" = NA)
i <- c()

for(i in 1:n) {
  
  train <- window(avg_dairy_ts[, i], end = c(year(max_sale_date - months(n_forecast)), month(max_sale_date - months(n_forecast))))
  test <- window(avg_dairy_ts[, i], start = c(year(max_sale_date - months(n_forecast - 1)), month(max_sale_date - months(n_forecast - 1))))
  
  fit_arima <- auto.arima(train)
  fc1 <- forecast(fit_arima, h = 5, level = c(95))
  autoplot(fc1)
  
  fit_ets <- ets(train)
  fc2 <- forecast(fit_ets, h = 5, level = c(95))
  
  dairy_products[i] -> model_results[i, 1]
  accuracy(fc1, avg_dairy_ts[, i])[4] -> model_results[i, 2]
  accuracy(fc2, avg_dairy_ts[, i])[4] -> model_results[i, 3]
  
}

model_results %>%
  gather(key = "metric", value = "error", -product) %>%
  ggplot(aes(product, error, fill = metric)) + geom_col(position = "dodge") +
  labs(title = "ARIMA vs. ETS") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

#ARIMA appears to outperform ETS for all products
