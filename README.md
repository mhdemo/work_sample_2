Dairy Sales Analysis and Forecast
================
Matthew Harris
04/07/2019

-   [Introduction](#introduction)
-   [Analysis Goals](#analysis-goals)
-   [Data Sources](#data-sources)
-   [Data Import](#data-import)
-   [Data Wrangling/Cleansing](#data-wranglingcleansing)
    -   [Data Inspection](#data-inspection)
    -   [Data Type Updates](#data-type-updates)
-   [EDA](#eda)

Introduction
------------

This will be used as a sample to display some of my analytical capabilities in R. This project will focus on demonstrating how useful R can be to perform analysis that is easy to reproduce and communicate. This is by no means an exhaustive demonstration of my proficiency with R, but should highlight common data analysis functions that I perform regularly.

Analysis Goals
--------------

Forecasting can be a useful when try to prepare for future events. In this work sample I will demonstrate some of my forecasting capabilities with U.S. dairy transactional data. I will begin the process by examining, cleaning, and transforming the data. Next I will perform some exploratory analysis to get a better understanding of the data. Last up I will run two models on the data, determine which one performs the best, and use that model to forecast price movements across all dairy products.

Data Sources
------------

The data used for this work sample can be found at the USDA [website.](https://mpr.datamart.ams.usda.gov/menu.do?path=Products\Dairy\All%20Dairy)

Data Import
-----------

Loading necessary packages for analysis.

``` r
library(readxl)
library(lubridate)
library(forecast)
library(plotly)
library(tidyverse)
```

Loading csv file containing the data.

``` r
butter <- read_csv("Raw Data/Datamart-Export_DY_WK100-Butter Prices and Sales_20190407_042246.csv")
cheese_barrel <- read_csv("Raw Data/Datamart-Export_DY_WK100-500 Pound Barrel Cheddar Cheese Prices, Sales, and Moisture Content_20190407_042246.csv")
cheese_block <- read_csv("Raw Data/Datamart-Export_DY_WK100-40 Pound Block Cheddar Cheese Prices and Sales_20190407_042246.csv")
dry_milk <- read_csv("Raw Data/Datamart-Export_DY_WK100-Nonfat Dry Milk Prices and Sales_20190407_042246.csv")
dry_whey <- read_csv("Raw Data/Datamart-Export_DY_WK100-Dry Whey Prices and Sales_20190407_042246.csv")
```

Data Wrangling/Cleansing
------------------------

### Data Inspection

In order to begin the wrangling/cleansing process I need to know the characteristics of the data.

``` r
butter %>%
  glimpse()
```

    ## Observations: 1,830
    ## Variables: 5
    ## $ `Week Ending Date` <chr> "03/30/2019", "03/30/2019", "03/30/2019", "...
    ## $ `Report Date`      <chr> "04/03/2019", "04/03/2019", "04/03/2019", "...
    ## $ Date               <chr> "03/02/2019", "03/09/2019", "03/16/2019", "...
    ## $ `Weighted Prices`  <dbl> 2.2610, 2.2792, 2.2905, 2.2600, 2.2956, 2.2...
    ## $ Sales              <dbl> 6482983, 4109617, 3998746, 5098388, 3150773...

I repeated this process for the other tables. All of the tables have the same amount of observations, but the `cheese_barrel` table contains two extra columns. I want to join all of these tables by rows so I will want to remove those additional columns in order to keep the data consistent.

``` r
#Removes moisture, and un adjusted price from cheese barrel table
cheese_barrel %>%
  select(-4, -7) %>%
  rename("Weighted Prices" = "Weighted Price adjusted to 38% moisture") -> cheese_barrel
```

Now that I am sure that the column names and content match I can go through the process of combining the data by rows. I've choosen to accomplish this by placing all the tables into a list and looping through each table to create a product name column. This will allow me to identify each product type once they are combined.

``` r
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

#Removes unnecessary objects
rm(butter, cheese_barrel, cheese_block, dairy_tables, dry_milk, dry_whey, i)
```

### Data Type Updates

Now that all of the data is in one table I can begin the process of updating the column headers and making any necessry class changes.

``` r
#Updates the column headers
dairy_data %>%
  colnames() %>%
  tolower() %>%
  str_replace_all("\\.", "_") -> colnames(dairy_data)

#Examine the data structure to determine which variable classes need to be updated or removed
dairy_data %>%
  head(10)
```

    ##    week_ending_date report_date       date weighted_prices   sales product
    ## 1        03/30/2019  04/03/2019 03/02/2019          2.2610 6482983  butter
    ## 2        03/30/2019  04/03/2019 03/09/2019          2.2792 4109617  butter
    ## 3        03/30/2019  04/03/2019 03/16/2019          2.2905 3998746  butter
    ## 4        03/30/2019  04/03/2019 03/23/2019          2.2600 5098388  butter
    ## 5        03/30/2019  04/03/2019 03/30/2019          2.2956 3150773  butter
    ## 6        03/23/2019  03/27/2019 03/23/2019          2.2600 5094805  butter
    ## 7        03/23/2019  03/27/2019 03/16/2019          2.2905 3998746  butter
    ## 8        03/23/2019  03/27/2019 03/09/2019          2.2792 4109617  butter
    ## 9        03/23/2019  03/27/2019 03/02/2019          2.2610 6482983  butter
    ## 10       03/23/2019  03/27/2019 02/23/2019          2.2594 4360621  butter

``` r
dairy_data %>%
  str()
```

    ## 'data.frame':    9150 obs. of  6 variables:
    ##  $ week_ending_date: chr  "03/30/2019" "03/30/2019" "03/30/2019" "03/30/2019" ...
    ##  $ report_date     : chr  "04/03/2019" "04/03/2019" "04/03/2019" "04/03/2019" ...
    ##  $ date            : chr  "03/02/2019" "03/09/2019" "03/16/2019" "03/23/2019" ...
    ##  $ weighted_prices : num  2.26 2.28 2.29 2.26 2.3 ...
    ##  $ sales           : num  6482983 4109617 3998746 5098388 3150773 ...
    ##  $ product         : chr  "butter" "butter" "butter" "butter" ...

The data appears to have duplicate rows for the price and sales columns due to overlap of the week ending date and report date columns. I only need one date column so I will remove those and filter only for distinct rows.

``` r
#Removes the week ending and reporting date columns. Updates the sale date column to class date
dairy_data %>%
  select(-week_ending_date, -report_date) %>%
  rename("sale_date" = "date") %>%
  mutate(sale_date = as_date(sale_date, format = "%m/%d/%Y", tz = "ETC")) %>%
  distinct() -> dairy_data
```

With that out of the way I can move to perform some exploratory data analysis on the data.

EDA
---

First up is a summary of our transformed data.

``` r
dairy_data %>%
  summary()
```

    ##    sale_date          weighted_prices      sales         
    ##  Min.   :2012-03-03   Min.   :0.2244   Min.   :       0  
    ##  1st Qu.:2014-04-05   1st Qu.:0.7547   1st Qu.: 6475652  
    ##  Median :2015-12-12   Median :1.5042   Median :10314002  
    ##  Mean   :2015-11-30   Mean   :1.3390   Mean   :11113304  
    ##  3rd Qu.:2017-08-26   3rd Qu.:1.7958   3rd Qu.:13703950  
    ##  Max.   :2019-03-30   Max.   :3.0130   Max.   :40800023  
    ##    product         
    ##  Length:4706       
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ##
