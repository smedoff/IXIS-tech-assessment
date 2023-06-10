
#' Title: Cleaning and Creating data
#' Author: Sarah Medoff
#' 
#' Purpose: This script will clean and construct the final data deliverable and 
#' will save all other relevent intermediate data sets that will be used in further 
#' data exploration.

library(tidyverse)
library(dplyr)
library(readxl)
library(openxlsx)
library(zoo)

rm(list = ls())

  # Reading in the sessionCounts.csv
  session.df <- read.csv(file.path("Data", "Source", 
                                   "DataAnalyst_Ecom_data_sessionCounts.csv")) %>% 
    # I personally like working with dates in a YYYY-MM-D format 
    mutate(dim_date = as.Date(dim_date, "%m/%d/%y"),
           # Creating a dim_year, dim_month var to match to carts.df
           dim_year = substr(dim_date, 1, 4) %>% as.numeric(),
           dim_month = substr(dim_date, 6,7) %>% as.numeric(),
           yearmonth = substr(dim_date, 1, 7) %>% as.yearmon() %>% as.character())
  
  # Reading in the addsToCart.csv
  carts.df <- read.csv(file.path("Data", "Source", 
                                 "DataAnalyst_Ecom_data_addsToCart.csv"))
  
  # Join both data sets together to preform the analysis 
  full.df <- session.df %>% 
    left_join(carts.df)
  # Usually I would code a code-check after a join operation to ensure accuracy
  # Do I expect the join to be a one-to-one match, if so code-check to make sure
  # observations are not repeated
  
#--------------
# Creating the sheets for the xlsx work book 
  
  # Sheet 1
  sheet1 <- full.df %>% 
    group_by(Month = dim_month, Device = dim_deviceCategory) %>% 
    summarize(Sessions = sum(sessions, na.rm = TRUE),
              Transactions = sum(transactions, na.rm = TRUE),
              QTY = sum(QTY, na.rm = TRUE)) %>% 
    mutate(ECR = Transactions/Sessions)
  
  
  # Sheet 2
  # Identify the most recent date in the data
  recent_yearmon <- max(full.df$dim_date) %>% substr(1, 7) %>% as.yearmon() %>% as.character()
  previous_yearmon <- (max(full.df$dim_date) - 30) %>% substr(1, 7) %>% as.yearmon() %>% as.character()
  
  # Filter for the most recent 2 months 
  recent_2months.df <- full.df %>% 
    filter(yearmonth %in% c(recent_yearmon, previous_yearmon)) %>% 
    select(-c(dim_date, dim_year, dim_month))
  
  # Lets look at the variable classes to id the best way to summarize 
  str(recent_2months.df)
  
  sheet2 <- recent_2months.df %>% 
    group_by(yearmonth) %>% 
    summarize(# Number of different browsers used in each month
              browsers = n_distinct(dim_browser),
              # Number of different device categories used in each month
              deviceCategory = n_distinct(dim_deviceCategory),
              # Total number of sessions in each month
              sessions = sum(sessions, na.rm = TRUE),
              # Total number of transactions taking place in each month
              transactions = sum(transactions, na.rm = TRUE),
              # Total quantity sold (?) in each month
              QTY = sum(QTY, na.rm = TRUE),
              # Total number of adds to cart in each month
              addsToCart = sum(addsToCart, na.rm = TRUE)) %>%
    # Transpose the df, I am sure theres a better dplyr func to preform a transpose
    t() %>% 
    data.frame() %>%
    # Rename columns based on year-month listed in the first row
    janitor::row_to_names(row_number = 1) %>% 
    # Assign row names to a column in the data frame 
    rownames_to_column("Metric") 
  
  # Preform the month difference 
  sheet2$Diff <- as.numeric(sheet2$`May 2013`) - as.numeric(sheet2$`Jun 2013`)
  
#--------------
# Creating the xlsx workbook
  
  wb <- createWorkbook()
  
  addWorksheet(wb, sheetName = "Monthly Data")
  addWorksheet(wb, sheetName = "Monthly Comparison")

  writeDataTable(wb, sheet = 1, x = sheet1,
                 colNames = TRUE, rowNames = FALSE,
                 tableStyle = "TableStyleLight9") 
  
  writeDataTable(wb, sheet = 2, x = sheet2,
                 colNames = TRUE, rowNames = FALSE,
                 tableStyle = "TableStyleLight9") 
  
  saveWorkbook(wb, file.path("Data", 
                             "Final Deliverable", 
                             "IXIS_tech_assessment.xlsx"), 
               overwrite = TRUE)  
  
  
#--------------
# Save all data sets for further analysis 
  save(previous_yearmon, recent_yearmon, full.df,
       file = file.path("Data", "Intermediate", "IXIS.RData"))
  
  # This portion of the code took about 2-3 hours to preform
  