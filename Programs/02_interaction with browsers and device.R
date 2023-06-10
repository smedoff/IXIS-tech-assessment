
#' Title: Interaction between browser and device
#' Author: Sarah Medoff
#' 
#' Purpose: This script explores the interaction between browser and device as it relates to 
#' a specific metric (e.g., transactions, sessions, or QTY) 
#' For this exercise we will only look at transactions but this methedology can be applied
#' across metrics

library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotrix)
library(zoo)
library(gt)
library(corrplot)

rm(list = ls()) 

  load(file = file.path("Data", "Intermediate", "IXIS.RData"))
  
  # Lets add a 'graphing date' object for graph axis ordering 
  full.df <- full.df %>% 
    mutate(graph_date = as.Date(paste0(dim_year, "-", 
                                       str_pad(dim_month, side = "left", 
                                               width = 2, 
                                               pad = "0"), 
                                       "-01")))
  
  total_transactions_my.df <- full.df %>% 
    group_by(graph_date, yearmonth) %>% 
    summarize(Total_transactions = sum(transactions, na.rm = TRUE))

#------------------
# What browsers x devices maximize transactions?   
  browser_devices.df <- full.df %>% 
    mutate(pair = paste0(dim_browser, "/ ", dim_deviceCategory))
  
  # Lets first look at which browsers/device pairs are most common in our data set
  # save this information as a vector
  top_browser_devices.v <- browser_devices.df %>% 
    # This first group_by will summarize the transactions for each browser-device
    # recorded in our data set. 
    group_by(pair) %>% 
    summarize(transactions = sum(transactions, na.rm = TRUE)) %>% 
    mutate(Perc = round(transactions/sum(total_transactions_my.df$Total_transactions)*100, 
                        digits = 2)) %>% 
    arrange(desc(Perc)) %>% 
    slice(1:5) %>% 
    pull(pair)
  
  # Lets add this grouped variable to the main, browser_device.df 
  browser_devices.df <- browser_devices.df %>% 
    mutate(group_pair = ifelse(pair %in% top_browser_devices.v, pair, "other"))
  
  # Create browser/ category groups 
  browser_devices_group.df <- browser_devices.df %>% 
    # Re calc percentages based on these groups
    group_by(group_pair) %>% 
    summarize(transactions = sum(transactions, na.rm = TRUE)) %>% 
    mutate(Perc = round(transactions/total_transactions*100, digits = 2)) %>% 
    arrange(desc(Perc))
  
  # Creating a pie chart of transaction by browser. This pie chart will show the proportion
  # of transactions executed by the top 5 browser groups. 
  png(height=600, width=800, 
      file=file.path("Results", "Figures", 
                     "Browswer_Category_Transaction_Pie3D.png"))
  pie3D(browser_devices_group.df$Perc,
        labels = paste0(browser_devices_group.df$group_pair, " (", 
                        browser_devices_group.df$Perc, "%)"),
        labelcex = 0.75,
        #mar = c(0, 10, 8, 10), #c(bottom, left, top, right)
        main = "Distribution of transactions by browswer")
  dev.off()
  
  # Now lets look how this changes from each month-year
  browswer_devices_top3_my.df <- browser_devices.df %>% 
    # Use the first group_by to find the percentages
    group_by(graph_date, yearmonth, group_pair) %>% 
    summarize(transactions = sum(transactions, na.rm = TRUE)) %>% 
    left_join(total_transactions_my.df) %>% 
    mutate(Perc = transactions/total_transactions) %>% 
    arrange(desc(Perc)) %>% 
    # This second group_by will choose the most common browser-device pair 
    # that maximizes transaction. This works becasue we arranged the data in 
    # descending order by percent. 
    group_by(graph_date, yearmonth) %>%
    slice(1:3)

  
  # Lets plot these as stacked bar graphs
  ggplot(browswer_devices_top3_my.df, aes(fill=group_pair, y=Perc, x=graph_date)) + 
    geom_bar(position="stack", stat="identity") + 
    scale_y_continuous(labels=function(x) paste0(round(x*100),"%")) + 
    labs(title = "Bar graph of Browser-Device pair over time",
         fill = "Browswer/ Device \n Pair") + 
    xlab("Month Year") + 
    ylab("Percent of Transactions") + 
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 20),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))
  ggsave(file = file.path("Results", "Figures", "browser_device_bar.png"), 
         device = "png", width = 900/72, height = 550/72, dpi = 72)  
  
  
  #----------
  # This portion of the code took ~3 hours
  