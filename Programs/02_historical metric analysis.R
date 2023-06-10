
#' Title: Historical Metric Analysis
#' Author: Sarah Medoff
#' 
#' Purpose: This script explores the historical time trends of a specific metric 
#' (e.g., transactions, sessions, or QTY) and how they related to each browser 
#' recorded in our sample. 

library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotrix)
library(zoo)
library(gt)
library(corrplot)

rm(list = ls()) 

source(file.path("hlpr func", "calc_standardization_grouping_var.R"))

  load(file = file.path("Data", "Intermediate", "IXIS.RData"))

  #--------------------
  # Lets look at how these metrics have changed with respect to browser
  # Well look at transactions first. This methodology could be applied to 
  # QTY and sessions as well 
  
  # Which browsers historically are the most used for transactions
  most_used_browswers.df <- full.df %>% 
    group_by(dim_browser) %>% 
    summarize(transactions = sum(transactions, na.rm = TRUE)) %>% 
    arrange(desc(transactions)) %>% 
    mutate(Perc = round(transactions/sum(transactions, na.rm = TRUE)*100))
  
  # Save a vector of the most used browsers
  top_browsers.v <- most_used_browswers.df %>% 
    # This works because you arranged in desc order by percent
    slice(1:5) %>% 
    pull(dim_browser)
  
  # Group browsers as top 5 and "other" for all other browsers used aside from the 
  # top 5
  browser_labs.df <- most_used_browswers.df %>% 
    mutate(lab_browser = ifelse(dim_browser %in% top_browsers.v, dim_browser, "Other")) %>%
    select(-dim_browser) %>% 
    group_by(lab_browser) %>% 
    summarize(transactions = sum(transactions, na.rm = TRUE)) %>% 
    arrange(desc(transactions)) %>% 
    mutate(Perc = round(transactions/sum(transactions, na.rm = TRUE)*100))
  
  # Creating a pie chart of transaction by browser. This pie chart will show the proportion
  # of transactions executed by the top 5 browser groups. 
  png(height=400, width=800, 
      file=file.path("Results", "Figures", 
                     "Browswer_Tranasaction_Pie3D.png"))
  pie3D(browser_labs.df$Perc,
        labels = paste0(browser_labs.df$lab_browser, " (", 
                        browser_labs.df$Perc, "%)"),
        labelcex = 0.75,
        #mar = c(0, 10, 8, 10), #c(bottom, left, top, right)
        main = "Distribution of transactions by browser")
  dev.off()
  
  # Lets see how composition of transactions by browser type has changed over time 
  # To accomplish this, we will make a stacked line graph
  browser.df <- full.df %>% 
    mutate(dim_browser = ifelse(dim_browser %in% top_browsers.v,
                                dim_browser, "Other")) %>% 
    group_by(dim_year, dim_month, dim_browser) %>% 
    summarize(yearmonth = unique(yearmonth),
              transactions = sum(transactions, na.rm = TRUE)) %>% 
    mutate(graph_date = as.Date(paste0(dim_year, "-", 
                                       str_pad(dim_month, side = "left", 
                                               width = 2, 
                                               pad = "0"), 
                                       "-01")))
  
  ggplot(browser.df, aes(x = graph_date, y = transactions, fill = dim_browser)) + 
    geom_area(position = 'stack') + 
    labs(fill = "Browser Type") + 
    xlab("Month Year") + 
    ylab("Number of Transactions") + 
    labs(title = "Time Trends of Total Transactions by Browser") + 
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 20),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))
  ggsave(file.path("Results", "Figures", "TT_browser_transactions.png"))
  
  # Its kind of hard to tell how the proportions of browser type has changed over time
  # To explore how the browsers composition as changed over time, lets plot the 
  # proportion of transactions used by each browser, over time
  total_browser_transactions_my.df <- browser.df %>% 
    group_by(dim_year, dim_month, yearmonth, graph_date) %>% 
    summarize(Total_transactions = sum(transactions, na.rm = TRUE))
  
  browser_perc.df <- browser.df %>% 
    left_join(total_browser_transactions_my.df) %>% 
    mutate(perc = round(transactions/Total_transactions, digits = 4))
  
  # --------------------
  # Code check----------
  # Make sure this left_join was a one-to-one match 
  if(nrow(browser_perc.df) != nrow(browser.df)){
    stop(print("STOP!! left_join is not a one-to-one match. browser types are duplicated
                 within month-year."))
  }
  # --------------------
  
  perc.p <- ggplot(browser_perc.df, aes(x = graph_date, y = perc)) + 
    geom_line(aes(color = dim_browser, group = dim_browser), size = 2) + 
    scale_y_continuous(labels=function(x) paste0(round(x*100),"%")) + 
    labs(color = "Browser Type") + 
    xlab("Month Year") + 
    ylab("Percentage of Transactions \n by Each Browser")  + 
    labs(title = "Percentage of Transactions \n by Browser") + 
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 20),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))
  #ggsave(file.path("Results", "Figures", "TT_browser_transactions_percent.png"))
  
  # There seems to be some negative correlation between the use of Chrome and Safari
  # lets plot these as standardized values to really compare time trends 
  browser_perc_top2.df <- browser_perc.df %>% 
    filter(dim_browser %in% c("Safari", "Chrome")) %>% 
    standardize_grouping_var.f(grouping_var = "dim_browser",
                               stdz_var = "perc")
  
  top2perc.p <- ggplot(browser_perc_top2.df, aes(x = graph_date, y = stdz_perc)) + 
    geom_line(aes(color = dim_browser, group = dim_browser), size = 2) + 
    scale_y_continuous(labels=function(x) paste0(round(x*100),"%")) + 
    labs(color = "Browser Type") + 
    xlab("Month Year") + 
    ylab("Percentage of Transactions \n by Each Browser")  + 
    labs(title = "Percentage of Transactions \n by Top Browsers") + 
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 20),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))
  
  g <- gridExtra::arrangeGrob(perc.p, top2perc.p, ncol = 2)
  ggsave(file = file.path("Results", "Figures", "perc_browser_transaction.png"), g, 
         device = "png", width = 900/72, height = 550/72, dpi = 72)

  
  #----------
  # This portion of the code took ~5 hours because I had to familiarize myself with 
  # the data and formulate questions around
  
  
  
