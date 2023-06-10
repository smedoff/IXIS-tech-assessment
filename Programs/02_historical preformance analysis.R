
#' Title: Historical data Exploration
#' Author: Sarah Medoff
#' 
#' Purpose: This script explores the historical time trends of the data. Cntl+F 
#' 'sm notes' to quickly search for key findings from the analysis. 

library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotrix)
library(zoo)
library(gt)
library(corrplot)

rm(list = ls()) 

source(file.path("hlpr func", "calc_perc_change.R"))

  load(file.path("Data",
                 "Intermediate",
                 "IXIS.RData"))
  
  baseline.y <- min(full.df$dim_date) %>% 
    substr(1, 7) %>% 
    as.yearmon() %>% 
    as.character()
  

#--------------------
# Lets look at the performance (transactions, qty, and sessions) over time 
  transaction_qty_sessions.df <- full.df %>% 
    group_by(dim_year, dim_month) %>% 
    summarize(yearmonth = unique(yearmonth),
              transactions = sum(transactions, na.rm = TRUE),
              QTY = sum(QTY, na.rm = TRUE),
              sessions = sum(sessions, na.rm = TRUE)) %>% 
    gather(Metric, Value, transactions, QTY, sessions) %>% 
    # Because transactions, qty, and sessions are all recorded at different scales 
    # (e.g., range(transactions) = [0,1398],  range(sessions) = [0,43559],
    # range(QTY) = [0, 2665]) we will have to scale the measurements to illustrate
    # all three time trends on one graph. To do this, we will calc the percentage change
    # from some base year. I arbitrarily chose this to be the first month-year of the sample
    calculating_percentage_change.f(time_variable = "yearmonth",
                                    time_baseline = baseline.y) %>% 
    mutate(graph_date = as.Date(paste0(dim_year, "-", 
                                       str_pad(dim_month, side = "left", 
                                               width = 2, 
                                               pad = "0"), 
                                       "-01")))
  
  # Lets record the average percentage change for the most recent year across all 
  # three metrics so we can include that information in our final plot
  recent_perc_change <- transaction_qty_sessions.df %>% 
    filter(yearmonth == recent_yearmon) %>% 
    pull(Perc_Change) %>% 
    mean()
  
  # Save as an R element so we can display this information in the ggplot title
  recent_perc_change <- round(recent_perc_change * 100, digits = 2)
  
  # Plot time trends of transactions, qty, and sessions
  metric_tt.p <- ggplot(transaction_qty_sessions.df, aes(graph_date, Perc_Change)) + 
    geom_line(aes(color = Metric, group = Metric), size = 2) + 
    scale_y_continuous(labels=function(x) paste0(round(x*100),"%")) +
    xlab("Month Year") + 
    ylab(paste0("Percentage Change from ", baseline.y)) + 
    labs(title = "Month-Year Time Trends",
         subtitle = paste0("Plot displays the month-year aggregates for Quantity, Sessions, ",
                           "and transactions over time. All three metrics rose an average of \n ",
                           recent_perc_change, "% as compared to the ", baseline.y, " values.")) + 
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))
  
  
  # Base on this graph, lets record the best month-year and worst month-years in the sample
  metric_preformance.df <- transaction_qty_sessions.df %>% 
    arrange(desc(Perc_Change)) %>% 
    group_by(Metric) %>%
    slice(1:2) %>% 
    mutate(Signal = "High Preformance Month") %>% 
    rbind(transaction_qty_sessions.df %>% 
            arrange(Perc_Change) %>% 
            group_by(Metric) %>%
            slice(1:2) %>% 
            mutate(Signal = "Low Preformance Month")) %>% 
    select(yearmonth, graph_date, Metric, Perc_Change, Signal)
  
  # sm notes:
  # QTY, sessions, and transactions all track each other pretty closely
  # These metrics have risen almost 100% since July 2012 (base year)
  # June, April 2013 was a high performance month
  # October 2012 was a low performance month
  
  #--------
  # Removing Time Trend Bias 
  # Part of these trends could be attributed to time trend bias. That is, bias that occurs 
  # naturally over time or across seasons. Lets plot a first differencing time trend 
  # to remove the time trend bias
  first_diff_dates.v <- full.df %>% 
    select(yearmonth) %>% 
    unique() %>% 
    pull(.)
  
  first_diff_dates.l <- lapply(2:length(first_diff_dates.v), FUN = function(p){
    print(p)
    one_row.df <- data.frame(
      Start = first_diff_dates.v[p-1],
      End = first_diff_dates.v[p])
    
  })
  
  first_diff_dates.df <- do.call(rbind, first_diff_dates.l) %>% 
    mutate(Period = 1:(n_distinct(transaction_qty_sessions.df$yearmonth)-1),
           TimePeriod = paste0(Start, "\n to \n", End))
    
  metric_fDiff.p <- transaction_qty_sessions.df %>% 
    ungroup() %>% 
    select(yearmonth, graph_date, Metric, Value) %>% 
    spread(Metric, Value) %>% 
    select(-yearmonth, -graph_date) %>% 
    apply(2, diff) %>% 
    data.frame() %>% 
    mutate(Period = 1:(n_distinct(transaction_qty_sessions.df$yearmonth)-1)) %>% 
    left_join(first_diff_dates.df) %>% 
    gather(Metric, Value, transactions, QTY, sessions) %>% 
  ggplot(aes(Period, Value)) + 
    geom_line(aes(color = Metric, group = Metric), size = 2) + 
    ylab("First Differencing") + 
    xlab("") + 
    labs(title = "First Differencing",
         subtitle = "Remove time trend bias by applying a first differencing technique") +
      scale_x_discrete(limits = first_diff_dates.df$Period, 
                       labels = unique(first_diff_dates.df$TimePeriod),
                       guide = guide_axis(check.overlap = TRUE)) + 
    theme(axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))
  
  g <- gridExtra::arrangeGrob(metric_tt.p, metric_fDiff.p)
  ggsave(file = file.path("Results", "Figures", "timeseries_metric.png"), g, 
         device = "png", width = 900/72, height = 550/72, dpi = 72)
  
  # sm notes:
  # After removing the time trend bias, we see that QTY and transactions are a lot more 
  # stable. Sessions is a much more volatile metric
  # Jan 2013 - Feb 2013 & March 2013 - April 2013 saw the biggest drop in sessions
  # Dec 2012 - Jan 2013 saw the biggest spike in sessions
  
  #----------
  # This portion of the code took ~2-3 hours
