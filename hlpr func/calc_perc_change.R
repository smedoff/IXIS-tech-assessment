
# Title: Calculating Percentage Change and Calculating Standardization on Grouping Variables
# Author: Sarah Medoff
# 
# Purpose: This helper function will calculate the percentage change from some baseline
# year and to calculate the standardization on grouping variables. These methods are used 
# to compare time trends across variables with different scales

#---------------
# Example Syntax
#df <- transaction_qty_sessions.df
#time_variable <- "yearmonth"
#time_baseline <- recent_yearmon

calculating_percentage_change.f <- function(df, time_variable, time_baseline){
  
  baseline.df <- df %>% 
    rename(TIME_VAR = time_variable) %>% 
    filter(TIME_VAR == time_baseline) %>% 
    rename(Base_Value = Value) %>% 
    ungroup() %>%
    select(-TIME_VAR, - dim_year, - dim_month)
  
  perc_change.df <- df %>% 
    left_join(baseline.df) %>% 
    mutate(Perc_Change = (Value - Base_Value)/Base_Value) %>% 
    select(-Base_Value)
  
  return(perc_change.df)
}

