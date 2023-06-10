
# Title: Calculating Standardization on Grouping Variables
# Author: Sarah Medoff
# 
# Purpose: This helper function will calculate the standardization on grouping variables. 
# These methods are used to compare time trends across variables with different scales

#---------------
# Example Syntax
#df <- browser_perc_top2.df
#grouping_var <- "dim_browser"
#stdz_var <- "perc"

standardize_grouping_var.f <- function(df, grouping_var, stdz_var){
  
  mean.df <- df %>% 
    rename(GROUP = grouping_var,
           VAR = stdz_var) %>% 
    group_by(GROUP) %>% 
    summarize(AVG = mean(VAR, na.rm = TRUE),
              STAN_DEV = sd(VAR)) %>% 
    doBy::renameCol("GROUP", grouping_var)
  
  stdz.df <- df %>% 
    left_join(mean.df) %>% 
    rename(VAR = stdz_var) %>% 
    mutate(VAR = as.numeric(VAR),
           STDZ_VAR = (VAR - AVG)/STAN_DEV) %>% 
    doBy::renameCol("VAR", stdz_var) %>% 
    doBy::renameCol("STDZ_VAR", paste0("stdz_", stdz_var)) %>% 
    select(-c(AVG, STAN_DEV))
  
  return(stdz.df)
}


