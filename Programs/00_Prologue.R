
#' Title: Setting up working directory
#' Author: Sarah Medoff
#' 
#' Purpose: This script sets up the users working directory to ensure all 
#' folders and files are placed in the right area. Additionally, this script 
#' can also set parameters like "Report Year" for further automation. 

rm(list = ls())

#---------
# Creating the function to create working directories
  creating_wd.f <- function(wd_path){
    
    if(!dir.exists(wd_path)){
      dir.create(wd_path)
    }else{
      print("Directory Exists")
    }
    
  }
  
#---------
# Creating the working directories 
  creating_wd.f("hlpr func")
  creating_wd.f("Results")
  creating_wd.f("Results/Figures")
  creating_wd.f("Data/Intermediate")
  creating_wd.f("Data/Final Deliverable")
  