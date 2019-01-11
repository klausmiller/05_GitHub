########################################################################################################################
#### This script analyzes the panel data frame for segments
########################################################################################################################

rm(list = ls())

################ General Comments ################

#### Required Packages for this script ####
# install.packages("dplyr") # rewrite nested functions with %>%
library(dplyr)
# install.packages("foreign") # import stata files
library(foreign)


#### Setup ####
user = 'linux'

if (user == 'mac'){
    load_data <- read.csv("/Users/lennartkraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/03_PanelData/panel_data_daily.csv", fileEncoding = '437')
  }
if (user == 'linux'){
    system.time(load_data <- read.csv("/home/kraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/03_PanelData/panel_data_daily.csv", fileEncoding = '437'))
    system.time(load_data <- read.dta("/home/kraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/03_PanelData/panel_data_daily.dta"))
  }
if (user == 'windows'){
    load_data <- read.csv("D:/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/03_PanelData/panel_data_daily.csv", fileEncoding = '437')
  }


load_data = 
  
  
  
  
############################################
#### save current file to shared folder ####
############################################

r_file_string =  rstudioapi::getActiveDocumentContext()$contents

r_file = r_file_string[1]
for (ticker in c(2:length(r_file_string))){
  print(ticker)
  r_file = paste(r_file,r_file_string[ticker],sep = '\n')
}
if (user == 'mac'){
  write(x = r_file, file = '/Users/lennartkraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/05_GitHub/01_Algorithms/01_R/GeneralAnalysis.R')
}
if (user == 'linux'){
  write(x = r_file, file = '/home/kraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/05_GitHub/01_Algorithms/01_R/GeneralAnalysis.R')
}
if (user == 'windows'){
  write(x = r_file, file = 'D:/Dropbox/03_Shared/01_KlausMiller_LennartKraft/05_GitHub/01_Algorithms/01_R/GeneralAnalysis.R')
}
#### ####


