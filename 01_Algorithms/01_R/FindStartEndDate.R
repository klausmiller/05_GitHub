########################################################################################################################
#### This scripts find the start and end dates of the cookies. 
#### It opens the standard feed csv and extracts the first and last point in time of those cookies, which are non empty.
########################################################################################################################

rm(list = ls())

################ General Comments ################
# 20.12.2018
# There were 84 user_id_64 extracted from the segment feed which could not be found in the standard feed. 
# Thus, there was no start or end date for them. It could be that the user id was incorrectly transferred to the input
# data. What seems strange is that the user_id_64 of the missings is much longer than the other ones: 1585382355783000000000000000000
# Apparently, the scientific format caused the problem. If we turn it off, everything works well. 
# 21.12.2018
# I tried to match while disabling the scientific format, however, there are more than 1000 unmatched
# cookies. Therefore, we should stick with the previous method, i.e. do not change anything in the
# code. Also do not use the format(., scientific = FALSE) There are some more options wich might work.
# For example, we could try using fuzzy matching of the cookie data files. There is the native package
# adist but the stringdist packages looks more promising. However, with our sample it might be 
# much easier to simply match the 90 missing cookies by hand.

#### Required Packages for this script ####
# install.packages("dplyr") # rewrite nested functions with %>%
library(dplyr)
#install.packages("stringr") # Required to parse number of sta_file
library(stringr)


#### Setup ####
user = 'windows'

# Depending on the format of scipen, open the correct stackedSegments file

scipen = 0

for (scipen in c(0,999)){
  
print(scipen)




if (scipen == 999){
  options(scipen = 999) 
  if (user == 'mac'){
    load_data <- read.csv("/Users/lennartkraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/StackedSegments_SciPen999.csv", fileEncoding = '437')
  }
  if (user == 'linux'){
    load_data <- read.csv("/home/kraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/StackedSegments_SciPen999.csv", fileEncoding = '437')
  }
  if (user == 'windows'){
    load_data <- read.csv("D:/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/StackedSegments_SciPen999.csv", fileEncoding = '437')
  }
  
} else {

  if (user == 'mac'){
    load_data <- read.csv("/Users/lennartkraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/StackedSegments.csv", fileEncoding = '437')
   }
  if (user == 'linux'){
    load_data <- read.csv("/home/kraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/StackedSegments.csv", fileEncoding = '437')
  }
  if (user == 'windows'){
    load_data <- read.csv("D:/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/StackedSegments.csv", fileEncoding = '437')
  }
}

# sort the segments according to their name in dataframe ####
# Explanation: The user_id_64, time, and Group.1 variable should appear first.
sample_segs = c(c('user_id_64','time',"Group.1"),c(sort(colnames(load_data[c(5:length(colnames(load_data)))]))))
input_data = load_data[,sample_segs]

# We create a data frame for the user_id_64, the start and the ending date
timestart = as.data.frame(unique(input_data$user_id_64))

colnames(timestart) <- c("user_id_64")
timestart$start = NA
timestart$end = NA
  
# Get all files in the standard feed
if (user == 'mac'){
}
if (user == 'linux'){
  sta_files <- list.files(path = "/home/kraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/03_StaFe/Users/", pattern = 'processed_stafe_*', full.names = TRUE, recursive = FALSE)
}
if (user == 'windows'){
  sta_files <- list.files(path = "D:/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/03_StaFe/Users/", pattern = 'processed_stafe_*', full.names = TRUE, recursive = FALSE)
}

# We now loop through all StaFiles and find the start and end date of observing the cookie. We put this info to timestart
ticker = 1
for (sta_file in sta_files){
#for (sta_file in sta_files[1:20]){
  # Open the csv of a cookie
  user_sta_df_read <- read.csv(sta_file ,sep = '\t')
  # Check where the cookie of the csv is in the timstart dataframe
  user_id_64_index = which(timestart$user_id_64 == user_sta_df_read$OTHUSER_ID_64[1])
  user_id_64_index_alternative = which(timestart$user_id_64 == str_match(sta_file, "processed_stafe_(.*?).csv")[,2])
  # If the standard file cookie exists in our sample, update the information of this cookie.
  if (length(user_id_64_index) > 0){
    timestart$start[user_id_64_index] = min(as.character(user_sta_df_read$hitTS))
    timestart$end[user_id_64_index] = max(as.character(user_sta_df_read$hitTS))
  } else if (length(user_id_64_index_alternative) > 0){
    timestart$start[user_id_64_index_alternative] =  min(as.character(user_sta_df_read$hitTS))
    timestart$end[user_id_64_index_alternative] = max(as.character(user_sta_df_read$hitTS))
    print("Could not find ")
    print("user_id_64_index")
    print("But could find")
    print(str(user_id_64_index_alternative))
  }
  print(scipen)
  print(length(sta_files) - ticker)
  ticker = ticker + 1
}

# Write the data to the output folder.
if (scipen == 999){
  if (user == 'mac'){
    write.csv(timestart, "/Users/lennartkraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/02_StaFe/timestart_SciPen999.csv")
  }
  if (user == 'linux'){
    write.csv(timestart, "/home/kraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/02_StaFe/timestart_SciPen999.csv")
  }
  if (user == 'windows'){
    write.csv(timestart, "D:/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/02_StaFe/timestart_SciPen999.csv")
  }
} else {
  if (user == 'mac'){
    write.csv(timestart, "/Users/lennartkraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/02_StaFe/timestart_everythingnormal.csv")
  }
  if (user == 'linux'){
    write.csv(timestart, "/home/kraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/02_StaFe/timestart_everythingnormal.csv")
  }
  if (user == 'windows'){
    write.csv(timestart, "D:/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/02_StaFe/timestart_everythingnormal.csv")
  }
}
  
}


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
  write(x = r_file, file = '/Users/lennartkraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/05_GitHub/01_Algorithms/01_R/FindStartEndDate.R')
}
if (user == 'linux'){
  write(x = r_file, file = '/home/kraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/05_GitHub/01_Algorithms/01_R/FindStartEndDate.R')
}
if (user == 'windows'){
  write(x = r_file, file = 'D:/Dropbox/03_Shared/01_KlausMiller_LennartKraft/05_GitHub/01_Algorithms/01_R/FindStartEndDate.R')
}
#### ####

