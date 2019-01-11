####################################################################################################################################################################################
#### This Script stacks and names the segments of all cookies in a folder into a panel dataframe. The id is the cookie and the time is the timestamp of the segment assignment. ####
####################################################################################################################################################################################

rm(list = ls())

################ General Comments ################
# Date
# No comments yet

###########################
#### Segment Feed data ####
###########################

#install.packages('fastDummies')
#install.packages('dummy')
#install.packages('doBy')
#install.packages('dplyr')
#install.packages('dummies')
#install.packages('rstudioapi')
library('fastDummies')
library('rstudioapi')
#library('dummy')
library('dummies')
library('doBy')
library('dplyr')

#options(scipen = 999) # do not use scientific format

#### Setup ####
working_folder = substr(rstudioapi::getSourceEditorContext()$path,1,nchar(rstudioapi::getSourceEditorContext()$path)-53)
user = 'linux'
if (user == 'mac'){
  path_seg_names = '/Users/lennartkraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/01_dokudatensatz/02_LookupFiles/'
}
if (user == 'linux'){
  path_seg_names = '/home/kraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/01_dokudatensatz/02_LookupFiles/'
}
if (user == 'windows'){
  path_seg_names = 'D:/Dropbox/03_Shared/01_KlausMiller_LennartKraft/01_dokudatensatz/02_LookupFiles/'
}

# Are we working with the big  or small sample? Depending on the sample, we load different data and store results on a different location
sample = 'big'
if (sample == 'small'){
  path_input_files = paste(working_folder,'01_Data/02_SampleDaten/01_SegmentFeedSample/',sep='')
  path_ouput_files = paste(working_folder,'01_Data/03_OutputDaten/01_SegmentFeed/',sep='')
} else {
  path_input_files = paste(substr(working_folder,1,nchar(working_folder)-47),'03_Shared/01_KlausMiller_LennartKraft/02_Data/01_SegFe/',sep='')
  path_ouput_files = paste(substr(working_folder,1,nchar(working_folder)-47),'03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/',sep='')
}

#### import segment names ####
seg_names <- read.csv(paste(path_seg_names,'segment_names.csv', sep = ""), sep = ';')

#### create file list ####
seg_files <- list.files(path = path_input_files, pattern = 'processed_segfe_*', full.names = TRUE, recursive = FALSE)

#### initilaize big segment data file ####
user_seg_df_read <- read.csv(seg_files[1],sep = '\t')
user_seg_df <- data.frame(dummy(user_seg_df_read$segment_id))
user_seg_df$time <- as.numeric(as.POSIXct(strptime(user_seg_df_read$datetime, "%Y-%m-%d %H:%M:%S")))
# collapse
user_seg_collapse = aggregate(user_seg_df, by =  list(c(user_seg_df$time)), FUN = 'sum')
# write id to segment files
user_seg_collapse$user_id_64 <- user_seg_df_read$user_id_64[1]
whole_seg = user_seg_collapse
countdown =  length(seg_files)

#### Add all the users and create a big panel ####
for (file_id in seg_files[1:length(seg_files)]){
#for (file_id in seg_files[1:100]){
  print(countdown)
  countdown =  countdown - 1
  user_seg_df_read <- read.csv(file_id,sep = '\t')
  user_seg_df <- data.frame(dummy(user_seg_df_read$segment_id))
  user_seg_df$time <- as.numeric(as.POSIXct(strptime(user_seg_df_read$datetime, "%Y-%m-%d %H:%M:%S")))
  # collapse
  user_seg_collapse = aggregate(user_seg_df, by =  list(c(user_seg_df$time)), FUN = 'sum')
  # write user name to file
  user_seg_collapse$user_id_64 <- user_seg_df_read$user_id_64[1]
  # append
  whole_seg = bind_rows(whole_seg,user_seg_collapse)
}

# put time to second row if you know that time is the 65th variable in the master data frame
# find position of time and then do it automatically
whole_seg = whole_seg[c(c(grep("user_id_64", colnames(whole_seg)),c(1:(grep("user_id_64", colnames(whole_seg))-1))),c((grep("user_id_64", colnames(whole_seg))+1):length(colnames(whole_seg))))]
whole_seg = whole_seg[c(c(c(1,grep("time", colnames(whole_seg))),c(2:(grep("time", colnames(whole_seg))-1))),c((grep("time", colnames(whole_seg))+1):length(colnames(whole_seg))))]

# change var names according to segmentNames.csv
for (var_num in c(4:(length(colnames(whole_seg))-0))){
  # long
  # find ID in dummies
  #var_id = as.numeric(gsub("segment_id.", "", colnames(user_seg_collapse)[var_num]))
  # find index of ID in seg names
  #indices <- which(seg_names$ID == var_id)
  #var_name = seg_names$NAME[indices]
  #colnames(whole_seg)[var_num] <- paste(var_name)
  #short
  if (length(which(seg_names$ID == as.numeric(gsub("segment_id.", "", colnames(whole_seg)[var_num])))) > 0){
    colnames(whole_seg)[var_num] <- paste(seg_names$NAME[which(seg_names$ID ==  as.numeric(gsub("segment_id.", "", colnames(whole_seg)[var_num])))])
    }
  }

#### Assign the right time to time varaiable. Because of the aggregation, the time variable was changes but the original time is saved and the Group.1 variable. #### 
whole_seg$time = whole_seg$Group.1

#### Store the results of the big file #### 
write.csv(whole_seg, paste(path_ouput_files, 'StackedSegments.csv', sep = ""))

###############################################
#### If we want to work only with a subset ####
###############################################

work_with_subset = 0
if (work_with_subset == 1){

#### Store the results of a selected smaller file #### 
data = as.data.frame(whole_seg$user_id_64)
data$time = whole_seg$time
data$id_ticker <- cumsum(!duplicated(data$`whole_seg$user_id_64`))

# gender
data$AS_sex_female = whole_seg$`AS_Geschlecht Frau rw (P) (496)`
data$AS_sex_male = whole_seg$`AS_Geschlecht Mann rw (P) (497)`

# change
data$AS_sex_change = ifelse(is.na(data$AS_sex_female) | is.na(data$AS_sex_male),0,1)

# non-missings
AS_sex_counter = sum(ifelse(is.na(data$AS_sex_female) & is.na(data$AS_sex_male),0,1))
# percentage change
AS_sec_perc_change = sum(data$AS_sex_change) / AS_sex_counter

#data$noname_sex = whole_seg$geschlecht
data$S_HF_female = whole_seg$`S_HF Geschlecht Frau`

# age
data$AS_age_14_19 = whole_seg$`AS_Alter 14-19 (P) (510)`
data$AS_age_18_39 = whole_seg$`AS_Alter 18-39 (P) (627)`
data$AS_age_18_49 = whole_seg$`AS_Alter 18-49 (P) (628)`
data$AS_age_20_39 = whole_seg$`AS_Alter 20-39 (P) (565)`
data$AS_age_20_49 = whole_seg$`AS_Alter 20-49 (P) (561)`
data$AS_age_30_39 = whole_seg$`AS_Alter 30-39 (P) (639)`
data$AS_age_30_49 = whole_seg$`AS_Alter 30-49 (P) (566)`
data$AS_age_30_59 = whole_seg$`AS_Alter 30-59 (P) (567)`
data$AS_age_50_100 = whole_seg$`AS_Alter ab 50 (P) (631)`

#change
data$AS_age_change = ifelse( ifelse(is.na(data$AS_age_14_19),1,0) + ifelse(is.na(data$AS_age_18_39),1,0) + 
                              ifelse(is.na(data$AS_age_18_49),1,0) + ifelse(is.na(data$AS_age_20_39),1,0) + 
                              ifelse(is.na(data$AS_age_20_49),1,0) + ifelse(is.na(data$AS_age_30_39),1,0) + 
                              ifelse(is.na(data$AS_age_30_49),1,0) + ifelse(is.na(data$AS_age_30_59),1,0) +
                              ifelse(is.na(data$AS_age_50_100),1,0) > 7,0,1)



# education
data$educ_isced2 = whole_seg$`S_Bildung Hauptschule (P) (617)`
data$educ_isced3 = whole_seg$`S_Bildung Realschule (P) (644)`
data$educ_isced4 = whole_seg$`S_Bildung Abi oder FH (P) (660)`
data$educ_isced5 = whole_seg$`S_Bildung Hochschule (P) (656)`


# income
data$income_500 = whole_seg$`AS_HHNE bis 500 (P) (607)`
data$income_500_1500 = whole_seg$`AS_HHNE 500-1500 (P) (636)`
data$income_1500_2500 = whole_seg$`AS_HNNE 1500-2500`
data$income_2500_4000 = whole_seg$`AS_HHNE 2500-4000 (P) (633)`
data$income_4000 = whole_seg$`AS_HHNE >4000 (P) (635)`

#### Save results of the smaller sample #### 
write.csv(data, paste(path_ouput_files, 'StackedSegmentsSelected.csv', sep = ""))
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
  write(x = r_file, file = '/Users/lennartkraft/dropbox/03_Shared/01_KlausMiller_LennartKraft/05_GitHub/01_Algorithms/01_R/StackAndCodeSegments.R')
  }
if (user == 'linux'){
  write(x = r_file, file = '/home/kraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/05_GitHub/01_Algorithms/01_R/StackAndCodeSegments.R')
  }
if (user == 'windows'){
  write(x = r_file, file = 'D:/Dropbox/dropbox/03_Shared/01_KlausMiller_LennartKraft/05_GitHub/01_Algorithms/01_R/StackAndCodeSegments.R')
  }

#### ####

