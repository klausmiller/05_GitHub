#################################################################################################
#### This script prepares the different dataframes, which can be used for the analysis.      ####
#### There are two different categories: Firstly, we can prepare dataframes, which aggregate ####
#### information on the dimension of cookies, timestamps, and categories.                    ####
#### Secondly, we can put the timestamps into periods to create a panel data set.            ####
#################################################################################################

rm(list = ls())

################ General Comments ################
# Date
# No Comments yet

#### Required Packages for this script ####
# install.packages("dplyr") # reqrite nested functions with %>%
library(dplyr)
options(scipen = 999) # do not use scientific format

#### Setup ####
user = 'windows'
if (user == 'mac'){
  # Explanation: Get the current working directory. It does not work with Knitr because the server is not able to connect to the API Server of R. Hence, we have to write down the path.
  working_folder = substr(rstudioapi::getSourceEditorContext()$path,1,nchar(rstudioapi::getSourceEditorContext()$path)-121)
  path_input_files = paste(substr(working_folder,1,nchar(working_folder)),'03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/',sep='')
  path_output_files = paste(substr(working_folder,1,nchar(working_folder)-47),'03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/',sep='')
}
if (user == 'linux'){
  working_folder = "~/Dropbox/"
  path_input_files = "~/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/"
  path_output_files = "03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/"
  # enter different paths#
}
if (user == 'windows'){
  working_folder = "D:/Dropbox/"
  path_input_files = "D:/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/"
  path_output_files = "03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/"
  # enter different paths#
}

#### Load the data ####
# start by loading the csv file. The fileEncoding was a little bit weird, this code finds out, which encoding is probably correct ####
file_encoding = 0
if (file_encoding == 1){
  codepages <- setNames(iconvlist(), iconvlist())
  codepages = codepages[c(1:50)]
  
  x <- lapply(codepages, function(enc) try(read.table("/Users/lennartkraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/StackedSegments.csv",
                                                      fileEncoding=enc,
                                                      nrows=3, header=TRUE, sep=",")))
  
  unique(do.call(rbind, sapply(x, dim)))
  
  maybe_ok <- sapply(x, function(x) isTRUE(all.equal(dim(x), c(3,460))))
  codepages[maybe_ok]
}

# Use correct encoding scheme to read the data
if (user == 'mac'){
  load_data <- read.csv("/Users/lennartkraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/StackedSegments.csv", fileEncoding = '437')
  }
if (user == 'linux'){
  load_data <- read.csv("/home/kraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/StackedSegments.csv", fileEncoding = '437')
  
}
if (user == 'windows'){
  load_data <- read.csv("D:/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/01_SegFe/StackedSegments.csv", fileEncoding = '437')
  
}





#### load the segment names ####
#path_seg_names = '/Users/lennartkraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/01_dokudatensatz/02_LookupFiles/'
#seg_names <- read.csv(paste(path_seg_names,'segment_names.csv', sep = ""), sep = ';')

#### sort the segments according to their name in dataframe ####
# Explanation: The user_id_64, time, and Group.1 variable should appear first.
sample_segs = c(c('user_id_64','time',"Group.1"),c(sort(colnames(load_data[c(5:length(colnames(load_data)))]))))
input_data = load_data[,sample_segs]

###############################
#### Dataframe Preparation ####
###############################

#####################################
#### Total number of data points ####
#####################################

# Explanation: We sum all values over the segments,time, and cookies while excluding NAs. Possible values are NA, 0 (cookie had a segment assigned to it at a different time stamp), 1 (cookie was assigned with segment at this time stamp), 2, ..., n (cookie was assigned n times with segment at this time stamp). We exclude the coloumns 1-3 because they containg the ID, time, and group.1 var.
datapoints_total = sum(input_data[c(4:length(colnames(input_data)))], na.rm = TRUE)
rm("datapoints_total")

###########################################
#### Number of data points per segment ####
###########################################

# Explanation: We sum all values over the time and cookies while excluding NAs. This gives us the number of segment assignments per segment. We exclude the coloumns 1-3 because they containg the ID, time, and group.1 var.
datapoints_segment = colSums(input_data[c(4:length(colnames(input_data)))], na.rm = TRUE)
rm("datapoints_segment")

##########################################
#### Number of data points per cookie ####
##########################################

# Explanation: We sum over all values of time and segments while excluding NAs. This gives us the number of segment assignments per cookie.
# Aggregate over timestamps within cookie. 
# Explanation: We first group by user and then sum over all coloumns. We devide everything by 1 to get the numeric class for segments.
datapoints_cookie_segment = input_data %>% group_by(user_id_64) %>% summarise_all(funs(sum(./1,na.rm = TRUE)))
# Aggregate over segments within cookie.
# Explanation: We sum over all segments within a cookie.
datapoints_cookie = as.data.frame(datapoints_cookie_segment$user_id_64)
datapoints_cookie$segmentassignments =   rowSums(datapoints_cookie_segment[c(4:length(colnames(input_data)))])
rm("datapoints_cookie_segment","datapoints_cookie")

#############################################
#### Number of data points per timestamp ####
#############################################

# Explanation: Sum over all rows to get the number of segment assignments per timestamp
datapoints_timestamp = rowSums(input_data[c(4:length(colnames(input_data)))], na.rm = TRUE)
hist(datapoints_timestamp, breaks = max(datapoints_timestamp), xlim = c(0,50))
# Alternatively, we can group by the timestamp and see, whether the timestamps are similar for cookies. We can use the aggregate function already used in the StackAndCodeSegments.r. We divide the data by 1 to get a numerical class for the dataframe.
#datapoints_timestamp2 = aggregate(input_data[c(grep("Group.1", colnames(input_data)):length(colnames(input_data)))]/1, by =  list(c(input_data$Group.1)), FUN = 'sum')
# There is hardly any overlap of timestamps across users. I also cross-checked with Excel. The share of timestamps which overlap is approximately 0.000536 (1 - 259048 / 259187)
rm("datapoints_timestamp")

#########################################################################################
#### Number of data points per period (year, month, week, day, hour, minute, second) ####
#########################################################################################

# Explanation: We start with a new dataframe and recode our time variable such that we can extract the year, ..., second.
# Create a new dataframe because new variables will be added
datapoints_period = input_data
# Convert time to the period to be considered
datapoints_period$period = as.POSIXct(as.numeric(input_data$time), origin="1970-01-01",tz="UTC")
datapoints_period$period = as.POSIXlt(datapoints_period$period)

#### Sort the data ####
# Explanation: Put the id, time, group.1, and period in front.
datapoints_period = datapoints_period[
  c(
    c(
      c(1:3),c(grep('period',colnames(datapoints_period)):length(colnames(datapoints_period)))
    )
    , c(4:c(grep('period', colnames(datapoints_period))-1))
  )
  ]

# We can create an identifier based on all time infos (including hour, minute, second). We can use this to create the finest possible panel data set.

#### Build identifier based on year = 1 until second = 6 ####
# Explanation: First, we create a list containing all the time-info from the period variable. We add the next granular time info until year = 1 or second = 6.
timelist = as.list(strsplit(gsub(':','_',gsub(' ','_',gsub('-','_',as.character(datapoints_period$period)))),'_'))
# Here, the loop to create the time-identifier starts.
granularity_period = 3
period_id = sapply(timelist, "[[", 1)
for (ticker in c(2:granularity_period)){
  period_id = paste(period_id,sapply(timelist, "[[", ticker), sep='_')
  print(ticker)
}

datapoints_period$period_id = period_id

#### create new dataframe with only the identifier and the segments ####
#### Explanation: We generate a new dataframe starting from the first segment (after "second") to the end. This includes the period_id which we will use to group the data. ####
datapoints_period_identifier = datapoints_period[c((1+grep('period',colnames(datapoints_period))[1]):length(colnames(datapoints_period)))]

# We first group by the period and then sum over all cookies the segment assignments
datapoints_periods_segments = datapoints_period_identifier %>% group_by(period_id) %>% summarise_all(funs(sum(./1,na.rm = TRUE)))
# Now we create a dataframe with the period_id and attach the number of datapoints afterwards
datapoints_period = as.data.frame(datapoints_periods_segments$period_id)
# Now we sum over all the segments to get the number of segment assignments per period
datapoints_period$segmentassignments = rowSums(datapoints_periods_segments[c(2:length(colnames(datapoints_periods_segments)))], na.rm = TRUE)

# plot(datapoints_period$`datapoints_periods_segments$period_id`, datapoints_period$segmentassignments)
plot(lowess(datapoints_period$`datapoints_periods_segments$period_id`, datapoints_period$segmentassignments, f = 1/5, iter = 100))
hist(datapoints_period$segmentassignments, breaks = 100, xlim = c(0,3500))
rm("datapoints_period", "datapoints_period_identifier", "datapoints_periods_segments", "period_id","timelist","granularity_period")

######################################################
#### Number of data points per cookie per segment ####
######################################################

#### Explanation: Aggregate over the user_id_64. Then delete the Group.1 variable which denotes time. ####
datapoints_cookie_segment = aggregate(input_data[c(grep("user_id_64", colnames(input_data)):length(colnames(input_data)))]/1, by =  list(c(input_data$user_id_64)), FUN = 'sum')
datapoints_cookie_segment = datapoints_cookie_segment[c(-4)]
datapoints_cookie_segment$user_id_64 = datapoints_cookie_segment$Group.1

#### If we want to know whether there exists a segment assignment within an individual, we can do so. This is a subcase analysis ####
temp = as.data.frame(ifelse(is.na(datapoints_cookie_segment[,4:length(colnames(datapoints_cookie_segment))]) ,0,1))
temp$user_id_64 = datapoints_cookie_segment$user_id_64
temp$time = datapoints_cookie_segment$time
datapoints_cookie_segment_yesno = temp
datapoints_cookie_segment_yesno = datapoints_cookie_segment_yesno[c(c(grep('user_id_64',colnames(datapoints_cookie_segment_yesno)):length(colnames(datapoints_cookie_segment_yesno))),c(1:(grep('user_id_64',colnames(datapoints_cookie_segment_yesno))-1)))]
rm("temp")
datapoints_cookie_segment_yesno = ifelse(datapoints_cookie_segment[c(grep("Group.1",colnames(datapoints_cookie_segment)):length(colnames(datapoints_cookie_segment)))] == "NA",0,1)
################

#### already done in "Number of data points per cookie ####
# We group by the user and aggregate over the time period to receive the number of segment assignment per cookie per period.
datapoints_cookie_segment = input_data %>% group_by(user_id_64) %>% summarise_all(funs(sum(./1,na.rm = TRUE)))
datapoints_cookie_segment = ifelse(datapoints_cookie_segment == "NA",0,datapoints_cookie_segment)

#### We then create a dataframe based on the list created before.####
datapoints_cookie_segment = do.call(rbind.data.frame, datapoints_cookie_segment)
colnames(datapoints_cookie_segment) = colnames(input_data)

rm("datapoints_cookie_segment","datapoints_cookie_segment_yesno")

######################################################
#### Number of data points per segment per period ####
######################################################

#### Explanation: Start by extracting the variable and aggregate it by period. ####
variable_number = length(colnames(input_data))
datapoints_segment_period = as.data.frame(input_data[,c(grep('time',colnames(input_data)):variable_number)])
datapoints_segment_period$period = as.POSIXct(as.numeric(input_data$time), origin="1970-01-01",tz="UTC")
datapoints_segment_period$period = as.POSIXlt(datapoints_segment_period$period)

#### Generate the period identifier ####
timelist = as.list(strsplit(gsub(':','_',gsub(' ','_',gsub('-','_',as.character(datapoints_segment_period$period)))),'_'))
# Here, the loop to create the time-identifier starts.
segment_period_id = sapply(timelist, "[[", 1)
segment_period_granularity = 3
for (ticker in c(2:segment_period_granularity)){
  segment_period_id = paste(segment_period_id,sapply(timelist, "[[", ticker), sep='_')
  print(ticker)
}
datapoints_segment_period$time = segment_period_id

#### convert all NAs to 0, 0 are those cells, for which there was no segment assignment for the individual over all timestamps. ####
datapoints_segment_period[is.na(col)] <- 0
temp = aggregate(col[(grep('time',colnames(datapoints_segment_period))+1):(grep('period',colnames(datapoints_segment_period))-1)]/1, by =  list(c(datapoints_segment_period$time)), FUN = 'sum')
colnames(temp) = c("period",colnames(temp[2:length(colnames(temp))]))
# Get rid of the aggregator Group.1
datapoints_segment_period = temp[c(-2)]

# Now we have the segment assignment for all periods in which there was at least one time stampt across all individuals in which segments were assigned. This probably covers all the periods on a daily basis. For more granularity, it makes sense to include those periods, in which no segments were assigned. This is done in the following part. It depends on the "segment_period_granularity" specified before.

#### Define, when the segment assignments started / ended. This is kind of arbitrary, most importantly it covers the start and end of segment assignments. ####
segment_period_start_id = as.character("2014_01_01_00_00_01")
segment_period_end_id = as.character("2016_12_31_00_00_01")

#### Now start to create an empty dataframe including all periods with specified granularity. ####
empty_segment_period = as.data.frame(as.numeric(as.POSIXct(strptime(segment_period_start_id, "%Y_%m_%d_%H_%M_%S"))))
colnames(empty_segment_period) = c("period")

#### Append rows until all periods are filled. ####
segment_period_start_id = as.numeric(as.POSIXct(strptime(segment_period_start_id, "%Y_%m_%d_%H_%M_%S")))
segment_period_end_id = as.numeric(as.POSIXct(strptime(segment_period_end_id, "%Y_%m_%d_%H_%M_%S")))
empty_segment_period = as.data.frame(segment_period_start_id)
colnames(empty_segment_period) = c("period")

ticker = 1
# Depending on the granularity, the jump_size is picked. It denotes, how many seconds are between two periods. Note, that there are Schaltjahre, we do not consider. There can be a gap of one day.
size_list = as.list(c(60,60,24,365/12,12))
size_jump = 1
for (ticker in c(1:(6-segment_period_granularity))){
  size_jump = size_jump*size_list[[ticker]]
  print(ticker)
}


# Until there are periods between the start and end date, add rows. The number of rows depends on the granularity which define the size_jump.
ticker = 1
while (empty_segment_period$period[ticker] < segment_period_end_id){
  period = segment_period_start_id + ticker * size_jump
  current_row = as.data.frame(period)
  empty_segment_period = bind_rows(empty_segment_period,current_row)
  ticker = ticker + 1
  print(ticker)
}

#### We create the period in the usual readible format and substitute the delimiters ####
empty_segment_period$period = as.character(as.POSIXct(as.numeric(empty_segment_period$period), origin="1970-01-01",tz="UTC"))

empty_timelist = strsplit(gsub(':','_',gsub(' ','_',gsub('-','_',as.character(empty_segment_period$period)))),'_')

#### Here, the loop to create the time-identifier starts. Depending on the granularity, it will be yyyy_mm or yyyy_mm_dd etc.####
empty_segment_period_id = sapply(empty_timelist, "[[", 1)

for (ticker in c(2:segment_period_granularity)){
  empty_segment_period_id = paste(empty_segment_period_id,sapply(empty_timelist, "[[", ticker), sep='_')
  print(ticker)
}

# Attach the new periods to the empty dataframe
empty_segment_period$period = empty_segment_period_id

#### Merge the empty and non-empty dataframe ####
datapoints_segment_period = merge(empty_segment_period, datapoints_segment_period, by.x = c("period"), all.x = TRUE)
rm("complete","current_row","datapoints_segment_period","empty_segment_period","empty_segment_period_timelist","empty_timelist","seg_names","segment_period_end_list","segment_period_start_list","size_list","temp","timelist","segment_period_end_id","segment_period_id","segment_period_granularity","size_jump","start_segment_period","ticker","variable_number")

#####################################################
#### Number of data points per cookie per period ####
#####################################################
# Note: We will also create the panel data in this section, in which the structure is
# cookie-id --> many periods --> different variables

# Explanation: We can start similar as we did in the calculation of segment assignments per period. 
# We only need to add an additional feature for the grouping variable. This will create a big panel file.
# Furthermore, we have to add empty rows for period, where no segments were assigned.

#### Create a new dataframe because new variables will be added ####
datapoints_cookie_period = input_data
# Convert time to the period to be considered
datapoints_cookie_period$period = as.POSIXct(as.numeric(input_data$time), origin="1970-01-01",tz="UTC")
datapoints_cookie_period$period = as.POSIXlt(datapoints_cookie_period$period)

#### Sort the data ####
# Explanation: Put the id, time, group.1, and period in front.
datapoints_cookie_period = datapoints_cookie_period[
  c(
    c(
      c(1:3),c(grep('period',colnames(datapoints_cookie_period)):length(colnames(datapoints_cookie_period)))
    )
    , c(4:c(grep('period', colnames(datapoints_cookie_period))-1))
  )
  ]
#### Create the identifier ####
# We can create an identifier based on all time infos (including hour, minute, second). We can use this to create the finest possible panel data set.
# Build identifier based on year = 1 until second = 6 AND the user_id_64.
# First, we put the time info into a list
timelist = as.list(strsplit(gsub(':','_',gsub(' ','_',gsub('-','_',as.character(datapoints_cookie_period$period)))),'_'))
# Here, the loop to create the user-time-identifier starts.
cookie_period_id = as.character(datapoints_cookie_period$user_id_64)

cookie_period_granularity = 2

for (ticker in c(1:cookie_period_granularity)){
  cookie_period_id = paste(cookie_period_id,sapply(timelist, "[[", ticker), sep='_')
  print(ticker)
}
datapoints_cookie_period$cookie_period_id = cookie_period_id

#### create new dataframe with only the identifier and the segments ####
# Explanation: We generate a new dataframe starting from the first segment (after "second") to the end. This includes the period_id which we will use to group the data.
datapoints_cookie_period_identifier = datapoints_cookie_period[c((1+grep('period',colnames(datapoints_cookie_period))[1]):length(colnames(datapoints_cookie_period)))]

#### This dataframe will be used for the big panel dataframe ####
# We first group by the user and period and then sum over all cookies the segment assignments
datapoints_cookie_period_segment = datapoints_cookie_period_identifier %>% group_by(cookie_period_id) %>% summarise_all(funs(sum(./1,na.rm = TRUE)))
rm("datapoints_cookie_period_identifier","datapoints_cookie_period")

#### This dataframe gives the number of segment assignments per period ####
# Now we create a dataframe with the cookie_period_id and attach the number of datapoints afterwards
datapoints_cookie_period = as.data.frame(datapoints_cookie_period_segment$cookie_period_id)
# Now we sum over all the segments to get the number of segment assignments per cookie per period
datapoints_cookie_period$segmentassignments = rowSums(datapoints_cookie_period_segment[c(2:length(colnames(datapoints_cookie_period_segment)))], na.rm = TRUE)

#### Create the Panel data frame ####
# Now we have to add row for each cookie for those periods, where no segments were assigned conditional on the granularity of cookie_period_id. Therefore, we have to know the starting points of each cookie and the ending point.
# This info is given in the following dataframe

if (user == 'mac'){
  timestart = read.csv( "/Users/lennartkraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/02_StaFe/timestart.csv")
}
if (user == 'linux'){
  timestart = read.csv( "/home/kraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/02_StaFe/timestart.csv")
}
if (user == 'windows'){
  timestart = read.csv( "D:/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/02_StaFe/timestart.csv")
}

# We substitute some of the characters.
timestart$start = gsub('-','_',timestart$start)
timestart$start = gsub(' ','_',timestart$start)
timestart$start = gsub(':','_',timestart$start)
timestart$end = gsub('-','_',timestart$end)
timestart$end = gsub(' ','_',timestart$end)
timestart$end = gsub(':','_',timestart$end)
timestart$end = as.numeric(as.POSIXct(strptime(timestart$end, "%Y_%m_%d_%H_%M_%S")))

ticker = 1
# Depending on the granularity, the jump_size is picked. It denotes, how many seconds are between two periods. Note, that there are Schaltjahre, we do not consider. There can be a gap of one day.
size_list = as.list(c(60,60,24,365/12,12))
size_jump = 1
for (ticker in c(1:(6-cookie_period_granularity))){
  size_jump = size_jump*size_list[[ticker]]
  print(ticker)
}

# Initialize empty data frame
whole_cookie_frame = data.frame(period = as.numeric(),
                                user_id_64 = as.character(),
                                stringsAsFactors=FALSE)

#### Create the basis for the Panel data frame, i.e. create a dataframe with user_id_64 and as many periods as defined by the granularity and the start and end date of the cookie ####
# start with first cookie and then loop over the rest
#for (ticker in c(884:884)){
for (ticker in c(1:length(timestart$user_id_64))){
  # Create a data frame for the current cookie
  current_cookie_frame = data.frame(
    period = as.character( as.numeric(as.POSIXct(strptime(timestart$start[ticker], "%Y_%m_%d_%H_%M_%S")))),
    user_id_64 = as.character(timestart$user_id_64[ticker]),
    stringsAsFactors=FALSE)
  
  # For this cookie, append new rows for all periods in between start and end
  current_cookie_period_frame = data.frame(
    period = as.numeric(as.POSIXct(strptime(timestart$start[ticker], "%Y_%m_%d_%H_%M_%S"))),
    user_id_64 = as.character(timestart$user_id_64[ticker]),
    stringsAsFactors=FALSE)
  
  # While there is space between the last period and the previous period attached to the data frame, continue
  ticker_period = 1
  if (is.na(current_cookie_period_frame$period[ticker_period]) | is.na(timestart$end[ticker])){
    # If there is either no start or no end date, go the next cookie
  }else{
    while (current_cookie_period_frame$period[ticker_period] < timestart$end[ticker]){
      next_period = data.frame( 
        period = (as.numeric(as.POSIXct(strptime(timestart$start[ticker], "%Y_%m_%d_%H_%M_%S"))) + ticker_period*size_jump), 
        user_id_64 = as.character(timestart$user_id_64[ticker]), 
        stringsAsFactors=FALSE)
      
      # Update the ticker_period
      ticker_period = ticker_period + 1
      # Attach the new row to the data frame of the current cookie.
      current_cookie_period_frame = bind_rows(current_cookie_period_frame,next_period)
    }
  }
  # Attach the new cookie dataframe to the dataframe collecting all cookies.
  whole_cookie_frame = bind_rows(whole_cookie_frame, current_cookie_period_frame)
  print(length(timestart$user_id_64) - ticker)
}  
# Get rid of some data
rm("current_cookie_frame","current_cookie_period_frame","next_period","timestart")

#### Make the period readible for users ####
# Now we transfer the periods back to a user readible format.
whole_cookie_frame$period = as.character(as.POSIXct(as.numeric(whole_cookie_frame$period), origin="1970-01-01",tz="UTC"))
whole_cookie_frame_MISSINGS = subset(whole_cookie_frame, is.na(whole_cookie_frame$period))
panel_cookie_frame = subset(whole_cookie_frame, is.na(whole_cookie_frame$period)==FALSE)

#### THIS IS A TEST
test = 0
if (test == 1){
panel_id_time = sub('_','+',panel_cookie_frame$cookie_period_id)
panel_id_time_list = strsplit(panel_id_time,'+')
panel_cookie_frame$subject_id = sapply(panel_id_time_list, "[[", 1)
panel_cookie_frame$time_id = sapply(panel_id_time_list, "[[", 2)
}
####

#### Write the data to the disk ####
if (user == 'mac'){
  write.csv(whole_cookie_frame,"Users/lennartkraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/03_PanelData/panel_data_empty.csv")
  write.csv(whole_cookie_frame_MISSINGS,"Users/lennartkraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/03_PanelData/panel_data_empty_MISSINGS.csv")
}
if (user == 'linux'){
  write.csv(whole_cookie_frame,"/home/kraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/03_PanelData/panel_data_empty.csv")
  write.csv(whole_cookie_frame_MISSINGS,"/home/kraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/03_PanelData/panel_data_empty_MISSINGS.csv")
}
if (user == 'windows'){
  write.csv(whole_cookie_frame,"D:/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/03_PanelData/panel_data_empty.csv")
  write.csv(whole_cookie_frame_MISSINGS,"D:/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/03_PanelData/panel_data_empty_MISSINGS.csv")
}

# And we create the identifier for the new dataframe based on the granularity used for the datapoints_cookie_segment_period.
timelist = as.list(strsplit(gsub(':','_',gsub(' ','_',gsub('-','_',as.character(panel_cookie_frame$period)))),'_'))

# Get rid of some data
rm("whole_cookie_frame","whole_cookie_frame_MISSINGS")

#### create the cookie_period identifier for the empty data frame ####
# Here, the loop to create the user-time-identifier starts with the user_id_64. 
# IMPORTANT: We have to get rid of those cookies, which have NA as start or ending date, because we cannot create the period identifier for them.
cookie_period_id = as.character(panel_cookie_frame$user_id_64)
for (ticker in c(1:cookie_period_granularity)){
  cookie_period_id = paste(cookie_period_id,sapply(timelist, "[[", ticker), sep='_')
  print(ticker)
}
panel_cookie_frame$cookie_period_id = cookie_period_id

# Get rid of some data
rm("cookie_period_id","timelist")

#### Merge the empty data frame with the non-empty dataframe ####
# Finally, we merge the dataframe collecting the user_id_64 and all periods between start end end of the cookie with the information of the datapoints_cookie_period_segment.

# Do some garbage collection first, we need the RAM
gc()

panel_cookie_frame_complete = merge(panel_cookie_frame, datapoints_cookie_period_segment, by = c("cookie_period_id"), all.x = TRUE)

#### Write the data to the disk ####
if (user == 'mac'){
  write.csv(panel_cookie_frame_complete,"Users/lennartkraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/03_PanelData/panel_data.csv")
}
if (user == 'linux'){
  write.csv(panel_cookie_frame_complete,"/home/kraft/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/03_PanelData/panel_data.csv")
}
if (user == 'windows'){
  write.csv(panel_cookie_frame_complete,"D:/Dropbox/03_Shared/01_KlausMiller_LennartKraft/02_Data/04_output/03_PanelData/panel_data.csv")
}

rm("datapoints_cookie_period", "datapoints_cookie_period_identifier", "cookie_period_id", "datapoints_cookie_period_segment", "ticker")


############################################
#### save current file to shared folder ####
############################################

# Write this script to the shared script folder.
r_file_string =  rstudioapi::getActiveDocumentContext()$contents

r_file = r_file_string[1]
for (ticker in c(2:length(r_file_string))){
  print(ticker)
  r_file = paste(r_file,r_file_string[ticker],sep = '\n')
}
write(x = r_file, file = paste('/Users/lennartkraft/dropbox/03_Shared/01_KlausMiller_LennartKraft/05_GitHub/01_Algorithms/01_R/DataframePreparation.R', sep = ''))
#### ####
