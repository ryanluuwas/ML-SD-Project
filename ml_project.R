#Packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(randomForest)
library(caret)
library(chron)
library(stringr)
library(qdap)

#Set work directory
#setwd("C:/Users/Ryan Luu/Desktop/UCI/MSBA FALL '19/BANA273 - Machine Learning Analytics/Assignment 3")
setwd("C:/Users/Ryan Luu/Documents/GitHub/ML-SD-Project/data")



#read file
data <- read.csv("get_it_done_2019_requests_datasd_v1.csv")


#Remove unnecessary columns
data <- subset(data, select =-c(service_request_id, service_request_parent_id, sap_notification_number, park_name, referred, date_updated))

data$date_requested <- ymd_hms(data$date_requested)

#Look at only closed cases
data <- data[which(data$status == 'Closed')]
data <- data[which(data$lat > 32.5444 &
                          data$lat < 33.4274 &
                          data$lng < -117.0301 &
                          data$lng > -117.6126),]



table(data$status)
#Make new column as months
data$month <- month(data$date_requested)

data$month[data$month == 1] = 'Jan'
data$month[data$month == 2] = 'Feb'
data$month[data$month == 3] = 'Mar'
data$month[data$month == 4] = 'Apr'
data$month[data$month == 5] = 'May'
data$month[data$month == 6] = 'Jun'
data$month[data$month == 7] = 'Jul'
data$month[data$month == 8] = 'Aug'
data$month[data$month == 9] = 'Sep'
data$month[data$month == 10] = 'Oct'
data$month[data$month == 11] = 'Nov'
data$month[data$month == 12] = 'Dec'

data$month <- as.factor(data$month)

data$case_time[data$case_age_days < 8] = 'Short'
data$case_time[data$case_age_days >= 8 & data$case_age_days < 31] = 'Medium'
data$case_time[data$case_age_days >= 31] = 'Long'

data$council_district = paste0('District', sep='_', data$council_district)


#New Column Time -> extract time from date_requested
data$time <- format(ymd_hms(data$date_requested), "%H:%M:%S")

#Convert time variable from chr to time
data$time <- chron(times=data$time)

#New column Date -> extracts date from date_requested
data['date']<-as.Date(data$date_requested)

#New column Season based on Date column
data$season[data$date >= "2019-01-01" & data$date < "2019-03-21"] = "Winter" 
data$season[data$date >= "2019-03-21" & data$date < "2019-06-21"] = "Spring" 
data$season[data$date >= "2019-06-21" & data$date < "2019-09-21"] = "Summer" 
data$season[data$date >= "2019-09-21" & data$date < "2019-12-21"] = "Fall" 

#New column TOD based on time column
data$tod[data$time >= "00:00:00" & data$time < "05:59:59"] = "Past Midnight" 
data$tod[data$time >= "06:00:00" & data$time < "11:59:59"] = "Morning" 
data$tod[data$time >= "12:00:00" & data$time < "17:59:59"] = "Afternoon" 
data$tod[data$time >= "18:00:00" & data$time < "23:59:59"] = "Evening" 



#Change Variable Type
data$month <- as.factor(data$month)
data$council_district <- as.factor(data$council_district)
data$comm_plan_code <- as.factor(data$comm_plan_code)
data$case_time <- as.factor(data$case_time)







#Take 10,000 random observations
data_s <- data[sample(nrow(data), 10000),]




#-----------------------------------------------------------------------------------------------



# 2018 dataset
data2018 <- read.csv("get_it_done_2018_requests_datasd_v1.csv")

data2018 <- subset(data2018, select =-c(service_request_id, service_request_parent_id, sap_notification_number, park_name, referred, date_updated))

data2018$date_requested <- ymd_hms(data2018$date_requested)

data2018 <- data2018[which(data2018$status == 'Closed'),]
data2018 <- data2018[which(data2018$lat > 32.5444 &
                         data2018$lat < 33.4274 &
                         data2018$lng < -117.0301 &
                         data2018$lng > -117.6126),]


#data2018 <- data2018[which(data2018$service_name == 'Graffiti Removal'),]

summary(data2018$case_age_days)

#Make new column as months
data2018$month <- month(data2018$date_requested)


data2018$month[data2018$month == 1] = 'Jan'
data2018$month[data2018$month == 2] = 'Feb'
data2018$month[data2018$month == 3] = 'Mar'
data2018$month[data2018$month == 4] = 'Apr'
data2018$month[data2018$month == 5] = 'May'
data2018$month[data2018$month == 6] = 'Jun'
data2018$month[data2018$month == 7] = 'Jul'
data2018$month[data2018$month == 8] = 'Aug'
data2018$month[data2018$month == 9] = 'Sep'
data2018$month[data2018$month == 10] = 'Oct'
data2018$month[data2018$month == 11] = 'Nov'
data2018$month[data2018$month == 12] = 'Dec'


#new column case age days categorical

#data2018$case_time[data2018$case_age_days < 8] = 'Short'
#data2018$case_time[data2018$case_age_days >= 8 & data2018$case_age_days < 31] = 'Medium'
#data2018$case_time[data2018$case_age_days >= 31] = 'Long'

#-------------------------

data2018$case_time[data2018$case_age_days < 8] = 'Short'
data2018$case_time[data2018$case_age_days >= 8] = 'Long'




data2018$council_district = paste0('District', sep='_', data2018$council_district)



#New Column Time -> extract time from date_requested
data2018$time <- format(ymd_hms(data2018$date_requested), "%H:%M:%S")

#Convert time variable from chr to time
data2018$time <- chron(times=data2018$time)

#New column Date -> extracts date from date_requested
data2018['date']<-as.Date(data2018$date_requested)

#New column Season based on Date column
data2018$season[data2018$date >= "2018-01-01" & data2018$date < "2018-03-21"] = "Winter" 
data2018$season[data2018$date >= "2018-03-21" & data2018$date < "2018-06-21"] = "Spring" 
data2018$season[data2018$date >= "2018-06-21" & data2018$date < "2018-09-21"] = "Summer" 
data2018$season[data2018$date >= "2018-09-21" & data2018$date < "2018-12-21"] = "Fall" 
data2018$season[data2018$date >= "2018-12-21" & data2018$date < "2019-03-21"] = "Winter" 

#New column TOD based on time column
data2018$tod[data2018$time >= "00:00:00" & data2018$time < "05:59:59"] = "Past Midnight" 
data2018$tod[data2018$time >= "06:00:00" & data2018$time < "11:59:59"] = "Morning" 
data2018$tod[data2018$time >= "12:00:00" & data2018$time < "17:59:59"] = "Afternoon" 
data2018$tod[data2018$time >= "18:00:00" & data2018$time < "23:59:59"] = "Evening" 







#convert variable type
data2018$month <- as.factor(data2018$month)
data2018$council_district <- as.factor(data2018$council_district)
data2018$comm_plan_code <- as.factor(data2018$comm_plan_code)
data2018$case_time <- as.factor(data2018$case_time)

str(data2018)


#Take 10,000 sample
data2018_s <- data2018[sample(nrow(data2018), 10000),]





#Feature Engineering for word count and character length

data2018_s['p_char'] <- str_count(data2018_s$public_description)

data2018_s['p_len'] <- word_count(data2018_s$public_description)
data2018_s$p_len[is.na(data2018_s$p_len)] <- 0



#-------------------------------------------------------------------------------------------
# Write CSV 
write.csv(data_s, file = "ml_project_2019sample_new.csv")
write.csv(data2018_s, file = "ml_project_2018sample_new.csv")

write.csv(data2018_s, file = "data2018_for_adam.csv")


