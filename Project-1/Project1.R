library(plyr)
library(reshape2)
filezip <- "MotoringData.zip"

if(!file.exists(filezip))
	{
	fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
      download.file(fileUrl, destfile="MotoringData.zip", method ="auto")
	}

## Unzip the dataset
if (!file.exists("activity.csv")) 
	{ 
  	unzip("MotoringData.zip") 
	}

## read the table

Activity <- read.csv("activity.csv")

#Convert to date format
Activity$date <- as.Date(as.character(Activity$date), "%Y-%m-%d")

#Omit NA
Activity <- na.omit(Activity)

#Calculate the total number of steps taken per day
Number_Steps_PerDay <- aggregate(steps ~ date, data = Activity, FUN= sum)

#Plot the histogram
hist(Number_Steps_PerDay$steps, col= "gold", main="Total of Steps Per Day", xlab ="Date")

#Media
Media_Per_Day <- aggregate(steps ~ date, data = Activity, FUN= media)

#Median

Median_Per_Day <- median(Number_Steps_PerDay$steps, FUN = median)

#Average of steps per day

Number_Steps_PerDay_Mean <- aggregate(steps ~ date, data = Activity, FUN= mean)

#Plot type l
plot(Number_Steps_PerDay_Mean$date, Number_Steps_PerDay_Mean$steps, type="l", main="The Average of Steps Per Day", ylab ="Number of  Steps", xlab ="Dates")

#Maximun of step per day

positi <- which(Number_Steps_PerDay_Mean$steps== max(Number_Steps_PerDay_Mean$steps))

#Date of maximun

Number_Steps_PerDay_Mean[positi,1]

#Number Total NA

NumberNA <- sum(is.na(Activity))

#Replace the NA with 0

Activity <- read.csv("activity.csv")
Data_Dat_withOut_NA <- replace(Activity, is.na(Activity), 0)

#Histogram with replace the NA with 0.
hist(Data_Dat_withOut_NA$steps, col= "gold", main="Total of Steps Per Day without NA", xlab ="Date")

#Mean and Median without NA
Number_Steps_PerDay_Mean <- aggregate(steps ~ date, data = Data_Dat_withOut_NA, FUN= mean)
Number_Steps_PerDay_Median <- aggregate(steps ~ date, data = Data_Dat_withOut_NA, FUN= median)

#Merge a column a weekday or weekend
#Convert to date format

Data_Dat_withOut_NA$date <- as.Date(as.character(Data_Dat_withOut_NA$date), "%Y-%m-%d")
weekday <- weekdays(Data_Dat_withOut_NA$date)
Data_Dat_withOut_NA_days <- cbind(Data_Dat_withOut_NA,weekday) 

Data_Dat_withOut_NA_days$weekday <- gsub("lunes|martes|miércoles|jueves|viernes", "weekday", Data_Dat_withOut_NA_days$weekday)
Data_Dat_withOut_NA_days$weekday <- gsub("sábado|domingo", "weekend", Data_Dat_withOut_NA_days$weekday)

#Convert to factor
Data_Dat_withOut_NA_days$weekday <- factor(Data_Dat_withOut_NA_days$weekday)