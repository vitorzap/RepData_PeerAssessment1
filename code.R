# Initial settings
rm(list=ls()) 
setwd("~/Documents/DataAnalyst/CouseraCourse/Reproducible-research/RepData_PeerAssessment1")
require(ggplot2)
require(dplyr)
require(lubridate)

# Loading the data
if (!file.exists("activity.zip")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","activity.zip", method="curl")
}
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}
activityFull=read.csv("activity.csv")
paste("Dataset with ",nrow(activityFull)," rows")  
head(activityFull)

# removing NA
activity = na.omit(activityFull)
head(activity)

#Total steps by date
sumStepsActivity = activity %>% group_by(date) %>% summarise(totalSteps = sum(steps))
head(sumStepsActivity)

#Ploting the histogram
colores=c(rbind(colors()[grepl("orange",colors())][1:15],colors()[grepl("blue",colors())][1:15]))
g=ggplot(sumStepsActivity,aes(totalSteps)) + geom_histogram(fill=colores) 
g = g + labs(x="Number of steps taken in a day", y="Number of Occurrences", 
             title="Total number of steps taken each day" )
g = g + theme_get() 
print(g)

#Mean and median
paste("Mean of steps taken each day",mean(sumStepsActivity$totalSteps), sep="=")
paste("Median number of steps taken each day",median(sumStepsActivity$totalSteps), sep="=")

#Mean steps by interval
IntervalActivity = activity %>% group_by(interval) %>% summarise(meanSteps = mean(steps))
head(IntervalActivity)

#Ploting
g=ggplot(IntervalActivity,aes(interval,meanSteps)) + geom_line(colour="blue") 
g = g + labs(x="Average steps", y="intervals (5 min)", 
             title="Daily activity" )
g = g + theme_get() 
print(g)

#maximum  average number of steps
paste("The 5-minute interval that contains maximum average of steps",
      IntervalActivity$interval[IntervalActivity$meanSteps==max(IntervalActivity$meanSteps)],
      sep="=")
paste("Average of steps at this interval",max(IntervalActivity$meanSteps),sep="=") 

#Analyzing the occurrence of NA in the dataset
for(i in 1:ncol(activityFull)) {
  #  label=paste("NA values in column ",names(activityFull)[i])
  print(paste(paste("NA values in column ",names(activityFull)[i]),sum(is.na(activityFull[,i]))))
}
paste("Total lines with NA",nrow(activityFull)-nrow(na.omit(activityFull)),sep="=")  
print(paste("Percentage of NA values",round(((nrow(activityFull)-nrow(na.omit(activityFull)))/nrow(activityFull))*100),"%"))

# Imputing missing values
activityImp = merge(activityFull,IntervalActivity, by="interval") 
activityImp$steps[is.na(activityImp$steps)] = activityImp$meanSteps[is.na(activityImp$steps)]
head(activityImp)
activityImp =  activityImp[,1:3]
head(activityImp)

#Histogram of the total number of steps taken each day
sumStepsActivityImp = activityImp  %>% group_by(date) %>% summarise(totalSteps = sum(steps))
head(sumStepsActivityImp)

colores=c(rbind(colors()[grepl("orange",colors())][1:15],colors()[grepl("blue",colors())][1:15]))
g=ggplot(sumStepsActivityImp,aes(totalSteps)) + geom_histogram(fill=colores) 
g = g + labs(x="Number of steps taken in a day", y="Number of Occurrences", 
             title="Total number of steps taken each day")
g = g + theme_get() 
print(g)

#Creating a dataset containing the information whether the date is weekend or not ("weekend" or "weekday")
activityKD = activityImp %>% 
  mutate(kindofday=ifelse(wday(as.Date(date, format = "%Y-%m-%d"))==1 | 
                            wday(as.Date(date, format = "%Y-%m-%d"))== 7,"weekend","weekday"))
head(activityKD)
#Summarizing
IntervalActivityKD = activityKD  %>% group_by(interval,kindofday) %>% summarise(meanSteps = mean(steps))
head(IntervalActivityKD)

#PLoting
g=ggplot(IntervalActivityKD,aes(interval,meanSteps)) + geom_line(aes(colour=kindofday))
g = g + labs(x="Average steps", y="intervals (5 min)", 
             title="Daily activity (Weekends x Weekdays)" )
g = g + theme_get() + facet_grid(kindofday ~ .)
print(g)
