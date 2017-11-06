library(ggplot2)
library(dplyr)

setwd("~/Documents/DataAnalyst/CouseraCourse/Reproducible-research/RepData_PeerAssessment1")
if (!file.exists("activity.zip")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","activity.zip", method="curl")
}
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}
activityFull=read.csv("activity.csv")

nrow(activity)
head(activity)
#retirnado os NA
activity = na.omit(activityFull)


activity = mutate(activity,date=as.Date(date, format = "%Y-%m-%d"))
#head(activity2)
#str(activity2)
sumStepsActivity = activity %>% mutate(date=as.Date(date, format = "%Y-%m-%d")) %>% 
                  group_by(date) %>% 
                  summarise(totalSteps = sum(steps))
head(sumStepsActivity)
#hist(activity$steps)
#hist(sumStepsActivity$totalSteps)
#hist(sumStepsActivity$totalSteps, breaks=nrow(sumStepsActivity))
#hist(sumStepsActivity$totalSteps, breaks=1000)


colores=c(rbind(colors()[grepl("orange",colors())][1:15],colors()[grepl("blue",colors())][1:15]))
g=ggplot(sumStepsActivity,aes(totalSteps)) + geom_histogram(fill=colores) 
g = g + labs(x="Number of steps taken in a day", y="Number of Occurrences", 
             title="Analysis of the occurrence of the number of steps in a daye" )
g = g + theme_get() 
print(g)

#media
mean(sumStepsActivity$totalSteps)
paste("Mean of steps taken each day",mean(sumStepsActivity$totalSteps), sep="=")
paste("Median number of steps taken each day",median(sumStepsActivity$totalSteps), sep="=")


IntervalActivity = activity %>% mutate(date=as.Date(date, format = "%Y-%m-%d")) %>% 
  group_by(interval) %>% 
  summarise(meanSteps = mean(steps))
head(IntervalActivity)

g=ggplot(IntervalActivity,aes(interval,meanSteps)) + geom_line(colour="blue") 
g = g + labs(x="Average steps", y="intervals (5 min)", 
             title="Daily activityy" )
g = g + theme_get() 
print(g)

IntervalActivity$interval[IntervalActivity$meanSteps==max(IntervalActivity$meanSteps)]
paste("The 5-minute interval that contains maximum average of steps",
      IntervalActivity$interval[IntervalActivity$meanSteps==max(IntervalActivity$meanSteps)],
      sep="=")
paste("Average of steps at this interval",max(IntervalActivity$meanSteps),sep="=")   


print ("Code used => activity = na.omit(activityFull)")
NoOfNAs=nrow(activityFull)-nrow(na.omit(activityFull))
paste("Total lines with NA",NoOfNAs,sep="=")  
# Which fields have "NA" values
for(i in 1:ncol(activityFull)) {
#  label=paste("NA values in column ",names(activityFull)[i])
  print(paste(paste("NA values in column ",names(activityFull)[i]),sum(is.na(activityFull[,i]))))
}

# Only the column "steps˜ have NA Values

# let's see what is the percentage os Na values
print(paste("Percentage of NA values",round((NoOfNAs/nrow(activityFull))*100),"%"))

#Strategy to fill the missing value
# I'lll create a new dataframe  just filling the missing values with the average value
activityfull %>% group_by(interval)

activityOK = merge(activityFull,IntervalActivity, by="interval") 
activityOK$steps[is.na(activityOK$steps)] = activityOK$meanSteps[is.na(activityOK$steps)]
head(activityOK)
activityOK =  activityOK[,1:3]
head(activityOK)

sumStepsActivityOK = activityOK  %>% group_by(date) %>% summarise(totalSteps = sum(steps))
head(sumStepsActivityOK)

colores=c(rbind(colors()[grepl("orange",colors())][1:15],colors()[grepl("blue",colors())][1:15]))
g=ggplot(sumStepsActivity,aes(totalSteps)) + geom_histogram(fill=colores) 
g = g + labs(x="Number of steps taken in a day", y="Number of Occurrences", 
             title="Analysis of the occurrence of the number of steps in a daye" )
g = g + theme_get() 
print(g)

activityOKx = mutate(activityOK,date=as.Date(date, format = "%Y-%m-%d"))
require(lubridate)
activityOKx = mutate(activityOK,weekday=weekdays(as.Date(date, format = "%Y-%m-%d")),
                                wd=wday(as.Date(date, format = "%Y-%m-%d")),
                               kindofday=ifelse(wday(as.Date(date, format = "%Y-%m-%d"))==1 | wday(as.Date(date, format = "%Y-%m-%d")) == 7,"weekend","weekday"))

activity2 = activityOK %>% mutate(kindofday=ifelse(wday(as.Date(date, format = "%Y-%m-%d"))==1 | wday(as.Date(date, format = "%Y-%m-%d")) == 7,"weekend","weekday"))



head(activity2)


IntervalActivityKD = activity2  %>% group_by(interval,kindofday) %>% summarise(meanSteps = mean(steps))
head(IntervalActivityKD)

g=ggplot(IntervalActivityKD,aes(interval,meanSteps)) + geom_line(aes(colour=kindofday))

g = g + labs(x="Average steps", y="intervals (5 min)", 
             title="Daily activity (Weekends x Weekdays)" )
g = g + theme_get() + facet_grid(kindofday ~ .)
print(g)
