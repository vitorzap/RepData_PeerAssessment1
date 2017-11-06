# Reproducible Research: Peeer Assessment 1
Vitor Zamprogno amancio Pereira  
11/5/2017  

This R Markdown document is the results of Reproducible Research Peer-graded Assignment: Course Project 1
submitted by Vitor Zamprogno Amancio Pereiras


This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip


The variables included in this dataset are:

-**steps**: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰)
-**date**: The date on which the measurement was taken in YYYY-MM-DD format
-**interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### Initial settings

```r
setwd("~/Documents/DataAnalyst/CouseraCourse/Reproducible-research/RepData_PeerAssessment1")
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
require(dplyr)
```

```
## Loading required package: dplyr
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## Loading and preprocessing the data


```r
if (!file.exists("activity.zip")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","activity.zip", method="curl")
}
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}
activityFull=read.csv("activity.csv")
paste("ADataset with ",nrow(activityFull)," rows")  
```

```
## [1] "ADataset with  17568  rows"
```

```r
head(activityFull)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
