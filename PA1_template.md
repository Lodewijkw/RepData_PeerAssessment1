---
title: "PeerRev1-coursera5"
author: "Lodewijk"
date: "January 29, 2018"
output: html_document
---



# Rmarkdown by Lodewijk Wennemers - Coursera data science

ading and preprocessing the data
Show any code that is needed to Load the data (i.e. read.csv())
* Make sure that you have the correct working directory before you run the R code!

```r
dat1<-read.csv("activity.csv")
```
Process/transform the data (if necessary) into a format suitable for your analysis

```r
dat1[,1]<-as.numeric(dat1[,1])
      dat1[,2]<-as.Date(dat1[,2])
      dat2<-aggregate(steps~date,dat1,FUN=sum)
```

#What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1.  Calculate the total number of steps taken per day
2.  If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3.  Calculate and report the mean and median of the total number of steps taken per day

```r
dat2<-aggregate(steps~date,dat1,FUN=sum)
mean(dat2$steps)
```

```
## [1] 10766.19
```

```r
median(dat2$steps)
```

```
## [1] 10765
```

```r
hist(dat2$steps,xlab="number of steps",ylab="number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

#What is the average daily activity pattern?
1.  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
dat3<-aggregate(steps~interval,dat1,FUN=sum)
plot(dat3$interval,dat3$steps,type="l",ylab="number of steps",xlab="interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
which(dat3$steps==max(dat3$steps))
```

```
## [1] 104
```


#Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(dat1))
```

```
## [1] 2304
```

```r
dat2<-dat1[is.na(dat1)]
```
2.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. 

I fill every NA with the average of that day


3.  Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
library(dplyr)
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

```r
meanday3<- dat1 %>% group_by(interval) 
meansteps<-function(x) replace(x,is.na(x),mean(x,na.rm=TRUE))
meanday3<- mutate(meanday3,steps=meansteps(steps))
```
4.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
meanday4<-aggregate(steps~date,meanday3,FUN=sum)

mean(meanday4$steps)
```

```
## [1] 10766.19
```

```r
median(meanday4$steps)
```

```
## [1] 10766.19
```

```r
hist(meanday4$steps)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)
#Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.  Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2.  Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
meanday5<-meanday3
meanday5$WeekendOrWeekday <- ifelse(weekdays(as.Date(meanday3$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")

library(ggplot2)

meanday5 <- (meanday5 %>% group_by(interval, WeekendOrWeekday) %>% summarise(Mean = mean(steps)))
ggplot(meanday5, mapping = aes(x = interval, y = Mean)) + geom_line() +
    facet_grid(WeekendOrWeekday ~.) 
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

