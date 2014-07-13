# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data



```r
activityOriginal <- read.csv('activity.csv',stringsAsFactors = F);
activityDF <- activityOriginal[complete.cases(activityOriginal),];
activityDF$date <- as.Date(activityDF$date, format = "%Y-%m-%d");
perDayDF <- aggregate(activityDF$steps, by=list(date = activityDF$date), FUN=sum);
colnames(perDayDF) <- c("date","steps");

avgPerInterval <- aggregate(activityDF$steps, by=list(interval = activityDF$interval), FUN=mean);

colnames(avgPerInterval) <- c("interval","meanSteps");
maxInterval <- avgPerInterval[order(avgPerInterval$meanSteps, decreasing=TRUE), ][1,];
```


## What is mean total number of steps taken per day?


```r
 hist(perDayDF$steps,main="Steps by Day",
       xlab="Number of steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

The mean steps taken per day is:


```r
mean(perDayDF$steps);
```

```
## [1] 10766
```

The median steps taken per day is:


```r
median(perDayDF$steps);
```

```
## [1] 10765
```



## What is the average daily activity pattern?


```r
 plot(avgPerInterval$interval,avgPerInterval$meanSteps,col="black", type="l",
       main="Daily Average Steps by 5 minutes interval",
       ylab="Average Steps",
       xlab="5 minutes interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps is:


```r
maxInterval$interval
```

```
## [1] 835
```
with 


```r
maxInterval$meanSteps
```

```
## [1] 206.2
```
average steps

## Inputing missing values


```r
library(plyr);

missingDF <- ddply(activityOriginal[!complete.cases(activityOriginal),], .(date,interval,steps), transform, steps = round(mean(activityDF$steps[activityDF$interval == interval]), digits = 0));

missingDF$date <- as.Date(missingDF$date, format = "%Y-%m-%d");

completeDF <- rbind(missingDF,activityDF);

completeDF$steps <- as.numeric(completeDF$steps);

perDayCompleteDF <- aggregate(completeDF$steps, by=list(date = completeDF$date), FUN=sum);
colnames(perDayCompleteDF) <- c("date","steps");

hist(perDayCompleteDF$steps,main="Steps by Day (Complete DataFrame)",
       xlab="Number of steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 


## Are there differences in activity patterns between weekdays and weekends?
