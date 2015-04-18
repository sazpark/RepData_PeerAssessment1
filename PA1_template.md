---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
unzip("activity.zip", exdir = "activity_data")
activity <- read.csv("activity_data/activity.csv")
```


```r
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
unique_dates <- unique(activity$date)
ndays <- length(unique_dates)
StepsPerDay <- numeric(ndays)
for (i in 1:ndays) {
        StepsPerDay[i] <- sum(activity$steps[activity$date==unique_dates[i]], na.rm=TRUE)
}
```

Slide 2
=======


```r
hist(StepsPerDay, breaks=10)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
summary(StepsPerDay)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

## What is mean total number of steps taken per day?

The mean number of steps taken per day is 9354.

## What is the average daily activity pattern?


```r
unique_intervals <- unique(activity$interval)
nintervals <- length(unique_intervals)
StepsPerInterval <- numeric(nintervals)
for (i in 1:nintervals) {
        StepsPerInterval[i] <- sum(activity$steps[activity$interval==unique_intervals[i]], na.rm=TRUE)
}
FiveMinuteInterval <- 1:nintervals
plot(FiveMinuteInterval,StepsPerInterval[FiveMinuteInterval],type="l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

The 104rd 5-minute interval, on average across all the 
days in the dataset, contains the maximum number of steps (1.0927 &times; 10<sup>4</sup>).

## Imputing missing values


```r
ok <- complete.cases(activity)
```

The number of rows with NAs is: 2304.

We will fill in the 5-minute intervals that have NA values with the mean value for the day.

```r
nimpute <- sum(!ok)
impute_indeces <- which(is.na(activity$steps))
for (i in 1:nimpute) {
        daynumber <- impute_indeces[i] %/% 288.0001 + 1
        activity$steps[impute_indeces[i]] <- StepsPerDay[daynumber]
}

StepsPerDayImputed <- numeric(ndays)
for (i in 1:ndays) {
        StepsPerDayImputed[i] <- sum(activity$steps[activity$date==unique_dates[i]], na.rm=TRUE)
}

summary(StepsPerDayImputed)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```
It doesn't appear to make a difference (apparently when there is a 5-minute interval missing, the
data for the entire day are missing)


## Are there differences in activity patterns between weekdays and weekends?



```r
require(lubridate)
```

```
## Loading required package: lubridate
```

```r
activity$day = ifelse(wday(activity$date)==1,7,wday(activity$date)-1)

activity$weekend <- factor(activity$day>4)
levels(activity$weekend)[levels(activity$weekend)=="TRUE"] <- "weekend"
levels(activity$weekend)[levels(activity$weekend)=="FALSE"] <- "weekday"
```

weekday_segments <- which(activity$weekend=="weekday")
weekend_segments <- which(activity$weekend=="weekend")

unique_intervals_weekend <- unique(activity$interval[weekend_segments])
unique_intervals_weekday <- unique(activity$interval[weekday_segments])

nintervals_weekend <- length(unique_intervals_weekend)
nintervals_weekday <- length(unique_intervals_weekday)

StepsPerIntervalWeekend <- numeric(nintervals_weekend)
StepsPerIntervalWeekday <- numeric(nintervals_weekday)

for (i in 1:nintervals_weekend) {
        StepsPerIntervalWeekend[i] <- sum(activity$steps[weekend_segments[i]])
}

for (i in 1:nintervals_weekday) {
        StepsPerIntervalWeekday[i] <- sum(activity$steps[weekday_segments[i]])
}

FiveMinuteIntervalWeekend <- 1:nintervals_weekend
plot(FiveMinuteIntervalWeekend,StepsPerIntervalWeekend[FiveMinuteIntervalWeekend],type="l")
FiveMinuteIntervalWeekday <- 1:nintervals_weekday
plot(FiveMinuteIntervalWeekday,StepsPerIntervalWeekday[FiveMinuteIntervalWeekday],type="l")
