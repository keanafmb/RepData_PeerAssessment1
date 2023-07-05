---
title: "Project 1 - Reproducible Research"
date: "2023-07-05"
author: Keana Bautista
output: 
    html_document:
        df_print: paged
        keep_md: yes
---
=========================



## Loading and pre-processing the data
We will first read the csv file and omit the NA values

```r
activity <- read.csv("activity.csv")
c_data <- na.omit(activity) 
c_data$date <- as.Date(c_data$date)
```

## What is the mean total number of steps taken per day?
To calculate the mean total number of steps taken per day, group first the data by date and take the sum of steps taken per day.

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
grouped_sum <- c_data %>% group_by(date) %>% summarise(sum_steps = sum(steps))
df <- grouped_sum %>% as.data.frame()
```

Here's a histogram of the number of steps taken each day.


```r
hist(df$sum_steps, main='Histogram of number of steps taken each day', xlab='Number of steps', ylab =' Count')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Then get the mean and median of the number of steps taken each day


```r
df_mean <- mean(df$sum_steps)
df_median <- median(df$sum_steps)
```

## What is the average daily activity pattern?
To get a time series plot of the average number of steps taken, group the data by interval and get the mean of the steps.


```r
library(dplyr)
grouped_mean <- c_data %>% group_by(interval) %>% summarise(mean_steps = mean(steps))
plot(grouped_mean, type="l", xlab='Five-minute Intervals', ylab='Average number of steps', main='Average Number of Steps Taken')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

To get the 5-minute interval that, on average, contains the maximum number of steps, use the function which.max()

```r
max_step_interval <- grouped_mean$interval[which.max(grouped_mean$mean_steps)]
```
The 5-minute interval the has the highest average number of steps was during 835

## Inputing missing values
Get the total number of missing values

```r
missing_data <- sum(is.na(activity$steps))
```

To fill in the missing (NA) data, we will get the mean of the number of steps of that particular interval over all days and assign them to the corresponsing NAs.


```r
mean_missing_data<- activity

for(i in 1:nrow(mean_missing_data)){
    if(is.na(mean_missing_data$steps[i])){
      x <- which(mean_missing_data$interval[i] == grouped_mean$interval)
      mean_missing_data$steps[i] <- grouped_mean[x,]$mean_steps
    }
}

mean_missing_data$date <-as.Date(mean_missing_data$date)
```

Group the filled data set by date and get the sum of steps


```r
library(dplyr)
mean_missing_steps <- mean_missing_data %>%
    group_by(date) %>%
    summarise(sum_steps=sum(steps))
```

Here's a histogram of the filled data set

```r
hist(mean_missing_steps$sum_steps, main='Histogram of number of steps taken each day', xlab='Number of steps', ylab = 'Count')
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

The mean and median of the total number of steps taken each day are as follows:


```r
filldata_mean <- mean(mean_missing_steps$sum_steps)
filldata_median <- median(mean_missing_steps$sum_steps)
```

The computed mean is 10766.1886792453 while the median is 10766.1886792453. Mean remains unchanged by the median has increased slightly since we input missing data based on average.

## Are there differences in activity patterns between weekdays and weekends?

First, create a weekday variable and define the days using weekdays() function. 

```r
mean_missing_data$day <- weekdays(mean_missing_data$date)
mean_missing_data$daytype <- "weekday"
mean_missing_data$daytype[mean_missing_data$day %in% c('Saturday', 'Sunday')] <- "weekend"
```

To calculate the mean weekday steps and the mean weekend steps, group the data by 5 minute interval


```r
library(dplyr)
daytype_mean <- mean_missing_data %>%
    group_by(daytype,interval) %>%
    summarise(mean_steps=mean(steps))
```

```
## `summarise()` has grouped output by 'daytype'. You can override using the
## `.groups` argument.
```

Here's a time series plot of  weekday vs weekends using ggplot2


```r
library(ggplot2)
qplot(interval, mean_steps, data=daytype_mean, type='l', main=' Average number of steps taken in weekdays vs. weekends', geom='line',
      xlab="Interval", ylab="Average number of steps taken", facets=daytype ~.)
```

```
## Warning: `qplot()` was deprecated in ggplot2 3.4.0.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```
## Warning in geom_line(type = "l"): Ignoring unknown parameters: `type`
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
