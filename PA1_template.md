---
title: "Reproductible research project 1"
author: "anonymous"
date: "26/09/2021"
output: html_document
---

```{r_setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


## Loading and preprocessing the data

The data is loaded using the read.csv function and stored in an object called "data"

```{r_loading, echo=TRUE}
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

Calculation of the total number of steps taken each day:

```{r_calculation_steps, echo=TRUE}
Stepsbydate <- aggregate(steps ~ date, data, sum)
```

Histogram representing the distribution:

```{r_plot_steps, echo=TRUE}
library(ggplot2)
p <- ggplot(Stepsbydate, aes(date, steps)) + geom_col(colour="white") + scale_x_discrete(guide = guide_axis(angle = 90))
p + labs(title="Total steps per day")
```

![Test Image 1](https://github.com/FB85/RepData_PeerAssessment1/blob/master/plot1.png)

Calculate and report the mean and median of the total number of steps taken per day

```{r_mean_median, echo=TRUE}
mean(Stepsbydate$steps)
## [1] 10766.19

median(Stepsbydate$steps)
## [1] 10765
```

## What is the average daily activity pattern?

Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r_time_series_plot, echo=TRUE}
Stepsbyinterval <- aggregate(steps ~ interval, data, mean)
plot(Stepsbyinterval$interval, Stepsbyinterval$steps, type = "l", xlab = "Time", ylab = "Steps", main = "Average daily activity")
```

![Test Image 2](https://github.com/FB85/RepData_PeerAssessment1/blob/master/plot2.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r_find_max_interval, echo=TRUE}
### find the max value
max(Stepsbyinterval$steps)
## [1] 206.1698

### find the interval with that value
subset(Stepsbyinterval, steps > 206)
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

```{r_nas, echo=TRUE}
sum(is.na(data))
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset:
I'll use the mean of the same interval

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r_imputing_nas, echo=TRUE}
### calculate mean by interval in a new dataframe
dfmeaninterval <- aggregate(data$steps, list(data$interval), na.rm = TRUE, mean)
names(dfmeaninterval) = c("interval","mean")

### merge with original dataframe
datawithmean <- merge(data, dfmeaninterval, by.x = "interval")

### add column to impute missing values
datawithmean$newsteps <- ifelse(is.na(datawithmean$steps), datawithmean$mean, datawithmean$steps)
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r_new_histogram, echo=TRUE}
### calculate total step by interval of the column with nas imputed
Stepsbydate2 <- aggregate(newsteps ~ date, datawithmean, sum)

### plot using ggplot2
p <- ggplot(Stepsbydate2, aes(date, newsteps)) + geom_col(colour="white") + scale_x_discrete(guide = guide_axis(angle = 90))
p + labs(title="Total steps per day")
```

Calculate new mean and median

```{r_new_mean_median, echo=TRUE}
mean(Stepsbydate2$newsteps)
median(Stepsbydate2$newsteps)
```

Those numbers almost don't differ from the original numbers omitting the missing values, this method does not have a big impact on them.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r_new_factor, echo=TRUE}
### convert date to date format
datawithmean$date <- as.Date(as.character(datawithmean$date), format = "%Y-%m-%d")

### add column with each day of the week
datawithmean$weekday <- weekdays(datawithmean$date)

### add column to determine the type of day
datawithmean$day <- ifelse(datawithmean$weekday == "samedi" | datawithmean$weekday == "dimanche" , "weekend", "weekday")
```

Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```{r_panel_plot, echo=TRUE}
### calculate total steps by interval + type of day
Stepsbyinterval2 <- aggregate(newsteps ~ interval + day, datawithmean, mean)

### plot data
ggplot(Stepsbyinterval2, aes(interval, newsteps), facets = day ~ .,) + geom_line() + facet_grid(rows = vars(day))
```

