---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
# Loading and preprocessing the data
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
dt <- read.csv("activity.csv", header = T)
# Checking the data
dim(dt)
```

```
## [1] 17568     3
```

```r
str(dt)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(dt)
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

```r
tail(dt)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

```r
# checking the missing values
missing_dt <- dt[is.na(dt$steps),]
dim(missing_dt)
```

```
## [1] 2304    3
```


## What is mean total number of steps taken per day?

```r
#1. Calculate the total number of steps taken per day
# The data without any missing values
dt1 <- dt[!is.na(dt$steps),]

# Calculate the total number of steps taken per day
total_number_steps <- with(dt, tapply(steps, as.factor(dt$date), sum, na.rm = T))
#2. Make a histogram of the total number of steps taken each day
hist(total_number_steps, main = "Histogram of total number of steps taken per day", xlab = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
#3. Calculate and report the mean and median of the total number of steps taken per day
summary(total_number_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

## What is the average daily activity pattern?

```r
#1. Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

mean_steps <- with(dt1, tapply(steps, dt1$interval, mean))
interval <- levels(as.factor(dt1$interval))
plot(interval, mean_steps, type = "l", main = "Time series plot of the \n average number of steps taken", xlab = "interval", ylab = "Mean steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

table <- data.frame(mean_steps, interval)
table[table$mean_steps==max(table$mean_steps),][2]
```

```
##     interval
## 835      835
```

## Imputing missing values

```r
#1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
# total number of missing values in the dataset
length(missing_dt$steps)
```

```
## [1] 2304
```

```r
#2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# in this exercise I am going to replace the missing values by the the average number of steps taken, averaged across all days.
# Using this method we do not affect this data
mean_steps <- with(dt1, tapply(steps, dt1$interval, mean))
missing_dt$steps <- mean_steps

#3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
#Create a new dataset that is equal to the original dataset but with the missing data filled in.

new_dt <- rbind(dt1, missing_dt)
new_dt <- new_dt[order(new_dt$date), ]

#4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

total_number_steps2 <- with(new_dt, tapply(steps, as.factor(new_dt$date), sum))
#Make a histogram of the total number of steps taken each day
hist(total_number_steps2, main = "Histogram of total number of steps taken per day", xlab = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#5.Calculate and report the mean and median of the total number of steps taken per day.
#Mean and median total number of steps taken per day WITHOUT filling in the missing values

summary(total_number_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

```r
#Mean and median total number of steps taken per day WITH filling in the missing values

summary(total_number_steps2)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

## Are there differences in activity patterns between weekdays and weekends?

```r
#1.Create a new factor variable in the dataset with two levels - “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

new_dt$days <- weekdays(as.Date(new_dt$date))
# find weekend features in the dataset
weekend_feature <- grep("Saturday|Sunday", new_dt$days, ignore.case = T)
# subset data of the weekend
weekend_dt<-  new_dt[weekend_feature, ]
weekend_dt$weekday <- "weekend"

# subset data of the weekday
weekday_dt <- subset(new_dt,new_dt$days!=weekend_feature)
```

```
## Warning in new_dt$days != weekend_feature: longer object length is not a
## multiple of shorter object length
```

```r
weekday_dt$weekday <- "weekday"

# create a new dataset containing 2 new variable "days" and weekday" 
# - days: indicates the days in the week
# - weekday: indicate the days are at the "weekend" or "weekday"
new_dt2 <- rbind(weekday_dt, weekend_dt)
```

```r
#2.Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

mean_number_steps <- aggregate(steps~ interval+weekday, new_dt2, mean)
g <- ggplot2::qplot(interval, steps, data = mean_number_steps, facets = weekday~.)
g + ggplot2::geom_line(size = 1) + ggplot2::ylab("Mean steps") + ggplot2::ggtitle("Average number of steps taken, \n averaged across all weekday days or weekend days ")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
##References
1.R Programming for Data Science, Roger D. Peng.
2.Exploratory Data Analyis with R, Roger D. Peng
3.Report Writing for Data Science in R, Roger D. Peng
