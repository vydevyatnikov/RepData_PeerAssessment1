---
title: "Reproducible Research: Peer Assessment 1"
author: "vydevyatnikov"
output: 
  html_document:
    keep_md: true
---



# Loading data and preprocessing data


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
library(ggplot2)
library(zoo)
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
library(chron)
```

```
## NOTE: The default cutoff when expanding a 2-digit year
## to a 4-digit year will change from 30 to 69 by Aug 2020
## (as for Date and POSIXct in base R.)
```

```r
data <- read.csv("activity.csv")
data$date <- as.Date(data$date, format = "%Y-%m-%d")
start <- as.POSIXlt("2012-10-01 00:00:00")
end <- as.POSIXlt("2012-11-30 23:55:00")
data <- mutate(data, time = seq(start, end, by = 300))
```

# Calculating mean total number of steps taken per day


```r
total_sum <- sum(data$steps, na.rm = TRUE)
data_with_NA <- aggregate(formula = steps~date, FUN = sum, data = data)
hist(data_with_NA$steps, main="Total number of steps per day", xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
Mean <- mean(data_with_NA$steps)
Median <- median(data_with_NA$steps)
```

**From calculations above we can see that the total number of steps taken during the observation period is equal to *570608*. Also we calculated mean of total number of steps which is equal to *1.0766189\times 10^{4}* and median which is equal to *10765*.**

# Average daily activity pattern


```r
data_for_plot <- data.frame(avg_steps = aggregate(formula = steps ~ interval, FUN = mean, data = data))
names(data_for_plot) <- c("time", "steps")
ggplot(data = data_for_plot, aes(x = time, y = steps)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
max_steps <- data_for_plot$time[which.max(data_for_plot$steps)]
```
**The interval *835* contains the maximum number of steps on average.**


# Imputing missing values

**We will fill NA with closest further non-NA value. That's our *strategy* :)**


```r
rows_with_NA <- nrow(data[is.na(data),])
data2 <- data
data2$steps <- na.locf(data2$steps, na.rm = FALSE, fromLast = TRUE)
data2$steps <- na.locf(data2$steps, na.rm = FALSE)
data_for_plot_with_no_NA <- aggregate(formula = steps ~ date, FUN = sum, data = data2)
z <- seq(from = as.Date("2012-10-01"), to = as.Date("2012-11-30"), by = 10)
ggplot(aes(x = as.Date(date), y = steps), data = data_for_plot_with_no_NA) + geom_histogram(stat = "identity") + scale_x_date(labels = z, breaks = z)
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
mean_steps_per_day <- mean(data_for_plot_with_no_NA$steps)
median_steps_per_day <- median(data_for_plot_with_no_NA$steps)
```

**The number of rows with missing values is 2304. After filling the NA's we calculated mean and median one more time and results are 9354.2295082 and 10395 respectively. Because of the method we have chosen for filling NA, results are lower than in the case with untouched NA.**

# Comparison of activity patterns in weekdays and weekends


```r
data3 <- mutate(data, weekdays = is.weekend(data$date))
weekdays <- replace(data3$weekdays, list = data3$weekdays, "weekend")
weekdays <- replace(weekdays, list = !data3$weekdays, "weekday")
data <- cbind(data, weekdays)
data3 <- aggregate(formula = steps ~ interval + weekdays, FUN = mean, data = data)
g2 <- ggplot(data = data3, aes(x = interval, y = steps))
g2 + geom_line(stat = "identity") + facet_grid(.~weekdays)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

**As we can see from the graph above, the weekend is characterized by a lower level of physical activity.**
