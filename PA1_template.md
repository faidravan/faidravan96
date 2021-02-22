---
title: "Untitled"
output:
  html_document: 
    keep_md: yes
  pdf_document: default
---



## R Markdown

First we'll store our data in a data frame so we can work more easily:


```r
chart<-read.csv("C:/Users/Φαιδρα/Documents/activity.csv")
chart$date <- as.POSIXct(chart$date, "%Y-%m-%d")
weekday <- weekdays(chart$date)
chart <- cbind(chart,weekday)
str(chart)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ weekday : chr  "Δευτέρα" "Δευτέρα" "Δευτέρα" "Δευτέρα" ...
```

```r
head(chart)
```

```
##   steps       date interval weekday
## 1    NA 2012-10-01        0 Δευτέρα
## 2    NA 2012-10-01        5 Δευτέρα
## 3    NA 2012-10-01       10 Δευτέρα
## 4    NA 2012-10-01       15 Δευτέρα
## 5    NA 2012-10-01       20 Δευτέρα
## 6    NA 2012-10-01       25 Δευτέρα
```

Then we plot a histogram of the total number of steps taken each day where we omit any NA values from the Steps column, :


```r
Steps<- aggregate(steps ~ date, chart, sum, na.rm=TRUE)
hist(Steps$steps, col = "lightgreen",breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

We calculate the mean and median number of steps taken each day:


```r
mean(Steps$steps)
```

```
## [1] 10766.19
```

```r
median(Steps$steps)
```

```
## [1] 10765
```

In this time series plot, we depict the the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):


```r
StepsPerInterval<- aggregate(steps ~ interval, chart, mean, na.rm=TRUE)
plot(StepsPerInterval, col = "darkgreen", type = "l")
```

![](PA1_template_files/figure-html/timeplot-1.png)<!-- -->


```r
MaxInterval<-  StepsPerInterval[which.max(StepsPerInterval$steps),"interval"]
```

The interval with the maximum number of steps is 835

We use the following code to calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs):


```r
NaValues<-sum(is.na(chart$steps))
```

The missing values in the Steps column are 2304.

We calculate the mean of all days, and replace these NA values with this value in the chart:


```r
chart1<-na.omit(chart, chart$steps)
fullmean<-mean(tapply(chart1$steps, chart1$date, mean))
chart[is.na(chart)] <- fullmean
```

The new chart is presented in this histogram:


```r
StepsNew<- aggregate(steps ~ date, chart, sum, na.rm=TRUE)
hist(StepsNew$steps , col = "lightgreen",breaks = seq(0,25000, by=2500), )
```

![](PA1_template_files/figure-html/histogram2-1.png)<!-- -->

The new mean and median values are:


```r
mean(StepsNew$steps)
```

```
## [1] 10766.19
```

```r
median(StepsNew$steps)
```

```
## [1] 10766.19
```
We can see now that the mean does not change after the replacement of the NA values, while the median changes very slightly. We can safely use this method. 

Now we create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
chart$date <- as.Date(strptime(chart$date, format="%Y-%m-%d"))
chart$day <- weekdays(chart$date)
for (i in 1:nrow(chart)) {
    if (chart[i,]$day %in% c("Σάββατο","Κυριακή")) {
        chart[i,]$day<-"weekend"
    }
    else{
        chart[i,]$day<-"weekday"
    }
}
StepsByDay <- aggregate(chart$steps ~ chart$interval + chart$day, chart, mean)
```

Then we make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis):


```r
names(StepsByDay) <- c("interval", "day", "steps")
library(lattice)

xyplot(steps ~ interval | day, StepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps", col = "darkgreen")
```

![](PA1_template_files/figure-html/weekdays hist-1.png)<!-- -->
