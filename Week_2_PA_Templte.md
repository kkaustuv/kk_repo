# Reproduciable Research - Week 2 Assignment
###1. Load the Data

```r
actv_data<-read.csv("activity.csv",header =TRUE)
```
#####a.Verify the Summary of the data

```r
summary(actv_data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
###2.Process and transform the data into a format suitable for your analysis

```r
actv_trfm_data <- na.omit(actv_data)
```
##What is the mean total of steps taken in a day?
####1. Total number of steps taken per Day

```r
library("dplyr")
```

```
## Warning: package 'dplyr' was built under R version 3.2.5
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
actv_day<-group_by(actv_trfm_data,date)
actv_day<-summarise(actv_day,steps=sum(steps))
```
####2. Histogram of total number of steps taken in a day

```r
library("ggplot2")
qplot(actv_day$steps,xlab="Total Number of Steps per Day",ylab="Count using binwidth 500")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Week_2_PA_Templte_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
------
####3. Mean and Median of Steps taken in a day

```r
mean(actv_day$steps)
```

```
## [1] 10766.19
```

```r
median(actv_day$steps)
```

```
## [1] 10765
```
----

##What is average daily activity Pattern?

####1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
actv_day_avg<-group_by(actv_trfm_data,interval)
actv_day_avg<-summarise(actv_day_avg,avg_steps=mean(steps))
ggplot(actv_day_avg,aes(interval,avg_steps)) +geom_line()
```

![](Week_2_PA_Templte_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
----
####2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
actv_day_avg[actv_day_avg$avg_steps==max(actv_day_avg$avg_steps),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval avg_steps
##      <int>     <dbl>
## 1      835  206.1698
```
----

##Imputing missing values

####1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```r
nrow(actv_data)-nrow(actv_trfm_data)
```

```
## [1] 2304
```
----
####2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
actv_impute<-merge(actv_data,actv_day_avg)
```
----
####3. Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
actv_impute$steps[is.na(actv_impute$steps)]<-actv_impute$avg_steps[is.na(actv_impute$steps)]
```
----
####4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
actv_imp_day<-group_by(actv_impute,date)
actv_imp_day<-summarise(actv_imp_day,steps=sum(steps))
qplot(actv_imp_day$steps,xlab = "Total Number of Steps per Day",ylab = "Count")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](Week_2_PA_Templte_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
mean(actv_imp_day$steps)
```

```
## [1] 10766.19
```

```r
median(actv_imp_day$steps)
```

```
## [1] 10766.19
```

----

##Are there differences in activity patterns between weekdays and weekends?

####1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
actv_impute$dayofweek<-weekdays(as.Date(actv_impute$date))
actv_impute$WeekDay <-as.factor(actv_impute$dayofweek=="Saturday"|actv_impute$dayofweek=="Sunday")
levels(actv_impute$WeekDay)<-c("Weekday","Weekend")
```

----
####2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
actv_weekday<-actv_impute[actv_impute$WeekDay=="Weekday",]
actv_weekend<-actv_impute[actv_impute$WeekDay=="Weekend",]
actv_weekday_avg<-group_by(actv_weekday,interval)
actv_weekday_avg<-summarise(actv_weekday_avg,avg_steps=mean(steps))
actv_weekday_avg$week<-"Weekday"
actv_weekend_avg<-group_by(actv_weekend,interval)
actv_weekend_avg<-summarise(actv_weekend_avg,avg_steps=mean(steps))
actv_weekend_avg$week<-"Weekend"

actv_imp_plot<-rbind(actv_weekday_avg,actv_weekend_avg)
ggplot(actv_imp_plot,aes(interval,avg_steps)) + geom_line() + facet_grid(week ~ .)
```

![](Week_2_PA_Templte_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
