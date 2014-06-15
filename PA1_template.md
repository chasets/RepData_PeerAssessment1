# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(ggplot2)
setwd("~/Documents/Aginity/classes/reproducible_research")
f <- 'assignment_1/RepData_PeerAssessment1/activity.csv'
df <- read.csv(f)
df$date=as.Date(df$date)
```

## What is mean total number of steps taken per day?


```r
qplot(steps, data=df, geom="histogram", binwidth=30)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean_steps <- mean(df$steps, na.rm=TRUE)
median_steps <- median(df$steps, na.rm=TRUE)
```
**The mean number of daily steps is 37.3826.**



**The median number of daily steps is 0.**


## What is the average daily activity pattern?


```r
# get the mean steps by interval
step_agg <- aggregate(df$steps, by=list(df$interval), FUN=mean, na.rm=TRUE)
colnames(step_agg) <- c("interval", "mean_steps")

ggplot(step_agg, aes(interval, mean_steps)) + geom_line() 
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
max_steps <- max(step_agg$mean_steps)
interval_with_max_steps <- step_agg[step_agg$mean_steps == max_steps, 1]
```

**The interval with the maximum number of steps (averaged across all days) is 835.**

## Imputing missing values

```r
rows_with_na <- sum(!complete.cases(df))

# impute missing to the daily mean
daily_means <- aggregate(df$steps, by=list(df$date), FUN=mean, na.rm=TRUE)
colnames(daily_means) <- c("date", "daily_mean")

# however there are 8 missing days, so first impute those to the overall mean
daily_means$daily_mean[is.na(daily_means$daily_mean)] <- mean(df$steps, na.rm=TRUE)

# line up everything in a single dataframe
df2 <- merge(x=df, y=daily_means, by="date", all.x=TRUE)

# replace all of the NA steps with their daily_mean
df2$steps[is.na(df2$steps)] <- df2$daily_mean[is.na(df2$steps)]

# give the same structure as the original df
new_df <- df2[,c(2,1,3)]
```
**The number of rows that have NA values is 2304. Only 'steps' has NA values. **


## Are there differences in activity patterns between weekdays and weekends?
