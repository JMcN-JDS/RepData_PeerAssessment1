# Course Project 1
==================

## Loading and preprocessing the data.

### Load the data (i.e. read.csv())

```r
url <- "/Users/jonathan.mcnulty/activity.csv"

activity <- read.csv(url, header = T, sep = ",")
```

### Process/transform the data (if necessary) into a format suitable for your analysis.


```r
library(lubridate)
activity$date <- ymd(activity$date)
```

## What is mean total number of steps taken per day?

### Calculate the total number of steps taken per day.


```r
totalStepsPerDay <- tapply(activity$steps, activity$date, FUN = sum, na.rm = T)
```

### Make a histogram of the total number of steps taken each day.


```r
library(ggplot2)
qplot(totalStepsPerDay, binwidth=1000, xlab="total number of steps taken each day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

### Calculate and report the mean and median of the total number of steps taken per day.


```r
mean(totalStepsPerDay, na.rm = T)
```

```
## [1] 9354.23
```

```r
median(totalStepsPerDay, na.rm = T)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

### Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
library(ggplot2)
means <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval),
                      FUN = mean, na.rm = T)
ggplot(data = means, aes(x = interval, y = steps)) +
        geom_line() +
        xlab("Five-Minute intervals") +
        ylab("Mean Number of Steps Taken for each Interval")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

### Which 5-minute interval, on average, across all the days in the dataset, contains the maximum number of steps?


```r
means[which.max(means$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values.

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)


```r
NAs <- is.na(activity$steps)
table(NAs) # How many missing
```

```
## NAs
## FALSE  TRUE 
## 15264  2304
```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.



### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
fillActivity <- function(steps, interval) {
        imputedVal <- NA
        if (!is.na(steps))
                imputedVal <- c(steps)
        else
                imputedVal <- (means[means$interval == interval, "steps"])
        return(imputedVal)
}
filledActivity <- activity
filledActivity$steps <- mapply(fillActivity, filledActivity$steps, filledActivity$interval)
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalStepsPerDay <- tapply(filledActivity$steps, filledActivity$date, FUN = sum)
qplot(totalStepsPerDay, binwidth = 1000, xlab="Mean Number of Steps Taken per Day")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

### ... and calculate and report the mean and median total number of steps taken per day.


```r
mean(totalStepsPerDay)
```

```
## [1] 10766.19
```

```r
median(totalStepsPerDay)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
daydummy <- function(date) {
        dayCategory  <- weekdays(date)
        if (dayCategory  %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
                return("weekday")
        else if (dayCategory  %in% c("Saturday", "Sunday"))
                return("weekend")
        else
                stop("invalid date")
}
```

### Make a panel plot containing a time series plot.


```r
library(stats)
means <- aggregate(steps ~ interval + dayCategory, data = filledActivity, mean)
```

```
## Error in eval(expr, envir, enclos): object 'dayCategory' not found
```

```r
library(ggplot2)
ggplot(means, aes(interval, steps)) + geom_line() + facet_grid(dayCategory ~ .) +
        xlab("5-minute interval") + ylab("Number of steps")
```

```
## Error in layout_base(data, rows, drop = drop): At least one layer must contain all variables used for facetting
```

```r
filledActivity$date <- as.Date(filledActivity$date)
filledActivity$dayCategory <- sapply(filledActivity$date, FUN = daydummy)

means <- aggregate(steps ~ interval + dayCategory , data = filledActivity, mean)
ggplot(means, aes(interval, steps)) + geom_line() + facet_grid(dayCategory  ~ .) +
        xlab("Five-Minute intervals") + ylab("Number of Steps Taken for each Interval")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)





