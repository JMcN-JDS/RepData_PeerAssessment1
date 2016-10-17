rm(list=ls())

## 1. Loading and preprocessing the data.

### (i) Load the data (i.e. read.csv())
url <- "/Users/jonathan.mcnulty/activity.csv"

activity <- read.csv(url, header = T, sep = ",")

### (ii) Process/transform the data (if necessary) into a format suitable for your analysis.
activity$date <- ymd(activity$date)


## 2. What is mean total number of steps taken per day?

### (i) Calculate the total number of steps taken per day.
totalStepsPerDay <- tapply(activity$steps, activity$date, FUN = sum, na.rm = T)

### (ii) Make a histogram of the total number of steps taken each day.
library(ggplot2)
qplot(totalStepsPerDay, binwidth=1000, xlab="total number of steps taken each day")

### (iii) Calculate and report the mean and median of the total number of steps taken per day.
mean(totalStepsPerDay, na.rm = T)
median(totalStepsPerDay, na.rm = T)


## 3. What is the average daily activity pattern?

### (i) Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-.axis)
means <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval),
                      FUN = mean, na.rm = T)

library(ggplot2)
ggplot(data = means, aes(x = interval, y = steps)) +
        geom_line() +
        xlab("Five-Minute intervals") +
        ylab("Mean Number of Steps Taken for each Interval")

### (ii) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
means[which.max(means$steps), ]


##4. Imputing missing values

### (i) Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰.s)
NAs <- is.na(activity$steps)
# How many missing
table(NAs)

### (ii) Devise a strategy for filling in all of the missing values in the dataset.
activity[, 1] <- lapply(activity, function(x) { 
        x[is.na(x)] <- mean(x, na.rm = T)
        x
})

### (iii) Create a new dataset that is equal to the original dataset but with the missing data filled in.
# Replace each missing value with the mean value of its 5-minute interval
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

### (iv) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
#        Do these values differ from the estimates from the first part of the assignment? 
#        What is the impact of imputing missing data on the estimates of the total daily number of steps?
totalStepsPerDay <- tapply(filledActivity$steps, filledActivity$date, FUN = sum)
qplot(totalStepsPerDay, binwidth = 1000, xlab="Mean Number of Steps Taken per Day")
mean(totalStepsPerDay)
median(totalStepsPerDay)


## 5. Are there differences in activity patterns between weekdays and weekends?

### (i) Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
library(lubridate)
daydummy <- function(date) {
        dayCategory  <- weekdays(filledActivity$date)
        if (dayCategory  %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
                return("weekday")
        else if (dayCategory  %in% c("Saturday", "Sunday"))
                return("weekend")
        else
                stop("invalid date")
}
filledActivity$date <- as.Date(filledActivity$date)
filledActivity$dayCategory <- sapply(filledActivity$date, FUN = daydummy)

### (ii) Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
#See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
library(stats)
means <- aggregate(steps ~ interval + dayCategory, data = filledActivity, mean)
library(ggplot2)
ggplot(means, aes(interval, steps)) + geom_line() + facet_grid(dayCategory  ~ .) +
        xlab("Five-Minute intervals") + ylab("Number of Steps Taken for each Interval")

filledActivity$dayCategory <- ifelse(as.POSIXlt(filledActivity$date)$wday %in% c(0,6), 'weekend', 'weekday')

library(stats)
meanFilledActivity <- aggregate(steps ~ interval + dayCategory, data=filledActivity, mean)
ggplot(meanFilledActivity, aes(interval, steps)) + 
        geom_line() + 
        facet_grid(dayCategory ~ .) +
        xlab("5-minute interval") + 
        ylab("average number of steps")


################################################################################################

# Moving the R file to the current wd
my.file.rename <- function(from, to) {
        todir <- dirname(to)
        if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
        file.rename(from = from,  to = to)
}

my.file.rename(from = "/Users/jonathan.mcnulty/Desktop/PA1_template.Rmd",
               to = "/Users/jonathan.mcnulty/PA1_template.Rmd")

# Processing R markdown file with knit2html() function in R, running it in console. 
library(knitr)
knit("/Users/jonathan.mcnulty/PA1_template.Rmd")
knit2html("/Users/jonathan.mcnulty/PA1_template.Rmd")







