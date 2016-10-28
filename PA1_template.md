Loading and preprocessing the data
----------------------------------

Show any code that is needed to Load the data (i.e. read.csv())
Process/transform the data (if necessary) into a format suitable for
your analysis

    library(ggplot2)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    setwd("~/R/RepData_PeerAssessment1-master")
    list.files()
    activity <- read.csv("activity.csv", stringsAsFactors = F)
    str(activity)

    ##  [1] "activity.csv"           "activity.zip"          
    ##  [3] "doc"                    "instructions_fig"      
    ##  [5] "PA1_template (2).Rmd"   "PA1_template.html"     
    ##  [7] "PA1_template__2_.md"    "PA1_template__2_.Rmd"  
    ##  [9] "PA1_template__2__files" "README.md"             
    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

What is mean total number of steps taken per day?
-------------------------------------------------

Calculate the total number of steps taken per day. Make a histogram of
the total number of steps taken each day. Mean and median of the total
number of steps taken per day.

    total.steps <- tapply(activity$steps, activity$date, FUN = sum, na.rm = TRUE)
    qplot(total.steps, binwidth = 1000, xlab = "total number of steps taken each day")

![](PA1_template__2__files/figure-markdown_strict/unnamed-chunk-2-1.png)

    step_mean <- mean(total.steps, na.rm = TRUE)
    step_median <- median(total.steps, na.rm = TRUE)
    step_mean
    step_median

    ## [1] 9354.23
    ## [1] 10395

What is the average daily activity pattern?
-------------------------------------------

Make a time series plot (i.e. type = "l") of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all days
(y-axis). Which 5-minute interval, on average across all the days in the
dataset, contains the maximum number of steps?

      averages <- aggregate(x=list(steps=activity$steps),
                            by=list(interval=activity$interval), 
                            FUN=mean, na.rm = TRUE)
      ggplot(data = averages, aes(x = interval, y = steps)) + geom_line() + xlab("5_minute_interval") + 
          ylab("average number of steps taken")

![](PA1_template__2__files/figure-markdown_strict/unnamed-chunk-3-1.png)

    averages[which.max(averages$steps), ]

    ##     interval    steps
    ## 104      835 206.1698

Imputing missing values
-----------------------

Devise a strategy for filling in all of the missing values in the
dataset. Create a new dataset that is equal to the original dataset but
with the missing data filled in. Make a histogram

    str(activity)
    fill <- function(steps, interval) {
        filled <- NA
        if (!is.na(steps)) 
            filled <- c(steps) else filled <- (averages[averages$interval == interval, "steps"])
        return(filled)
    }
    filled.data <- activity
    filled.data$steps <- mapply(fill, filled.data$steps, filled.data$interval)

    total.steps <- tapply(filled.data$steps, filled.data$date, FUN = sum)
    qplot(total.steps, binwidth = 1000, xlab = "total number of steps taken each day")

![](PA1_template__2__files/figure-markdown_strict/unnamed-chunk-4-1.png)

    mean(total.steps)
    median(total.steps)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ## [1] 10766.19
    ## [1] 10766.19

Differences in activity patterns between weekdays and weekends?
---------------------------------------------------------------

For this part the weekdays() function may be of some help here. Use the
dataset with the filled-in missing values for this part. In this part I
create new variables weekday and DayType to find if weekend or wekday.
The I do the plots.

    weekday.or.weekend <- function(date) {
        day <- weekdays(date)
        if (day %in% c("lunes", "martes", "miércoles", "jueves", "viernes")) 
            return("weekday") else if (day %in% c("sábado", "domingo")) 
            return("weekend") else stop("invalid date")
    }

    filled.data$date <- as.Date(filled.data$date)
    filled.data$day <- sapply(filled.data$date, FUN = weekday.or.weekend)

    averages <- aggregate(steps ~ interval + day, data = filled.data, mean)
    ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
        xlab("5-minute interval") + ylab("Number of steps")

![](PA1_template__2__files/figure-markdown_strict/unnamed-chunk-5-1.png)

    head(filled.data)
    table(filled.data$day)

    ##       steps       date interval     day
    ## 1 1.7169811 2012-10-01        0 weekday
    ## 2 0.3396226 2012-10-01        5 weekday
    ## 3 0.1320755 2012-10-01       10 weekday
    ## 4 0.1509434 2012-10-01       15 weekday
    ## 5 0.0754717 2012-10-01       20 weekday
    ## 6 2.0943396 2012-10-01       25 weekday
    ## 
    ## weekday weekend 
    ##   12960    4608
