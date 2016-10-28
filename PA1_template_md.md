Loading and preprocessing the data
----------------------------------

    library(ggplot2)
    data <- read.csv("activity.csv")

What is mean total number of steps taken per day?
-------------------------------------------------

    totalstep <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
    qplot(totalstep, binwidth=1000, xlab="steps taken each day")

![](PA1_template_md_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    mean(totalstep, na.rm=TRUE)
    median(totalstep, na.rm=TRUE)

    ## [1] 9354.23
    ## [1] 10395

Average daily activity pattern
------------------------------

    averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                          FUN=mean, na.rm=TRUE)
    ggplot(data=averages, aes(x=interval, y=steps)) +
        geom_line() +
        xlab("5-minute interval") +
        ylab("average number of steps taken")

![](PA1_template_md_files/figure-markdown_strict/unnamed-chunk-2-1.png)

On average across all the days in the dataset, the 5-minute interval
contains the maximum number of steps?

    averages[which.max(averages$steps),]

    ##     interval    steps
    ## 104      835 206.1698

Imputing missing values
-----------------------

    missing <- is.na(data$steps)
    table(missing)

    ## missing
    ## FALSE  TRUE 
    ## 15264  2304

Fill the missing values with the mean valu.

    fill.value <- function(steps, interval) {
        filled <- NA
        if (!is.na(steps))
            filled <- c(steps)
        else
            filled <- (averages[averages$interval==interval, "steps"])
        return(filled)
    }
    filled.data <- data
    filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

Histogram of the total number of steps taken each day and calculate the
mean and median total number of steps.

    totalstep <- tapply(filled.data$steps, filled.data$date, FUN=sum)
    qplot(totalstep, binwidth=1000, xlab="total number of steps taken each day")

![](PA1_template_md_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    mean(totalstep)
    median(totalstep)

    ## [1] 10766.19
    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

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

![](PA1_template_md_files/figure-markdown_strict/unnamed-chunk-6-1.png)
