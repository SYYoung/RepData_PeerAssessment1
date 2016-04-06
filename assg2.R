assg2_1 <- function() {
    # step 1: code for reading in the dataset and/or processing data
    act_data <- read.csv("activity.csv",sep=",", stringsAsFactor=FALSE)
    step_sum <- with(act_data, tapply(steps, date, sum, na.rm=TRUE))
    
    # step 2: histogram of the total number of steps taken each day
    hist(step_sum, xlab="total number of steps taken each day")
    
    # step 3: mean and median number of steps taken each day
    mean_step <- mean(step_sum, na.rm=TRUE)
    median_step <- median(step_sum, na.rm=TRUE)
    print(paste("mean of total number of steps taken each day is: ", mean_step))
    print(paste("median of total number of steps taken each day is: ", median_step))

    # step 4,5: time series plot of the average number of steps taken
    five_step_avg <- with(act_data, tapply(steps, interval, mean, na.rm=TRUE))
    plot(names(five_step_avg), five_step_avg, type="l", xlab="5-min interval",
         ylab="average number of steps taken", 
         main="Time series plot of the average number of steps taken")
    max_time <- which(five_step_avg==max(five_step_avg, na.rm=TRUE))
    abline(v=names(five_step_avg[max_time]))
    print(paste(names(max_time), "contains the max num of steps",
                five_step_avg[max_time]))
    
    # step 6: Code to describe and show a strategy for imputing missing data
    # let's see how many NAs are there
    total_na_step <- sum(is.na(act_data$steps))
    print(paste("Total number of missing values in the dataset is: ",total_na_step))
    # set the NAs = mean of that date
    step_avg <- with(act_data, tapply(steps, date, mean, na.rm=TRUE))
    step_avg[is.na(step_avg)] <- 0
    step_na <- is.na(act_data$steps)
    step_na_date <- act_data$date[step_na]
    # all the NAs have been replaced by the corresponding daily average values
    new_act_data <- act_data
    new_act_data$steps[step_na] <- step_avg[step_na_date]
    # make the histogram, mean and median again
    new_step_sum <- with(new_act_data, tapply(steps, date, sum, na.rm=TRUE))
    new_mean_step <- mean(new_step_sum, na.rm=TRUE)
    new_median_step <- median(new_step_sum, na.rm=TRUE)
    print(paste("mean of total number of steps taken each day is: ", new_mean_step))
    print(paste("median of total number of steps taken each day is: ", new_median_step))
    par(mfrow=c(1,2))
    hist(step_sum, main="original data")
    hist(new_step_sum, main="imputing missing values")
    table(step_sum)
    table(new_step_sum)
    
    # step 8 panel plot comparing the average number of steps taken across
    # weekdays and weekends
    # create a new factor variable in the dataset with 2 levels : weekday
    # and weekend
    week <- c("weekday","weekday","weekday","weekday","weekday","weekend","weekend")
    names(week) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    act_data$week <- week[weekdays(as.Date(act_data$date))]
    weekday_act_data <- subset(act_data, week=="weekday")
    weekend_act_data <- subset(act_data, week=="weekend")
    weekday_step <- with(weekday_act_data, tapply(steps, interval, sum, 
                                                  na.rm=TRUE))
    weekend_step <- with(weekend_act_data, tapply(steps, interval, sum, 
                                                  na.rm=TRUE))
    rng <- range(weekend_step, weekday_step)
    par(mfrow=c(2,1))
    plot(names(weekday_step), weekday_step, type="l", xlab="5-min interval",
         ylab="average number of steps taken", ylim=rng,
         main="Activity pattern in weekdays")
    plot(names(weekend_step), weekend_step, type="l", xlab="5-min interval",
         ylab="average number of steps taken", ylim=rng,
         main="Activity pattern in weekends")
    
    act_data
}