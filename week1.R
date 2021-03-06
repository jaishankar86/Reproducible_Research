#Loading and preprocessing the data
setwd("C:/Users/JAI/Downloads/Cousera Data Science Specialization/5. Reproducible Research/week1//repdata-data-activity/")
data <- read.csv("activity.csv", header= T)

#What is mean total number of steps taken per day?
#Calculate the total number of steps taken per day
steps_day <- tapply(data$steps, data$date, sum, na.rm=TRUE)

#Make a histogram of the total number of steps taken each day
qplot(steps_day, xlab='Total steps per day', ylab='Frequency')

#Calculate and report the mean and median of the total number of steps taken per day
stepsByDayMean <- mean(steps_day)
stepsByDayMedian <- median(steps_day)

#What is the average daily activity pattern?
averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=data$steps),
                                      by=list(interval=data$interval), FUN=mean, na.rm=TRUE)

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken") 

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
mostSteps <- which.max(averageStepsPerTimeBlock$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averageStepsPerTimeBlock[mostSteps,'interval'])

#Imputing missing values
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
numMissingValues <- length(which(is.na(data$steps)))

#Create a new dataset that is equal to the original dataset but with the missing data filled in.
dataImputed <- data
dataImputed$steps <- impute(data$steps, fun=mean)
#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
stepsByDayImputed <- tapply(dataImputed$steps, dataImputed$date, sum)

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
qplot(stepsByDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
stepsByDayMeanImputed <- mean(stepsByDayImputed)
stepsByDayMedianImputed <- median(stepsByDayImputed)

#Are there differences in activity patterns between weekdays and weekends?

#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

dataImputed$dateType <-  ifelse(as.POSIXlt(dataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
averageddataImputed <- aggregate(steps ~ interval + dateType, data=dataImputed, mean)
ggplot(averageddataImputed, aes(interval, steps)) + 
  geom_line() + 
  facet_grid(dateType ~ .) +
  xlab("5-minute interval") + 
  ylab("avarage number of steps")

