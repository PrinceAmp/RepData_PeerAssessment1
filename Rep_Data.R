setwd("~/Data Science/course projects/Reproducible research/repdata%2Fdata%2Factivity")

#read in file
activity <- read.csv("activity.csv")
head(activity)

#Calculate the total number of steps taken per day
dailysteps <- aggregate(steps ~ date,activity,FUN=sum)

#Make a histogram of the total number of steps taken each day
stepsHist <- hist(dailysteps$steps, main = "Total Daily Steps", xlab = "# of steps")

#Calculate and report the mean and median of the total number of steps taken per day
meanSteps <- mean(dailysteps$steps, na.rm = TRUE)
medianSteps <- median(dailysteps$steps, na.rm = TRUE)

meanSteps
medianSteps

#mean steps by intervals
meanStepInt <- aggregate(steps ~ interval,activity,mean)

#Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of 
#the 5-minute interval (x-axis) and the average number of steps taken,
#averaged across all days (y-axis)
ggplot(data = meanStepInt, aes(x = interval, y = steps)) 
geom_line()

#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
intMax <- meanStepInt[which.max(meanStepInt$steps),]

#Calculate and report the total number of missing values in the dataset 
#(i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
valuesMissing <- is.na(activity$steps)

#Devise a strategy for filling in all of the missing values

#There are 17568 missing values. I will replace these missing 
#values with the 5-day average of that respective interval.

#Create a new dataset that is equal to the original dataset but with 
subset_Activity <- transform(activity,steps = 
ifelse(is.na(activity$steps),
meanStepInt$steps[match(activity$interval,meanStepInt$interval)],
activity$steps))

#Make a histogram of the total number of steps taken each day and
#and report the mean and median.
stepsbyint <- aggregate(steps ~ date,subset_Activity,FUN = sum)
hist(stepsbyint$steps, main = "Updated # of steps by day", xlab = "# of steps")

#Create a new factor variable in the dataset with two levels - "weekend" and "weekday"
day_type <- function(date) {
  day <- weekdays(date)
  if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
    return ("weekeday")
  else if (day %in% c('Saturday', 'Sunday'))
    return ("weekend")
  else
    stop ("data format not recognized")
}

subset_Activity$date <- as.Date(subset_Activity$date)
subset_Activity$day <- sapply(subset_Activity$date, FUN = day_type)

# Make a panel plot containnig a time-series plot of the 5-minute interval
# and the average number of steps taken across all weekdays or weekends
meanStepsperday <- aggregate(steps ~ interval + day, subset_Activity,mean)

#plot
ggplot(data = meanStepsperday, aes(x = interval, y = steps)) 
  geom_line() 




