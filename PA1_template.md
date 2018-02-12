Reproducible Research Peer Assessment Assignment - Week #2
========================================================


## Loading in and preprocessing data

Before we begin, it is imperative that the reader set the working directory to be where this R Markdown file (and subsequently all of the data for the analysis) is located.  This can be done with the `setwd()` command.

The first thing we obviously need to do is read in the data and convert the dates into a `POSIXlt` class. The data have been provided in a `.zip` archive file.  What we will do is unarchive the `.zip` file and read the data in using `read.csv()`.  After, the dates will be transformed into the `POSIXlt` class.

```{r cache=TRUE}
unzip("activity.zip") # Unzip archive
dat <- read.csv("activity.csv") # Read in data file

# Turn the date data into a valid date class
# Allows for easier processing
# Dates are in YYYY-MM-DD format
dates <- strptime(dat$date, "%Y-%m-%d")
dat$date <- dates

# Keep a list of all possible days
uniqueDates <- unique(dates)
# Keep a list of all possible intervals
uniqueIntervals <- unique(dat$interval)
```

As for `uniqueDates` and `uniqueIntervals`, these are variables that store a list of all possible dates and intervals.  These will primarily be used to help plot our necessary data and for data processing.

## What is the mean total number of steps taken per day?

First, let's plot a histogram of the total number of steps taken for each day.  Before we do this, let's split up the data into individual data frames where each data frame represents the data for a particular day.  After this, we will create a vector that accumulates all of the steps taken for each day and let it get stored into a vector.  Each element in this vector represents the total number of steps taken for a particular day (61 in total).  It should be noted that `NA` values will be ignored for the time being.  After, we will plot a histogram where the x-axis represents the particular day in question, while the y-axis denotes how many steps were taken in total for each day.  We will also calculate what the mean and median number of steps per day were as well.

```{r cache=TRUE, fig.width=11, fig.height=6}
# Part 2 - Create a histogram of the total number of steps taken
# each day
# First split up the data frame for steps by day
stepsSplit <- split(dat$steps, dates$yday)

# Next find the total number of steps over each day
totalStepsPerDay <- sapply(stepsSplit, sum, na.rm=TRUE)

# Plot a (pseudo) histogram where the x-axis denotes the day
# and the y-axis denotes the total number of steps taken 
# for each day
plot(uniqueDates, totalStepsPerDay, main="Histogram of steps taken each day", 
     xlab="Date (October to November 2012)", ylab="Frequency", type="h", lwd=4, col="blue")
```

The mean steps per day are:

```{r cache=TRUE}
meanStepsPerDay <- sapply(stepsSplit, mean, na.rm=TRUE)
meanDataFrame <- data.frame(date=uniqueDates, meanStepsPerDay=meanStepsPerDay, row.names=NULL)
meanDataFrame
```

The median steps per day are:
```{r cache=TRUE}
medianStepsPerDay <- sapply(stepsSplit, median, na.rm=TRUE)
medianDataFrame <- data.frame(date=uniqueDates, medianStepsPerDay=medianStepsPerDay, row.names=NULL)
medianDataFrame
```

## What is the average daily activity pattern?

What we now need to do is split up this data again so that individual data frames represent the steps taken **over each time interval**.  As such, there will be a data frame for interval 5, another data frame for interval 10 and so on.  Once we extract out these individual data frames, we thus compute the mean for each time interval.  It is imperative to note that we again will ignore `NA` values.  We will thus plot the data as a time-series plot (of `type="l"`).  Once we have done this, we will locate where in the time-series plot the maximum is located and will draw a red vertical line to denote this location:

```{r cache=TRUE, fig.width=11, fig.height=6}
# Part 3 - Time-series plot (type="l")
# x-axis - Time interval (5, 10, 15, ...)
# y-axis - Average number of steps taken across all days for this time interval

# Split up the data according to the interval
intervalSplit <- split(dat$steps, dat$interval)

# Find the average amount of steps per time interval - ignore NA values
averageStepsPerInterval <- sapply(intervalSplit, mean, na.rm=TRUE)

# Plot the time-series graph
plot(uniqueIntervals, averageStepsPerInterval, type="l",
     main="Average number of steps per interval across all days", 
     xlab="Interval", ylab="Average # of steps across all days", 
     lwd=2, col="blue")

# Find the location of where the maximum is
maxIntervalDays <- max(averageStepsPerInterval, na.rm=TRUE)
maxIndex <- as.numeric(which(averageStepsPerInterval == maxIntervalDays))

# Plot a vertical line where the max is
maxInterval <- uniqueIntervals[maxIndex]
abline(v=maxInterval, col="red", lwd=3)
```

With reference to the above plot, the interval that records the maximum number of steps averaged across all days is:

```{r cache=TRUE}
maxInterval
```

## Imputing missing values

First, let's calculate the total number of missing values there are.  This denotes the total number of observations that did not have any steps recorded (i.e. those rows which are `NA`)

```{r cache=TRUE}
# Part 4 - Calculate total amount of missing values in the data set
# Use complete.cases to find a logical vector that returns TRUE
# if it is a complete row (a.k.a. no NA values) and FALSE otherwise
completeRowsBool <- complete.cases(dat$steps)
numNA <- sum(as.numeric(!completeRowsBool))
numNA
```

The strategy that we will use to fill in the missing values in the data set is to replace all `NA` values with the mean of that particular 5-minute interval the observation falls on.  Now that we have devised this strategy, let's replace all of the `NA` values with the aforementioned strategy.

```{r cache=TRUE}
# Modify the meanStepsPerDay vector that contains the mean steps taken
# for this 5 minute interval
# Each day consists of 288 intervals and there are 61 days in total
# First remove NaN values and replace with 0.  
# NaN values are produced when the entire day was filled with NA values
# Essentially the mean and median would be zero anyway!
meanStepsPerDay[is.nan(meanStepsPerDay)] <- 0

# Now create a replicated vector 288 times
# The reason why we're doing this is because the slots
# in the vector naturally line up with the interval for
# a particular day.  Now, all we have to do is find where
# in the data set there are missing steps, and simply do
# a copy from one vector to the other
meanColumn <- rep(meanStepsPerDay, 288)

# The steps before replacement
rawSteps <- dat$steps

# Find any values that are NA in the raw steps data
stepsNA <- is.na(rawSteps)

# Now replace these values with their corresponding mean
rawSteps[stepsNA] <- meanColumn[stepsNA]

# Throw these back into a new data frame
datNew <- dat
datNew$steps <- rawSteps
```

Now that this is finished, let's plot a histogram of the new data:

```{r cache=TRUE, fig.width=11, fig.height=12}
# Repeat Part 2 now
# First split up the data frame for steps by day
stepsSplitNew <- split(datNew$steps, dates$yday)

# Next find the total number of steps over each day
# There should not be an NA values and so we don't need
# to set the flag
totalStepsPerDayNew <- sapply(stepsSplitNew, sum)

# Plot a (pseudo) histogram where the x-axis denotes the day
# and the y-axis denotes the total number of steps taken 
# for each day
par(mfcol=c(2,1))
# Plot the original histogram first
plot(uniqueDates, totalStepsPerDay, main="Histogram of steps taken each day before imputing", 
     xlab="Date (October to November 2012)", ylab="Frequency", type="h", lwd=4, col="blue")
# Plot the modified histogram after
plot(uniqueDates, totalStepsPerDayNew, main="Histogram of steps taken each day after imputing", 
     xlab="Date (October to November 2012)", ylab="Frequency", type="h", lwd=4, col="blue")
```

With this new data, let's calculate the mean over all days (like in Part 2).  As a side-by-side comparison, we will place the data before imputing, as well as the new one in the same data frame.  Bear in mind that we have replaced all of the `NaN` values to `0`.  As such, the mean steps per day of the new data are:

```{r cache=TRUE}
meanStepsPerDayNew <- sapply(stepsSplitNew, mean)
meanDataFrameNew <- data.frame(date=uniqueDates, meanStepsPerDay=meanStepsPerDay, 
                               meanStepsPerDayNew=meanStepsPerDayNew, row.names=NULL)
meanDataFrameNew
```

Like the above, the median steps per day are:
```{r cache=TRUE}
medianStepsPerDayNew <- sapply(stepsSplitNew, median)
medianDataFrameNew <- data.frame(date=uniqueDates, medianStepsPerDay=medianStepsPerDay, 
                                 medianStepsPerDayNew=medianStepsPerDayNew, row.names=NULL)
medianDataFrameNew
```

By looking at the above data frames, the only values that have changed are those days where all of the observations were missing (i.e. those days having all zeroes / `NA`).  The rest of the observations have stayed the same, which actually make sense.  The reason why this is is because if you insert more data values into a vector that are the mean of that particular vector, taking the mean will still give you the same answer.  With regards to the median vector, those rows that were filled with `NA` had all of their observations on that day filled with the mean value and so the median would obviously be the mean as well.

## Are there differences in activity patterns between weekdays and weekends?

With the new data set we have just created, we are going to split up the data into two data frames - one data frame consists of all steps taken on a weekday, while the other data frame consists of all steps taken on a weekend.  The following `R` code illustrates this for us:

```{r cache=TRUE}
# Part 5 - Now split up the data so that it's sorted by weekday or weekend
# We have casted the dates to a POSIXlt class so wday is part of this class
# wday is an integer ranging from 0 to 6 that represents the day of the week
# 0 is for Sunday, 1 is for Monday, going up to 6 for Saturday
# Store this into wdays
wdays <- dates$wday

# Create a new factor variable that classifies the day as either a weekday or weekend
# First, create a numeric vector with 2 levels - 1 is for a weekday, 2 for a weekend
classifywday <- rep(0, 17568) # 17568 observations overall

# Any days that are from Monday to Friday, set the numeric vector in these positions
# as 1
classifywday[wdays >= 1 & wdays <= 5] <- 1

# Any days that are on Saturday or Sunday, set the numeric vector in these positions
# as 2
classifywday[wdays == 6 | wdays == 0] <- 2

# Create a new factor variable that has labels Weekdays and Weekends
daysFactor <- factor(classifywday, levels=c(1,2), labels=c("Weekdays", "Weekends"))

# Create a new column that contains this factor for each day
datNew$typeOfDay <- daysFactor

# Now split up into two data frames
datWeekdays <- datNew[datNew$typeOfDay == "Weekdays", ]
datWeekends <- datNew[datNew$typeOfDay == "Weekends", ]
```

Now that we have accomplished this, let's split up the data for each data frame so that we will have two sets of individual data frames.  One set is for weekdays and within this data frame are individual data frames.  Each data frame contains the steps for each interval recorded on a weekday.  The other set is for weekends, and within this data frame are individual data frames.  Like previously, each data frame here contains the steps for each interval recorded on a weekday.  Once we have these two sets of data frames, we will now calculate the mean amount of steps for each interval for the weekdays data frame and weekends data frame.  This will result in two vectors - one for the weekdays and the other for weekends.  The following `R` code does this for us:

```{r cache=TRUE, fig.width=11, fig.height=12}
# Further split up the Weekdays and Weekends into their own intervals
datSplitWeekdays <- split(datWeekdays$steps, datWeekdays$interval)
datSplitWeekends <- split(datWeekends$steps, datWeekends$interval)

# Find the average for each interval
meanStepsPerWeekdayInterval <- sapply(datSplitWeekdays, mean)
meanStepsPerWeekendInterval <- sapply(datSplitWeekends, mean)

par(mfcol=c(2,1))
plot(uniqueIntervals, meanStepsPerWeekdayInterval, type="l",
     main="Average number of steps per interval across all weekdays", 
     xlab="Interval", ylab="Average # of steps across all weekdays", 
     lwd=2, col="blue")
plot(uniqueIntervals, meanStepsPerWeekendInterval, type="l",
     main="Average number of steps per interval across all weekends", 
     xlab="Interval", ylab="Average # of steps across all weekends", 
     lwd=2, col="blue")
```

