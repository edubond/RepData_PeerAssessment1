---
"FitBit Data Analysis"
---

## Loading and preprocessing the data
If not downloaded, download the zip file. Unzip the getdata-projectfiles-UCI HAR Dataset.zip file, read the activity.csv file and see the structure.
```{r}
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
         download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "getdata-projectfiles-UCI HAR Dataset.zip")
     }
unzip("getdata-projectfiles-UCI HAR Dataset.zip")
data <- read.csv("activity.csv")
str(data)
```


## What is mean total number of steps taken per day?
Calculate the mean of the total number of steps per day, see the structure of the new data, plot a histogram of it and calculate the mean and the median of steps taken per day.

```{r}
data1 <- aggregate(steps ~ date, data, sum)
str(data1)
hist(data1$steps, lwd = 10, col = "dark red", ylim = c(0,30), xlab = "Number of steps", main = "Histogram of the number of steps")

data1mean <- mean(data1$steps)
data1median <- median(data1$steps)

```
![Hist1](https://github.com/edubond/RepData_PeerAssessment1/blob/master/figures/Hist1.png)


The mean is `10766`.
The median is `10765`.

## What is the average daily activity pattern?
Calculate the average daily activity pattern and plot a time series of it.
Calculate the 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps.

```{r}
data2 <- aggregate(steps ~ interval, data, mean)
str(data2)
plot(data2$interval, data2$steps, type = "l", xlab = "Interval", ylab = "Number of steps",
     main = "Average daily steps plot", lwd = 3)
maxint <- data2[which.max(data2$steps), 1]

```
![TS2](https://github.com/edubond/RepData_PeerAssessment1/blob/master/figures/TimeSeries2.png)
The interval is: `8:35`.

## Imputing missing values
Calculate and report the total number of missing values in the dataset. Create a new dataset that is equal to the original dataset but with the missing data filled in. This is made using the 5min interval mean to replace the missing values.
Make a histogram of the total number of steps taken each day and a histogram of the new total number of steps taken each day with fixed missing values. Calculate and report the mean and median total number of steps taken per day.

```{r}
mv <- sum(is.na(data$steps))

```
The missing values are: `2304`.
```{r}
data2 <- data
nas <- is.na(data2$steps)
avg5minint<- tapply(data2$steps, data2$interval, mean, na.rm=TRUE)
data2$steps[nas] <- avg5minint[as.character(data2$interval[nas])]

totstep1 <- aggregate(steps ~ date, data, sum)
totstep2 <- aggregate(steps ~ date, data2, sum)


hist(totstep1$steps, lwd = 10, col = "dark red", ylim = c(0,40), xlab = "Number of steps", main = "Histogram of the number of steps")
 
hist(totstep2$steps, lwd = 10, col = "yellow", ylim = c(0,40), xlab = "Number of steps", main = "Histogram of the number of steps including fixed missing values")  

data2mean <- mean(totstep2$steps)
data2median <- median(totstep2$steps)
```

![Hist3_1](https://github.com/edubond/RepData_PeerAssessment1/blob/master/figures/Hist3_1.png)
![Hist3_2](https://github.com/edubond/RepData_PeerAssessment1/blob/master/figures/Hist3_2.png)
The fixed mean is:`10766`.
The fixed median is:`10766`.
## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day. Make a panel plot containing a time series plot.


```{r}
library(ggplot2)

wd <- c("lunedì", "martedì", "mercoledì", "giovedì", "venerdì")
data2$dow = as.factor(ifelse(is.element(weekdays(as.Date(data2$date)), wd), "Weekday", "Weekend"))
totstepdow <- aggregate(steps ~ interval + dow, data2, mean)

ggplot(totstepdow, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dow ~ .) +
    xlab("Interval") + 
    ylab("Number of steps")
```
![Plot4](https://github.com/edubond/RepData_PeerAssessment1/blob/master/figures/Plot4_png)
