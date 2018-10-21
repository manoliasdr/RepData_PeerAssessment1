My first project in Reproducible Research
============================================
## 1.Loading and preprocessing the data

- Installing ggplot2,plyr
```{r}
library(ggplot2)
library(plyr)
```

- Convert the days to english
```{r}
Sys.setlocale("LC_TIME", "English")
```

- Read Data
```{r}
activity<-read.csv("C:/Users/user/Desktop/activity.csv")
summary(activity)
```

- Processing the Data
```{r}
activity$day <- weekdays(as.Date(activity$date))
activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")
```

- Pulling data without nas
```{r}
clean <- activity[!is.na(activity$steps),]
weekdays(Sys.Date()+0:6)
```

## 2.What is mean total number of steps taken per day?

```{r}
summary_table <- aggregate(activity$steps ~ activity$date, FUN=sum, )
colnames(summary_table)<- c("Date", "Steps")

hist(summary_table$Steps,main = "Total number of steps per day", 
     xlab="Total steps per day", col = "lightcoral",
     ylim = c(0,20), breaks = seq(0,25000, by=2500))
mean(summary_table$Steps)
median(summary_table$Steps)
```

![picture](https://github.com/manoliasdr/RepData_PeerAssessment1/blob/master/histogram1.png)

The mean  of the total number of steps taken per day is `r mean(summary_table$Steps)` steps

and the median is `r median(summary_table$Steps)` steps.



## 3.What is the average daily activity pattern?

- Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
clean <- activity[!is.na(activity$steps),]
p <- ggplot(ddply(clean, .(interval), summarize, Avg = mean(steps)), aes(x=interval, y=Avg), 
            xlab = "Interval", ylab="Average Number of Steps")
p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")

```

![picture](https://github.com/manoliasdr/RepData_PeerAssessment1/blob/master/timeline1.png)

- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
interval_table <- ddply(clean, .(interval), summarize, Avg = mean(steps))
maxSteps <- max(interval_table$Avg)
interval_table[interval_table$Avg==maxSteps,1]
```

The 5-minute interval which had the maximum number of steps was the `r interval_table[interval_table$Avg==maxSteps,1]` interval.

## 4.Imputing missing values

- Calculate and report the total number of missing values in the dataset 

```{r}
sum(is.na(activity$steps))
```

The total number of missing values is `r sum(is.na(activity$steps)) `.

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
average_table <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))
nd<- activity[is.na(activity$steps),]
new_data_1<-merge(nd, average_table, by=c("interval", "day"))
new_data_2<- new_data_1[,c(6,4,1,2,5)]
colnames(new_data_2)<- c("steps", "date", "interval", "day", "DateTime")
mergeData <- rbind(clean, new_data_2)
summary_table_2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum, )
colnames(summary_table_2)<- c("Date", "Steps")
```

- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
mean(summary_table_2$Steps)
median(summary_table_2$Steps)
hist(summary_table_2$Steps,xlab="Steps", 
     main = "Total Steps per Day with NAs Fixed", col="deeppink3"
     ,ylim = c(0,20), breaks = seq(0,25000, by=2500))
```

![picture](https://github.com/manoliasdr/RepData_PeerAssessment1/blob/master/histogram2.png)

Although the means have changed,it seems that the overall shape of the distribution has not changed.

## 4.Are there differences in activity patterns between weekdays and weekends?

- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
  if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
  {y <- "Weekend"} else 
  {y <- "Weekday"}
  y
})
activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
```

- Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
``` {r}
plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
  geom_line() +
  labs(title = "Average daily steps by type of date", x = "Interval",
       y = "Average number of steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
```

![picture](https://github.com/manoliasdr/RepData_PeerAssessment1/blob/master/timeline2.png)

The step activity trends are different based on whether the day occurs on a weekend or not.