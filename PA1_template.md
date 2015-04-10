#Reproducible Research: Peer assessment 1
*by Adam Hadar*

###Loading and preprocessing the data

```{r}
library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')
unzip("repdata-data-activity.zip")
data <- read.csv("activity.csv")
```

###What is mean total number of steps taken per day?

1.Calculate the total number of steps taken per day
```{r}
ignore_na <- na.omit(data)
steps_per_day <- aggregate(steps ~ date, data, sum)
```

2.Make a histogram of the total number of steps taken each day
```{r}
library(ggplot2)
ggplot(aes(x = steps), data = steps_per_day) + 
        geom_histogram(binwidth = 2500, color = 'black', fill = "dark red")+
        labs(title="Total Steps Taken per Day",
        x = "Number of Steps per Day", y = "Steps Count") 
```

3.Calculate and report the mean and median of the total number of steps taken per day

- mean
```{r}
mean(steps_per_day$steps)
```
- median
```{r}
median(steps_per_day$steps) 
```

###What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
steps_per_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_per_interval$interval,steps_per_interval$steps, type="l", xlab="5-min Interval", ylab="Average Num of Steps",main="Average Daily Activity Pattern")
```

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
max_interval <- steps_per_interval[which.max(steps_per_interval$steps),1]
max_interval
```


###Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
count_NA <- sum(is.na(data))
count_NA
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
```{r}
na_fill <- function(data, pervalue) {
        na_index <- which(is.na(data$steps))
        na_replace <- unlist(lapply(na_index, FUN=function(idx){
                interval = data[idx,]$interval
                pervalue[pervalue$interval == interval,]$steps
        }))
        fill_steps <- data$steps
        fill_steps[na_index] <- na_replace
        fill_steps
}
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
data_fill <- data.frame(  
        steps = na_fill(data, steps_per_interval),  
        date = data$date,  
        interval = data$interval)
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
totsteps <- aggregate(steps ~ date, data = data_fill, sum, na.rm = TRUE)
ggplot(aes(x = steps), data = totsteps) + 
     geom_histogram(binwidth = 2500, color = 'black', fill = '#099DD9')+
        labs(title="Steps Taken per Day", 
             x = "Total Steps Per Day", y = "Frequency of Total Steps in a day")
```

- mean
```{r}
mean(totsteps$steps)
```
- median
```{r}
median(totsteps$steps) 
```
we can see a slight difference in the median: before filling the data the median was  10,765 and after is 10,766.189. The mean didn't change.












