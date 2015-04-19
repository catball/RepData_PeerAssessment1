# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
The data are compressed in a .zip file in the repository, so what we have to do is to uncompress and store them in a dataset in R. It is performed by the following code. A structure function is called on the dataset.


```r
read_data <- function() {
    fname = "activity.zip"
    source_url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    if(!file.exists(fname)) {
        download.file(source_url, destfile=fname, method="curl")
    }
    con <- unz(fname, "activity.csv")
    tbl <- read.csv(con, header=T, colClasses=c("numeric", "character", "numeric"))
    tbl$interval <- factor(tbl$interval)
    tbl$date <- as.Date(tbl$date, format="%Y-%m-%d")
    tbl
}
tbl <- read_data()
```
Exam the data


```r
summary(tbl)
```

```
##      steps             date               interval    
##  Min.   :  0.00   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   5      :   61  
##  Median :  0.00   Median :2012-10-31   10     :   61  
##  Mean   : 37.38   Mean   :2012-10-31   15     :   61  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   20     :   61  
##  Max.   :806.00   Max.   :2012-11-30   25     :   61  
##  NA's   :2304                          (Other):17202
```

```r
str(tbl)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

## What is mean total number of steps taken per day?
For the first two questions we will need a file that does not contain missing values.
The missing values will be excluded temporarily from our analysis.“activity_rm” is created for this reason.


```r
activity_rm<-tbl[which(!is.na(tbl$steps)),]
```
The number of steps taken is measured in timeslots, 5-min intervals.
In order to compute the total number of steps taken for each day, we aggregate the data by day.


```r
perday<-tapply(activity_rm$steps, activity_rm$date, sum)
```
Now per day dataset contains the total number of steps taken for each day of Oct. and nov.
Make a histogram of the total steps taken each day.


```r
hist(perday,10, main ="Total number of steps taken per day", xlab = "")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

The mean of total steps during a whole day is 10766, while the median of the total steps is 10765.


```r
mean(perday)
```

```
## [1] 10766.19
```

```r
median(perday)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
In order to explore the data throughout the day, we need to aggregate the dataset by the intervals. A per interval array is created and a time series plot will also be created.
The x-axis point labels are the names of the intervals in the dataset. The coding of the interval names is that 500 should be conidered as 5:00 and 1000 as 10:00. So, one can consider th x-axis as a fuull 24-hour-day starting from midnight and ending at the next midnight hour.


```r
dailyact<-tapply(activity_rm$steps, activity_rm$interval, mean)
plot(y = dailyact, x = names(dailyact), type = "l", xlab = "5-Minute-Interval", 
    main = "Daily Activity Pattern", ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

Finally, we find out that the interval with the maximum average number of steps throughout the days is 835 with 206.1698 steps.


```r
dailyact[dailyact==max(dailyact)]
```

```
##      835 
## 206.1698
```


## Imputing missing values
As discussed earlier in this report, there are a number os days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Let's remind ourselves how many there were regarding the steps variable, and let's check that the other two variables do not have any missing data.


```r
sum(is.na(tbl$steps))
```

```
## [1] 2304
```

```r
sum(is.na(tbl))
```

```
## [1] 2304
```

The count of missing values for the column of steps equals to the total number missing in the whole dataset, so we can be sure that the intervals and the dates do not have any.

So, 2304 missing values is a percentage of 13.11% on the total observations, so obviously there will be some bias.

In order to exclude the bias we have to come up with a method for filling in all of the missing values in the dataset. Some quick ways are to use the mean/median for that day, or the mean for that 5-minute interval, etc.

We will go with the option of using the mean of the 5-minute interval, and thus we will now reate a new dataset that is equal to the original dataset but with the missing data filled in.

```r
act_new <- tbl
act_new[which(is.na(act_new$steps)),1]<-
        dailyact[as.character(act_new[which(is.na(act_new$steps)),3])]
```

No missing values in the new dataset.


```r
sum(is.na(act_new))
```

```
## [1] 0
```

Make the same histogram as above in order to visually see if there is a big difference.


```r
perday_new<-tapply(act_new$steps, act_new$date, sum)
par(mfrow=c(1,2))
hist(perday,10, main = "Total number of steps taken per day", xlab = "Steps"
     , ylim =c(0, 25))
abline(v = median(perday), col = 4, lwd = 4)
hist(perday_new,10, main = "Total number of steps taken per day  
     (missing values replaced with mean of interval)", xlab = "Steps",
     ylim =c(0, 25))
abline(v = median(perday_new), col = 4, lwd = 4)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

Calculate the mean and median

```r
mean(perday_new)
```

```
## [1] 10766.19
```

```r
median(perday_new)
```

```
## [1] 10766.19
```

The impact of inputing missing data is minimal as only the median seems changed a bit.

```r
mean(perday_new)-mean(perday)
```

```
## [1] 0
```

```r
median(perday_new)-median(perday)
```

```
## [1] 1.188679
```


## Are there differences in activity patterns between weekdays and weekends?
We create factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
act_new$wd<-weekdays(act_new$date)
act_new$fwd<- as.factor(c("weekend", "weekday"))
act_new[act_new$wd == "Sunday" | act_new$wd == "Saturday" ,5]<- factor("weekend")
act_new[!(act_new$wd == "Sunday" | act_new$wd == "Saturday"),5 ]<- factor("weekday")
```

Now we create two aggregated arrays for the total number of steps taken per 5-minyute time interval for weekdays and weekends, and make a graph to compare.


```r
act_new_we <- subset(act_new, fwd == "weekend") 
act_new_wd <- subset(act_new, fwd == "weekday") 
dailyact_we<-tapply(act_new_we$steps, act_new_we$interval, mean)
dailyact_wd<-tapply(act_new_wd$steps, act_new_wd$interval, mean)
par(mfrow=c(2,1))
plot(y = dailyact_wd, x = names(dailyact_wd), type = "l", xlab = "5-Minute Interval", 
     main = "Daily Activity Pattern on Weekdays", ylab = "Average number of steps", 
     ylim =c(0, 250))
plot(y = dailyact_we, x = names(dailyact_we), type = "l", xlab = "5-Minute Interval", 
     main = "Daily Activity Pattern on Weekends", ylab = "Average number of steps", 
     ylim =c(0, 250))
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png) 

From the two graphs,we observe that activity on the weekends tends to be more spread out over the day compared to the weekdays. This could be due to the fact that activities on weekdays mostly follow a work related routine, whereas weekends tend to be more adhoc
