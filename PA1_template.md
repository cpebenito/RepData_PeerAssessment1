---
title: "Peer Assessment 1"
author: "cpebenito"
date: "Saturday, May 16, 2015"
output: html_document
---

Loading the Assignment dataset into R


```r
activity <- read.csv("activity.csv")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```


**What is mean total number of steps taken per day?**

*1.Calculate the total number of steps taken per day*


```r
act2 <- aggregate(. ~ date, data = activity, FUN=sum)
```

*2. Make a histogram of the total number of steps taken each day*

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

*3. Calculate and report the mean and median of the total number of steps taken per day*


```r
mean(act2$steps)
```

```
## [1] 10766.19
```

```r
median(act2$steps)
```

```
## [1] 10765
```

**What is the average daily activity pattern?**


```r
act3 <- aggregate(. ~ interval, data = activity, FUN=mean)
```

*Series Plot showing the average daily activity pattern*


```r
plot(act3$interval, act3$steps, type="l", xlab="Interval", ylab="Average Steps", main="Time Series of Steps per Interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

*Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*


```r
act4 <- subset(act3, select= -date)
act4[which.max(act4$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


**Imputing missing values**


```r
sum(is.na(activity))
```

```
## [1] 2304
```


```r
if (!require(mice)) {
  install.packages("mice", repos="http://cran.rstudio.com/bin/windows/contrib/3.2/mice_2.22.zip") 
  require(mice)
  library("Rcpp")
  library("lattice")
  library("mice")
}
```



```r
fillup <- activity[c("steps", "date", "interval")]
summary(fillup)
set.seed(200)
imputed <- complete(mice(fillup))
summary(imputed)
```

*Make a histogram of the total number of steps taken each day*


```r
impute2 <- aggregate(. ~ date, data = imputed, FUN=sum)

hist(impute2$steps, xlab="Steps", main="Histogram of Steps per Day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

*Calculate and report the mean and median total number of steps taken per day, which include imputed values*


```r
mean(impute2$steps)
```

```
## [1] 11015
```

```r
median(impute2$steps)
```

```
## [1] 11162
```

**What is the impact of imputing missing data on the estimates of the total daily number of steps?**


```r
if (!require(ggplot2)) {
    install.packages("ggplot2", repos="http://cran.us.r-project.org")
    require(ggplot2)
}
library(ggplot2)
set.seed(800)

act2$source <- 'withNAs'
impute2$source <- 'noNAs'
dat <- rbind(act2, impute2)
```
  
  In the histogram shown below, the blue lines represent the mean (solid) and median (dotted) of the dataset withNAs and the red lines represent the dataset noNAs. There was a slight increase on the values when the NAs were imputed using the MICE(Multivariate Imputation by Chained Equations) Package in R.
 
 

```r
 ggplot(dat, aes(steps, fill=source)) + geom_histogram( alpha=0.5 ) +
  geom_vline(aes(xintercept=mean(steps)), data=act2, color="blue") +
  geom_vline(aes(xintercept=mean(steps)), data=impute2, color="red") +
  geom_vline(aes(xintercept=median(steps)), data=act2, color="blue", 
             linetype="dotted", size=1) +
  geom_vline(aes(xintercept=median(steps)), data=impute2, color="red", 
             linetype="dotted", size=1)
```

![plot of chunk histogram showing difference between imputed value and original value](figure/histogram showing difference between imputed value and original value-1.png) 

  


Again for purposes of comparison mean(with NAs) = 1.0766 &times; 10<sup>4</sup> while mean(no NAs) = 1.1015 &times; 10<sup>4</sup>. On the other hand, median(with NAs) = 1.0765 &times; 10<sup>4</sup> while median(no NAs) = 1.1162 &times; 10<sup>4</sup>



**Are there differences in activity patterns between weekdays and weekends?**

*The time series plot below shows the activity patterns between a weekday and weekend activity.*


```r
## I'm sure there's a better way of classifying weekday and weekend, but this is the best I could do for now with the deadline ##

imputed$date2 <- as.POSIXct(imputed$date, tz="")
imputed$day <- weekdays(imputed$date2)

imputed$dayno<- factor(imputed$day,
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), labels = c(1,2,3,4,5,6,7))
                         
imputed$wcat <- as.integer(imputed$dayno)
  
imputed$weekcat <- ifelse(imputed$wcat <= 4, "weekday", "weekend")


g <- ggplot(imputed, aes(interval, steps))
g + geom_line() + facet_wrap( ~ weekcat, ncol = 1) + labs(title = "Activity Pattern")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

**Weekday activity starts off early, while Weekend activity starts at a later time. However, Weekend activity are more continuous and extended throughout the day compared to Weekday.**



```r
sessionInfo()
```

```
## R version 3.2.0 (2015-04-16)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 7 x64 (build 7601) Service Pack 1
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] knitr_1.10.5    ggplot2_1.0.1   mice_2.22       lattice_0.20-31
## [5] Rcpp_0.11.5    
## 
## loaded via a namespace (and not attached):
##  [1] randomForest_4.6-10 digest_0.6.8        MASS_7.3-40        
##  [4] grid_3.2.0          plyr_1.8.1          gtable_0.1.2       
##  [7] formatR_1.2         magrittr_1.5        evaluate_0.7       
## [10] scales_0.2.4        stringi_0.4-1       reshape2_1.4.1     
## [13] rpart_4.1-9         labeling_0.3        proto_0.3-10       
## [16] tools_3.2.0         stringr_1.0.0       munsell_0.4.2      
## [19] colorspace_1.2-6    nnet_7.3-9
```



