Reproducible Research: Peer Assessment 1
========================================================

## Loading and preprocessing the data


```r
##Download and unzip the file
if(!file.exists("./data")){dir.create("./data")}
file<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(file,destfile="./data/Data.zip", method='curl')
```

```
## Warning: comando ejecutado 'curl
## "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" -o
## "./data/Data.zip"' tiene estatus 60
```

```
## Warning in download.file(file, destfile = "./data/Data.zip", method =
## "curl"): download had nonzero exit status
```

```r
unzip(zipfile="./data/Data.zip",exdir="./data")

rm(file)

##New path
path <- file.path("./data")

##Open file
data <- read.csv(file.path(path,"activity.csv"), colClasses = c("integer", "Date", "factor"), header=T)
data$month <- as.numeric(format(data$date, "%m"))


##Complete data
c_data<-na.omit(data)
```

## What is mean total number of steps taken per day?

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

```r
t_steps<-tapply(c_data$steps, c_data$date, sum, na.rm=T)
qplot(t_steps, binwidth = 1000, xlab = "total number of steps taken each day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean(t_steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(t_steps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
avg <- aggregate(c_data$steps, list(interval = as.numeric(as.character(c_data$interval))), FUN = "mean")
names(avg)[2] <- "mean"

ggplot(avg, aes(interval, mean)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

## Imputing missing values


```r
library(Hmisc)
```

```
## Warning: package 'Hmisc' was built under R version 3.1.3
```

```
## Loading required package: grid
## Loading required package: lattice
## Loading required package: survival
## Loading required package: splines
## Loading required package: Formula
```

```
## Warning: package 'Formula' was built under R version 3.1.3
```

```
## 
## Attaching package: 'Hmisc'
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
library(ggplot2)

describe(data)
```

```
## data 
## 
##  4  Variables      17568  Observations
## ---------------------------------------------------------------------------
## steps 
##       n missing  unique    Info    Mean     .05     .10     .25     .50 
##   15264    2304     617    0.62   37.38     0.0     0.0     0.0     0.0 
##     .75     .90     .95 
##    12.0    86.0   252.8 
## 
## lowest :   0   1   2   3   4, highest: 786 789 794 802 806 
## ---------------------------------------------------------------------------
## date 
##       n missing  unique 
##   17568       0      61 
## 
## lowest : 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05
## highest: 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30 
## ---------------------------------------------------------------------------
## interval 
##       n missing  unique 
##   17568       0     288 
## 
## lowest : 0    10   100  1000 1005, highest: 935  940  945  950  955  
## ---------------------------------------------------------------------------
## month 
##       n missing  unique    Info    Mean 
##   17568       0       2    0.75   10.49 
## 
## 10 (8928, 51%), 11 (8640, 49%) 
## ---------------------------------------------------------------------------
```

```r
nrow(data[is.na(data$steps)==T,])
```

```
## [1] 2304
```

```r
data<-merge(data,avg,all.x=T)
data$imp_steps<-ifelse(is.na(data$steps)==T,data$mean,data$steps)

t_steps2 <- tapply(data$imp_steps, data$date, FUN = sum)
qplot(t_steps2, binwidth = 1000, xlab = "Total number of steps taken each day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

## Are there differences in activity patterns between weekdays and weekends?


```r
data$days_type <- factor(format(data$date, "%A"))
data$days_type2<-ifelse(data$days_type=="sábado" | data$days_type=="domingo", "weekend","weekday")

avg2 <- aggregate(data$imp_steps,list(as.numeric(data$interval),data$days_type2), mean)
names(avg2) <- c("interval","day_type","mean")
avg2<-avg2[order(avg2[,1]),]
rownames(avg2)<-1:nrow(avg2)

library(lattice)
xyplot(avg2$mean ~ avg2$interval | avg2$day_type, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

