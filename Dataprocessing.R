data<-read.csv("activity.csv")
library(dplyr)

#remove NAs
dataWithoutNA<- na.omit(data)

#Find total number of steps taken each day.
dataS <- aggregate(steps~date, dataWithoutNA, sum)
#Create a histogram of total number of steps taken each day.
hist(dataS$steps, main = "Total number of steps taken each day", xlab = "Steps", col = 1)

# Finding mean and median of steps.
dataMean <- mean(dataS$steps)
dataMedian <- median(dataS$steps)

#Average number of steps taken each day.
stepsInterval<- aggregate(steps ~ interval, data, mean)

#Time series plot of 5-minute interval and the average number of steps taken
plot(stepsInterval$interval, stepsInterval$steps, type = "l", main = "Average daily activity pattern", xlab = "Intervals", ylab = "Steps")

#Maximum number of steps in a interval.
stepMax <- which.max(stepsInterval$steps)

#Total number of missing values.
totalNA <- sum(is.na(data))

#We will replace the NA by mean for that 5-minute interval.
dataC <- dataWithoutNA %>% select(steps, interval) %>% group_by(interval) %>% summarise_each(funs = mean)
dataCopy <- data
for(i in 1:nrow(dataCopy)){
  if(is.na(dataCopy$steps[i])){
    interval <- dataCopy$interval[i]
    dataCopy$steps[i] <- dataC$steps[dataC$interval == interval]
  }
}

#New dataset with imputed values instead of NA with total number of steps each day.
dataN <-aggregate(steps ~ date, dataCopy, sum)

#Create a Histogram of the total number of steps taken each day.
hist(dataN$steps, main = "Histogram of total number of steps taken each day(Imputed)", xlab = "Steps")

#Finding mean and median of steps of the imputed data.
dataMean2 <- mean(dataN$steps)
dataMedian2 <- median(dataN$steps)

library(ggplot2)
#Creating a factor variable with two levels weekdays and weekends.
dataFac <- data %>% mutate(day = weekdays(as.Date(data$date)))
for(i in 1:nrow(dataFac)){
  if(dataFac$day[i] == "Saturday"){
    dataFac$days[i] <- "weekend"
  }
  else if(dataFac$day[i] == "Sunday"){
    dataFac$days[i] <- "weekend"
  }
  else{
    dataFac$days[i] <- "weekday"
  }
}

dataFac$days <- as.factor(dataFac$days)

#Create a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.
avgData<- aggregate(steps ~ interval + days, dataFac, mean)
g<- ggplot(avgData, aes(interval, steps))
g + facet_grid(days~.) + geom_line()
