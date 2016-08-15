###save the csv file into the working directory
activitydf<-read.csv("activity.csv")

####load dplyr package
library(dplyr)

###1 What is mean total number of steps taken per day?
###ignore the NAs and calculate the sum of steps per date
totalstepsdf<-filter(activitydf, !is.na(steps)) %>% group_by(date) %>% summarize(total_steps=sum(steps))

####create the histogram of the total number of step per day
hist(totalstepsdf$total_steps, col="red", main="Total Steps per Day", xlab="Daily Number of Steps")

###calculate the mean
mean(totalstepsdf$total_steps)

###calculate the median
median(totalstepsdf$total_steps)


### 2 What is the average daily activity pattern?

mean_interval_stepsdf<-filter(activitydf, !is.na(steps)) %>% group_by(interval) %>% summarize(avg_steps=mean(steps))



plot(mean_interval_stepsdf$interval, mean_interval_stepsdf$avg_steps, type='l', xlab="Intervals", ylab="Avg Steps", main="Average Daily Pettern")

####Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
###maximum 5-minute interval
mean_interval_stepsdf[which.max(mean_interval_stepsdf$avg_steps),]



###Calculate and report the total number of missing values in the dataset 
length(activitydf[,1])-sum(complete.cases(activitydf))

#####Devise a strategy for filling in all of the missing values in the dataset.
####all the NA values appear in some specific days were we do not have any info.
nadf<-filter(activitydf, is.na(steps))
table(nadf$date)

###for that reason i will assign for every NA the average number of steps

newdf<-activitydf

newdf[is.na(newdf[,1]),1]<-mean(activitydf$steps, na.rm=TRUE)

totalstepsnewdf<-select(newdf, 1:3) %>% group_by(date) %>% summarize(total_steps=sum(steps))
hist(totalstepsnewdf$total_steps, col="red", main="Total Steps per Day", xlab="Daily Number of Steps")

mean(totalstepsnewdf$total_steps)  ###we get the same
median(totalstepsnewdf$total_steps)

#######Are there differences in activity patterns between weekdays and weekends?

cleandf<-filter(activitydf, !is.na(steps))

cleandf$date<-weekdays(as.Date(cleandf$date, format = "%Y-%m-%d"))
cleandf$weekend<-cleandf$date
cleandf[which((cleandf[,2] %in% c("Saturday", "Sunday"))),4]<-"Weekend"

cleandf[which(!(cleandf[,2] %in% c("Saturday", "Sunday"))),4]<-"Weekday"

mean_interval_stepsdfv2<-select(cleandf, 1:4) %>% group_by(interval, weekend) %>% summarize(avg_steps=mean(steps))



library(lattice)
mean_interval_stepsdfv2<-transform(mean_interval_stepsdfv2, weekend<-factor(weekend))
xyplot(avg_steps~interval | weekend, 
data=mean_interval_stepsdfv2, type='l', layout=c(1,2), ylab="Mean Steps per Interval")



xyplot(activitydf$steps~activitydf$interval, type="l")

strptime(activitydf$date, format = "%Y-%m-%d")

weekdays(strptime(activitydf$date, format = "%Y-%m-%d"))

which(is.na(activitydf[,1])&activitydf[,3]==2350)








###save the csv file into the working directory
activitydf<-read.csv("activity.csv")
####ignore the NAs
cleandf<-activitydf[complete.cases(activitydf),]


####histogram of daily number of steps
hist(tapply(cleandf$steps, cleandf$date, sum), col="red", main="Total Steps per Day", xlab="Daily Number of Steps")
mean(tapply(cleandf$steps, cleandf$date, sum))

####
library(dplyr)
totalstepsdf<-filter(activitydf, !is.na(steps)) %>% group_by(date) %>% summarize(total_steps=sum(steps))
hist(totalstepsdf$total_steps, col="red", main="Total Steps per Day", xlab="Daily Number of Steps")
mean(totalstepsdf$total_steps)
median(totalstepsdf$total_steps)


group_by()


plot(activitydf$steps)
hist(activitydf$steps)
hist(activitydf$steps, na.rm=TRUE)
length(activitydf$steps==0)
length(activitydf$steps==0)
which(activitydf$steps==0)
length(which(activitydf$steps==0))
length(which(activitydf$steps==NA))
mean(activitydf$steps)
mean(activitydf$steps, na.rm=TRUE)
lapply(activitydf$date, mean(activitydf$steps, na.rm=TRUE))
lapply(activitydf$date, activitydf$steps, mean)
tapply(activitydf$date, activitydf$steps, mean)
tapply(activitydf$date, activitydf$steps, mean na.rm=TRUE)
tapply(activitydf$date, activitydf$steps, mean, na.rm=TRUE)
Lapply(activitydf$date, activitydf$steps, mean, na.rm=TRUE)
lapply(activitydf$date, activitydf$steps, mean, na.rm=TRUE)
?tapply
lapply(activitydf$steps, activitydf$date, mean, na.rm=TRUE)
tapply(activitydf$steps, activitydf$date, mean, na.rm=TRUE)
tapply(activitydf$steps, activitydf$date, mean)
table(tapply(activitydf$steps, activitydf$date, mean))
class(tapply(activitydf$steps, activitydf$date, mean))
dim(tapply(activitydf$steps, activitydf$date, mean))
test<-tapply(activitydf$steps, activitydf$date, mean, na.rm=TRUE)
test
test[1]
sum(test)
sum(test, na.rm=TRUE)
mean(test, na.rm=TRUE)
hist(test, na.rm=TRUE)
hist(test, na.rm=TRUE)
barplot(test, na.rm=TRUE)
hist(test, na.rm=TRUE)
barplot(test, na.rm=TRUE)
barplot(test, na.rm=TRUE)
hist(test, na.rm=TRUE)
boxplot(test, na.rm=TRUE)
boxplot(test)
boxplot(test)
median(test, na.rm=TRUE)
mean(test, na.rm=TRUE)
unique(activitydf$interval)
table(activitydf$interval)
savehistory("C:/From Desktop to Laptop/coursera/reproducable/history_stoudio.Rhistory")
