#Read data
setwd("C:/Users/USUARIO/Desktop/Specialization/5 reproducible")
data = read.csv("activity.csv", header = TRUE)

#Count NA values
str(data)
NAcon = is.na(data$steps) #only in steps variable
table(NAcon)

count=complete.cases(data) # for the entire dataset
table(count)

#Imputing NA

class(data$date)
data$date = as.Date(data$date, format = "%Y-%m-%d") #covert to date format https://www.statmethods.net/input/dates.html

data$steps2 = ifelse (is.na(data$steps),
                      by(data, data$date, FUN = function (x)mean(data$steps, na.rm = TRUE)),
                      data$steps)# puting the mean 


data$steps = NULL
table(complete.cases(data))

stepsbyday=aggregate(data$steps2, by = list(date = data$date), FUN = sum)


hist(stepsbyday$x, xlab = "Steps", main = "Histogram of number of steps by day", breaks = "sturges")

#mean of steps by day

mean(stepsbyday$x)

#median of steps by day

median(stepsbyday$x)


#daily pattern
pattern <- aggregate(data$steps2, by = list(interval = data$interval),
                     FUN=mean)

plot(pattern,type = "l",main = "mean number of steps across all days",
     xlab = "interval", ylab = "Steps")


#extract weekday and weekend
data$typeday = ifelse(as.POSIXlt(data$date)$wday %in% c(0,6), "weekends","weekdays")

final<-aggregate(steps2 ~ interval + typeday, data=data, mean)

plot(final,type = "l",main = "mean number of steps",
     xlab = "interval", ylab = "Steps", col = data$typeday)

library(lattice)
xyplot(steps2~interval|typeday, data=final, type="l",
       layout = c(2,1), main = "Average steps", 
       ylab = "number of steps",xlab = "Interval")

