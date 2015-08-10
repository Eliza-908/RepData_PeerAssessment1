setwd("/Users/Eliza/datasciencecoursera/RepData_PeerAssessment1")
data <- read.csv("activity.csv")
completeData <- data[complete.cases(data),]
perDay <- tapply(completeData$steps, completeData$date, sum, na.rm = TRUE)
hist(perDay, col = 'blue', breaks = 10, main = "Steps Taken Plot", xlab = "Steps per Day")
perDayMean <- round(mean(perDay[!is.na(perDay)]),3)
perDayMedian <- median(perDay[!is.na(perDay)])

stepsPerInt <- aggregate(x=list(meanSteps=completeData$steps), by=list(interval=completeData$interval), FUN=mean)
plot(stepsPerInt$interval, stepsPerInt$meanSteps, type = 'l', col = "red", main = "Step Intervals", xlab = "Interval", ylab = "Average Steps")
maxSteps <- stepsPerInt[which.max(stepsPerInt$meanSteps),]
maxInterval <- maxSteps[1,1]

numberMissVals <- length(which(is.na(data$steps)))
dataImputed <- data
dataImputed$steps[is.na(dataImputed$steps)] = mean(dataImputed$steps, na.rm=TRUE)
perDayImp <- tapply(dataImputed$steps, dataImputed$date, sum, na.rm = TRUE)
hist(perDayImp, col = 'blue', breaks = 10, main = "Steps Taken Imputed Values", xlab = "Steps per Day")
perDayMeanImp <- mean(perDayImp)
perDayMedianImp <- median(perDayImp)

dataImputed$dateType <-  ifelse(as.POSIXlt(dataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
par(mfrow=c(1,2))