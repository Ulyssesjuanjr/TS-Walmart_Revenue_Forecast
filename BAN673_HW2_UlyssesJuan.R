#******QUESTIONS 1***********************
##Part A

#get forecast library 
library(forecast)

# Set working directory to import data
setwd("C:\\Users\\ulyss\\Downloads")

# Create data frame.
case2.data <- read.csv("case2.csv")

head(case2.data)
tail(case2.data)
summary(case2.data$Revenue)


## CREATE TIME SERIES DATA SET.

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
Revenue.ts <- ts(case2.data$Revenue,start = c(2005, 1), end = c(2020, 2), freq = 4)
head(Revenue.ts)


##Part b
plot(Revenue.ts, 
     xlab = "Time", ylab = "Revenue (in millions)", ylim = c(70000, 150000), bty = "l",
     xaxt = "n", xlim = c(2005, 2022), main = "Time Series Quartely Revenue", lwd = 2) 
axis(1, at = seq(2005, 2022, 1), labels = format(seq(2005, 2022, 1)))
lines(Revenue.ts, col = "blue", lty = 1, lwd = 2)



###QUESTION 2
#Part A
length(Revenue.ts)

nValid <- 16
nTrain <- length(Revenue.ts) - nValid
nTrain

train.ts <- window(Revenue.ts, start = c(2005, 1), end = c(2016, 2))
valid.ts <- window(Revenue.ts, start = c(2016,  3), end = c(2020, 2))
train.ts
valid.ts

##Part b

#linear trend
regression.linear <- tslm(train.ts ~ trend)
summary(regression.linear)
regression.linear.pred <- forecast(regression.linear, h = 16, level = 0)
regression.linear.pred

#quadratic
regression.quad <- tslm(train.ts ~ trend + I(trend^2))
summary(regression.quad)
regression.quad.pred <- forecast(regression.quad, h = 16, level = 0)
regression.quad.pred

#seasonality
regression.trend.seas <- tslm(train.ts ~  season)
summary(regression.trend.seas)
regression.trend.seas.pred <- forecast(regression.trend.seas, h = 16, level = 0)
regression.trend.seas.pred

#linear trend and seasonality
regression.linear.seas<-tslm(train.ts ~ trend  + season)
summary(regression.linear.seas)
regression.linear.seas.pred <- forecast(regression.linear.seas, h = 16, level = 0)
regression.linear.seas.pred

#quad trend and seasonality
regression.quad.seas<-tslm(train.ts ~ trend + I(trend^2) + season)
summary(regression.quad.seas)
regression.quad.seas.pred <- forecast(regression.quad.seas, h = 16, level = 0)
regression.quad.seas.pred


#part c
round(accuracy(regression.linear.pred, Revenue.ts), 3)
round(accuracy(regression.quad.pred, Revenue.ts), 3)
round(accuracy(regression.trend.seas.pred, Revenue.ts), 3)
round(accuracy(regression.linear.seas.pred, Revenue.ts), 3)
round(accuracy(regression.quad.seas.pred, Revenue.ts), 3)


###QUESTION 3

##part a

#linear trend and seasonality
regression.linear.seas.full<-tslm(Revenue.ts ~ trend  + season)
summary(regression.linear.seas.full)
regression.linear.seas.full.pred <- forecast(regression.linear.seas.full, h = 4, level = 0)
regression.linear.seas.full.pred


plot(regression.linear.seas.full.pred,
xlab = "Time", ylab = "Revenue (in millions)", ylim = c(0, 160000), bty = "l",
xaxt = "n", xlim = c(2005, 2021),
      main = "Regression Linear Trend & Seasonality Forecast", flty = 2)
 axis(1, at = seq(2005, 2021, 1), labels = format(seq(2005, 2021, 1)))
 lines(regression.linear.seas.full.pred$fitted, col = "blue", lwd = 2)
 lines(Revenue.ts)
# 
#  Plot on the chart vertical lines and horizontal arrows
#  describing training, validation, and future prediction intervals.
 lines(c(2020.25, 2020.25), c(0, 150000))
#lines(c(2020, 2020), c(0, 500))
#
 text(2013, 160000, "Training")

 text(2021, 160000, "Future")

#quad trend and seasonality
regression.quad.seas.full<-tslm(Revenue.ts ~ trend + I(trend^2) + season)
summary(regression.quad.seas.full)
regression.quad.seas.full.pred <- forecast(regression.quad.seas.full, h = 4, level = 0)
regression.quad.seas.full.pred


plot(regression.quad.seas.full.pred,
     xlab = "Time", ylab = "Revenue (in millions)", ylim = c(0, 160000), bty = "l",
     xaxt = "n", xlim = c(2005, 2021),
     main = "Regression Quadratic Trend & Seasonality Forecast", flty = 2)
axis(1, at = seq(2005, 2021, 1), labels = format(seq(2005, 2021, 1)))
lines(regression.quad.seas.full.pred$fitted, col = "blue", lwd = 2)
lines(Revenue.ts)
# 
#  Plot on the chart vertical lines and horizontal arrows
#  describing training, validation, and future prediction intervals.
lines(c(2020.25, 2020.25), c(0, 150000))
#lines(c(2020, 2020), c(0, 500))
#
text(2015, 10, "Training")

text(2022, 10, "Future")


##part b

round(accuracy((naive(Revenue.ts))$fitted, Revenue.ts), 3)
round(accuracy((snaive(Revenue.ts))$fitted, Revenue.ts), 3)
round(accuracy(regression.linear.seas.full.pred$fitted, Revenue.ts), 3)
round(accuracy(regression.quad.seas.full.pred$fitted, Revenue.ts), 3)

