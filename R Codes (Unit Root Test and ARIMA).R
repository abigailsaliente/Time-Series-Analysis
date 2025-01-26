
library(ggplot2)   #for plots
library(ggfortify) #for plots
library(tseries)   #Unit Root Tests
library(forecast)  #For auto.arima
library(stats)     #for Ljung-Box test

###############################
#READ AND PLOT THE TIME SERIES#
###############################

er <- read.table("Peso-Dollar Exchange Rate.txt",header=T)
read.ts <- ts(er[[2]],frequency=12,start=c(1980,1),end=c(2022,8))

autoplot(read.ts,
     main="Peso-Dollar Exchange Rate: Jan 1980 - Aug 2022",
     ylab="Exchange Rate (USD to Peso)", xlab="")

autoplot(diff(read.ts),
     main="Differenced Series of the Peso-Dollar Exchange Rate",
     ylab="y(t)-y(t-1)", xlab="")

###########
#ADF TESTS#
###########

#Performing ADF tests on the original and differenced Peso-Dollar exchange rates
adf.test(read.ts, alternative="stationary")
adf.test(diff(read.ts), alternative="stationary")

#can also invoke the Phillips-Perron and the KPSS test
#PP.test(read.ts)
#PP.test(diff(read.ts))
#kpss.test(read.ts, null="Trend")
#kpss.test(diff(read.ts), null="Trend")

##############
#CORRELOGRAMS#
##############

#acf and pacf of Peso-Dollar exchange rate data
autoplot(acf(read.ts),main="ACF of Peso-Dollar Exchange Rate")
autoplot(pacf(read.ts),main="PACF of Peso-Dollar Exchange Rate")

#acf and pacf of the differenced Peso-Dollar exchange rate series
autoplot(acf(diff(read.ts)),main="ACF of the Differenced Exchange Rate Series")
autoplot(pacf(diff(read.ts)),main="PACF of the Differenced Exchange Rate Series")

##################
#ARIMA ESTIMATION#
##################

pder.arima <- Arima(read.ts,order=c(0,1,1),include.drift = TRUE)
summary(pder.arima)
pder.arima2 <- Arima(read.ts,order=c(1,1,0),include.drift = TRUE)
summary(pder.arima2)
pder.arima3 <- Arima(read.ts,order=c(1,1,1),include.drift = TRUE)
summary(pder.arima3)

checkresiduals(pder.arima)
Box.test(residuals(pder.arima), lag = 1, type = "Ljung")

pder.pred <- forecast(pder.arima, h = 48, level=c(97.5))
autoplot(pder.pred,
         main="Four-year ARIMA(0,1,1) forecasts for Peso-Dollar exchange rate",
         ylab="Exchange Rate (USD to Peso)", xlab="")

#AUTOMATIC ARIMA SELECTION
pder.auto<-auto.arima(read.ts,max.order = 12,trace=TRUE,seasonal=TRUE)
summary(pder.auto)


#SEASONAL ARIMA (converted into excel)
RGDP_constant_2018_prices <- read_excel("C:/Users/romualdo saliente jr/Downloads/RGDP-_constant-2018-prices_.xlsx")
rgdp.ts <- ts(read.table("RGDP (constant 2018 prices).txt",header = T),frequency=4,start=c(2000,1),end=c(2022,2))
rgdp.ts <- ts(RGDP_constant_2018_prices$rgdp, frequency = 4, start = c(2000, 1), end = c(2022, 2))

library(ggplot2)
autoplot(rgdp.ts, ts.colour="blue") +
  ggtitle("Quartely GDP of the Philippines", subtitle = "Q1 2000 - Q2 2022 (at constant 2018 prices)") +
  ylab("Real GDP") +
  xlab("Year") +
  labs(caption = "Source: National Statistics Authority") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),plot.caption = element_text(hjust = 0))

autoplot(acf(rgdp.ts),main="ACF of Real GDP")
autoplot(pacf(rgdp.ts),main="PACF of Real GDP")

#acf and pacf of the differenced Peso-Dollar exchange rate series
autoplot(acf(diff(rgdp.ts)),main="ACF of the Differenced RGDP Series")
autoplot(pacf(diff(rgdp.ts)),main="PACF of the Differenced RGDP Series")

rgdp.auto<-auto.arima(rgdp.ts,max.order = 12,trace=TRUE,seasonal=TRUE,lambda=0)
#lambda = 0 is used when the series is to undergo a logarithmic transformation
summary(rgdp.auto)

checkresiduals(rgdp.auto)
Box.test(residuals(rgdp.auto), lag = 1, type = "Ljung")

rgdp.pred <- forecast(rgdp.auto, h = 16, level=c(97.5))
autoplot(rgdp.pred, main="Four-year forecasts for Real GDP")

