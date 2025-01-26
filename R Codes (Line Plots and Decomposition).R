
#the library command is used to load packages
library(ggplot2)   #for plots
library(ggfortify) #for plots
library(xts)       #for STL decomposition
library(forecast)  #for plots
library(mFilter)   #for HP filter
library(x12)       #for x12 ARIMA decomposition


########################
#DEFINING A TIME SERIES#
########################

#read a table with first row as header
er <- read.table("Peso Dollar Exchange Rate.txt",header=T)

#define a time series data using ts function 
read.ts <- ts(er[[2]],frequency=12,start=c(1980,1),end=c(2022,8))
  #must know start and end dates beforehand. 
  #[[2]] since the exchange rate is at the second column
  #use frequency=12 for monthly data

########################
#CREATING A TIME PLOT###
########################

#01 basic line plot
plot(read.ts)

#02 line plot with chart elements
plot(read.ts,
     main="Peso-Dollar Exchange Rate: Jan 1980 - Aug 2022",
     ylab="Exchange Rate (USD to Peso)", xlab="")

#03 line plot using the ggplot2 package's autoplot
autoplot(read.ts, ts.colour="blue") +
   ggtitle("Average Monthly Peso-Dollar Exchange Rates", subtitle = "(Jan 1980 - Aug 2022)") +
   ylab("Exchange Rate (USD to Peso)") +
   labs(caption = "Source: Bangko Sentral ng Pilipinas") +
   theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),plot.caption = element_text(hjust = 0))

#For inflation data
inf <- read.table("Monthly Inflation Rate (100=2018).txt",header=T)
inf.ts <- ts(inf[[2]],frequency=12,start=c(2018,1),end=c(2022,8))
autoplot(inf.ts, ts.colour="blue") +
  ggtitle("Year-on-year Inflation Rates, All Items", subtitle = "(Jan 2018 - Aug 2022)") +
  ylab("Inflation Rate") +
  labs(caption = "Source: National Statistics Authority") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),plot.caption = element_text(hjust = 0))

###########################
#HODRICK-PRESCOTT FILTER###
###########################

#HP filter
exp_data <- read.table("export.txt",header=T)

export.hp <- hpfilter(exp_data$export, freq=14400, type="lambda") #perform HP filter
exp_data <- cbind(exp,export.hp$trend)                            #merge the filtered data
head(exp_data)
colnames(exp_data)[3] <- "hp_trend"                               #change column name
head(exp_data)
plot(export.hp)

###########################
#TIME SERIES DECOMPOSITION#
###########################

#time series decomposition using MOVING AVERAGES
rgdp.ts <- ts(read.table("RGDP (constant 2018 prices).txt",header = T),frequency=4,start=c(2000,1),end=c(2022,2))

#line plot with elements
autoplot(rgdp.ts, ts.colour="blue") +
  ggtitle("Quartely GDP of the Philippines", subtitle = "Q1 2000 - Q2 2022 (at constant 2018 prices)") +
  ylab("Real GDP") +
  xlab("Year") +
  labs(caption = "Source: National Statistics Authority") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),plot.caption = element_text(hjust = 0))

#01 MOVING AVERAGE DECOMPOSITION
decomp_rgdp <- decompose(rgdp.ts,"multiplicative")
#plot components automatically
autoplot(decomp_rgdp) + 
 ggtitle("Decomposition of RGDP using moving averages (multiplicative)")

#overlay the t-c component over the original series
autoplot(decomp_rgdp$x, series="Observed") +
  autolayer(decomp_rgdp$trend, series="Trend-Cycle") +
  xlab("Year") + ylab("RGDP") +
  ggtitle("Decomposition of RGDP (multiplicative)") +
  scale_colour_manual(values=c("Observed"="grey65","Trend-Cycle"="red"),
                      breaks=c("Observed","Trend-Cycle"))

#plot components manually
par(mfrow=c(2,2)) #run this par if you want a single panel of four graphs
plot(rgdp.ts,ylab="Observed",xlab="")
plot(decomp_rgdp$trend,ylab="Trend-Cycle",xlab="")
plot(decomp_rgdp$seasonal,ylab="Seasonality",xlab="")
plot(decomp_rgdp$random,ylab="Irregular",xlab="")
par(mfrow=c(1,1)) #run this par if you to revert to a single panel of one graph

#02 SEASONAL AND TREND DECOMPOSITION USING LOESS
#time series decomposition using Seasonal and Trend decomposition using Loess
#In STL decomposition, seasonality indices are allowed to vary
decomp_stl_rgdp <- mstl(rgdp.ts)
autoplot(decomp_stl_rgdp) + 
    ggtitle("STL DECOMPOSITION USING LOESS (locally weighted scatterplot smoothing)")

#03 DECOMPOSITION USING X12-ARIMA
decomp_x12_rgdp <- x12(rgdp.ts)
#plot components manually, autoplot does not work with x12 package output
par(mfrow=c(2,2)) #run this par if you want a single panel of four graphs
plot(rgdp.ts,ylab="Observed",xlab="")
plot(decomp_x12_rgdp@d12,ylab="Trend-Cycle",xlab="")
plot(decomp_x12_rgdp@d10,ylab="Seasonality",xlab="")
plot(decomp_x12_rgdp@d13,ylab="Irregular",xlab="")
par(mfrow=c(1,1)) #run this par if you to revert to a single panel of one graph

#OTHER EXAMPLES
#Irregularity Component
export.ts <- ts(exp_data[[2]],frequency=12,start=c(1991,1),end=c(2022,6))
decomp_exp <- decompose(export.ts,"multiplicative")
autoplot(decomp_exp)
autoplot(decomp_exp$random,ts.colour="red",main="Irregularity Component of Exports Data")+
  theme(plot.title = element_text(hjust = 0.5))

pder_data <- read.table("Peso-Dollar Exchange Rate (1980Jan-2022Aug).txt",header=T)
pder.ts <- ts(pder_data[[1]],frequency=12,start=c(1980,1),end=c(2022,8))
autoplot(pder.ts,main="Peso-Dollar Exchange Rate\n January 1980 - August 2022)")+
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Exchange Rate")

decomp_pder <- decompose(pder.ts,"multiplicative")
autoplot(decomp_pder$random)