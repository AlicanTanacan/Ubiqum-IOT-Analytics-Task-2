### ----------------------- IOT ANALYTICS -------------------------- ###
### ---------- Task 2: Visualize and Analyze Energy Data ----------- ###
### --------------------- by Alican Tanaçan ------------------------ ###
### ----------- Version 7: Modelization and Forecasting ------------ ###

### Libraries ----
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(zoo)
library(plotly)
library(gridExtra)
library(devtools)
library(bbplot)
library(scales)
library(forecast)
library(ggfortify)
library(caret)
library(prophet)

### Import The Clean Energy Data ----
NewData <- readRDS(file = "EnergyDataHolidaysAndTemp.rds")

### Modelization ----

## Preparing The Analysis Data
fcdata1 <- NewData %>% 
  group_by(Date.x) %>% 
  summarise(meanGAP = mean(GAP), 
            meanSM0 = mean(SM0),
            meanSM1 = mean(SM1),
            meanSM2 = mean(SM2),
            meanSM3 = mean(SM3),
            meanGRP = mean(GRP),
            meanGI = mean(GI),
            meanTempF = mean(AvgTempF),
            meanTempC = mean(AvgTempC))

fcdata2 <- NewData %>% 
  mutate(YEAR = year(DateTime), 
         MONTH = month(DateTime),
         WEEK = week(DateTime)) %>%
  group_by(YEAR, MONTH, WEEK) %>% 
  summarise(meanGAP = mean(GAP), 
            meanSM0 = mean(SM0),
            meanSM1 = mean(SM1),
            meanSM2 = mean(SM2),
            meanSM3 = mean(SM3),
            meanGRP = mean(GRP),
            meanGI = mean(GI),
            meanTempF = mean(AvgTempF),
            meanTempC = mean(AvgTempC))

## Time Series Creations

# By Month
tsbymonth <- ts(fcdata1, start = c(2007), frequency = 12)

tsbymonthGAP <- ts(fcdata1$meanGAP, start = c(2007), frequency = 12)

tsbymonthSM0 <- ts(fcdata1$meanSM0, start = c(2007), frequency = 12)

tsbymonthSM1 <- ts(fcdata1$meanSM1, start = c(2007), frequency = 12)

tsbymonthSM2 <- ts(fcdata1$meanSM2, start = c(2007), frequency = 12)

tsbymonthSM3 <- ts(fcdata1$meanSM3, start = c(2007), frequency = 12)

# By Week

tsbyweek <- ts(fcdata2, start = c(2007), frequency = 52)

### Visualization and Decomposition ----

# All Variables By Month
plot(tsbymonth[,c("meanGAP",
                  "meanSM0",
                  "meanSM1",
                  "meanSM2",
                  "meanSM3")], 
     main = "Average Energy Used By Month")

plot(tsbymonth[,c("meanGAP",
                  "meanGRP",
                  "meanGI",
                  "meanTempF",
                  "meanTempC")], 
     main = "Average GAP, GRP, GI and Temp By Month")

# All Variables By Week
plot(tsbyweek[,c("meanGAP",
                  "meanSM0",
                  "meanSM1",
                  "meanSM2",
                  "meanSM3")], 
     main = "Average Energy Used By Week")

plot(tsbyweek[,c("meanGAP",
                  "meanGRP",
                  "meanGI",
                  "meanTempF",
                  "meanTempC")], 
     main = "Average GAP, GRP, GI and Temp By Week")

# Global Energy Consumed By Month 
plot(decompose(tsbymonth[,"meanGAP"], 
               type = c("additive")))

components_tsbymonthGAP <- decompose(tsbymonthGAP)
plot(components_tsbymonthGAP)

# Energy Used In The Rest Of The House
plot(decompose(tsbymonth[,"meanSM0"], 
               type = c("additive")))

components_tsbymonthSM0 <- decompose(tsbymonthSM0)
plot(components_tsbymonthSM0)

# Energy Used In Kitchen
plot(decompose(tsbymonth[,"meanSM1"], 
               type = c("additive")))

components_tsbymonthSM1 <- decompose(tsbymonthSM1)
plot(components_tsbymonthSM1)

# Energy Used In Laundry
plot(decompose(tsbymonth[,"meanSM2"],
               type = c("multiplicative"))) 

components_tsbymonthSM2 <- decompose(tsbymonthSM2)
plot(components_tsbymonthSM2)

# Energy Used In Heater & AC
plot(decompose(tsbymonth[,"meanSM3"],
               type = c("additive")))

components_tsbymonthSM3 <- decompose(tsbymonthSM3)
plot(components_tsbymonthSM3)

# Average GRP In Time Series
plot(decompose(tsbymonth[,"meanGRP"], 
               type = c("additive")))

# Average GI In Time Series
plot(decompose(tsbymonth[,"meanGI"], 
               type = c("additive")))

# Average Temp (F) In Time Series
plot(decompose(tsbymonth[,"meanTempF"],
               type = c("multiplicative"))) 

# Average Temp (C) In Time Series
plot(decompose(tsbymonth[,"meanTempC"],
               type = c("additive")))

### Forecasting with HoltWinters, Arima, Linear and Naive ----

# Removing Seasonal Components
tsbymonthGAPAdjusted <- tsbymonthGAP - components_tsbymonthGAP$seasonal
autoplot(tsbymonthGAPAdjusted)
plot(decompose(tsbymonthGAPAdjusted))

tsbymonthSM0Adjusted <- tsbymonthSM0 - components_tsbymonthSM0$seasonal
autoplot(tsbymonthSM0Adjusted)
plot(decompose(tsbymonthSM0Adjusted))

tsbymonthSM1Adjusted <- tsbymonthSM1 - components_tsbymonthSM1$seasonal
autoplot(tsbymonthSM1Adjusted)
plot(decompose(tsbymonthSM1Adjusted))

tsbymonthSM2Adjusted <- tsbymonthSM2 - components_tsbymonthSM2$seasonal
autoplot(tsbymonthSM2Adjusted)
plot(decompose(tsbymonthSM2Adjusted))

tsbymonthSM3Adjusted <- tsbymonthSM3 - components_tsbymonthSM3$seasonal
autoplot(tsbymonthSM3Adjusted)
plot(decompose(tsbymonthSM3Adjusted))

# Applying HoltWinters
holtwintersGAP <- HoltWinters(tsbymonthGAPAdjusted, beta = F, gamma = F)
plot(holtwintersGAP, ylim = c(0, 25))

holtwintersSM0 <- HoltWinters(tsbymonthSM0Adjusted, beta = F, gamma = F)
plot(holtwintersSM0, ylim = c(0, 15))

holtwintersSM1 <- HoltWinters(tsbymonthSM1Adjusted, beta = F, gamma = F)
plot(holtwintersSM1, ylim = c(0, 3))

holtwintersSM2 <- HoltWinters(tsbymonthSM2Adjusted, beta = F, gamma = F)
plot(holtwintersSM2, ylim = c(0, 3))

holtwintersSM3 <- HoltWinters(tsbymonthSM3Adjusted, beta = F, gamma = F)
plot(holtwintersSM3, ylim = c(0, 10))

# HoltWinters Forecasts For 24 Months
fchwgap <- forecast(holtwintersGAP, h = 24)
plot(fchwgap, main = "Forecast of GAP from HoltWinters in months")

fchwsm0 <- forecast(holtwintersSM0, h = 24)
plot(fchwsm0, main = "Forecast of Rest from HoltWinters in months")

fchwsm1 <- forecast(holtwintersSM1, h = 24)
plot(fchwsm1, main = "Forecast of Kitchen from HoltWinters in months")

fchwsm2 <- forecast(holtwintersSM2, h = 24)
plot(fchwsm2, main = "Forecast of Laundry from HoltWinters in months")

fchwsm3 <- forecast(holtwintersSM3, h = 24)
plot(fchwsm3, main = "Forecast of Heater from HoltWinters in months")

# Arima Forecasts For 12 Months
tsGAParima <- auto.arima(tsbymonthGAP)
fcGAParima <- forecast(tsGAParima, h = 12)
plot(fcGAParima, 
     col = "red",
     lwd = 3,
     main = "Global Power 2011 Forecast For 12 Months With Arima Model",
     ylab = "Watt-Hour",
     xlab = "Time")

tsSM0arima <- auto.arima(tsbymonthSM0)
fcSM0arima <- forecast(tsSM0arima, h = 12)
plot(fcSM0arima, 
     col = "darkorange",
     lwd = 3,
     main = "Rest of Electical Power 2011 Forecast For 12 Months With Arima Model",
     ylab = "Watt-Hour",
     xlab = "Time") 

tsSM1arima <- auto.arima(tsbymonthSM1)
fcSM1arima <- forecast(tsSM1arima, h = 12)
plot(fcSM1arima,
     col = "darkgreen",
     lwd = 3,
     main = "Kitchen 2011 Forecast For 12 Months With Arima Model",
     ylab = "Watt-Hour",
     xlab = "Time") 

tsSM2arima <- auto.arima(tsbymonthSM2)
fcSM2arima <- forecast(tsSM2arima, h = 12)
plot(fcSM2arima, 
     col = "purple",
     lwd = 3,
     main = "Laundry Room 2011 Forecast For 12 Months With Arima Model",
     ylab = "Watt-Hour",
     xlab = "Time") 

tsSM3arima <- auto.arima(tsbymonthSM3)
fcSM3arima <- forecast(tsSM3arima, h = 12)
plot(fcSM3arima,
     col = "blue",
     lwd = 3,
     main = "Heater & AC 2011 Forecast For 12 Months With Arima Model",
     ylab = "Watt-Hour",
     xlab = "Time") 

# Linear Model Forecasts For 12 Months
tsbymonthGAPlm <- tslm(tsbymonthGAP~season + trend, tsbymonthGAP)
fcbymonthGAPlm <- forecast(tsbymonthGAPlm, h = 12)
autoplot(fcbymonthGAPlm)

tsadjGAPlm <- tslm(tsbymonthGAPAdjusted~season + trend, tsbymonthGAPAdjusted)
fcadjGAPlm <- forecast(tsadjGAPlm, h = 12)
autoplot(fcadjGAPlm)

# Naive Method Forecasts For 12 Months
plot(naive(tsbymonthGAP, h = 12))
plot(naive(tsbymonthGAPAdjusted, h = 12))

# Seasonal Naive Method For 12 Months
seasonalnaiveGAP <- snaive(tsbymonthGAP, h = 12)
plot(seasonalnaiveGAP1, 
     col = "red",
     lwd = 3,
     main = "Global Active Power 2011 Forecast For 12 Months with Seasonal Naive Method",
     ylab = "Watt-Hour",
     xlab = "Time") 

seasonalnaiveSM0 <- snaive(tsbymonthSM0, h = 12)
plot(seasonalnaiveSM0, 
     col = "darkorange",
     lwd = 3,
     main = "Rest of Electrical Usage 2011 Forecast For 12 Months with Seasonal Naive Method",
     ylab = "Watt-Hour",
     xlab = "Time") 

seasonalnaiveSM1 <- snaive(tsbymonthSM1, h = 12)
plot(seasonalnaiveSM1, 
     col = "darkgreen",
     lwd = 3,
     main = "Kitchen 2011 Forecast For 12 Months with Seasonal Naive Method",
     ylab = "Watt-Hour",
     xlab = "Time") 

seasonalnaiveSM2 <- snaive(tsbymonthSM2, h = 12)
plot(seasonalnaiveSM2, 
     col = "purple",
     lwd = 3,
     main = "Laundry 2011 Forecast For 12 Months with Seasonal Naive Method",
     ylab = "Watt-Hour",
     xlab = "Time") 

seasonalnaiveSM3 <- snaive(tsbymonthSM3, h = 12)
plot(seasonalnaiveSM3, 
     col = "blue",
     lwd = 3,
     main = "Heater & AC 2011 Forecast For 12 Months with Seasonal Naive Method",
     ylab = "Watt-Hour",
     xlab = "Time") 
### Forecasting 2010 As Test Data ----

# Training And Test Data Creation
NewData %>% 
  filter(year(DateTime) < 2010) -> trainset

NewData %>% 
  filter(year(DateTime) >= 2010) -> testset

trainset %>% 
  group_by(Date.x) %>% 
  summarise(meanGAP = mean(GAP), 
            meanSM0 = mean(SM0),
            meanSM1 = mean(SM1),
            meanSM2 = mean(SM2),
            meanSM3 = mean(SM3)) -> rdytrainset

testset %>% 
  group_by(Date.x) %>% 
  summarise(meanGAP = mean(GAP), 
            meanSM0 = mean(SM0),
            meanSM1 = mean(SM1),
            meanSM2 = mean(SM2),
            meanSM3 = mean(SM3)) -> rdytestset

## Analyzing Global Active Power Predictions

# Time Series Creation For Train And Test Sets 
tstrainsetGAP <- ts(rdytrainset$meanGAP, 
                    start = c(2007), 
                    frequency = 12)
tstestsetGAP <- ts(rdytestset$meanGAP, 
                   start = c(2010), 
                   frequency = 12)

# Linear Model Forecast
tstrainGAPlm <- tslm(tstrainsetGAP~season + trend, tstrainsetGAP)
fctrainGAPlm <- forecast(tstrainGAPlm, h = 12)
plot(fctrainGAPlm, 
     main = "Global Active Power 2010 Forecast With Linear Model")
lines(tstestsetGAP, 
      col = "red", 
      lwd = 2)
summary(tstrainGAPlm)

# Arima Model Forecast
tstrainGAParima <- auto.arima(tstrainsetGAP)
fctrainGAParima <- forecast(tstrainGAParima, h = 12)
plot(fctrainGAParima, 
     main = "Global Active Power 2010 Forecast With Arima Model", 
     cex.main = 2)
lines(tstestsetGAP, 
      col = "red", 
      lwd = 2)

# HoltWinters Model Forecast
tstrainGAPhw <- HoltWinters(tstrainsetGAP)
fctrainGAPhw <- forecast(tstrainGAPhw, h = 12)
plot(fctrainGAPhw, 
     main = "Global Active Power 2010 Forecast With HoltWinters", 
     cex.main = 2)
lines(tstestsetGAP, 
      col = "red", 
      wd = 2)

## Analyzing Kitchen Predictions

# Time Series Creation For Train And Test Sets 
tstrainsetSM1 <- ts(rdytrainset$meanSM1, 
                    start = c(2007), 
                    frequency = 12)
tstestsetSM1 <- ts(rdytestset$meanSM1, 
                   start = c(2010), 
                   frequency = 12)

# Linear Model Forecast
tstrainSM1lm <- tslm(tstrainsetSM1~season + trend, tstrainsetSM1)
fctrainSM1lm <- forecast(tstrainSM1lm, h = 12)
plot(fctrainSM1lm, 
     main = "Kitchen 2010 Forecast With Linear Model")
lines(tstestsetSM1, 
      col = "red", 
      lwd = 2)
summary(tstrainSM1lm)

# Arima Model Forecast
tstrainSM1arima <- auto.arima(tstrainsetSM1)
fctrainSM1arima <- forecast(tstrainSM1arima, h = 12)
plot(fctrainSM1arima, 
     main = "Kitchen 2010 Forecast With Arima Model", 
     cex.main = 2)
lines(tstestsetSM1, 
      col = "red", 
      lwd = 2)
summary(tstrainSM1arima)

# HoltWinters Model Forecast
tstrainSM1hw <- HoltWinters(tstrainsetSM1)
fctrainSM1hw <- forecast(tstrainSM1hw, h = 12)
plot(fctrainSM1hw, 
     main = "KItchen 2010 Forecast With HoltWinters", 
     cex.main = 2)
lines(tstestsetSM1, 
      col = "red", 
      lwd = 2)

## Analyzing Laundry Predictions

# Time Series Creation For Train And Test Sets 
tstrainsetSM2 <- ts(rdytrainset$meanSM2, 
                    start = c(2007), 
                    frequency = 12)
tstestsetSM2 <- ts(rdytestset$meanSM2, 
                   start = c(2010), 
                   frequency = 12)

# Linear Model Forecast
tstrainSM2lm <- tslm(tstrainsetSM2~season + trend, tstrainsetSM2)
fctrainSM2lm <- forecast(tstrainSM2lm, h = 12)
plot(fctrainSM2lm, 
     main = "Laundry 2010 Forecast With Linear Model")
lines(tstestsetSM2, 
      col = "red", 
      lwd = 2)
summary(tstrainSM2lm)

# Arima Model Forecast
tstrainSM2arima <- auto.arima(tstrainsetSM2)
fctrainSM2arima <- forecast(tstrainSM2arima, h = 12)
plot(fctrainSM2arima,
     main = "Laundry 2010 Forecast With Arima Model", 
     cex.main = 2)
lines(tstestsetSM2, 
      col = "red", 
      lwd = 2)
summary(tstrainSM2arima)

# HoltWinters Model Forecast
tstrainSM2hw <- HoltWinters(tstrainsetSM2)
fctrainSM2hw <- forecast(tstrainSM2hw, h = 12)
plot(fctrainSM2hw, 
     main = "Laundry 2010 Forecast With HoltWinters", 
     cex.main = 2)
lines(tstestsetSM2, 
      col = "red", 
      lwd = 2)

## Analyzing Heater & AC Predictions

# Time Series Creation For Train And Test Sets 
tstrainsetSM3 <- ts(rdytrainset$meanSM3, 
                    start = c(2007), 
                    frequency = 12)
tstestsetSM3 <- ts(rdytestset$meanSM3, 
                   start = c(2010), 
                   frequency = 12)

# Linear Model Forecast
tstrainSM3lm <- tslm(tstrainsetSM3~season + trend, tstrainsetSM3)
fctrainSM3lm <- forecast(tstrainSM3lm, h = 12)
plot(fctrainSM3lm, 
     main = "Heater & AC 2010 Forecast With Linear Model")
lines(tstestsetSM3, 
      col = "red", 
      lwd = 2)
summary(tstrainSM3lm)

# Arima Model Forecast
tstrainSM3arima <- auto.arima(tstrainsetSM3)
fctrainSM3arima <- forecast(tstrainSM3arima, h = 12)
plot(fctrainSM3arima, 
     main = "Heater & AC 2010 Forecast With Arima Model", 
     cex.main = 2)
lines(tstestsetSM3, 
      col = "red", 
      lwd = 2)
summary(tstrainSM3arima)

# HoltWinters Model Forecast
tstrainSM3hw <- HoltWinters(tstrainsetSM3)
fctrainSM3hw <- forecast(tstrainSM3hw, h = 12)
plot(fctrainSM3hw, 
     main = "Heater & AC 2010 Forecast With HoltWinters", 
     cex.main = 2)
lines(tstestsetSM3, 
      col = "red", 
      lwd = 2)


