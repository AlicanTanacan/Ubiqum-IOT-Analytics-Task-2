### ----------------------- IOT ANALYTICS -------------------------- ###
### ---------- Task 2: Visualize and Analyze Energy Data ----------- ###
### --------------------- by Alican Tanaçan ------------------------ ###
### ---------- Version 8: Extra Analysis With Joined Data ---------- ###

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

## Total Energy Consumption
NewData %>% 
  filter(year(DateTime) != 2006) %>% 
  group_by(YEAR = year(DateTime)) %>% 
  summarise(Totalkwh = (sum(GAP) / 1000),
            sumSM0kwh = (sum(SM0) / 1000),
            sumSM1kwh = (sum(SM1) / 1000),
            sumSM2kwh = (sum(SM2) / 1000),
            sumSM3kwh = (sum(SM3) / 1000)) %>% 
  mutate(TotalCostEuro = c(Totalkwh * 0.16),
         SM0CostEuro = c(sumSM0kwh * 0.16),
         SM1CostEuro = c(sumSM1kwh * 0.16),
         SM2CostEuro = c(sumSM2kwh * 0.16),
         SM3CostEuro = c(sumSM3kwh * 0.16)) %>% 
  round() -> EnergyYearlyCostData

## Average Sub-Meter 0,1,2,3 by Hours on December 2007
## Week 50
NewData %>% 
  filter(year(DateTime) == 2007 & 
           week(DateTime) == 50) %>%  
  group_by(HOUR = hour(DateTime), DAY = day(DateTime)) %>%
  summarise(MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3),
            MeanTempF = mean(AvgTempF),
            MeanTempC = mean(AvgTempC)) %>% 
  ggplot(aes(HOUR)) +
  geom_line(aes(y = MeanSM0), color = "red") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_line(aes(y = MeanTempF), color = "yellow") +
  geom_line(aes(y = MeanTempC), color = "orange") +
  labs(title = "Average Sub-Meter 0,1,2,3 by Hours on 50th Week of 2007") + 
  ylab("Watt-Hours") + 
  xlab("Hours") + 
  theme_light() +
  facet_wrap(~DAY) -> plot2007week50
plot2007week50

## Week 49
NewData %>% 
  filter(year(DateTime) == 2007 & 
           week(DateTime) == 49) %>%  
  group_by(HOUR = hour(DateTime), DAY = day(DateTime)) %>%
  summarise(MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3),
            MeanTempF = mean(AvgTempF),
            MeanTempC = mean(AvgTempC)) %>% 
  ggplot(aes(HOUR)) +
  geom_line(aes(y = MeanSM0), color = "red") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_line(aes(y = MeanTempF), color = "yellow") +
  geom_line(aes(y = MeanTempC), color = "orange") +
  labs(title = "Average Sub-Meter 0,1,2,3 by Hours on 49th Week of 2007") + 
  ylab("Watt-Hours") + 
  xlab("Hours") + 
  theme_light() +
  facet_wrap(~DAY) -> plot2007week49
plot2007week49

## Yearly Energy Consumption With Temperature Data
NewData %>% 
  filter(year(DateTime) != 2006) %>% 
  group_by(DATE = week(DateTime), YEAR = year(DateTime)) %>%
  summarise(MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3),
            MeanTempC = mean(AvgTempC)) %>% 
  ggplot(aes(DATE)) +
  geom_line(aes(y = MeanSM0), color = "red") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_line(aes(y = MeanTempC), color = "darkorange") +
  geom_label(aes(x = 53, y = 14, label = "rest"), color = "red", size = 4) +
  geom_label(aes(x = 53, y = 0, label = "kitchen"), color = "darkgreen", size = 4) +
  geom_label(aes(x = 53, y = 3, label = "laundry"), color = "purple", size = 4) +
  geom_label(aes(x = 53, y = 9, label = "heater"), color = "blue", size = 4) +
  geom_label(aes(x = 53, y = 6, label = "Temp°C"), color = "darkorange", size = 4) +
  labs(title = "Average Sub-Meter 0,1,2,3 and Temp by Weeks in Each Year") + 
  ylab("Watt-Hours") + 
  xlab("Week") + 
  theme_light() +
  facet_wrap(~YEAR) -> plotSMTempYearsbyWeek
plotSMTempYearsbyWeek

## Average Temperature in 32nd Week of 2009
NewData %>% 
  filter(year(DateTime) == 2009 &
           week(DateTime) == 32) %>% 
  summarise(Avg_Temp_C = mean(AvgTempC)) %>% 
  summary() # 21.59 cantigrate degrees

## Preparing The Forecasting Data
fcdata3 <- NewData %>% 
  group_by(Date.x) %>% 
  summarise(Avg_Global_Power = mean(GAP), 
            Avg_Rest = mean(SM0),
            Avg_Kitchen = mean(SM1),
            Avg_Laundry = mean(SM2),
            Avg_Heater = mean(SM3),
            Avg_Temp_C = mean(AvgTempC))

## Time Series Creations

# By Month
tsbymonth <- ts(fcdata3, start = c(2007), frequency = 12)

tsbymonthGAP <- ts(fcdata3$Avg_Global_Power, start = c(2007), frequency = 12)

tsbymonthSM0 <- ts(fcdata3$Avg_Rest, start = c(2007), frequency = 12)

tsbymonthSM1 <- ts(fcdata3$Avg_Kitchen, start = c(2007), frequency = 12)

tsbymonthSM2 <- ts(fcdata3$Avg_Laundry, start = c(2007), frequency = 12)

tsbymonthSM3 <- ts(fcdata3$Avg_Heater, start = c(2007), frequency = 12)

tsbymonthTempC <- ts(fcdata3$Avg_Temp_C, start = c(2007), frequency = 12)

# All Variables By Month
plot(tsbymonth[,c("Avg_Global_Power",
                  "Avg_Rest",
                  "Avg_Kitchen",
                  "Avg_Heater",
                  "Avg_Temp_C")], 
     main = "Average Energy Usage and Temperature By Month")

# Global Energy Decomposition
decomposed_tsbymonthGAP <- decompose(tsbymonthGAP)
plot(decomposed_tsbymonthGAP, 
     col = "red",
     lwd = 4)
# Rest of Energy Used Decomposition
decomposed_tsbymonthSM0 <- decompose(tsbymonthSM0)
plot(decomposed_tsbymonthSM0, 
     col = "darkorange",
     lwd = 4)
# Kitchen Decomposition
decomposed_tsbymonthSM1 <- decompose(tsbymonthSM1)
plot(decomposed_tsbymonthSM1, 
     col = "darkgreen",
     lwd = 4)
# Laundry Decomposition
decomposed_tsbymonthSM2 <- decompose(tsbymonthSM2)
plot(decomposed_tsbymonthSM2, 
     col = "purple",
     lwd = 4)
# Heater Decomposition
decomposed_tsbymonthSM3 <- decompose(tsbymonthSM3)
plot(decomposed_tsbymonthSM3, 
     col = "blue",
     lwd = 4)
# Temperature Decomposition
decomposed_tsbymonthTemp <- decompose(tsbymonthTempC)
plot(decomposed_tsbymonthTemp, 
     col = "yellow",
     lwd = 4)

# Trend Rest of the House
trend_tsbymonthSM0 <- decompose(tsbymonthSM0)$trend
plot(trend_tsbymonthSM0, 
     col = "darkorange",
     lwd = 4,
     main = "Trend of Electric Usage in the Rest of the House",
     ylab = "Watt-Hour")
# Trend Kitchen
trend_tsbymonthSM1 <- decompose(tsbymonthSM1)$trend
plot(trend_tsbymonthSM1, 
     col = "darkgreen",
     lwd = 4,
     main = "Trend of Electric Usage in the Kitchen",
     ylab = "Watt-Hour")
# Trend Laundry
trend_tsbymonthSM2 <- decompose(tsbymonthSM2)$trend
plot(trend_tsbymonthSM2, 
     col = "purple",
     lwd = 4,
     main = "Trend of Electric Usage in the Laundry",
     ylab = "Watt-Hour")
# Trend Heater & AC
trend_tsbymonthSM3 <- decompose(tsbymonthSM3)$trend
plot(trend_tsbymonthSM3, 
     col = "blue",
     lwd = 4,
     main = "Trend of Electric Usage in the Heater & AC",
     ylab = "Watt-Hour")
# Trend Temperature
trend_tsbymonthTempC <- decompose(tsbymonthTempC)$trend
plot(trend_tsbymonthTempC, 
     col = "brown",
     lwd = 4,
     main = "Trend of Temperature",
     ylab = "°C")

