### ----------------------- IOT ANALYTICS -------------------------- ###
### ---------- Task 2: Visualize and Analyze Energy Data ----------- ###
### --------------------- by Alican Tanaçan ------------------------ ###
### ----- Version 4: Analyses According to the Plan of Attack ------ ###

### Libraries ----
library(dplyr)
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

### Import The Clean Data ----
EnergyData <- readRDS(file = "OriginalEnergyCleanData.rds")

### According to the Plan of Attack ----

## Visualize a Single Day with Plotly
## Subset the 9th day of January 2008 - All observations
houseDay <- filter(EnergyData, year(DateTime) == 2008 & 
                               month(DateTime) == 1 & 
                               day(DateTime) == 9)

## Plot sub-meter 1
plot_ly(houseDay, 
        x = ~houseDay$DateTime, 
        y = ~houseDay$SM1, 
        type = 'scatter', 
        mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, 
        x = ~houseDay$DateTime, 
        y = ~houseDay$SM1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%
  add_trace(y = ~houseDay$SM2, 
            name = 'Laundry Room', 
            mode = 'lines') %>%
  add_trace(y = ~houseDay$SM3, 
            name = 'Water Heater & AC', 
            mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Reducing Granularity
## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay9 <- filter(EnergyData, 
                    year(DateTime) == 2008 & 
                    month(DateTime) == 1 & 
                    day(DateTime) == 9 & 
                    (minute(DateTime) == 0 | 
                     minute(DateTime) == 10 | 
                     minute(DateTime) == 20 | 
                     minute(DateTime) == 30 | 
                     minute(DateTime) == 40 | 
                     minute(DateTime) == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay9, 
        x = ~houseDay9$DateTime, 
        y = ~houseDay9$SM1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%
  add_trace(y = ~houseDay9$SM2, 
            name = 'Laundry Room',
            mode = 'lines') %>%
  add_trace(y = ~houseDay9$SM3,
            name = 'Water Heater & AC', 
            mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## More Visualizations
## Sub-meter 1, 2 and 3 with 10 Minute frequency on 10th Jan 2008
EnergyData %>% 
  filter(year(DateTime) == 2008 & 
         month(DateTime) == 1 & 
         day(DateTime) == 10 & 
         (minute(DateTime) == 0 | 
          minute(DateTime) == 10 | 
          minute(DateTime) == 20 | 
          minute(DateTime) == 30 | 
          minute(DateTime) == 40 | 
          minute(DateTime) == 50)) %>% 
  plot_ly(x = ~DateTime, 
          y = ~SM1, 
          name = 'Kitchen', 
          type = 'scatter', 
          mode = 'lines') %>%
  add_trace(y = ~SM2, 
            name = 'Laundry Room',
            mode = 'lines') %>%
  add_trace(y = ~SM3,
            name = 'Water Heater & AC', 
            mode = 'lines') %>%
  layout(title = "Power Consumption January 10th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Sub-meter 1, 2 and 3 with 10 Minute frequency on 2nd week of Jan 2008
EnergyData %>% 
  filter(year(DateTime) == 2008 & 
         month(DateTime) == 1 & 
         week(DateTime) == 2 & 
         (minute(DateTime) == 0 | 
          minute(DateTime) == 10 | 
          minute(DateTime) == 20 | 
          minute(DateTime) == 30 | 
          minute(DateTime) == 40 | 
          minute(DateTime) == 50)) %>% 
  plot_ly(x = ~DateTime, 
          y = ~SM1, 
          name = 'Kitchen', 
          type = 'scatter', 
          mode = 'lines') %>%
  add_trace(y = ~SM2, 
            name = 'Laundry Room',
            mode = 'lines') %>%
  add_trace(y = ~SM3,
            name = 'Water Heater & AC', 
            mode = 'lines') %>%
  layout(title = "Power Consumption on 2nd Week of January 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Sub-meter 1, 2 and 3 with 6 Hour frequency on 2nd week of Jan 2008
EnergyData %>% 
  filter(year(DateTime) == 2008 & 
           month(DateTime) == 1 & 
           week(DateTime) == 2 & 
           (hour(DateTime) == 6 | 
            hour(DateTime) == 12 | 
            hour(DateTime) == 18 | 
            hour(DateTime) == 24)) %>% 
  plot_ly(x = ~DateTime, 
          y = ~SM1, 
          name = 'Kitchen', 
          type = 'scatter', 
          mode = 'lines') %>%
  add_trace(y = ~SM2, 
            name = 'Laundry Room',
            mode = 'lines') %>%
  add_trace(y = ~SM3,
            name = 'Water Heater & AC', 
            mode = 'lines') %>%
  layout(title = "Power Consumption on 2nd Week of January 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(EnergyData, 
                            weekdays(DateTime) == "Monday" & 
                            hour(DateTime) == 20 & 
                            minute(DateTime) == 1)

## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$SM3, 
                         frequency = 52, 
                         start = c(2007,1))

## Plot sub-meter 3 with autoplot
autoplot(tsSM3_070809weekly)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, 
         ts.colour = 'red', 
         xlab = "Time", 
         ylab = "Watt Hours", 
         main = "Sub-meter 3")

## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)

## Apply time series linear regression to the sub-meter 3 ts object and 
## use summary to obtain R2 and RMSE from the model you built
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, 
                           h = 20)

## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)

## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)

## Plot decomposed sub-meter 3 
plot(components070809SM3weekly)

## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)

## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very 
## small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, 
                              beta=FALSE, 
                              gamma=FALSE)

plot(tsSM3_HW070809, 
     ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, 
     ylim = c(0, 20), 
     ylab= "Watt-Hours", 
     xlab="Time - Sub-meter 3")

## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, 
                               h = 25, 
                               level = c(10,25))

## Plot only the forecasted area
plot(tsSM3_HW070809forC, 
     ylim = c(0, 20), 
     ylab= "Watt-Hours", 
     xlab="Time - Sub-meter 3", 
     start(2010))
