### ----------------------- IOT ANALYTICS -------------------------- ###
### ---------- Task 2: Visualize and Analyze Energy Data ----------- ###
### --------------------- by Alican Tanaçan ------------------------ ###
### ------- Version 6: Joining Holiday and Temperature Data -------- ###

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

### Import The Clean Energy Data ----
EnergyData <- readRDS(file = "OriginalEnergyCleanData.rds")

## Take out the year 2006 since it has only 1 month of data
EnergyData %>% 
  filter(year(DateTime) != 2006) -> EnergyData

### Import The Holiday and Weather Temperature Data ----
HolidayWeatherData <- read.csv("SceauxHolidayAndWeatherData.csv")

## Convert "Date" variable in HolidayWeatherData into a date format
HolidayWeatherData$Date <- as.Date(HolidayWeatherData$Date, format='%m/%d/%Y')

## Convert "DateTime" from POSIXlt to POSIXct 
HolidayWeatherData$Date <- as.POSIXct(HolidayWeatherData$Date, "%Y/%m/%d")

## Add the time zone
attr(HolidayWeatherData$Date, "tzone") <- "Europe/Paris"

## Joining Two Data Sets
EnergyData$id <- date(EnergyData$DateTime)

HolidayWeatherData$id <- date(HolidayWeatherData$Date)

temp <- left_join(EnergyData, HolidayWeatherData, by = "id")