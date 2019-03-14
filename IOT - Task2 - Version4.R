### ----------------------- IOT ANALYTICS -------------------------- ###
### ---------- Task 2: Visualize and Analyze Energy Data ----------- ###
### --------------------- by Alican Tanaçan ------------------------ ###
### ------ Version 4: Data Analysis and Visualization Part 3 ------- ###

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

### Data Visualization by Dplyr and GGPlot2 ----

## Average Sub-Meter 0,1,2,3 by Hours on 8th Week  of 2007
EnergyData %>% 
  filter(year(DateTime) == 2007 & 
         week(DateTime) == 8) %>%  
  group_by(HOUR = hour(DateTime), DAY = day(DateTime)) %>%
  summarise(MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(HOUR)) +
  geom_line(aes(y = MeanSM0), color = "red") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_label(aes(x = 24, y = 46, label = "rest"), color = "red", size = 4) +
  geom_label(aes(x = 24, y = 28, label = "kitchen"), color = "darkgreen", size = 4) +
  geom_label(aes(x = 24, y = 34, label = "laundry"), color = "purple", size = 4) +
  geom_label(aes(x = 24, y = 40, label = "heater"), color = "blue", size = 4) +
  labs(title = "Average Sub-Meter 0,1,2,3 by Hours on 8th Week of 2007") + 
  ylab("Watt-Hours") + 
  xlab("Hours") + 
  theme_light() +
  facet_wrap(~DAY) -> plot2007week8
plot2007week8

## Average Sub-Meter 0,1,2,3 by Hours on 9th Week  of 2007
EnergyData %>% 
  filter(year(DateTime) == 2007 & 
           week(DateTime) == 9) %>%  
  group_by(HOUR = hour(DateTime), 
           DAY = day(DateTime)) %>%
  summarise(MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(HOUR)) +
  geom_line(aes(y = MeanSM0), color = "red") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_label(aes(x = 24, y = 46, label = "rest"), color = "red", size = 4) +
  geom_label(aes(x = 24, y = 28, label = "kitchen"), color = "darkgreen", size = 4) +
  geom_label(aes(x = 24, y = 34, label = "laundry"), color = "purple", size = 4) +
  geom_label(aes(x = 24, y = 40, label = "heater"), color = "blue", size = 4) +
  labs(title = "Average Sub-Meter 0,1,2,3 by Hours on 9th Week of 2007") + 
  ylab("Watt-Hours") + 
  xlab("Hours") + 
  theme_light() +
  facet_wrap(~DAY) -> plot2007week9
plot2007week9

## Sub-meter 1, 2 and 3 with 3 Hour Frequency on 8th week of 2008
EnergyData %>% 
  filter(year(DateTime) == 2008 & 
           week(DateTime) == 8 & 
           (hour(DateTime) == 3 | 
              hour(DateTime) == 6 | 
              hour(DateTime) == 9 |
              hour(DateTime) == 12 |
              hour(DateTime) == 15 | 
              hour(DateTime) == 18 |
              hour(DateTime) == 21 |
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
  layout(title = "Power Consumption on 8th Week of 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)")) -> plot2008week8
plot2008week8

## Sub-meter 1, 2 and 3 with 3 Hour Frequency on 9th week of 2008
EnergyData %>% 
  filter(year(DateTime) == 2008 & 
           week(DateTime) == 9 & 
           (hour(DateTime) == 3 | 
              hour(DateTime) == 6 | 
              hour(DateTime) == 9 |
              hour(DateTime) == 12 |
              hour(DateTime) == 15 | 
              hour(DateTime) == 18 |
              hour(DateTime) == 21 |
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
  layout(title = "Power Consumption on 9th Week of 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)")) -> plot2008week9
plot2008week9

## Sub-meter 1, 2 and 3 with 3 Hour Frequency on 10th week of 2008
EnergyData %>% 
  filter(year(DateTime) == 2008 & 
           week(DateTime) == 10 &
           (hour(DateTime) == 3 | 
              hour(DateTime) == 6 | 
              hour(DateTime) == 9 |
              hour(DateTime) == 12 |
              hour(DateTime) == 15 | 
              hour(DateTime) == 18 |
              hour(DateTime) == 21 |
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
  layout(title = "Power Consumption on 10th Week of 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)")) -> plot2008week10
plot2008week10

## Average Sub-Meter 0,1,2,3 by Hours on 32nd Week of 2008
EnergyData %>% 
  filter(year(DateTime) == 2008 & 
           week(DateTime) == 32) %>%  
  group_by(HOUR = hour(DateTime), 
           DAY = day(DateTime)) %>%
  summarise(MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(HOUR)) +
  geom_line(aes(y = MeanSM0), color = "red") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_label(aes(x = 24, y = 46, label = "rest"), color = "red", size = 4) +
  geom_label(aes(x = 24, y = 28, label = "kitchen"), color = "darkgreen", size = 4) +
  geom_label(aes(x = 24, y = 34, label = "laundry"), color = "purple", size = 4) +
  geom_label(aes(x = 24, y = 40, label = "heater"), color = "blue", size = 4) +
  labs(title = "Average Sub-Meter 0,1,2,3 by Hours on 32nd Week of 2008") + 
  ylab("Watt-Hours") + 
  xlab("Hours") + 
  theme_light() +
  facet_wrap(~DAY) -> plot2008week32
plot2008week32

## Average Sub-Meter 0,1,2,3 by Hours on 33rd Week of 2008
EnergyData %>% 
  filter(year(DateTime) == 2008 & 
           week(DateTime) == 33) %>%  
  group_by(HOUR = hour(DateTime), 
           DAY = day(DateTime)) %>%
  summarise(MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(HOUR)) +
  geom_line(aes(y = MeanSM0), color = "red") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_label(aes(x = 24, y = 46, label = "rest"), color = "red", size = 4) +
  geom_label(aes(x = 24, y = 28, label = "kitchen"), color = "darkgreen", size = 4) +
  geom_label(aes(x = 24, y = 34, label = "laundry"), color = "purple", size = 4) +
  geom_label(aes(x = 24, y = 40, label = "heater"), color = "blue", size = 4) +
  labs(title = "Average Sub-Meter 0,1,2,3 by Hours on 33rd Week of 2008") + 
  ylab("Watt-Hours") + 
  xlab("Hours") + 
  theme_light() +
  facet_wrap(~DAY) -> plot2008week33
plot2008week33

## Average Sub-Meter 0,1,2,3 by Hours on 35th Week of 2008
EnergyData %>% 
  filter(year(DateTime) == 2008 & 
           week(DateTime) == 35) %>%  
  group_by(HOUR = hour(DateTime), 
           DAY = day(DateTime)) %>%
  summarise(MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(HOUR)) +
  geom_line(aes(y = MeanSM0), color = "red") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_label(aes(x = 24, y = 46, label = "rest"), color = "red", size = 4) +
  geom_label(aes(x = 24, y = 28, label = "kitchen"), color = "darkgreen", size = 4) +
  geom_label(aes(x = 24, y = 34, label = "laundry"), color = "purple", size = 4) +
  geom_label(aes(x = 24, y = 40, label = "heater"), color = "blue", size = 4) +
  labs(title = "Average Sub-Meter 0,1,2,3 by Hours on 35th Week of 2008") + 
  ylab("Watt-Hours") + 
  xlab("Hours") + 
  theme_light() +
  facet_wrap(~DAY) -> plot2008week35
plot2008week35

## Average Sub-Meter 0,1,2,3 by Hours on 30th Weekof 2009
EnergyData %>% 
  filter(year(DateTime) == 2009 & 
           week(DateTime) == 30) %>%  
  group_by(HOUR = hour(DateTime), 
           DAY = day(DateTime)) %>%
  summarise(MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(HOUR)) +
  geom_line(aes(y = MeanSM0), color = "red") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_label(aes(x = 24, y = 46, label = "rest"), color = "red", size = 4) +
  geom_label(aes(x = 24, y = 28, label = "kitchen"), color = "darkgreen", size = 4) +
  geom_label(aes(x = 24, y = 34, label = "laundry"), color = "purple", size = 4) +
  geom_label(aes(x = 24, y = 40, label = "heater"), color = "blue", size = 4) +
  labs(title = "Average Sub-Meter 0,1,2,3 by Hours on 30th Week of 2009") + 
  ylab("Watt-Hours") + 
  xlab("Hours") + 
  theme_light() +
  facet_wrap(~DAY) -> plot2009week30
plot2009week30

## Average Sub-Meter 0,1,2,3 by Hours on 31st Weekof 2009
EnergyData %>% 
  filter(year(DateTime) == 2009 & 
           week(DateTime) == 31) %>%  
  group_by(HOUR = hour(DateTime), 
           DAY = day(DateTime)) %>%
  summarise(MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(HOUR)) +
  geom_line(aes(y = MeanSM0), color = "red") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_label(aes(x = 24, y = 46, label = "rest"), color = "red", size = 4) +
  geom_label(aes(x = 24, y = 28, label = "kitchen"), color = "darkgreen", size = 4) +
  geom_label(aes(x = 24, y = 34, label = "laundry"), color = "purple", size = 4) +
  geom_label(aes(x = 24, y = 40, label = "heater"), color = "blue", size = 4) +
  labs(title = "Average Sub-Meter 0,1,2,3 by Hours on 31st Week of 2009") + 
  ylab("Watt-Hours") + 
  xlab("Hours") + 
  theme_light() +
  facet_wrap(~DAY) -> plot2009week31
plot2009week31

## Average Sub-Meter 0,1,2,3 by Hours on 32nd Weekof 2009
EnergyData %>% 
  filter(year(DateTime) == 2009 & 
           week(DateTime) == 32) %>%  
  group_by(HOUR = hour(DateTime), 
           DAY = day(DateTime)) %>%
  summarise(MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(HOUR)) +
  geom_line(aes(y = MeanSM0), color = "red") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_label(aes(x = 24, y = 46, label = "rest"), color = "red", size = 4) +
  geom_label(aes(x = 24, y = 28, label = "kitchen"), color = "darkgreen", size = 4) +
  geom_label(aes(x = 24, y = 34, label = "laundry"), color = "purple", size = 4) +
  geom_label(aes(x = 24, y = 40, label = "heater"), color = "blue", size = 4) +
  labs(title = "Average Sub-Meter 0,1,2,3 by Hours on 32nd Week of 2009") + 
  ylab("Watt-Hours") + 
  xlab("Hours") + 
  theme_light() +
  facet_wrap(~DAY) -> plot2009week32
plot2009week32