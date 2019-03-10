### ----------------------- IOT ANALYTICS -------------------------- ###
### ---------- Task 2: Visualize and Analyze Energy Data ----------- ###
### --------------------- by Alican Tanaçan ------------------------ ###
### ------ Version 2: Data Analysis and Visualization Part 1 ------- ###

## Libraries ----
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(plotly)
library(gridExtra)
library(devtools)
library(bbplot)
library(scales)

## Import The Clean Data ----
EnergyData <- readRDS(file = "OriginalEnergyCleanData.rds")

# Inspect the data
str(EnergyData)
summary(EnergyData)

## Data Visualization by Dplyr and GGPlot2 ----

# Smooth Line Graphs:

# Average Global Active Power Used by Day
EnergyData %>% 
  group_by(DATE = date(DateTime)) %>% 
  summarise(MeanGAP = mean(GAP_kWh)) %>%
  ggplot(aes(DATE, MeanGAP)) + 
  geom_line(color = "red") + 
  geom_smooth(se = F) +
  labs(title = "Average Global Active Power Used by Day") + 
  ylab("kWh") + 
  xlab("Time") + 
  theme_light() -> plot1
ggplotly(plot1)

# Average Sub-Meter 0 Power Used by Day
EnergyData %>% 
  group_by(DATE = date(DateTime)) %>% 
  summarise(MeanSM0 = mean(SM0)) %>%
  ggplot(aes(DATE, MeanSM0)) + 
  geom_line(color = "orange") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 0 Power Used by Day") + 
  ylab("kWh") + 
  xlab("Time") + 
  theme_light() -> plot2
ggplotly(plot2)

# Average Sub-Meter 1 Power Used by Day
EnergyData %>% 
  group_by(DATE = date(DateTime)) %>% 
  summarise(MeanSM1 = mean(SM1)) %>%
  ggplot(aes(DATE, MeanSM1)) + 
  geom_line(color = "green") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 1 Power Used by Day") + 
  ylab("kWh") + 
  xlab("Time") + 
  theme_light() -> plot3
ggplotly(plot3)

# Average Sub-Meter 1 Power Used in 2007
EnergyData %>%
  filter(year(DateTime) == 2007) %>% 
  group_by(DATE = week(DateTime)) %>% 
  summarise(MeanSM1 = mean(SM1)) %>%
  ggplot(aes(DATE, MeanSM1)) + 
  geom_line(color = "green") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 1 Power Used in 2007") + 
  ylab("kWh") + 
  xlab("Week") + 
  theme_light() -> plot3.1
ggplotly(plot3.1)

# Average Sub-Meter 1 Power Used in 2008
EnergyData %>%
  filter(year(DateTime) == 2008) %>% 
  group_by(DATE = week(DateTime)) %>% 
  summarise(MeanSM1 = mean(SM1)) %>%
  ggplot(aes(DATE, MeanSM1)) + 
  geom_line(color = "green") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 1 Power Used in 2008") + 
  ylab("kWh") + 
  xlab("Week") + 
  theme_light() -> plot3.2
ggplotly(plot3.2)

# Average Sub-Meter 1 Power Used in 2009
EnergyData %>%
  filter(year(DateTime) == 2009) %>% 
  group_by(DATE = week(DateTime)) %>% 
  summarise(MeanSM1 = mean(SM1)) %>%
  ggplot(aes(DATE, MeanSM1)) + 
  geom_line(color = "green") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 1 Power Used in 2009") + 
  ylab("kWh") + 
  xlab("Week") + 
  theme_light() -> plot3.3
ggplotly(plot3.3)

# Average Sub-Meter 1 Power Used in 2010
EnergyData %>%
  filter(year(DateTime) == 2010) %>% 
  group_by(DATE = week(DateTime)) %>% 
  summarise(MeanSM1 = mean(SM1)) %>%
  ggplot(aes(DATE, MeanSM1)) + 
  geom_line(color = "green") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 1 Power Used in 2010") + 
  ylab("kWh") + 
  xlab("Week") + 
  theme_light() -> plot3.4
ggplotly(plot3.4)

# See all Sub-Meter 1 plots in one scren
grid.arrange(plot3.1, plot3.2, plot3.3, plot3.4)

# Average Sub-Meter 2 Power Used by Day
EnergyData %>% 
  group_by(DATE = date(DateTime)) %>% 
  summarise(MeanSM2 = mean(SM2)) %>%
  ggplot(aes(DATE, MeanSM2)) + 
  geom_line(color = "purple") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 2 Power Used by Day") + 
  ylab("kWh") + 
  xlab("Time") + 
  theme_light() -> plot4
ggplotly(plot4)

# Average Sub-Meter 2 Power Used in 2007
EnergyData %>%
  filter(year(DateTime) == 2007) %>% 
  group_by(DATE = week(DateTime)) %>% 
  summarise(MeanSM2 = mean(SM2)) %>%
  ggplot(aes(DATE, MeanSM2)) + 
  geom_line(color = "purple") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 2 Power Used in 2007") + 
  ylab("kWh") + 
  xlab("Week") + 
  theme_light() -> plot4.1
ggplotly(plot4.1)

# Average Sub-Meter 2 Power Used in 2008
EnergyData %>%
  filter(year(DateTime) == 2008) %>% 
  group_by(DATE = week(DateTime)) %>% 
  summarise(MeanSM2 = mean(SM2)) %>%
  ggplot(aes(DATE, MeanSM2)) + 
  geom_line(color = "purple") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 2 Power Used in 2008") + 
  ylab("kWh") + 
  xlab("Week") + 
  theme_light() -> plot4.2
ggplotly(plot4.2)

# Average Sub-Meter 2 Power Used in 2009
EnergyData %>%
  filter(year(DateTime) == 2009) %>% 
  group_by(DATE = week(DateTime)) %>% 
  summarise(MeanSM2 = mean(SM2)) %>%
  ggplot(aes(DATE, MeanSM2)) + 
  geom_line(color = "purple") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 2 Power Used in 2009") + 
  ylab("kWh") + 
  xlab("Week") + 
  theme_light() -> plot4.3
ggplotly(plot4.3)

# Average Sub-Meter 2 Power Used in 2010
EnergyData %>%
  filter(year(DateTime) == 2010) %>% 
  group_by(DATE = week(DateTime)) %>% 
  summarise(MeanSM2 = mean(SM2)) %>%
  ggplot(aes(DATE, MeanSM2)) + 
  geom_line(color = "purple") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 2 Power Used in 2010") + 
  ylab("kWh") + 
  xlab("Week") + 
  theme_light() -> plot4.4
ggplotly(plot4.4)

# See all Sub-Meter 2 plots in one scren
grid.arrange(plot4.1, plot4.2, plot4.3, plot4.4)

# Average Sub-Meter 3 Power Used by Day
EnergyData %>% 
  group_by(DATE = date(DateTime)) %>% 
  summarise(MeanSM3 = mean(SM3)) %>%
  ggplot(aes(DATE, MeanSM3)) + 
  geom_line(color = "blue") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 3 Power Used by Day") + 
  ylab("kWh") + 
  xlab("Time") + 
  theme_light() -> plot5
ggplotly(plot5)

# Average Sub-Meter 3 Power Used in 2007
EnergyData %>%
  filter(year(DateTime) == 2007) %>% 
  group_by(DATE = week(DateTime)) %>% 
  summarise(MeanSM3 = mean(SM3)) %>%
  ggplot(aes(DATE, MeanSM3)) + 
  geom_line(color = "blue") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 3 Power Used in 2007") + 
  ylab("kWh") + 
  xlab("Week") + 
  theme_light() -> plot5.1
ggplotly(plot5.1)

# Average Sub-Meter 3 Power Used in 2008
EnergyData %>%
  filter(year(DateTime) == 2008) %>% 
  group_by(DATE = week(DateTime)) %>% 
  summarise(MeanSM3 = mean(SM3)) %>%
  ggplot(aes(DATE, MeanSM3)) + 
  geom_line(color = "blue") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 3 Power Used in 2008") + 
  ylab("kWh") + 
  xlab("Week") + 
  theme_light() -> plot5.2
ggplotly(plot5.2)

# Average Sub-Meter 3 Power Used in 2009
EnergyData %>%
  filter(year(DateTime) == 2009) %>% 
  group_by(DATE = week(DateTime)) %>% 
  summarise(MeanSM3 = mean(SM3)) %>%
  ggplot(aes(DATE, MeanSM3)) + 
  geom_line(color = "blue") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 3 Power Used in 2009") + 
  ylab("kWh") + 
  xlab("Week") + 
  theme_light() -> plot5.3
ggplotly(plot5.3)

# Average Sub-Meter 3 Power Used in 2010
EnergyData %>%
  filter(year(DateTime) == 2010) %>% 
  group_by(DATE = week(DateTime)) %>% 
  summarise(MeanSM3 = mean(SM3)) %>%
  ggplot(aes(DATE, MeanSM3)) + 
  geom_line(color = "blue") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 3 Power Used in 2010") + 
  ylab("kWh") + 
  xlab("Week") + 
  theme_light() -> plot5.4
ggplotly(plot5.4)

# See all Sub-Meter 3 plots in one scren
grid.arrange(plot5.1, plot5.2, plot5.3, plot5.4)

# Average Energy Power Used by Day
EnergyData %>% 
  group_by(DATE = date(DateTime)) %>% 
  summarise(MeanGAP = mean(GAP_kWh),
            MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(DATE)) +
  geom_smooth(aes(y = MeanGAP), se = T, color = "red") +
  geom_smooth(aes(y = MeanSM0), se = T, color = "orange") +
  geom_smooth(aes(y = MeanSM1), se = T, color = "green") +
  geom_smooth(aes(y = MeanSM2), se = T, color = "purple") +
  geom_smooth(aes(y = MeanSM3), se = T, color = "blue") +
  labs(title = "Average Energy Power Used by Day") + 
  ylab("kWh") + 
  xlab("Time") + 
  theme_light() -> plot6
ggplotly(plot6)

# See all "by Day" plots in one scren
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6)

# Analysis by years:
# Average Energy Power Used in 2007
EnergyData %>% 
  filter(year(DateTime) == 2007) %>%
  group_by(DATE = month(DateTime)) %>% 
  summarise(MeanGAP = mean(GAP_kWh),
            MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(DATE)) +
  geom_smooth(aes(y = MeanGAP), se = T, color = "red") +
  geom_smooth(aes(y = MeanSM0), se = T, color = "orange") +
  geom_smooth(aes(y = MeanSM1), se = T, color = "green") +
  geom_smooth(aes(y = MeanSM2), se = T, color = "purple") +
  geom_smooth(aes(y = MeanSM3), se = T, color = "blue") +
  bbc_style() +
  labs(title = "Average Energy Power Used in 2007") + 
  ylab("kWh") + 
  xlab("Month") +
  theme_light() -> plot7
ggplotly(plot7)

# Average Energy Power Used in 2008
EnergyData %>% 
  filter(year(DateTime) == 2008) %>%
  group_by(DATE = month(DateTime)) %>% 
  summarise(MeanGAP = mean(GAP_kWh),
            MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(DATE)) +
  geom_smooth(aes(y = MeanGAP), se = T, color = "red") +
  geom_smooth(aes(y = MeanSM0), se = T, color = "orange") +
  geom_smooth(aes(y = MeanSM1), se = T, color = "green") +
  geom_smooth(aes(y = MeanSM2), se = T, color = "purple") +
  geom_smooth(aes(y = MeanSM3), se = T, color = "blue") +
  bbc_style() +
  labs(title = "Average Energy Power Used in 2008") + 
  ylab("kWh") + 
  xlab("Month") + 
  theme_light() -> plot8
ggplotly(plot8)

# Average Energy Power Used in 2009
EnergyData %>% 
  filter(year(DateTime) == 2009) %>%
  group_by(DATE = month(DateTime)) %>% 
  summarise(MeanGAP = mean(GAP_kWh),
            MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(DATE)) +
  geom_smooth(aes(y = MeanGAP), se = T, color = "red") +
  geom_smooth(aes(y = MeanSM0), se = T, color = "orange") +
  geom_smooth(aes(y = MeanSM1), se = T, color = "green") +
  geom_smooth(aes(y = MeanSM2), se = T, color = "purple") +
  geom_smooth(aes(y = MeanSM3), se = T, color = "blue") +
  bbc_style() +
  labs(title = "Average Energy Power Used in 2009") + 
  ylab("kWh") + 
  xlab("Month") + 
  theme_light() -> plot9
ggplotly(plot9)

# Average Energy Power Used in 2010
EnergyData %>% 
  filter(year(DateTime) == 2010) %>%
  group_by(DATE = month(DateTime)) %>% 
  summarise(MeanGAP = mean(GAP_kWh),
            MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(DATE)) +
  geom_smooth(aes(y = MeanGAP), se = T, color = "red") +
  geom_smooth(aes(y = MeanSM0), se = T, color = "orange") +
  geom_smooth(aes(y = MeanSM1), se = T, color = "green") +
  geom_smooth(aes(y = MeanSM2), se = T, color = "purple") +
  geom_smooth(aes(y = MeanSM3), se = T, color = "blue") +
  bbc_style() +
  labs(title = "Average Energy Power Used in 2010") + 
  ylab("kWh") + 
  xlab("Month") + 
  theme_light() -> plot10
ggplotly(plot10)

# See all plots in one scren
grid.arrange(plot7, plot8, plot9, plot10)
