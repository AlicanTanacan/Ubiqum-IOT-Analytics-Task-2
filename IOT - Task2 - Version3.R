### ----------------------- IOT ANALYTICS -------------------------- ###
### ---------- Task 2: Visualize and Analyze Energy Data ----------- ###
### --------------------- by Alican Tanaçan ------------------------ ###
### ------ Version 3: Data Analysis and Visualization Part 2 ------- ###

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

## Data Visualization by Dplyr and GGPlot2 ----
# Average Sub-Meter 0,1,2 and 3 by Weeks in Each Year
EnergyData %>% 
  filter(year(DateTime) != 2006) %>% 
  group_by(DATE = week(DateTime), YEAR = year(DateTime)) %>%
  summarise(MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(DATE)) +
  geom_line(aes(y = MeanSM0), color = "red") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_label(aes(x = 53, y = 14, label = "rest"), color = "red", size = 4) +
  geom_label(aes(x = 53, y = 0, label = "kitchen"), color = "darkgreen", size = 4) +
  geom_label(aes(x = 53, y = 3, label = "laundry"), color = "purple", size = 4) +
  geom_label(aes(x = 53, y = 9, label = "heater"), color = "blue", size = 4) +
  labs(title = "Average Sub-Meter 0,1,2,3 by Weeks in Each Year") + 
  ylab("Watt-Hours") + 
  xlab("Week") + 
  theme_light() +
  facet_wrap(~YEAR) -> plotAllYearsbyWeek
plotAllYearsbyWeek

# Average Sub-Meter 0,1,2 and 3 by Days in Each Month in 2007
EnergyData %>% 
  filter(year(DateTime) == 2007) %>%
  group_by(DATE = day(DateTime), MONTH = month(DateTime)) %>%
  summarise(MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(DATE)) +
  geom_line(aes(y = MeanSM0), color = "red") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_label(aes(x = 31, y = 30, label = "rest"), color = "red", size = 3) +
  geom_label(aes(x = 31, y = 21, label = "kitchen"), color = "darkgreen", size =3) +
  geom_label(aes(x = 31, y = 24, label = "laundry"), color = "purple", size =3) +
  geom_label(aes(x = 31, y = 27, label = "heater"), color = "blue", size =3) +
  labs(title = "Average Sub-Meter 0,1,2,3 by Days in Each Month in 2007") + 
  ylab("Watt-Hours") + 
  xlab("Day") + 
  theme_light() +
  facet_wrap(~MONTH) -> plot2007
plot2007

# Average Sub-Meter 0,1,2 and 3 by Days in Each Month in 2008
EnergyData %>% 
  filter(year(DateTime) == 2008) %>%
  group_by(DATE = day(DateTime), MONTH = month(DateTime)) %>%
  summarise(MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(DATE)) +
  geom_line(aes(y = MeanSM0), color = "red") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_label(aes(x = 31, y = 30, label = "rest"), color = "red", size = 3) +
  geom_label(aes(x = 31, y = 21, label = "kitchen"), color = "darkgreen", size =3) +
  geom_label(aes(x = 31, y = 24, label = "laundry"), color = "purple", size =3) +
  geom_label(aes(x = 31, y = 27, label = "heater"), color = "blue", size =3) +
  labs(title = "Average Sub-Meter 0,1,2,3 by Days in Each Month in 2008") + 
  ylab("Watt-Hours") + 
  xlab("Day") + 
  theme_light() +
  facet_wrap(~MONTH) -> plot2008
plot2008

# Average Sub-Meter 0,1,2 and 3 by Days in Each Month in 2009
EnergyData %>% 
  filter(year(DateTime) == 2009) %>%
  group_by(DATE = day(DateTime), MONTH = month(DateTime)) %>%
  summarise(MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(DATE)) +
  geom_line(aes(y = MeanSM0), color = "red") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_label(aes(x = 31, y = 30, label = "rest"), color = "red", size = 3) +
  geom_label(aes(x = 31, y = 21, label = "kitchen"), color = "darkgreen", size =3) +
  geom_label(aes(x = 31, y = 24, label = "laundry"), color = "purple", size =3) +
  geom_label(aes(x = 31, y = 27, label = "heater"), color = "blue", size =3) +
  labs(title = "Average Sub-Meter 0,1,2,3 by Days in Each Month in 2009") + 
  ylab("Watt-Hours") + 
  xlab("Day") + 
  theme_light() +
  facet_wrap(~MONTH) -> plot2009
plot2009

# Average Sub-Meter 0,1,2 and 3 by Days in Each Month in 2010
EnergyData %>% 
  filter(year(DateTime) == 2010) %>%
  group_by(DATE = day(DateTime), MONTH = month(DateTime)) %>%
  summarise(MeanSM0 = mean(SM0),
            MeanSM1 = mean(SM1),
            MeanSM2 = mean(SM2),
            MeanSM3 = mean(SM3)) %>% 
  ggplot(aes(DATE)) +
  geom_line(aes(y = MeanSM0), color = "red") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_label(aes(x = 31, y = 30, label = "rest"), color = "red", size = 3) +
  geom_label(aes(x = 31, y = 21, label = "kitchen"), color = "darkgreen", size =3) +
  geom_label(aes(x = 31, y = 24, label = "laundry"), color = "purple", size =3) +
  geom_label(aes(x = 31, y = 27, label = "heater"), color = "blue", size =3) +
  labs(title = "Average Sub-Meter 0,1,2,3 by Days in Each Month in 2010") + 
  ylab("Watt-Hours") + 
  xlab("Day") + 
  theme_light() +
  facet_wrap(~MONTH) -> plot2010
plot2010

# Average Energy Used in Every Week Day (to see the family's daily pattern)
EnergyData %>%
  mutate(weekday = wday(DateTime, label = T, abbr = F, 
                        locale = "us", week_start = 1)) %>% 
  filter(year(DateTime) != 2006) %>% 
  group_by(weekday, HOUR = hour(DateTime)) %>% 
  summarise(MeanSM0 = round(mean(SM0), 2),
            MeanSM1 = round(mean(SM1), 2),
            MeanSM2 = round(mean(SM2), 2),
            MeanSM3 = round(mean(SM3), 2))  %>% 
  ggplot(aes(HOUR)) +
  geom_line(aes(y = MeanSM0), color = "darkorange") +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_label(aes(x = 24, y = 20, label = "rest"), color = "darkorange", size = 4) +
  geom_label(aes(x = 24, y = 15, label = "kitchen"), color = "darkgreen", size = 4) +
  geom_label(aes(x = 24, y = 10, label = "laundry"), color = "purple", size = 4) +
  geom_label(aes(x = 24, y = 5, label = "heater"), color = "blue", size = 4) +
  labs(title = "Average Sub-Meter 0,1,2,3 by Hours on Each Week Day") + 
  ylab("Watt-Hours") + 
  xlab("Hours") + 
  theme_light() +
  facet_wrap(~weekday) -> plotallSMweekdays
plotallSMweekdays

# Average Global Active Power by Hours on Each Week Day
EnergyData %>%
  mutate(weekday = wday(DateTime, label = T, abbr = F, 
                        locale = "us", week_start = 1)) %>% 
  filter(year(DateTime) != 2006) %>% 
  group_by(weekday, HOUR = hour(DateTime)) %>% 
  summarise(MeanGAP = round(mean(GAP), 2)) %>% 
  ggplot(aes(HOUR)) +
  geom_line(aes(y = MeanGAP), color = "red") +
  geom_label(aes(x = 15, y = 35, label = "Global Active Power"), color = "red", size = 4) +
  labs(title = "Average Global Active Power by Hours on Each Week Day") + 
  ylab("Watt-Hours") + 
  xlab("Hours") + 
  theme_light() +
  facet_wrap(~weekday) -> plotGAPweekdays
plotGAPweekdays

# Average Sub-Meter 0 (Energy used in the rest of the house) by Hours on Each Week Day
EnergyData %>%
  mutate(weekday = wday(DateTime, label = T, abbr = F, 
                        locale = "us", week_start = 1)) %>% 
  filter(year(DateTime) != 2006) %>% 
  group_by(weekday, HOUR = hour(DateTime)) %>% 
  summarise(MeanSM0 = round(mean(SM0), 2)) %>% 
  ggplot(aes(HOUR)) +
  geom_line(aes(y = MeanSM0), color = "darkorange") +
  geom_label(aes(x = 15, y = 20, label = "Rest Of House"), color = "darkorange", size = 4) +
  labs(title = "Average Sub-Meter 0 by Hours on Each Week Day") + 
  ylab("Watt-Hours") + 
  xlab("Hours") + 
  theme_light() +
  facet_wrap(~weekday) -> plotSM0weekdays
plotSM0weekdays

# Average Sub-Meter 1 by Hours on Each Week Day
EnergyData %>%
  mutate(weekday = wday(DateTime, label = T, abbr = F, 
                        locale = "us", week_start = 1)) %>% 
  filter(year(DateTime) != 2006) %>% 
  group_by(weekday, HOUR = hour(DateTime)) %>% 
  summarise(MeanSM1 = round(mean(SM1), 2)) %>% 
  ggplot(aes(HOUR)) +
  geom_line(aes(y = MeanSM1), color = "darkgreen") +
  geom_label(aes(x = 15, y = 5, label = "Kitchen"), color = "darkgreen", size = 4) +
  labs(title = "Average Sub-Meter 1 by Hours on Each Week Day") + 
  ylab("Watt-Hours") + 
  xlab("Hours") + 
  theme_light() +
  facet_wrap(~weekday) -> plotSM1weekdays
plotSM1weekdays

# Average Sub-Meter 2 by Hours on Each Week Day
EnergyData %>%
  mutate(weekday = wday(DateTime, label = T, abbr = F, 
                        locale = "us", week_start = 1)) %>% 
  filter(year(DateTime) != 2006) %>% 
  group_by(weekday, HOUR = hour(DateTime)) %>% 
  summarise(MeanSM2 = round(mean(SM2), 2)) %>% 
  ggplot(aes(HOUR)) +
  geom_line(aes(y = MeanSM2), color = "purple") +
  geom_label(aes(x = 15, y = 6, label = "Laundry"), color = "purple", size = 4) +
  labs(title = "Average Sub-Meter 2 by Hours on Each Week Day") + 
  ylab("Watt-Hours") + 
  xlab("Hours") + 
  theme_light() +
  facet_wrap(~weekday) -> plotSM2weekdays
plotSM2weekdays

# Average Sub-Meter 3 by Hours on Each Week Day
EnergyData %>%
  mutate(weekday = wday(DateTime, label = T, abbr = F, 
                        locale = "us", week_start = 1)) %>% 
  filter(year(DateTime) != 2006) %>% 
  group_by(weekday, HOUR = hour(DateTime)) %>% 
  summarise(MeanSM3 = round(mean(SM3), 2)) %>% 
  ggplot(aes(HOUR)) +
  geom_line(aes(y = MeanSM3), color = "blue") +
  geom_label(aes(x = 15, y = 20, label = "Heater"), color = "blue", size = 4) +
  labs(title = "Average Sub-Meter 3 by Hours on Each Week Day") + 
  ylab("Watt-Hours") + 
  xlab("Hours") + 
  theme_light() +
  facet_wrap(~weekday) -> plotSM3weekdays
plotSM3weekdays
