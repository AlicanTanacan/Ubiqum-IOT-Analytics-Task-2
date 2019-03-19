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

# How much energy in total and in sub-meters consumed every year?
# How much did the household paid for their energy consumption yearly/monthly?
# Average cost of electricity in France is 14.72 euro cents per kWh. With taxes
# the cost may go up to 0.16 euro per kWh.
EnergyData %>% 
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

# Yearly power usage in pie charts
EnergyYearlyCostData %>% 
  filter(YEAR == 2007) %>% 
  select(sumSM0kwh, sumSM1kwh, sumSM2kwh, sumSM3kwh) %>% 
  plot_ly(labels = ~c("Sub-Meter 0 (Rest)", 
                      "Sub-Meter 1 (Kitchen)", 
                      "Sub-Meter 2 (Laundy)",
                      "Sub-Meter 3 (Heater)"), 
          values = ~c(sumSM0kwh, sumSM1kwh, sumSM2kwh, sumSM3kwh), 
          type = "pie",   
          textposition = "inside",
          textinfo = "label+value+percent",
          insidetextfont = list(color = "#FFFFFF"),
          showlegend = F) %>% 
  layout(title = "Total Energy Consumed in 2007",
         xaxis = list(showgrid = FALSE, 
                      zeroline = FALSE, 
                      showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, 
                      zeroline = FALSE, 
                      showticklabels = FALSE)) -> pie2007power

EnergyYearlyCostData %>% 
  filter(YEAR == 2008) %>% 
  select(sumSM0kwh, sumSM1kwh, sumSM2kwh, sumSM3kwh) %>% 
  plot_ly(labels = ~c("Sub-Meter 0 (Rest)", 
                      "Sub-Meter 1 (Kitchen)", 
                      "Sub-Meter 2 (Laundy)",
                      "Sub-Meter 3 (Heater)"), 
          values = ~c(sumSM0kwh, sumSM1kwh, sumSM2kwh, sumSM3kwh), 
          type = "pie",   
          textposition = "inside",
          textinfo = "label+value+percent",
          insidetextfont = list(color = "#FFFFFF"),
          showlegend = F) %>% 
  layout(title = "Total Energy Consumed in 2008",
         xaxis = list(showgrid = FALSE, 
                      zeroline = FALSE, 
                      showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, 
                      zeroline = FALSE, 
                      showticklabels = FALSE)) -> pie2008power

EnergyYearlyCostData %>% 
  filter(YEAR == 2009) %>% 
  select(sumSM0kwh, sumSM1kwh, sumSM2kwh, sumSM3kwh) %>% 
  plot_ly(labels = ~c("Sub-Meter 0 (Rest)", 
                      "Sub-Meter 1 (Kitchen)", 
                      "Sub-Meter 2 (Laundy)",
                      "Sub-Meter 3 (Heater)"), 
          values = ~c(sumSM0kwh, sumSM1kwh, sumSM2kwh, sumSM3kwh), 
          type = "pie",   
          textposition = "inside",
          textinfo = "label+value+percent",
          insidetextfont = list(color = "#FFFFFF"),
          showlegend = F) %>% 
  layout(title = "Total Energy Consumed in 2009",
         xaxis = list(showgrid = FALSE, 
                      zeroline = FALSE, 
                      showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, 
                      zeroline = FALSE, 
                      showticklabels = FALSE)) -> pie2009power

EnergyYearlyCostData %>% 
  filter(YEAR == 2010) %>% 
  select(sumSM0kwh, sumSM1kwh, sumSM2kwh, sumSM3kwh) %>% 
  plot_ly(labels = ~c("Sub-Meter 0 (Rest)", 
                      "Sub-Meter 1 (Kitchen)", 
                      "Sub-Meter 2 (Laundy)",
                      "Sub-Meter 3 (Heater)"), 
          values = ~c(sumSM0kwh, sumSM1kwh, sumSM2kwh, sumSM3kwh), 
          type = "pie",   
          textposition = "inside",
          textinfo = "label+value+percent",
          insidetextfont = list(color = "#FFFFFF"),
          showlegend = F) %>% 
  layout(title = "Total Energy Consumed in 2010",
         xaxis = list(showgrid = FALSE, 
                      zeroline = FALSE, 
                      showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, 
                      zeroline = FALSE, 
                      showticklabels = FALSE)) -> pie2010power

pie2007power
pie2008power
pie2009power
pie2010power

# Monthly power usage and energy cost data sets
EnergyData %>% 
  filter(year(DateTime) == 2007) %>% 
  group_by(month(DateTime)) %>% 
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
  round() -> Energy2007MonthlyCostData

EnergyData %>% 
  filter(year(DateTime) == 2008) %>% 
  group_by(month(DateTime)) %>% 
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
  round() -> Energy2008MonthlyCostData
Energy2008MonthlyCostData

EnergyData %>% 
  filter(year(DateTime) == 2009) %>% 
  group_by(month(DateTime)) %>% 
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
  round() -> Energy2009MonthlyCostData
Energy2009MonthlyCostData

EnergyData %>% 
  filter(year(DateTime) == 2010) %>% 
  group_by(month(DateTime)) %>% 
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
  round() -> Energy2010MonthlyCostData
Energy2010MonthlyCostData

# Extra Costs & Waste of Energy Calculations on Some Family Holidays
# 23 Feb 2007 - 04 Mar 2007 Holiday Analysis
EnergyData %>% 
  filter(date(DateTime) > "2007-02-22" & date(DateTime) < "2007-03-04") %>% 
  group_by(day(DateTime)) %>% 
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
  round() -> Cost2007Holiday
Cost2007Holiday
sum(Cost2007Holiday$TotalCostEuro) # 14 Euro!

# 23 Feb 2008 - 04 Mar 2007 Holiday Analysis
EnergyData %>% 
  filter(date(DateTime) > "2008-02-23" & date(DateTime) < "2008-03-04") %>% 
  group_by(day(DateTime)) %>% 
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
  round() -> Cost2008Holiday
Cost2008Holiday
sum(Cost2008Holiday$TotalCostEuro) # 22 Euro!

# 06 Aug 2008 - 31 Aug 2008 Holiday Analysis
EnergyData %>% 
  filter(date(DateTime) > "2008-08-06" & date(DateTime) < "2008-08-31") %>% 
  group_by(day(DateTime)) %>% 
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
  round() -> Cost2008Holiday2
Cost2008Holiday2
sum(Cost2008Holiday2$TotalCostEuro) # 24 Euro!

# 24 Jul 2009 - 09 Aug 2009 Holiday Analysis
EnergyData %>% 
  filter(date(DateTime) > "2009-07-24" & date(DateTime) < "2009-08-09") %>% 
  group_by(day(DateTime)) %>% 
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
  round() -> Cost2009Holiday
Cost2009Holiday
sum(Cost2009Holiday$TotalCostEuro) # 23 Euro!

# 27 Jul 2010 - 14 Aug 2010 Holiday Analysis
EnergyData %>% 
  filter(date(DateTime) > "2010-07-27" & date(DateTime) < "2010-08-14") %>% 
  group_by(day(DateTime)) %>% 
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
  round() -> Cost2010Holiday
Cost2010Holiday
sum(Cost2010Holiday$TotalCostEuro) # 20 Euro!

## Data Visualization by Dplyr and GGPlot2 ----

# Smooth Line Graphs:

# Average Global Active Power Used by Day
EnergyData %>% 
  group_by(DATE = date(DateTime)) %>% 
  summarise(MeanGAP = mean(GAP)) %>%
  ggplot(aes(DATE, MeanGAP)) + 
  geom_line(color = "red") + 
  geom_smooth(se = F) +
  labs(title = "Average Global Active Power Used by Day") + 
  ylab("Watt-Hours") + 
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
  ylab("Watt-Hours") + 
  xlab("Time") + 
  theme_light() -> plot2
ggplotly(plot2)

# Average Sub-Meter 0 Power Used in 2007
EnergyData %>%
  filter(year(DateTime) == 2007) %>% 
  group_by(DATE = week(DateTime)) %>% 
  summarise(MeanSM0 = mean(SM0)) %>%
  ggplot(aes(DATE, MeanSM0)) + 
  geom_line(color = "orange") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 0 Power Used in 2007") + 
  ylab("Watt-Hours") + 
  xlab("Week") + 
  theme_light() -> plot2.1
ggplotly(plot2.1)

# Average Sub-Meter 0 Power Used in 2008
EnergyData %>%
  filter(year(DateTime) == 2008) %>% 
  group_by(DATE = week(DateTime)) %>% 
  summarise(MeanSM0 = mean(SM0)) %>%
  ggplot(aes(DATE, MeanSM0)) + 
  geom_line(color = "orange") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 0 Power Used in 2008") + 
  ylab("Watt-Hours") + 
  xlab("Week") + 
  theme_light() -> plot2.2
ggplotly(plot2.2)

# Average Sub-Meter 0 Power Used in 2009
EnergyData %>%
  filter(year(DateTime) == 2009) %>% 
  group_by(DATE = week(DateTime)) %>% 
  summarise(MeanSM0 = mean(SM0)) %>%
  ggplot(aes(DATE, MeanSM0)) + 
  geom_line(color = "orange") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 0 Power Used in 2009") + 
  ylab("Watt-Hours") + 
  xlab("Week") + 
  theme_light() -> plot2.3
ggplotly(plot2.3)

# Average Sub-Meter 0 Power Used in 2010
EnergyData %>%
  filter(year(DateTime) == 2010) %>% 
  group_by(DATE = week(DateTime)) %>% 
  summarise(MeanSM0 = mean(SM0)) %>%
  ggplot(aes(DATE, MeanSM0)) + 
  geom_line(color = "orange") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 0 Power Used in 2010") + 
  ylab("Watt-Hours") + 
  xlab("Week") + 
  theme_light() -> plot2.4
ggplotly(plot2.4)

# See all Sub-Meter 0 plots in one scren
grid.arrange(plot2.1, plot2.2, plot2.3, plot2.4)

# Average Sub-Meter 1 Power Used by Day
EnergyData %>% 
  group_by(DATE = date(DateTime)) %>% 
  summarise(MeanSM1 = mean(SM1)) %>%
  ggplot(aes(DATE, MeanSM1)) + 
  geom_line(color = "green") + 
  geom_smooth(se = F) +
  labs(title = "Average Sub-Meter 1 Power Used by Day") + 
  ylab("Watt-Hours") + 
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
  ylab("Watt-Hours") + 
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
  ylab("Watt-Hours") + 
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
  ylab("Watt-Hours") + 
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
  ylab("Watt-Hours") + 
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
  ylab("Watt-Hours") + 
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
  ylab("Watt-Hours") + 
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
  ylab("Watt-Hours") + 
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
  ylab("Watt-Hours") + 
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
  ylab("Watt-Hours") + 
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
  ylab("Watt-Hours") + 
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
  ylab("Watt-Hours") + 
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
  ylab("Watt-Hours") + 
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
  ylab("Watt-Hours") + 
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
  ylab("Watt-Hours") + 
  xlab("Week") + 
  theme_light() -> plot5.4
ggplotly(plot5.4)

# See all Sub-Meter 3 plots in one scren
grid.arrange(plot5.1, plot5.2, plot5.3, plot5.4)

# Average Energy Power Used by Day
EnergyData %>% 
  group_by(DATE = date(DateTime)) %>% 
  summarise(MeanGAP = mean(GAP),
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
  ylab("Watt-Hours") + 
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
  summarise(MeanGAP = mean(GAP),
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
  ylab("Watt-Hours") + 
  xlab("Month") +
  theme_light() -> plot7
ggplotly(plot7)

# Average Energy Power Used in 2008
EnergyData %>% 
  filter(year(DateTime) == 2008) %>%
  group_by(DATE = month(DateTime)) %>% 
  summarise(MeanGAP = mean(GAP),
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
  ylab("Watt-Hours") + 
  xlab("Month") + 
  theme_light() -> plot8
ggplotly(plot8)

# Average Energy Power Used in 2009
EnergyData %>% 
  filter(year(DateTime) == 2009) %>%
  group_by(DATE = month(DateTime)) %>% 
  summarise(MeanGAP = mean(GAP),
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
  ylab("Watt-Hours") + 
  xlab("Month") + 
  theme_light() -> plot9
ggplotly(plot9)

# Average Energy Power Used in 2010
EnergyData %>% 
  filter(year(DateTime) == 2010) %>%
  group_by(DATE = month(DateTime)) %>% 
  summarise(MeanGAP = mean(GAP),
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
  ylab("Watt-Hours") + 
  xlab("Month") + 
  theme_light() -> plot10
ggplotly(plot10)

# See all plots in one scren
grid.arrange(plot7, plot8, plot9, plot10)
