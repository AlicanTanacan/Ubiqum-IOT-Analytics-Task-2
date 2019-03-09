### ----------------------- IOT ANALYTICS -------------------------- ###
### ---------- Task 2: Visualize and Analyze Energy Data ----------- ###
### --------------------- by Alican Tanaçan ------------------------ ###
### -- Version 1: Obtain & Preprocess Original Power Consump.Data -- ###

## Libraries ----
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(plotly)
library(gridExtra)
library(devtools)
library(bbplot)

## Import The Original Data ----
# Donwload the data to your R working directory from:
# http://archive.ics.uci.edu/ml/machine-learning-databases/00235/
Data <- read.table("household_power_consumption.txt",
                                header = T,
                                sep = ";",
                                stringsAsFactors = F,
                                na.strings = c("?"," "))

## Preprocess ----
# Omit NA's, rename variables and correct data types
Data2 <- 
  Data %>%
  na.omit(Data) %>% 
  rename(GAP = Global_active_power,
         GRP = Global_reactive_power,
         GI = Global_intensity,
         SM1 = Sub_metering_1,
         SM2 = Sub_metering_2,
         SM3 = Sub_metering_3) %>%
  mutate_at(c("GAP",
              "GRP",
              "Voltage",
              "GI",
              "SM1",
              "SM2",
              "SM3"), as.numeric) %>% 
  mutate_at(c("Date",
              "Time"), as.character)

# Convert "Date" variable into a date format
Data2$Date <- as.Date(Data2$Date, format='%d/%m/%Y')

# Combine "Date" and "Time" attributes in a new attribute column
Data3 <- cbind(Data2, 
               paste(Data2$Date,Data2$Time), 
               stringsAsFactors = F)

# Give the new attribute in the 10th column a header name 
colnames(Data3)[10] <-"DateTime"

# Remove unnecessary attributes
Data3[, c(1,2)] <- NULL

# Move the "DateTime" attribute to the 1st column within the dataset
Data3 <- Data3[,c(ncol(Data3), 1:(ncol(Data3)-1))]

# Convert global active/reactive power to kWh and define 'Sub-Meter 0' variable, which 
# represents the active power consumed every minute (in watt hour) in the 
# household by electrical equipment not measured in sub-meters 1, 2 and 3. We
# also prepare other variables for further investigation by correcting their values
# and orders.
Data3$GAP_kWh <- (Data3$GAP * 1000/60)
Data3$GRP_kWh <- (Data3$GRP * 1000/60)

Data3$SM0 <- (Data3$GAP_kWh -
              Data3$SM1 - 
              Data3$SM2 - 
              Data3$SM3)

Data3$GAP <- NULL
Data3$GRP <- NULL

Data4 <- Data3[,c("DateTime",
                  "GAP_kWh",
                  "SM0",
                  "SM1",
                  "SM2",
                  "SM3",
                  "GRP_kWh",
                  "Voltage",
                  "GI")]

# Convert "DateTime" from POSIXlt to POSIXct 
Data4$DateTime <- as.POSIXct(Data3$DateTime, "%Y/%m/%d %H:%M:%S")

# Add the time zone
attr(Data4$DateTime, "tzone") <- "Europe/Paris"

# Create new time period attributes with lubridate
Data4$Year <- year(Data4$DateTime)
Data4$Month <- month(Data4$DateTime)
Data4$MonthName <- month(Data4$DateTime, label = T)

# Create another variable that shows year and month in one column
Data4$Date <- as.yearmon(paste(Data4$Year, 
                               Data4$Month,
                               sep = "-"))

# Inspect the data types
str(Data4)

# Change Data Types
Data4 <- 
  Data4 %>% 
  mutate_at(c("Year", 
              "Date", 
              "Month",
              "MonthName"), 
            as.factor)

# Mutate Seasons Variable and Define Season for Each Year by Date
Data5 <- 
  Data4 %>% 
  mutate(
    Seasons = 
      case_when(Date %in% c("Dec 2006", "Jan 2007", "Feb 2007") ~ "Winter 2007",
                Date %in% c("Mar 2007", "Apr 2007", "May 2007") ~ "Spring 2007",
                Date %in% c("Jun 2007", "Jul 2007", "Aug 2007") ~ "Summer 2007",
                Date %in% c("Sep 2007", "Oct 2007", "Nov 2007") ~ "Fall 2007",
                Date %in% c("Dec 2007", "Jan 2008", "Feb 2008") ~ "Winter 2008",
                Date %in% c("Mar 2008", "Apr 2008", "May 2008") ~ "Spring 2008",
                Date %in% c("Jun 2008", "Jul 2008", "Aug 2008") ~ "Summer 2008",
                Date %in% c("Sep 2008", "Oct 2008", "Nov 2008") ~ "Fall 2008",
                Date %in% c("Dec 2008", "Jan 2009", "Feb 2009") ~ "Winter 2009",
                Date %in% c("Mar 2009", "Apr 2009", "May 2009") ~ "Spring 2009",
                Date %in% c("Jun 2009", "Jul 2009", "Aug 2009") ~ "Summer 2009",
                Date %in% c("Sep 2009", "Oct 2009", "Nov 2009") ~ "Fall 2009",
                Date %in% c("Dec 2009", "Jan 2010", "Feb 2010") ~ "Winter 2010",
                Date %in% c("Mar 2010", "Apr 2010", "May 2010") ~ "Spring 2010",
                Date %in% c("Jun 2010", "Jul 2010", "Aug 2010") ~ "Summer 2010",
                Date %in% c("Sep 2010", "Oct 2010", "Nov 2010") ~ "Fall 2010" ))

# Mutate Season Variable for Every Month
Data5 <-
  Data5 %>% 
  mutate(
    SeasonName = ifelse(MonthName %in% c("Dec", "Jan", "Feb"), "Winter",
                        ifelse(MonthName %in% c("Mar", "Apr", "May"), "Spring",
                               ifelse(MonthName %in% c("Jun", "Jul", "Aug"), "Summer", 
                                      "Fall"))))

# Change Data Type of SeasonName
Data5$SeasonName <- as.factor(Data5$SeasonName)

# Put Seasons in Correct Order
Data5$Seasons <- factor(Data5$Seasons, levels =c("Winter 2007",
                                                 "Spring 2007",
                                                 "Summer 2007",
                                                 "Fall 2007",
                                                 "Winter 2008",
                                                 "Spring 2008",
                                                 "Summer 2008",
                                                 "Fall 2008",
                                                 "Winter 2009",
                                                 "Spring 2009",
                                                 "Summer 2009",
                                                 "Fall 2009",
                                                 "Winter 2010",
                                                 "Spring 2010",
                                                 "Summer 2010",
                                                 "Fall 2010"), ordered = T)

# Check the ordered seasons
levels(Data5$Seasons)
head(Data5$Seasons)

# Define ready data by disselecting irrelevant variables
ReadyData <- subset(Data5, select = -c(Year:MonthName))
str(ReadyData)

# Save the clean data as rds
saveRDS(ReadyData, file = "OriginalEnergyCleanData.rds")