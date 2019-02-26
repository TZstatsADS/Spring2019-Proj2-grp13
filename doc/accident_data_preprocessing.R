##  Project2 - yz3383
##  Accident data from AIDS system (https://www.asias.faa.gov)


# Check required packages
if (!require("tidyverse")) install.packages('tidyverse')
if(!require("lubridate")) install.packages('lubridate')
library(tidyverse)
library(lubridate)

# Data Pre-processing
data.78.88 <- read.csv("../data/AIDS_78-88.csv",na.strings=c("NA","NaN", ""))
data.89.07 <- read.csv("../data/AIDS_89-07.csv",na.strings=c("NA","NaN", ""))
data.08.18 <- read.csv("../data/AIDS_08-18.csv",na.strings=c("NA","NaN", ""))
data.18.19 <- read.csv("../data/AIDS_18-19.csv",na.strings=c("NA","NaN", ""))
dataset <- rbind(data.78.88,data.89.07,data.08.18,data.18.19)
rm(data.78.88,data.89.07,data.08.18,data.18.19)

dataset <- dataset %>%
  select('AIDS.Report.Number','Local.Event.Date','Event.City',
         'Event.State','Event.Airport','Operator','Aircraft.Make',
         'Aircraft.Damage','Primary.Flight.Type') %>%
  mutate(Event.State = as.character(Event.State)) %>%
  filter(Event.State %in% state.abb) %>%
  mutate(Local.Event.Date = as.Date(Local.Event.Date,format = "%d-%b-%y"))


# Count accident within each month for each year
month.data <- dataset %>%
  mutate(year = year(Local.Event.Date),month = month(Local.Event.Date)) %>%
  group_by(year,month) %>%
  summarise(count = n())


# Different airports in state
airport.data <- dataset %>%
  select(Local.Event.Date,Event.State,Event.Airport,Operator,Aircraft.Make) %>%
  mutate(year = year(Local.Event.Date)) %>%
  group_by(year,Event.State) %>%
  summarise(count = n()) %>% 
  arrange(year,Event.State)


# Major Aircraft Maker 
major.company = c("LOCKHEED","BOEING","AIRBUS")

aircraft.make <- dataset %>%
  mutate(year = year(Local.Event.Date)) %>%
  select(Aircraft.Make,AIDS.Report.Number,year) %>%
  filter(Aircraft.Make %in% major.company) %>%
  group_by(Aircraft.Make,year) %>%
  summarise(count = n())  %>%
  arrange(desc(count))

# Major Operator
# UNITED AIRLINES has two type of names, so I take both of them first then merge them into one
major.operator = c("DELTA AIR LINES INC","SPIRIT AIRLINES INC","JETBLUE AIRWAYS CORPORATION",
                   "UNITED AIRLINES, INC.","UNITED AIR LINES INC","FRONTIER AIRLINES INC","ALASKA AIRLINES INC",
                   "SOUTHWEST AIRLINES CO","HAWAIIAN AIRLINES INC","AMERICAN AIRLINES INC",
                   "SKYWEST AIRLINES INC","EXPRESSJET AIRLINES INC","ENVOY AIR INC")

operator <- dataset %>%
  mutate(year = year(Local.Event.Date)) %>%
  select(Operator,AIDS.Report.Number,year) %>%
  filter(Operator != "",Operator %in% major.operator) %>%
  group_by(Operator,year) %>%
  summarise(count = n())

operator$Operator <- gsub('UNITED AIR LINES INC',replacement = "UNITED AIRLINES, INC.",operator$Operator)

# Accident Type and Damage
main.reasons <- c('ILLEGAL','PERSONAL','AIR TAXI','SCHEDULED AIR CARRIER','BUSINESS')

accident.reason <- dataset %>%
  select(AIDS.Report.Number,Aircraft.Damage,Primary.Flight.Type)

accident.reason$Primary.Flight.Type <- gsub("^AIR.*","AIR TAXI",accident.reason$Primary.Flight.Type)
accident.reason$Primary.Flight.Type <- gsub("^ILLE.*","ILLEGAL",accident.reason$Primary.Flight.Type)
accident.reason$Primary.Flight.Type <- gsub(".*OPERATOR","SUPPLIMENTAL/COMMERCIAL",accident.reason$Primary.Flight.Type)
accident.reason$Primary.Flight.Type <- gsub(".*HELICOPTER","HELICOPTER",accident.reason$Primary.Flight.Type)

accident.reason <- accident.reason %>%
  filter(Primary.Flight.Type %in% main.reasons & Aircraft.Damage != 'UNKNOWN') %>%
  na.omit()

# Save all cleaned data

write.csv(month.data, file = "../output/accident_month.csv", row.names = TRUE)
write.csv(airport.data, file = "../output/accident_state.csv", row.names = TRUE)
write.csv(aircraft.make, file = "../output/accident_aircraft.csv", row.names = TRUE)
write.csv(operator, file = "../output/accident_operator.csv", row.names = TRUE)
write.csv(accident.reason, file = "../output/accident_reason.csv", row.names = TRUE)