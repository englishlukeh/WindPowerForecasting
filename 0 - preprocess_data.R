#### Import everything and prepare data ####

library(dplyr)
library(tsibble)
library("readxl")
library(fpp3)
library(zoo)
library(stats)
library(foreach)
library(doParallel)
library(lightgbm)
library(distributional)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set this to your working directory

wind_speed <- read_excel("WindSpeed_2020-2021.xlsx")
wind_speed <- wind_speed %>%
  mutate(PCTimeStamp = as_datetime(PCTimeStamp)) %>%
  as_tsibble(index = PCTimeStamp)%>%
  filter_index("2019" ~ .) #ignore incorrectly entered dates

power_generation <- readr::read_csv("power_generation.csv")
power_generation <- power_generation %>%
  mutate(PCTimeStamp = as_datetime(PCTimeStamp)) %>%
  as_tsibble(index = PCTimeStamp) %>%
  filter_index("2019" ~ .) %>% #ignore incorrectly entered dates
  select(-1, -28, -29) #ignore first column of CSV

#get turbine names
node_names <- names(power_generation)
node_names <- node_names[-1]

#convert long table to wide table (i.e., create keys for group/subgroup)
tmp_wind <- pivot_longer(wind_speed, cols=-1, names_to = "Subgroup", values_to = "Wind_Speed")
tmp_wind["Group"] <- substr(tmp_wind$Subgroup, 1, 1)

tmp_power <- pivot_longer(power_generation, cols=-1, names_to = "Subgroup", values_to = "Power")
tmp_power["Group"] <- substr(tmp_power$Subgroup, 1, 1)

#merge all data into one tsibble with keys group (A, B, etc.) and subgroup (A1, A2, etc.)
dataset <- merge(tmp_wind, tmp_power,by=c("PCTimeStamp", "Group", "Subgroup"))
dataset <- dataset %>% as_tsibble(index = PCTimeStamp, key = c("Group", "Subgroup"))


#we firstly remove error values (power or wind speed <= 0 and NA)
dataset[is.na(dataset)] <- 0
dataset <- dataset %>%
  filter(Wind_Speed > 0) %>%
  filter(Power > 0)

#now we fill the gaps and NA values using naive interpolation
dataset <- fill_gaps(dataset, .full = TRUE)
dataset <- dataset %>%
  fill(-1, .direction = "downup")

#convert wind power to energy (kW -> kWh). We perform this to ensure additivity up the hierarchy in cross-sectional and temporal dimensions
# Wind speed as a predictor is kept in units m/s and the mean is taken in hierarchies
# (10 min * 1 hr/60min = 1/6 hr)
dataset <- dataset %>% mutate(Energy = Power * 1/6)
dataset <- select(dataset, -c(Power))


#form hierarchy
min10_power <- dataset %>%
  aggregate_key(Group/Subgroup, Energy = sum(Energy), Wind_Speed = mean(Wind_Speed)) %>% group_by_key()

#20 minutely
min20_power <- min10_power %>%
  index_by(Time = ~ lubridate::floor_date(., "20 minutes")) %>%
  summarise(
    Energy = sum(Energy), Wind_Speed = mean(Wind_Speed)
  ) %>%
  group_by_key()

#30 minutely
min30_power <- min10_power %>%
  index_by(Time = ~ lubridate::floor_date(., "30 minutes")) %>%
  summarise(
    Energy = sum(Energy), Wind_Speed = mean(Wind_Speed)
  ) %>%
  group_by_key()

#1 hourly
hr1_power <- min10_power %>%
  index_by(Time = ~ lubridate::floor_date(., "1 hour")) %>%
  summarise(
    Energy = sum(Energy), Wind_Speed = mean(Wind_Speed)
  ) %>%
  group_by_key()

# change index name to Time for uniformity with 20-minutely, 30-minutely, 1-hourly data
min10_power <- min10_power %>%
  rename(Time = PCTimeStamp)

saveRDS(min10_power, file = "min10.rds")
saveRDS(min20_power, file = "min20.rds")
saveRDS(min30_power, file = "min30.rds")
saveRDS(hr1_power, file = "hr1.rds")