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

turbineA1 <- readr::read_csv("Turbine_Data_Penmanshiel_01_2020-01-01_-_2021-01-01_1042.csv") %>% select(1,2,62) %>% mutate(Group = "A", Subgroup = "A1") %>% slice_head(n=52680)
turbineA2 <- readr::read_csv("Turbine_Data_Penmanshiel_02_2020-01-01_-_2021-01-01_1043.csv") %>% select(1,2,62) %>% mutate(Group = "A", Subgroup = "A2") %>% slice_head(n=52680)
turbineA4 <- readr::read_csv("Turbine_Data_Penmanshiel_04_2020-01-01_-_2021-01-01_1044.csv") %>% select(1,2,62) %>% mutate(Group = "A", Subgroup = "A4") %>% slice_head(n=52680)
turbineA5 <- readr::read_csv("Turbine_Data_Penmanshiel_05_2020-01-01_-_2021-01-01_1045.csv") %>% select(1,2,62) %>% mutate(Group = "A", Subgroup = "A5") %>% slice_head(n=52680)
turbineA6 <- readr::read_csv("Turbine_Data_Penmanshiel_06_2020-01-01_-_2021-01-01_1046.csv") %>% select(1,2,62) %>% mutate(Group = "A", Subgroup = "A6") %>% slice_head(n=52680)
turbineA7 <- readr::read_csv("Turbine_Data_Penmanshiel_07_2020-01-01_-_2021-01-01_1047.csv") %>% select(1,2,62) %>% mutate(Group = "A", Subgroup = "A7") %>% slice_head(n=52680)
turbineA8 <- readr::read_csv("Turbine_Data_Penmanshiel_08_2020-01-01_-_2021-01-01_1048.csv") %>% select(1,2,62) %>% mutate(Group = "A", Subgroup = "A8") %>% slice_head(n=52680)
turbineA9 <- readr::read_csv("Turbine_Data_Penmanshiel_09_2020-01-01_-_2021-01-01_1049.csv") %>% select(1,2,62) %>% mutate(Group = "A", Subgroup = "A9") %>% slice_head(n=52680)
turbineA10 <- readr::read_csv("Turbine_Data_Penmanshiel_10_2020-01-01_-_2021-01-01_1050.csv") %>% select(1,2,62) %>% mutate(Group = "A", Subgroup = "A10") %>% slice_head(n=52680)
turbineA11 <- readr::read_csv("Turbine_Data_Penmanshiel_11_2020-01-01_-_2021-01-01_1051.csv") %>% select(1,2,62) %>% mutate(Group = "A", Subgroup = "A11") %>% slice_head(n=52680)
turbineA12 <- readr::read_csv("Turbine_Data_Penmanshiel_12_2020-01-01_-_2021-01-01_1052.csv") %>% select(1,2,62) %>% mutate(Group = "A", Subgroup = "A12") %>% slice_head(n=52680)
turbineA13 <- readr::read_csv("Turbine_Data_Penmanshiel_13_2020-01-01_-_2021-01-01_1053.csv") %>% select(1,2,62) %>% mutate(Group = "A", Subgroup = "A13") %>% slice_head(n=52680)
turbineA14 <- readr::read_csv("Turbine_Data_Penmanshiel_14_2020-01-01_-_2021-01-01_1054.csv") %>% select(1,2,62) %>% mutate(Group = "A", Subgroup = "A14") %>% slice_head(n=52680)
turbineA15 <- readr::read_csv("Turbine_Data_Penmanshiel_15_2020-01-01_-_2021-01-01_1056.csv") %>% select(1,2,62) %>% mutate(Group = "A", Subgroup = "A15") %>% slice_head(n=52680)
turbineB1 <- readr::read_csv("Turbine_Data_Kelmarsh_1_2020-01-01_-_2021-01-01_228.csv") %>% select(1,2,62) %>% mutate(Group = "B", Subgroup = "B1") %>% slice_head(n=52680)
turbineB2 <- readr::read_csv("Turbine_Data_Kelmarsh_2_2020-01-01_-_2021-01-01_229.csv") %>% select(1,2,62) %>% mutate(Group = "B", Subgroup = "B2") %>% slice_head(n=52680)
turbineB3 <- readr::read_csv("Turbine_Data_Kelmarsh_3_2020-01-01_-_2021-01-01_230.csv") %>% select(1,2,62) %>% mutate(Group = "B", Subgroup = "B3") %>% slice_head(n=52680)
turbineB4 <- readr::read_csv("Turbine_Data_Kelmarsh_4_2020-01-01_-_2021-01-01_231.csv") %>% select(1,2,62) %>% mutate(Group = "B", Subgroup = "B4") %>% slice_head(n=52680) 
turbineB5 <- readr::read_csv("Turbine_Data_Kelmarsh_5_2020-01-01_-_2021-01-01_232.csv") %>% select(1,2,62) %>% mutate(Group = "B", Subgroup = "B5") %>% slice_head(n=52680)
turbineB6 <- readr::read_csv("Turbine_Data_Kelmarsh_6_2020-01-01_-_2021-01-01_233.csv") %>% select(1,2,62) %>% mutate(Group = "B", Subgroup = "B6") %>% slice_head(n=52680)

dataset <- rbind(turbineA1,turbineA2,turbineA4,turbineA5,turbineA6,turbineA7,turbineA8,turbineA9,turbineA10,turbineA11,turbineA12,turbineA13,turbineA14,turbineA15,turbineB1,turbineB2,turbineB3,turbineB4,turbineB5,turbineB6)
colnames(dataset) <- c("PCTimeStamp", "Wind_Speed", "Power", "Group", "Subgroup")
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