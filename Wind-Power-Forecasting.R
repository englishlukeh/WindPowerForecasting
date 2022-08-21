#### Import everything and prepare data ####

library(dplyr)
library(tsibble)
library("readxl")
library(fpp3)
library(zoo)

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

#form hierarchy
min10_power <- dataset %>%
  aggregate_key(Group/Subgroup, Power = sum(Power), Wind_Speed = sum(Wind_Speed)) %>% group_by_key()

#20 minutely
min20_power <- min10_power %>%
  group_by_key() %>%
  index_by(Time = ~ lubridate::floor_date(., "20 minutes")) %>%
  summarise(
    Power = sum(Power), Wind_Speed = sum(Wind_Speed)
  )

#30 minutely
min30_power <- min10_power %>%
  group_by_key() %>%
  index_by(Time = ~ lubridate::floor_date(., "30 minutes")) %>%
  summarise(
    Power = sum(Power), Wind_Speed = sum(Wind_Speed)
  )

#1 hourly
hr1_power <- min10_power %>%
  group_by_key() %>%
  index_by(Time = ~ lubridate::floor_date(., "1 hour")) %>%
  summarise(
    Power = sum(Power), Wind_Speed = sum(Wind_Speed)
  )

min10_power <- min10_power %>%
  rename(Time = PCTimeStamp)




#### Check correlations ####

#check correlation for hourly
for (val in node_names) {
  tmp <- hr1_power %>%
    filter(Subgroup == val)
  
  Filename = paste(val, "_hourly.png", sep = "")
  tmp_plot <- ggplot(tmp, aes(x = Wind_Speed, y = Power)) + 
  geom_point() +
  labs(x = "Wind Speed (m/s)",
       y = "Average Power (kW)", title = paste("Hourly Data, Turbine:", val))
  
  ggsave(Filename, tmp_plot)
}

#check correlation for 10 minutely
for (val in node_names) {
  tmp <- min10_power %>%
    filter(Subgroup == val)
  
  Filename = paste(val, "_10minutely.png", sep = "")
  tmp_plot <- ggplot(tmp, aes(x = Wind_Speed, y = Power)) + 
    geom_point() +
    labs(x = "Wind Speed (m/s)",
         y = "Average Power (kW)", title = paste("10-Minutely Data, Turbine:", val))
  
  ggsave(Filename, tmp_plot)
}

#check correlation for 20 minutely
for (val in node_names) {
  tmp <- min20_power %>%
    filter(Subgroup == val)
  
  Filename = paste(val, "_20minutely.png", sep = "")
  tmp_plot <- ggplot(tmp, aes(x = Wind_Speed, y = Power)) + 
    geom_point() +
    labs(x = "Wind Speed (m/s)",
         y = "Average Power (kW)", title = paste("20-Minutely Data, Turbine:", val))
  
  ggsave(Filename, tmp_plot)
}

#check correlation for 30 minutely
for (val in node_names) {
  tmp <- min30_power %>%
    filter(Subgroup == val)
  
  Filename = paste(val, "_30minutely.png", sep = "")
  tmp_plot <- ggplot(tmp, aes(x = Wind_Speed, y = Power)) + 
    geom_point() +
    labs(x = "Wind Speed (m/s)",
         y = "Average Power (kW)", title = paste("30-Minutely Data, Turbine:", val))
  
  ggsave(Filename, tmp_plot)
}





#### Benchmark models ####

# now let's do some forecasting!
# split into training and test data (11 months + 1 month)
training <- min10_power %>%
  filter_index(~ "2021-06-30")
test <- min10_power %>%
  filter_index("2021-06" ~ .)


# or to speed up, we just use 1 day of data and 10-minutely
# take one month
min10_benchmark <- min10_power %>%
  filter_index("2021-06-30")

# set up TSCV with last day as rolling origin (90% training/10% test)
min10_tr_benchmark <- min10_benchmark %>%
  stretch_tsibble(.init = 130, .step = 1) %>%
  relocate(Time, Group, Subgroup, .id)

# we must do this since td, bu only works with strictly hierarchical time series
# initialize the accuracy tibble then loop through the rest
fc_benchmark_accuracy <- min10_tr_benchmark %>%
  filter(.id == 1) %>%
  update_tsibble(key = c(Group, Subgroup)) %>%
  model(naive_model = NAIVE(Power)) %>%
  reconcile(
    bu_naive = bottom_up(naive_model),
    td_naive = top_down(naive_model),
    mo_naive = middle_out(naive_model),
    ols_naive = min_trace(naive_model, method = "ols"),
    mint_naive = min_trace(naive_model, method = "mint_shrink")
  ) %>%
  forecast(h=1) %>% 
  accuracy(min10_power) %>% mutate(.id = 1)

# start the loop for later TSCV
for (ind in c(2:14))
{
  fc_benchmark_accuracy <- min10_tr_benchmark %>%
    filter(.id == ind) %>%
    update_tsibble(key = c(Group, Subgroup)) %>%
    model(naive_model = NAIVE(Power)) %>%
    reconcile(
      bu_naive = bottom_up(naive_model),
      td_naive = top_down(naive_model),
      mo_naive = middle_out(naive_model),
      ols_naive = min_trace(naive_model, method = "ols"),
      mint_naive = min_trace(naive_model, method = "mint_shrink")
    ) %>%
    forecast(h=1) %>% 
    accuracy(min10_power) %>% mutate(.id = ind) %>% bind_rows(fc_benchmark_accuracy)
}


#### Only using lagged wind speed (advice from Dr Abolghasemi) ####
# or to speed up, we just use 1 day of data and 10-minutely
min10_tr <- min10_power %>%
  filter_index("2021-06-30")

# set up TSCV with last day as rolling origin (90% training/10% test)
min10_tr <- min10_tr %>%
  stretch_tsibble(.init = 130, .step = 1) %>%
  relocate(Time, Group, Subgroup, .id) %>% group_by(Group, Subgroup, .id)

min10_tr <- min10_tr %>% mutate(
  `WMA` = slider::slide_dbl(Wind_Speed, mean,
                            .before = 6, .after = -1, .complete = TRUE),
  `WMSD` = slider::slide_dbl(Wind_Speed, sd,
                             .before = 6, .after = -1, .complete = TRUE),
  `PMA` = slider::slide_dbl(Power, mean,
                            .before = 6, .after = -1, .complete = TRUE),
  `PMSD` = slider::slide_dbl(Power, sd,
                             .before = 6, .after = -1, .complete = TRUE),
  `lag_wind1` = lag(Wind_Speed, 1),
  `lag_wind2` = lag(Wind_Speed, 2),
  `lag_wind3` = lag(Wind_Speed, 3),
  `lag_power1` = lag(Power, 1),
  `lag_power2` = lag(Power, 2),
  `lag_power3` = lag(Power, 3),
  `is_q1` = as.integer(quarter(Time)==1),
  `is_q2` = as.integer(quarter(Time)==2),
  `is_q3` = as.integer(quarter(Time)==3),
  `is_00` = as.integer(hour(Time) == 0),
  `is_01` = as.integer(hour(Time) == 1),
  `is_02` = as.integer(hour(Time) == 2),
  `is_03` = as.integer(hour(Time) == 3),
  `is_04` = as.integer(hour(Time) == 4),
  `is_05` = as.integer(hour(Time) == 5),
  `is_06` = as.integer(hour(Time) == 6),
  `is_07` = as.integer(hour(Time) == 7),
  `is_08` = as.integer(hour(Time) == 8),
  `is_09` = as.integer(hour(Time) == 9),
  `is_10` = as.integer(hour(Time) == 10),
  `is_11` = as.integer(hour(Time) == 11),
  `is_12` = as.integer(hour(Time) == 12),
  `is_13` = as.integer(hour(Time) == 13),
  `is_14` = as.integer(hour(Time) == 14),
  `is_15` = as.integer(hour(Time) == 15),
  `is_16` = as.integer(hour(Time) == 16),
  `is_17` = as.integer(hour(Time) == 17),
  `is_18` = as.integer(hour(Time) == 18),
  `is_19` = as.integer(hour(Time) == 19),
  `is_20` = as.integer(hour(Time) == 20),
  `is_21` = as.integer(hour(Time) == 21),
  `is_22` = as.integer(hour(Time) == 22)
)


# copied from below
fit_total <- min10_tr %>%
  filter(.id == 1) %>%
  update_tsibble(key = c(Group, Subgroup)) %>%
  model(mod1 = TSLM(Power ~ WMA + WMSD + PMA + PMSD + lag_wind1 + lag_wind2 + lag_wind3 + lag_power1 + lag_power2 + lag_power3 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
  reconcile(
    bu_mod1 = bottom_up(mod1),
    td_mod1 = top_down(mod1),
    mo_mod1 = middle_out(mod1),
    ols_mod1 = min_trace(mod1, method = "ols"),
    mint_mod1 = min_trace(mod1, method = "mint_shrink")
  )

# add new data
min10_tr_test <- append_row(min10_tr, 1, keep_all = TRUE) %>% 
  mutate(
  `WMA` = slider::slide_dbl(Wind_Speed, mean,
                            .before = 6, .after = -1, .complete = TRUE),
  `WMSD` = slider::slide_dbl(Wind_Speed, sd,
                             .before = 6, .after = -1, .complete = TRUE),
  `PMA` = slider::slide_dbl(Power, mean,
                            .before = 6, .after = -1, .complete = TRUE),
  `PMSD` = slider::slide_dbl(Power, sd,
                             .before = 6, .after = -1, .complete = TRUE),
  `lag_wind1` = lag(Wind_Speed, 1),
  `lag_wind2` = lag(Wind_Speed, 2),
  `lag_wind3` = lag(Wind_Speed, 3),
  `lag_power1` = lag(Power, 1),
  `lag_power2` = lag(Power, 2),
  `lag_power3` = lag(Power, 3),
  `is_q1` = as.integer(quarter(Time)==1),
  `is_q2` = as.integer(quarter(Time)==2),
  `is_q3` = as.integer(quarter(Time)==3),
  `is_q4` = as.integer(quarter(Time)==4),
  `is_00` = as.integer(hour(Time) == 0),
  `is_01` = as.integer(hour(Time) == 1),
  `is_02` = as.integer(hour(Time) == 2),
  `is_03` = as.integer(hour(Time) == 3),
  `is_04` = as.integer(hour(Time) == 4),
  `is_05` = as.integer(hour(Time) == 5),
  `is_06` = as.integer(hour(Time) == 6),
  `is_07` = as.integer(hour(Time) == 7),
  `is_08` = as.integer(hour(Time) == 8),
  `is_09` = as.integer(hour(Time) == 9),
  `is_10` = as.integer(hour(Time) == 10),
  `is_11` = as.integer(hour(Time) == 11),
  `is_12` = as.integer(hour(Time) == 12),
  `is_13` = as.integer(hour(Time) == 13),
  `is_14` = as.integer(hour(Time) == 14),
  `is_15` = as.integer(hour(Time) == 15),
  `is_16` = as.integer(hour(Time) == 16),
  `is_17` = as.integer(hour(Time) == 17),
  `is_18` = as.integer(hour(Time) == 18),
  `is_19` = as.integer(hour(Time) == 19),
  `is_20` = as.integer(hour(Time) == 20),
  `is_21` = as.integer(hour(Time) == 21),
  `is_22` = as.integer(hour(Time) == 22)
  )

  # forecast with new data
  fc_accuracy <- fit_total %>%
    forecast(new_data = min10_tr_test %>% 
    group_by(Group, Subgroup) %>% 
    filter(.id == 1) %>% 
    update_tsibble(key = c(Group, Subgroup)) %>%
    slice_tail(n = 1)) %>% 
    accuracy(min10_power) %>% mutate(.id = 1)
  
# start the loop for later TSCV
for (ind in c(2:14))
{
  fit_total <- min10_tr %>%
    filter(.id == ind) %>%
    update_tsibble(key = c(Group, Subgroup)) %>%
    model(mod1 = TSLM(Power ~ WMA + WMSD + PMA + PMSD + lag_wind1 + lag_wind2 + lag_wind3 + lag_power1 + lag_power2 + lag_power3 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  # forecast with new data
  fc_accuracy <- fit_total %>% 
    forecast(new_data = min10_tr_test %>% 
    group_by(Group, Subgroup) %>% 
    filter(.id == ind) %>% 
    update_tsibble(key = c(Group, Subgroup)) %>% 
    slice_tail(n = 1)) %>%
    accuracy(min10_power) %>% 
    mutate(.id = ind) %>%
    bind_rows(fc_accuracy)
}

  

#### Compare benchmark to linear regression ####

accuracy_error <- fc_accuracy %>% 
 group_by(.model, Group, Subgroup) %>%
 summarise(TotalMASE = sum(MASE), TotalRMSSE = sum(RMSSE))

accuracy_benchmark_error <- fc_benchmark_accuracy %>%
  group_by(.model, Group, Subgroup)  %>% 
  summarise(TotalMASE = sum(MASE), TotalRMSSE = sum(RMSSE))

