#### Import everything and prepare data ####

library(dplyr)
library(tsibble)
library("readxl")
library(fpp3)
library(zoo)
library(stats)
library(foreach)
library(doParallel)

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

# change index name to Time for uniformity with 20-minutely, 30-minutely, 1-hourly data
min10_power <- min10_power %>%
  rename(Time = PCTimeStamp)

# set the proportion of training set
TrainingProportion <- 0.95



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





#### Benchmark models - 10 minutely ####
# we use TSCV, forecasting on rolling origin basis

# initialize our data set
min10_benchmark <- min10_power 
#%>% filter_index("2021-06-30")

# compute number of entries
N = nrow(min10_power)/n_keys(min10_power)  

# initialize our accuracy tibble
fc10_benchmark_accuracy <- NULL;

# set up parallelisation
registerDoParallel(cl <- makeCluster(15))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc10_benchmark_accuracy <- foreach(i = seq(ceiling(N*TrainingProportion),N-1,1), .combine = bind_rows, .packages = c("fpp3")) %dopar%
{
  
  # take training set from elements 1 up to i
  min10_tr_benchmark <- min10_benchmark %>%
    slice_head(n = i)
  
  fc_benchmark_accuracy <- NULL;
  
  # initialize the accuracy tibble
  fc_benchmark_accuracy <- fc_benchmark_accuracy %>%
    bind_rows(min10_tr_benchmark %>%
                model(naive_model = NAIVE(Power)) %>%
                forecast(h=1) %>% 
                accuracy(min10_power) %>% mutate(.id = i))
  
  return(fc_benchmark_accuracy)
}
  stopCluster(cl)



#### Benchmark models - 20 minutely ####
# we use TSCV, forecasting on rolling origin basis

# initialize our data set
min20_benchmark <- min20_power %>% group_by(Group, Subgroup)
#%>% filter_index("2021-06-30")

# compute number of entries
N = nrow(min20_power)/n_keys(min20_power)  

# initialize our accuracy tibble
fc20_benchmark_accuracy <- NULL;

# set up parallelisation
registerDoParallel(cl <- makeCluster(15))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc20_benchmark_accuracy <- foreach(i = seq(ceiling(N*TrainingProportion),N-1,1), .combine = bind_rows, .packages = c("fpp3")) %dopar%
  {
    
    # take training set from elements 1 up to i
    min20_tr_benchmark <- min20_benchmark %>%
      slice_head(n = i)
    
    fc_benchmark_accuracy <- NULL;
    
    # initialize the accuracy tibble
    fc_benchmark_accuracy <- fc_benchmark_accuracy %>%
      bind_rows(min20_tr_benchmark %>%
                  model(naive_model = NAIVE(Power)) %>%
                  forecast(h=1) %>% 
                  accuracy(min20_power) %>% mutate(.id = i))
    
    return(fc_benchmark_accuracy)
  }
stopCluster(cl)



#### Benchmark models - 30 minutely ####
# we use TSCV, forecasting on rolling origin basis

# initialize our data set
min30_benchmark <- min30_power %>% group_by(Group, Subgroup)
#%>% filter_index("2021-06-30")

# compute number of entries
N = nrow(min30_power)/n_keys(min30_power)  

# initialize our accuracy tibble
fc30_benchmark_accuracy <- NULL;

# set up parallelisation
registerDoParallel(cl <- makeCluster(15))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc30_benchmark_accuracy <- foreach(i = seq(ceiling(N*TrainingProportion),N-1,1), .combine = bind_rows, .packages = c("fpp3")) %dopar%
  {
    
    # take training set from elements 1 up to i
    min30_tr_benchmark <- min30_benchmark %>%
      slice_head(n = i)
    
    fc_benchmark_accuracy <- NULL;
    
    # initialize the accuracy tibble
    fc_benchmark_accuracy <- fc_benchmark_accuracy %>%
      bind_rows(min30_tr_benchmark %>%
                  model(naive_model = NAIVE(Power)) %>%
                  forecast(h=1) %>% 
                  accuracy(min30_power) %>% mutate(.id = i))
    
    return(fc_benchmark_accuracy)
  }
stopCluster(cl)



#### Benchmark models - 1 hourly ####
# we use TSCV, forecasting on rolling origin basis

# initialize our data set
hr1_benchmark <- hr1_power %>% group_by(Group, Subgroup)
#%>% filter_index("2021-06-30")

# compute number of entries
N = nrow(hr1_power)/n_keys(hr1_power)  

# initialize our accuracy tibble
fc1_benchmark_accuracy <- NULL;

# set up parallelisation
registerDoParallel(cl <- makeCluster(15))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc1_benchmark_accuracy <- foreach(i = seq(ceiling(N*TrainingProportion),N-1,1), .combine = bind_rows, .packages = c("fpp3")) %dopar%
  {
    
    # take training set from elements 1 up to i
    hr1_tr_benchmark <- hr1_benchmark %>%
      slice_head(n = i)
    
    fc_benchmark_accuracy <- NULL;
    
    # initialize the accuracy tibble
    fc_benchmark_accuracy <- fc_benchmark_accuracy %>%
      bind_rows(hr1_tr_benchmark %>%
                  model(naive_model = NAIVE(Power)) %>%
                  forecast(h=1) %>% 
                  accuracy(hr1_power) %>% mutate(.id = i))
    
    return(fc_benchmark_accuracy)
  }
stopCluster(cl)




#### Time series linear regression with feature engineering - 10 minutely ####
# again we use TSCV to evaluate the model
min10 <- min10_power
# %>%  filter_index("2021-06-30")

# compute number of entries
N = nrow(min10_power)/n_keys(min10_power)  

# initialize accuracy tibble
fc10_accuracy <- NULL;

# set up parallelisation
registerDoParallel(cl <- makeCluster(7))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc10_accuracy <- foreach(i = seq(ceiling(N*TrainingProportion),N-1,1), .combine = bind_rows, .packages = c("fpp3")) %dopar%
{
  # take training set from elements 1 up to i
  min10_tr <- min10 %>%
    slice_head(n = i)
  
  # initialize accuracy tibble
  fc_accuracy <- NULL;
  
  # compute features on data set
  min10_tr <- min10_tr %>% mutate(
    `WMA2` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 3, .after = -1, .complete = TRUE),
    `WMA3` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 4, .after = -1, .complete = TRUE),
    `WMA4` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 5, .after = -1, .complete = TRUE),
    `WMA5` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 6, .after = -1, .complete = TRUE),
    `WMA6` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 7, .after = -1, .complete = TRUE),
    `WMSD2` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 3, .after = -1, .complete = TRUE),
    `WMSD3` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 4, .after = -1, .complete = TRUE),
    `WMSD4` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 5, .after = -1, .complete = TRUE),
    `WMSD5` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 6, .after = -1, .complete = TRUE),
    `WMSD6` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 7, .after = -1, .complete = TRUE),				 
    `PMA2` = slider::slide_dbl(Power, mean,
                               .before = 3, .after = -1, .complete = TRUE),
    `PMA3` = slider::slide_dbl(Power, mean,
                               .before = 4, .after = -1, .complete = TRUE),
    `PMA4` = slider::slide_dbl(Power, mean,
                               .before = 5, .after = -1, .complete = TRUE),
    `PMA5` = slider::slide_dbl(Power, mean,
                               .before = 6, .after = -1, .complete = TRUE),
    `PMA6` = slider::slide_dbl(Power, mean,
                               .before = 7, .after = -1, .complete = TRUE),					
    `PMSD2` = slider::slide_dbl(Power, sd,
                                .before = 3, .after = -1, .complete = TRUE),
    `PMSD3` = slider::slide_dbl(Power, sd,
                                .before = 4, .after = -1, .complete = TRUE),
    `PMSD4` = slider::slide_dbl(Power, sd,
                                .before = 5, .after = -1, .complete = TRUE),
    `PMSD5` = slider::slide_dbl(Power, sd,
                                .before = 6, .after = -1, .complete = TRUE),
    `PMSD6` = slider::slide_dbl(Power, sd,
                                .before = 7, .after = -1, .complete = TRUE),
    `lag_wind1` = lag(Wind_Speed, 1),
    `lag_wind2` = lag(Wind_Speed, 2),
    `lag_wind3` = lag(Wind_Speed, 3),
    `lag_wind4` = lag(Wind_Speed, 4),
    `lag_wind5` = lag(Wind_Speed, 5),
    `lag_wind6` = lag(Wind_Speed, 6),
    `lag_power1` = lag(Power, 1),
    `lag_power2` = lag(Power, 2),
    `lag_power3` = lag(Power, 3),
    `lag_power4` = lag(Power, 4),
    `lag_power5` = lag(Power, 5),
    `lag_power6` = lag(Power, 6),
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
  
  # compute fit
  fit_total <- min10_tr %>%
    model(mod1 = TSLM(Power ~ WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_power1 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  # add new data for forecasting 1 step ahead
  min10_tr_test <- append_row(min10_tr, 1, keep_all = TRUE) %>% 
    mutate(
      `WMA2` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 3, .after = -1, .complete = TRUE),
      `WMA3` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 4, .after = -1, .complete = TRUE),
      `WMA4` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 5, .after = -1, .complete = TRUE),
      `WMA5` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 6, .after = -1, .complete = TRUE),
      `WMA6` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 7, .after = -1, .complete = TRUE),
      `WMSD2` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 3, .after = -1, .complete = TRUE),
      `WMSD3` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 4, .after = -1, .complete = TRUE),
      `WMSD4` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 5, .after = -1, .complete = TRUE),
      `WMSD5` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 6, .after = -1, .complete = TRUE),
      `WMSD6` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 7, .after = -1, .complete = TRUE),				 
      `PMA2` = slider::slide_dbl(Power, mean,
                                 .before = 3, .after = -1, .complete = TRUE),
      `PMA3` = slider::slide_dbl(Power, mean,
                                 .before = 4, .after = -1, .complete = TRUE),
      `PMA4` = slider::slide_dbl(Power, mean,
                                 .before = 5, .after = -1, .complete = TRUE),
      `PMA5` = slider::slide_dbl(Power, mean,
                                 .before = 6, .after = -1, .complete = TRUE),
      `PMA6` = slider::slide_dbl(Power, mean,
                                 .before = 7, .after = -1, .complete = TRUE),					
      `PMSD2` = slider::slide_dbl(Power, sd,
                                  .before = 3, .after = -1, .complete = TRUE),
      `PMSD3` = slider::slide_dbl(Power, sd,
                                  .before = 4, .after = -1, .complete = TRUE),
      `PMSD4` = slider::slide_dbl(Power, sd,
                                  .before = 5, .after = -1, .complete = TRUE),
      `PMSD5` = slider::slide_dbl(Power, sd,
                                  .before = 6, .after = -1, .complete = TRUE),
      `PMSD6` = slider::slide_dbl(Power, sd,
                                  .before = 7, .after = -1, .complete = TRUE),
      `lag_wind1` = lag(Wind_Speed, 1),
      `lag_wind2` = lag(Wind_Speed, 2),
      `lag_wind3` = lag(Wind_Speed, 3),
      `lag_wind4` = lag(Wind_Speed, 4),
      `lag_wind5` = lag(Wind_Speed, 5),
      `lag_wind6` = lag(Wind_Speed, 6),
      `lag_power1` = lag(Power, 1),
      `lag_power2` = lag(Power, 2),
      `lag_power3` = lag(Power, 3),
      `lag_power4` = lag(Power, 4),
      `lag_power5` = lag(Power, 5),
      `lag_power6` = lag(Power, 6),
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
  
  # forecast with new data
  fc_accuracy <- fc_accuracy %>%
    bind_rows(fit_total %>%
    forecast(new_data = min10_tr_test %>% 
               group_by(Group, Subgroup) %>% 
               slice_tail(n = 1)) %>% 
    accuracy(min10_power) %>% mutate(.id = i))
  
  return(fc_accuracy)
}
stopCluster(cl)



#### Time series linear regression with feature engineering - 20 minutely ####
# again we use TSCV to evaluate the model
min20 <- min20_power
# %>%  filter_index("2021-06-30")

# compute number of entries
N = nrow(min20_power)/n_keys(min20_power)  

# initialize accuracy tibble
fc20_accuracy <- NULL;

# set up parallelisation
registerDoParallel(cl <- makeCluster(15))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc20_accuracy <- foreach(i = seq(ceiling(N*TrainingProportion),N-1,1), .combine = bind_rows, .packages = c("fpp3")) %dopar%
{
  # take training set from elements 1 up to i
  min20_tr <- min20 %>%
    slice_head(n = i)
  
  # initialize accuracy tibble
  fc_accuracy <- NULL;
  
  # compute features on data set
  min20_tr <- min20_tr %>% mutate(
    `WMA2` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 3, .after = -1, .complete = TRUE),
    `WMA3` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 4, .after = -1, .complete = TRUE),
    `WMA4` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 5, .after = -1, .complete = TRUE),
    `WMA5` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 6, .after = -1, .complete = TRUE),
    `WMA6` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 7, .after = -1, .complete = TRUE),
    `WMSD2` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 3, .after = -1, .complete = TRUE),
    `WMSD3` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 4, .after = -1, .complete = TRUE),
    `WMSD4` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 5, .after = -1, .complete = TRUE),
    `WMSD5` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 6, .after = -1, .complete = TRUE),
    `WMSD6` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 7, .after = -1, .complete = TRUE),				 
    `PMA2` = slider::slide_dbl(Power, mean,
                               .before = 3, .after = -1, .complete = TRUE),
    `PMA3` = slider::slide_dbl(Power, mean,
                               .before = 4, .after = -1, .complete = TRUE),
    `PMA4` = slider::slide_dbl(Power, mean,
                               .before = 5, .after = -1, .complete = TRUE),
    `PMA5` = slider::slide_dbl(Power, mean,
                               .before = 6, .after = -1, .complete = TRUE),
    `PMA6` = slider::slide_dbl(Power, mean,
                               .before = 7, .after = -1, .complete = TRUE),					
    `PMSD2` = slider::slide_dbl(Power, sd,
                                .before = 3, .after = -1, .complete = TRUE),
    `PMSD3` = slider::slide_dbl(Power, sd,
                                .before = 4, .after = -1, .complete = TRUE),
    `PMSD4` = slider::slide_dbl(Power, sd,
                                .before = 5, .after = -1, .complete = TRUE),
    `PMSD5` = slider::slide_dbl(Power, sd,
                                .before = 6, .after = -1, .complete = TRUE),
    `PMSD6` = slider::slide_dbl(Power, sd,
                                .before = 7, .after = -1, .complete = TRUE),
    `lag_wind1` = lag(Wind_Speed, 1),
    `lag_wind2` = lag(Wind_Speed, 2),
    `lag_wind3` = lag(Wind_Speed, 3),
    `lag_wind4` = lag(Wind_Speed, 4),
    `lag_wind5` = lag(Wind_Speed, 5),
    `lag_wind6` = lag(Wind_Speed, 6),
    `lag_power1` = lag(Power, 1),
    `lag_power2` = lag(Power, 2),
    `lag_power3` = lag(Power, 3),
    `lag_power4` = lag(Power, 4),
    `lag_power5` = lag(Power, 5),
    `lag_power6` = lag(Power, 6),
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
  
  # compute fit
  fit_total <- min20_tr %>%
    model(mod1 = TSLM(Power ~ WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_power1 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  # add new data for forecasting 1 step ahead
  min20_tr_test <- append_row(min20_tr, 1, keep_all = TRUE) %>% 
    mutate(
      `WMA2` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 3, .after = -1, .complete = TRUE),
      `WMA3` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 4, .after = -1, .complete = TRUE),
      `WMA4` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 5, .after = -1, .complete = TRUE),
      `WMA5` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 6, .after = -1, .complete = TRUE),
      `WMA6` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 7, .after = -1, .complete = TRUE),
      `WMSD2` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 3, .after = -1, .complete = TRUE),
      `WMSD3` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 4, .after = -1, .complete = TRUE),
      `WMSD4` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 5, .after = -1, .complete = TRUE),
      `WMSD5` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 6, .after = -1, .complete = TRUE),
      `WMSD6` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 7, .after = -1, .complete = TRUE),				 
      `PMA2` = slider::slide_dbl(Power, mean,
                                 .before = 3, .after = -1, .complete = TRUE),
      `PMA3` = slider::slide_dbl(Power, mean,
                                 .before = 4, .after = -1, .complete = TRUE),
      `PMA4` = slider::slide_dbl(Power, mean,
                                 .before = 5, .after = -1, .complete = TRUE),
      `PMA5` = slider::slide_dbl(Power, mean,
                                 .before = 6, .after = -1, .complete = TRUE),
      `PMA6` = slider::slide_dbl(Power, mean,
                                 .before = 7, .after = -1, .complete = TRUE),					
      `PMSD2` = slider::slide_dbl(Power, sd,
                                  .before = 3, .after = -1, .complete = TRUE),
      `PMSD3` = slider::slide_dbl(Power, sd,
                                  .before = 4, .after = -1, .complete = TRUE),
      `PMSD4` = slider::slide_dbl(Power, sd,
                                  .before = 5, .after = -1, .complete = TRUE),
      `PMSD5` = slider::slide_dbl(Power, sd,
                                  .before = 6, .after = -1, .complete = TRUE),
      `PMSD6` = slider::slide_dbl(Power, sd,
                                  .before = 7, .after = -1, .complete = TRUE),
      `lag_wind1` = lag(Wind_Speed, 1),
      `lag_wind2` = lag(Wind_Speed, 2),
      `lag_wind3` = lag(Wind_Speed, 3),
      `lag_wind4` = lag(Wind_Speed, 4),
      `lag_wind5` = lag(Wind_Speed, 5),
      `lag_wind6` = lag(Wind_Speed, 6),
      `lag_power1` = lag(Power, 1),
      `lag_power2` = lag(Power, 2),
      `lag_power3` = lag(Power, 3),
      `lag_power4` = lag(Power, 4),
      `lag_power5` = lag(Power, 5),
      `lag_power6` = lag(Power, 6),
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
  
  # forecast with new data
  fc_accuracy <- fc_accuracy %>%
    bind_rows(fit_total %>%
                forecast(new_data = min20_tr_test %>% 
                           group_by(Group, Subgroup) %>% 
                           slice_tail(n = 1)) %>% 
                accuracy(min20_power) %>% mutate(.id = i))
  
  return(fc_accuracy)
}
stopCluster(cl)



#### Time series linear regression with feature engineering - 30 minutely ####
# again we use TSCV to evaluate the model
min30 <- min30_power
# %>%  filter_index("2021-06-30")

# compute number of entries
N = nrow(min30_power)/n_keys(min30_power)  

# initialize accuracy tibble
fc30_accuracy <- NULL;

# set up parallelisation
registerDoParallel(cl <- makeCluster(15))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc30_accuracy <- foreach(i = seq(ceiling(N*TrainingProportion),N-1,1), .combine = bind_rows, .packages = c("fpp3")) %dopar%
{
  # take training set from elements 1 up to i
  min30_tr <- min30 %>%
    slice_head(n = i)
  
  # initialize accuracy tibble
  fc_accuracy <- NULL;
  
  # compute features on data set
  min30_tr <- min30_tr %>% mutate(
    `WMA2` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 3, .after = -1, .complete = TRUE),
    `WMA3` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 4, .after = -1, .complete = TRUE),
    `WMA4` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 5, .after = -1, .complete = TRUE),
    `WMA5` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 6, .after = -1, .complete = TRUE),
    `WMA6` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 7, .after = -1, .complete = TRUE),
    `WMSD2` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 3, .after = -1, .complete = TRUE),
    `WMSD3` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 4, .after = -1, .complete = TRUE),
    `WMSD4` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 5, .after = -1, .complete = TRUE),
    `WMSD5` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 6, .after = -1, .complete = TRUE),
    `WMSD6` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 7, .after = -1, .complete = TRUE),				 
    `PMA2` = slider::slide_dbl(Power, mean,
                               .before = 3, .after = -1, .complete = TRUE),
    `PMA3` = slider::slide_dbl(Power, mean,
                               .before = 4, .after = -1, .complete = TRUE),
    `PMA4` = slider::slide_dbl(Power, mean,
                               .before = 5, .after = -1, .complete = TRUE),
    `PMA5` = slider::slide_dbl(Power, mean,
                               .before = 6, .after = -1, .complete = TRUE),
    `PMA6` = slider::slide_dbl(Power, mean,
                               .before = 7, .after = -1, .complete = TRUE),					
    `PMSD2` = slider::slide_dbl(Power, sd,
                                .before = 3, .after = -1, .complete = TRUE),
    `PMSD3` = slider::slide_dbl(Power, sd,
                                .before = 4, .after = -1, .complete = TRUE),
    `PMSD4` = slider::slide_dbl(Power, sd,
                                .before = 5, .after = -1, .complete = TRUE),
    `PMSD5` = slider::slide_dbl(Power, sd,
                                .before = 6, .after = -1, .complete = TRUE),
    `PMSD6` = slider::slide_dbl(Power, sd,
                                .before = 7, .after = -1, .complete = TRUE),
    `lag_wind1` = lag(Wind_Speed, 1),
    `lag_wind2` = lag(Wind_Speed, 2),
    `lag_wind3` = lag(Wind_Speed, 3),
    `lag_wind4` = lag(Wind_Speed, 4),
    `lag_wind5` = lag(Wind_Speed, 5),
    `lag_wind6` = lag(Wind_Speed, 6),
    `lag_power1` = lag(Power, 1),
    `lag_power2` = lag(Power, 2),
    `lag_power3` = lag(Power, 3),
    `lag_power4` = lag(Power, 4),
    `lag_power5` = lag(Power, 5),
    `lag_power6` = lag(Power, 6),
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
  
  # compute fit
  fit_total <- min30_tr %>%
    model(mod1 = TSLM(Power ~ WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_power1 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  # add new data for forecasting 1 step ahead
  min30_tr_test <- append_row(min30_tr, 1, keep_all = TRUE) %>% 
    mutate(
      `WMA2` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 3, .after = -1, .complete = TRUE),
      `WMA3` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 4, .after = -1, .complete = TRUE),
      `WMA4` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 5, .after = -1, .complete = TRUE),
      `WMA5` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 6, .after = -1, .complete = TRUE),
      `WMA6` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 7, .after = -1, .complete = TRUE),
      `WMSD2` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 3, .after = -1, .complete = TRUE),
      `WMSD3` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 4, .after = -1, .complete = TRUE),
      `WMSD4` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 5, .after = -1, .complete = TRUE),
      `WMSD5` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 6, .after = -1, .complete = TRUE),
      `WMSD6` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 7, .after = -1, .complete = TRUE),				 
      `PMA2` = slider::slide_dbl(Power, mean,
                                 .before = 3, .after = -1, .complete = TRUE),
      `PMA3` = slider::slide_dbl(Power, mean,
                                 .before = 4, .after = -1, .complete = TRUE),
      `PMA4` = slider::slide_dbl(Power, mean,
                                 .before = 5, .after = -1, .complete = TRUE),
      `PMA5` = slider::slide_dbl(Power, mean,
                                 .before = 6, .after = -1, .complete = TRUE),
      `PMA6` = slider::slide_dbl(Power, mean,
                                 .before = 7, .after = -1, .complete = TRUE),					
      `PMSD2` = slider::slide_dbl(Power, sd,
                                  .before = 3, .after = -1, .complete = TRUE),
      `PMSD3` = slider::slide_dbl(Power, sd,
                                  .before = 4, .after = -1, .complete = TRUE),
      `PMSD4` = slider::slide_dbl(Power, sd,
                                  .before = 5, .after = -1, .complete = TRUE),
      `PMSD5` = slider::slide_dbl(Power, sd,
                                  .before = 6, .after = -1, .complete = TRUE),
      `PMSD6` = slider::slide_dbl(Power, sd,
                                  .before = 7, .after = -1, .complete = TRUE),
      `lag_wind1` = lag(Wind_Speed, 1),
      `lag_wind2` = lag(Wind_Speed, 2),
      `lag_wind3` = lag(Wind_Speed, 3),
      `lag_wind4` = lag(Wind_Speed, 4),
      `lag_wind5` = lag(Wind_Speed, 5),
      `lag_wind6` = lag(Wind_Speed, 6),
      `lag_power1` = lag(Power, 1),
      `lag_power2` = lag(Power, 2),
      `lag_power3` = lag(Power, 3),
      `lag_power4` = lag(Power, 4),
      `lag_power5` = lag(Power, 5),
      `lag_power6` = lag(Power, 6),
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
  
  # forecast with new data
  fc_accuracy <- fc_accuracy %>%
    bind_rows(fit_total %>%
                forecast(new_data = min30_tr_test %>% 
                           group_by(Group, Subgroup) %>% 
                           slice_tail(n = 1)) %>% 
                accuracy(min30_power) %>% mutate(.id = i))
  
  return(fc_accuracy)
}
stopCluster(cl)


#### Time series linear regression with feature engineering - 1 hourly ####
# again we use TSCV to evaluate the model
hr1 <- hr1_power
# %>%  filter_index("2021-06-30")

# compute number of entries
N = nrow(hr1_power)/n_keys(hr1_power)  

# initialize accuracy tibble
fc1_accuracy <- NULL;

# set up parallelisation
registerDoParallel(cl <- makeCluster(15))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc1_accuracy <- foreach(i = seq(ceiling(N*TrainingProportion),N-1,1), .combine = bind_rows, .packages = c("fpp3")) %dopar%
{
  # take training set from elements 1 up to i
  hr1_tr <- hr1 %>%
    slice_head(n = i)
  
  # initialize accuracy tibble
  fc_accuracy <- NULL;
  
  # compute features on data set
  hr1_tr <- hr1_tr %>% mutate(
    `WMA2` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 3, .after = -1, .complete = TRUE),
    `WMA3` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 4, .after = -1, .complete = TRUE),
    `WMA4` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 5, .after = -1, .complete = TRUE),
    `WMA5` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 6, .after = -1, .complete = TRUE),
    `WMA6` = slider::slide_dbl(Wind_Speed, mean,
                               .before = 7, .after = -1, .complete = TRUE),
    `WMSD2` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 3, .after = -1, .complete = TRUE),
    `WMSD3` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 4, .after = -1, .complete = TRUE),
    `WMSD4` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 5, .after = -1, .complete = TRUE),
    `WMSD5` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 6, .after = -1, .complete = TRUE),
    `WMSD6` = slider::slide_dbl(Wind_Speed, sd,
                                .before = 7, .after = -1, .complete = TRUE),				 
    `PMA2` = slider::slide_dbl(Power, mean,
                               .before = 3, .after = -1, .complete = TRUE),
    `PMA3` = slider::slide_dbl(Power, mean,
                               .before = 4, .after = -1, .complete = TRUE),
    `PMA4` = slider::slide_dbl(Power, mean,
                               .before = 5, .after = -1, .complete = TRUE),
    `PMA5` = slider::slide_dbl(Power, mean,
                               .before = 6, .after = -1, .complete = TRUE),
    `PMA6` = slider::slide_dbl(Power, mean,
                               .before = 7, .after = -1, .complete = TRUE),					
    `PMSD2` = slider::slide_dbl(Power, sd,
                                .before = 3, .after = -1, .complete = TRUE),
    `PMSD3` = slider::slide_dbl(Power, sd,
                                .before = 4, .after = -1, .complete = TRUE),
    `PMSD4` = slider::slide_dbl(Power, sd,
                                .before = 5, .after = -1, .complete = TRUE),
    `PMSD5` = slider::slide_dbl(Power, sd,
                                .before = 6, .after = -1, .complete = TRUE),
    `PMSD6` = slider::slide_dbl(Power, sd,
                                .before = 7, .after = -1, .complete = TRUE),
    `lag_wind1` = lag(Wind_Speed, 1),
    `lag_wind2` = lag(Wind_Speed, 2),
    `lag_wind3` = lag(Wind_Speed, 3),
    `lag_wind4` = lag(Wind_Speed, 4),
    `lag_wind5` = lag(Wind_Speed, 5),
    `lag_wind6` = lag(Wind_Speed, 6),
    `lag_power1` = lag(Power, 1),
    `lag_power2` = lag(Power, 2),
    `lag_power3` = lag(Power, 3),
    `lag_power4` = lag(Power, 4),
    `lag_power5` = lag(Power, 5),
    `lag_power6` = lag(Power, 6),
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
  
  # compute fit
  fit_total <- hr1_tr %>%
    model(mod1 = TSLM(Power ~ WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_power1 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  # add new data for forecasting 1 step ahead
  hr1_tr_test <- append_row(hr1_tr, 1, keep_all = TRUE) %>% 
    mutate(
      `WMA2` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 3, .after = -1, .complete = TRUE),
      `WMA3` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 4, .after = -1, .complete = TRUE),
      `WMA4` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 5, .after = -1, .complete = TRUE),
      `WMA5` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 6, .after = -1, .complete = TRUE),
      `WMA6` = slider::slide_dbl(Wind_Speed, mean,
                                 .before = 7, .after = -1, .complete = TRUE),
      `WMSD2` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 3, .after = -1, .complete = TRUE),
      `WMSD3` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 4, .after = -1, .complete = TRUE),
      `WMSD4` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 5, .after = -1, .complete = TRUE),
      `WMSD5` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 6, .after = -1, .complete = TRUE),
      `WMSD6` = slider::slide_dbl(Wind_Speed, sd,
                                  .before = 7, .after = -1, .complete = TRUE),				 
      `PMA2` = slider::slide_dbl(Power, mean,
                                 .before = 3, .after = -1, .complete = TRUE),
      `PMA3` = slider::slide_dbl(Power, mean,
                                 .before = 4, .after = -1, .complete = TRUE),
      `PMA4` = slider::slide_dbl(Power, mean,
                                 .before = 5, .after = -1, .complete = TRUE),
      `PMA5` = slider::slide_dbl(Power, mean,
                                 .before = 6, .after = -1, .complete = TRUE),
      `PMA6` = slider::slide_dbl(Power, mean,
                                 .before = 7, .after = -1, .complete = TRUE),					
      `PMSD2` = slider::slide_dbl(Power, sd,
                                  .before = 3, .after = -1, .complete = TRUE),
      `PMSD3` = slider::slide_dbl(Power, sd,
                                  .before = 4, .after = -1, .complete = TRUE),
      `PMSD4` = slider::slide_dbl(Power, sd,
                                  .before = 5, .after = -1, .complete = TRUE),
      `PMSD5` = slider::slide_dbl(Power, sd,
                                  .before = 6, .after = -1, .complete = TRUE),
      `PMSD6` = slider::slide_dbl(Power, sd,
                                  .before = 7, .after = -1, .complete = TRUE),
      `lag_wind1` = lag(Wind_Speed, 1),
      `lag_wind2` = lag(Wind_Speed, 2),
      `lag_wind3` = lag(Wind_Speed, 3),
      `lag_wind4` = lag(Wind_Speed, 4),
      `lag_wind5` = lag(Wind_Speed, 5),
      `lag_wind6` = lag(Wind_Speed, 6),
      `lag_power1` = lag(Power, 1),
      `lag_power2` = lag(Power, 2),
      `lag_power3` = lag(Power, 3),
      `lag_power4` = lag(Power, 4),
      `lag_power5` = lag(Power, 5),
      `lag_power6` = lag(Power, 6),
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
  
  # forecast with new data
  fc_accuracy <- fc_accuracy %>%
    bind_rows(fit_total %>%
                forecast(new_data = hr1_tr_test %>% 
                           group_by(Group, Subgroup) %>% 
                           slice_tail(n = 1)) %>% 
                accuracy(hr1_power) %>% mutate(.id = i))
  
  return(fc_accuracy)
}
stopCluster(cl)


  

#### Compare benchmark to linear regression - 10 minutely ####

accuracy_benchmark_error <- fc_benchmark_accuracy %>%
  group_by(.model, Group, Subgroup)  %>% 
  summarise(TotalMASE = mean(MASE), TotalRMSSE = mean(RMSSE))

# Compute benchmark accuracy for Level 0
accuracy10_benchmark_L0 <- accuracy_benchmark_error %>% 
  filter(is_aggregated(Group), is_aggregated(Subgroup))

# Compute benchmark accuracy for Level 1
accuracy10_benchmark_L1 <- accuracy_benchmark_error %>%
  filter(is_aggregated(Subgroup), !is_aggregated(Group)) %>%
  group_by(.model) %>%
  summarise(TotalMASE = mean(TotalMASE), TotalRMSSE = mean(TotalRMSSE))

# Compute benchmark accuracy for Level 2
accuracy10_benchmark_L2 <- accuracy_benchmark_error %>%
  filter(!is_aggregated(Subgroup), !is_aggregated(Group)) %>%
  group_by(.model) %>%
  summarise(TotalMASE = mean(TotalMASE), TotalRMSSE = mean(TotalRMSSE))


accuracy10_error <- fc10_accuracy %>%
  group_by(.model, Group, Subgroup)  %>% 
  summarise(TotalMASE = mean(MASE), TotalRMSSE = mean(RMSSE))

# Compute accuracy for Level 0
accuracy10_L0 <- accuracy10_error %>% 
  filter(is_aggregated(Group), is_aggregated(Subgroup))

# Compute accuracy for Level 1
accuracy10_L1 <- accuracy10_error %>%
  filter(is_aggregated(Subgroup), !is_aggregated(Group)) %>%
  group_by(.model) %>%
  summarise(TotalMASE = mean(TotalMASE), TotalRMSSE = mean(TotalRMSSE))

# Compute accuracy for Level 2
accuracy10_L2 <- accuracy10_error %>%
  filter(!is_aggregated(Subgroup), !is_aggregated(Group)) %>%
  group_by(.model) %>%
  summarise(TotalMASE = mean(TotalMASE), TotalRMSSE = mean(TotalRMSSE))



#### Compare benchmark to linear regression - 20 minutely ####

accuracy20_benchmark_error <- fc20_benchmark_accuracy %>%
  group_by(.model, Group, Subgroup)  %>% 
  summarise(TotalMASE = mean(MASE), TotalRMSSE = mean(RMSSE))

# Compute benchmark accuracy for Level 0
accuracy20_benchmark_L0 <- accuracy20_benchmark_error %>% 
  filter(is_aggregated(Group), is_aggregated(Subgroup))

# Compute benchmark accuracy for Level 1
accuracy20_benchmark_L1 <- accuracy20_benchmark_error %>%
  filter(is_aggregated(Subgroup), !is_aggregated(Group)) %>%
  group_by(.model) %>%
  summarise(TotalMASE = mean(TotalMASE), TotalRMSSE = mean(TotalRMSSE))

# Compute benchmark accuracy for Level 2
accuracy20_benchmark_L2 <- accuracy20_benchmark_error %>%
  filter(!is_aggregated(Subgroup), !is_aggregated(Group)) %>%
  group_by(.model) %>%
  summarise(TotalMASE = mean(TotalMASE), TotalRMSSE = mean(TotalRMSSE))

accuracy20_error <- fc20_accuracy %>%
  group_by(.model, Group, Subgroup)  %>% 
  summarise(TotalMASE = mean(MASE), TotalRMSSE = mean(RMSSE))

# Compute benchmark accuracy for Level 0
accuracy20_L0 <- accuracy20_error %>% 
  filter(is_aggregated(Group), is_aggregated(Subgroup))

# Compute benchmark accuracy for Level 1
accuracy20_L1 <- accuracy20_error %>%
  filter(is_aggregated(Subgroup), !is_aggregated(Group)) %>%
  group_by(.model) %>%
  summarise(TotalMASE = mean(TotalMASE), TotalRMSSE = mean(TotalRMSSE))

# Compute benchmark accuracy for Level 2
accuracy20_L2 <- accuracy20_error %>%
  filter(!is_aggregated(Subgroup), !is_aggregated(Group)) %>%
  group_by(.model) %>%
  summarise(TotalMASE = mean(TotalMASE), TotalRMSSE = mean(TotalRMSSE))





#### Compare benchmark to linear regression - 30 minutely ####

accuracy30_benchmark_error <- fc30_benchmark_accuracy %>%
  group_by(.model, Group, Subgroup)  %>% 
  summarise(TotalMASE = mean(MASE), TotalRMSSE = mean(RMSSE))

# Compute benchmark accuracy for Level 0
accuracy30_benchmark_L0 <- accuracy30_benchmark_error %>% 
  filter(is_aggregated(Group), is_aggregated(Subgroup))

# Compute benchmark accuracy for Level 1
accuracy30_benchmark_L1 <- accuracy30_benchmark_error %>%
  filter(is_aggregated(Subgroup), !is_aggregated(Group)) %>%
  group_by(.model) %>%
  summarise(TotalMASE = mean(TotalMASE), TotalRMSSE = mean(TotalRMSSE))

# Compute benchmark accuracy for Level 2
accuracy30_benchmark_L2 <- accuracy30_benchmark_error %>%
  filter(!is_aggregated(Subgroup), !is_aggregated(Group)) %>%
  group_by(.model) %>%
  summarise(TotalMASE = mean(TotalMASE), TotalRMSSE = mean(TotalRMSSE))

accuracy30_error <- fc30_accuracy %>%
  group_by(.model, Group, Subgroup)  %>% 
  summarise(TotalMASE = mean(MASE), TotalRMSSE = mean(RMSSE))

# Compute benchmark accuracy for Level 0
accuracy30_L0 <- accuracy30_error %>% 
  filter(is_aggregated(Group), is_aggregated(Subgroup))

# Compute benchmark accuracy for Level 1
accuracy30_L1 <- accuracy30_error %>%
  filter(is_aggregated(Subgroup), !is_aggregated(Group)) %>%
  group_by(.model) %>%
  summarise(TotalMASE = mean(TotalMASE), TotalRMSSE = mean(TotalRMSSE))

# Compute benchmark accuracy for Level 2
accuracy30_L2 <- accuracy30_error %>%
  filter(!is_aggregated(Subgroup), !is_aggregated(Group)) %>%
  group_by(.model) %>%
  summarise(TotalMASE = mean(TotalMASE), TotalRMSSE = mean(TotalRMSSE))




#### Compare benchmark to linear regression - 1 hourly ####

accuracy1_benchmark_error <- fc1_benchmark_accuracy %>%
  group_by(.model, Group, Subgroup)  %>% 
  summarise(TotalMASE = mean(MASE), TotalRMSSE = mean(RMSSE))

# Compute benchmark accuracy for Level 0
accuracy1_benchmark_L0 <- accuracy1_benchmark_error %>% 
  filter(is_aggregated(Group), is_aggregated(Subgroup))

# Compute benchmark accuracy for Level 1
accuracy1_benchmark_L1 <- accuracy1_benchmark_error %>%
  filter(is_aggregated(Subgroup), !is_aggregated(Group)) %>%
  group_by(.model) %>%
  summarise(TotalMASE = mean(TotalMASE), TotalRMSSE = mean(TotalRMSSE))

# Compute benchmark accuracy for Level 2
accuracy1_benchmark_L2 <- accuracy1_benchmark_error %>%
  filter(!is_aggregated(Subgroup), !is_aggregated(Group)) %>%
  group_by(.model) %>%
  summarise(TotalMASE = mean(TotalMASE), TotalRMSSE = mean(TotalRMSSE))

accuracy1_error <- fc1_accuracy %>%
  group_by(.model, Group, Subgroup)  %>% 
  summarise(TotalMASE = mean(MASE), TotalRMSSE = mean(RMSSE))

# Compute benchmark accuracy for Level 0
accuracy1_L0 <- accuracy1_error %>% 
  filter(is_aggregated(Group), is_aggregated(Subgroup))

# Compute benchmark accuracy for Level 1
accuracy1_L1 <- accuracy1_error %>%
  filter(is_aggregated(Subgroup), !is_aggregated(Group)) %>%
  group_by(.model) %>%
  summarise(TotalMASE = mean(TotalMASE), TotalRMSSE = mean(TotalRMSSE))

# Compute benchmark accuracy for Level 2
accuracy1_L2 <- accuracy1_error %>%
  filter(!is_aggregated(Subgroup), !is_aggregated(Group)) %>%
  group_by(.model) %>%
  summarise(TotalMASE = mean(TotalMASE), TotalRMSSE = mean(TotalRMSSE))
