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

#form hierarchy
min10_power <- dataset %>%
  aggregate_key(Group/Subgroup, Power = sum(Power), Wind_Speed = sum(Wind_Speed)) %>% group_by_key()

#20 minutely
min20_power <- min10_power %>%
  index_by(Time = ~ lubridate::floor_date(., "20 minutes")) %>%
  summarise(
    Power = mean(Power), Wind_Speed = mean(Wind_Speed)
  ) %>%
  group_by_key()

#30 minutely
min30_power <- min10_power %>%
  index_by(Time = ~ lubridate::floor_date(., "30 minutes")) %>%
  summarise(
    Power = mean(Power), Wind_Speed = mean(Wind_Speed)
  ) %>%
  group_by_key()

#1 hourly
hr1_power <- min10_power %>%
  index_by(Time = ~ lubridate::floor_date(., "1 hour")) %>%
  summarise(
    Power = mean(Power), Wind_Speed = mean(Wind_Speed)
  ) %>%
  group_by_key()

# change index name to Time for uniformity with 20-minutely, 30-minutely, 1-hourly data
min10_power <- min10_power %>%
  rename(Time = PCTimeStamp)

saveRDS(min10_power, file = "min10.rds")
saveRDS(min20_power, file = "min20.rds")
saveRDS(min30_power, file = "min30.rds")
saveRDS(hr1_power, file = "hr1.rds")

# set the proportion of training set
TrainingProportion <- 0.90
# set number of threads
numThreads = 3;

library(ggpubr, include.only = 'ggarrange') # include one function

#### Load .rds files ####
min10_power <- readRDS(file = "min10.rds")
min20_power <- readRDS(file = "min20.rds")
min30_power <- readRDS(file = "min30.rds")
hr1_power <- readRDS(file = "hr1.rds")

#### descriptive statistics ####
A_10 <- min10_power %>% filter(Group == "A", is_aggregated(Subgroup))
A1_10 <- min10_power %>% filter(Group == "A", Subgroup == "A1")
A2_10 <- min10_power %>% filter(Group == "A", Subgroup == "A2")
A4_10 <- min10_power %>% filter(Group == "A", Subgroup == "A4")
A5_10 <- min10_power %>% filter(Group == "A", Subgroup == "A5")
A6_10 <- min10_power %>% filter(Group == "A", Subgroup == "A6")
A7_10 <- min10_power %>% filter(Group == "A", Subgroup == "A7")
A8_10 <- min10_power %>% filter(Group == "A", Subgroup == "A8")
A9_10 <- min10_power %>% filter(Group == "A", Subgroup == "A9")
A10_10 <- min10_power %>% filter(Group == "A", Subgroup == "A10")
A11_10 <- min10_power %>% filter(Group == "A", Subgroup == "A11")
A12_10 <- min10_power %>% filter(Group == "A", Subgroup == "A12")
A13_10 <- min10_power %>% filter(Group == "A", Subgroup == "A13")
A14_10 <- min10_power %>% filter(Group == "A", Subgroup == "A14")
A15_10 <- min10_power %>% filter(Group == "A", Subgroup == "A15")

B_10 <- min10_power %>% filter(Group == "B", is_aggregated(Subgroup))
B1_10 <- min10_power %>% filter(Group == "B", Subgroup == "B1")
B2_10 <- min10_power %>% filter(Group == "B", Subgroup == "B2")
B3_10 <- min10_power %>% filter(Group == "B", Subgroup == "B3")
B4_10 <- min10_power %>% filter(Group == "B", Subgroup == "B4")
B5_10 <- min10_power %>% filter(Group == "B", Subgroup == "B5")
B6_10 <- min10_power %>% filter(Group == "B", Subgroup == "B6")

boxplot(A1_10$Power, A2_10$Power, A4_10$Power, A5_10$Power, A6_10$Power, A7_10$Power, A8_10$Power, A9_10$Power, A10_10$Power, A11_10$Power, A12_10$Power, A13_10$Power, A14_10$Power, A15_10$Power, B1_10$Power, B2_10$Power, B3_10$Power, B4_10$Power, B5_10$Power, B6_10$Power,
        names = c("A1", "A2", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12", "A13", "A14", "A15", "B1", "B2", "B3", "B4", "B5", "B6"),
        xlab="Wind Turbine Index",
        ylab="Total Energy Generated (kW)",
        col=c("red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "orange", "orange", "orange", "orange", "orange", "orange"),
        border = "brown",
        las=2,
        horizontal = FALSE
)

agg_10 <- min10_power %>% filter(is_aggregated(Group), is_aggregated(Subgroup))

par(mar = c(5, 5, 5, 5))
boxplot(A_10$Power, B_10$Power, C_10$Power, D_10$Power, agg_10$Power,
        names = c("A", "B", "C", "D", "Aggregated"),
        xlab="Wind Farm Index",
        ylab="Total Energy Generated (kW)
        ",
        col=c("red", "orange", "yellow", "green", "grey"),
        border = "brown",
        las=1,
        horizontal = FALSE
)

min10_power %>% filter(Group == "A", Subgroup == "A1") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "A", Subgroup == "A2") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "A", Subgroup == "A4") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "A", Subgroup == "A5") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "A", Subgroup == "A6") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "A", Subgroup == "A7") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "A", Subgroup == "A8") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "A", Subgroup == "A9") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "A", Subgroup == "A10") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "A", Subgroup == "A11") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "A", Subgroup == "A12") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "A", Subgroup == "A13") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "A", Subgroup == "A14") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "A", Subgroup == "A15") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "A", is_aggregated(Subgroup)) %>% as.data.frame() %>% select(Power) %>% summary()

min10_power %>% filter(Group == "B", Subgroup == "B1") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "B", Subgroup == "B2") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "B", Subgroup == "B3") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "B", Subgroup == "B4") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "B", Subgroup == "B5") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "B", Subgroup == "B6") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "B", Subgroup == "B7") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "B", Subgroup == "B8") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "B", Subgroup == "B9") %>% as.data.frame() %>% select(Power) %>% summary()
min10_power %>% filter(Group == "B", is_aggregated(Subgroup)) %>% as.data.frame() %>% select(Power) %>% summary()

min10_power %>% filter(is_aggregated(Group), is_aggregated(Subgroup)) %>% as.data.frame() %>% select(Power) %>% summary()

plota <- hr1_power %>% filter(!is_aggregated(Group), !is_aggregated(Subgroup), Group == "A") %>% filter_index("2021-01-01"~"2021-01-31") %>% autoplot()
plotb <- hr1_power %>% filter(!is_aggregated(Group), !is_aggregated(Subgroup), Group == "B") %>% filter_index("2021-01-01"~"2021-01-31") %>% autoplot()
plotc <- hr1_power %>% filter(!is_aggregated(Group), !is_aggregated(Subgroup), Group == "C") %>% filter_index("2021-01-01"~"2021-01-31") %>% autoplot()
plotd <- hr1_power %>% filter(!is_aggregated(Group), !is_aggregated(Subgroup), Group == "D") %>% filter_index("2021-01-01"~"2021-01-31") %>% autoplot()

tmp_plot <- ggpubr::ggarrange(plotc, plotd,
                              labels = c("C", "D"),
                              ncol = 1, nrow = 2)

tmp_plot + theme(text=element_text(size=20))

plotabcdnoagg <- hr1_power %>% filter(is_aggregated(Subgroup), !is_aggregated(Group)) %>% filter_index("2021-01-01"~"2021-01-31") %>% autoplot()
plotabcdagg <- hr1_power %>% filter(is_aggregated(Group), is_aggregated(Subgroup)) %>% filter_index("2021-01-01"~"2021-01-31") %>% autoplot()

ggpubr::ggarrange(plotabcdnoagg, plotabcdagg,
                  labels = c("A", "B"),
                  ncol = 1, nrow = 2)


#### Benchmark models - 10 minutely ####
# we use TSCV, forecasting on rolling origin basis

# compute number of entries
N = nrow(min10_power)/n_keys(min10_power)

# initialize our accuracy tibble
fc10_benchmark <- NULL;

# set up parallelisation
registerDoParallel(cl <- makeCluster(numThreads))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc10_benchmark <- foreach(i = seq(ceiling(N*TrainingProportion),N-6,6), .combine = bind_rows, .packages = c("fpp3")) %dopar%
  {
    gc()
    
    # take training set from elements 1 up to i
    min10_tr_benchmark <- min10_power %>%
      slice_head(n = i)
    
    fc_benchmark <- NULL;
    
    # initialize the accuracy tibble
    fc_benchmark <- fc_benchmark %>%
      bind_rows(min10_tr_benchmark %>%
                  model(naive_model = NAIVE(Power)) %>%
                  forecast(h=6))
    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      mod_tmp <- min10_tr_benchmark %>%
        model(naive_model = NAIVE(Power))
      fitted <- mod_tmp %>%
        fitted()
      residuals <- mod_tmp %>%
        residuals()
      saveRDS(fitted, file = "fc10_benchmark_fitted.rds")
      saveRDS(residuals, file = "fc10_benchmark_residuals.rds")
    }
    
    return(fc_benchmark)
  }
stopCluster(cl)

saveRDS(fc10_benchmark, file = "fc10_benchmark.rds")

#### Benchmark models - 20 minutely ####
# we use TSCV, forecasting on rolling origin basis

# compute number of entries
N = nrow(min20_power)/n_keys(min20_power)

# initialize our accuracy tibble
fc20_benchmark <- NULL;

# set up parallelisation
registerDoParallel(cl <- makeCluster(numThreads))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc20_benchmark <- foreach(i = seq(ceiling(N*TrainingProportion),N-3,3), .combine = bind_rows, .packages = c("fpp3")) %dopar%
  {
    
    gc()
    
    # take training set from elements 1 up to i
    min20_tr_benchmark <- min20_power %>%
      slice_head(n = i)
    
    fc_benchmark <- NULL;
    
    # initialize the accuracy tibble
    fc_benchmark <- fc_benchmark %>%
      bind_rows(min20_tr_benchmark %>%
                  model(naive_model = NAIVE(Power)) %>%
                  forecast(h=3))
    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      mod_tmp <- min20_tr_benchmark %>%
        model(naive_model = NAIVE(Power))
      fitted <- mod_tmp %>%
        fitted()
      residuals <- mod_tmp %>%
        residuals()
      saveRDS(fitted, file = "fc20_benchmark_fitted.rds")
      saveRDS(residuals, file = "fc20_benchmark_residuals.rds")
    }
    
    return(fc_benchmark)
  }
stopCluster(cl)

saveRDS(fc20_benchmark, file = "fc20_benchmark.rds")


#### Benchmark models - 30 minutely ####
# we use TSCV, forecasting on rolling origin basis

# compute number of entries
N = nrow(min30_power)/n_keys(min30_power)

# initialize our accuracy tibble
fc30_benchmark <- NULL;

# set up parallelisation
registerDoParallel(cl <- makeCluster(numThreads))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc30_benchmark <- foreach(i = seq(ceiling(N*TrainingProportion),N-2,2), .combine = bind_rows, .packages = c("fpp3")) %dopar%
  {
    gc()
    
    # take training set from elements 1 up to i
    min30_tr_benchmark <- min30_power %>%
      slice_head(n = i)
    
    fc_benchmark <- NULL;
    
    # initialize the accuracy tibble
    fc_benchmark <- fc_benchmark %>%
      bind_rows(min30_tr_benchmark %>%
                  model(naive_model = NAIVE(Power)) %>%
                  forecast(h=2))
    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      mod_tmp <- min30_tr_benchmark %>%
        model(naive_model = NAIVE(Power))
      fitted <- mod_tmp %>%
        fitted()
      residuals <- mod_tmp %>%
        residuals()
      saveRDS(fitted, file = "fc30_benchmark_fitted.rds")
      saveRDS(residuals, file = "fc30_benchmark_residuals.rds")
    }
    
    return(fc_benchmark)
  }
stopCluster(cl)

saveRDS(fc30_benchmark, file = "fc30_benchmark.rds")


#### Benchmark models - 1 hourly ####
# we use TSCV, forecasting on rolling origin basis

# compute number of entries
N = nrow(hr1_power)/n_keys(hr1_power)

# initialize our accuracy tibble
fc1_benchmark <- NULL;

# set up parallelisation
registerDoParallel(cl <- makeCluster(numThreads))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc1_benchmark <- foreach(i = seq(ceiling(N*TrainingProportion),N-1,1), .combine = bind_rows, .packages = c("fpp3")) %dopar%
  {
    gc()
    
    # take training set from elements 1 up to i
    hr1_tr_benchmark <- hr1_power %>%
      slice_head(n = i)
    
    fc_benchmark <- NULL;
    
    # initialize the accuracy tibble
    fc_benchmark <- fc_benchmark %>%
      bind_rows(hr1_tr_benchmark %>%
                  model(naive_model = NAIVE(Power)) %>%
                  forecast(h=1))
    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      mod_tmp <- hr1_tr_benchmark %>%
        model(naive_model = NAIVE(Power))
      fitted <- mod_tmp %>%
        fitted()
      residuals <- mod_tmp %>%
        residuals()
      saveRDS(fitted, file = "fc1_benchmark_fitted.rds")
      saveRDS(residuals, file = "fc1_benchmark_residuals.rds")
    }
    
    return(fc_benchmark)
  }
stopCluster(cl)

saveRDS(fc1_benchmark, file = "fc1_benchmark.rds")

#### Time series linear regression with feature engineering - 10 minutely ####
# again we use TSCV to evaluate the model
min10 <- min10_power
# %>%  filter_index("2021-06-30")

# compute number of entries
N = nrow(min10_power)/n_keys(min10_power)

# initialize accuracy tibble
fc10_lr <- NULL;

# compute features on data set
min10 <- min10 %>% mutate(
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
  `lag_wind7` = lag(Wind_Speed, 7),
  `lag_wind8` = lag(Wind_Speed, 8),
  `lag_wind9` = lag(Wind_Speed, 9),
  `lag_wind10` = lag(Wind_Speed, 10),
  `lag_wind11` = lag(Wind_Speed, 11),
  `lag_power1` = lag(Power, 1),
  `lag_power2` = lag(Power, 2),
  `lag_power3` = lag(Power, 3),
  `lag_power4` = lag(Power, 4),
  `lag_power5` = lag(Power, 5),
  `lag_power6` = lag(Power, 6),
  `lag_power7` = lag(Power, 7),
  `lag_power8` = lag(Power, 8),
  `lag_power9` = lag(Power, 9),
  `lag_power10` = lag(Power, 10),
  `lag_power11` = lag(Power, 11),
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

min10 <- min10 %>% mutate(
  `WMA2_l1` = lag(WMA2,1),
  `WMA3_l1` = lag(WMA3,1),
  `WMA4_l1` = lag(WMA4,1),
  `WMA5_l1` = lag(WMA5,1),
  `WMA6_l1` = lag(WMA6,1),
  `WMSD2_l1` = lag(WMSD2,1),
  `WMSD3_l1` = lag(WMSD3,1),
  `WMSD4_l1` = lag(WMSD4,1),
  `WMSD5_l1` = lag(WMSD5,1),
  `WMSD6_l1` = lag(WMSD6,1),
  `PMA2_l1` = lag(PMA2, 1),
  `PMA3_l1` = lag(PMA3, 1),
  `PMA4_l1` = lag(PMA4, 1),
  `PMA5_l1` = lag(PMA5, 1),
  `PMA6_l1` = lag(PMA6, 1),
  `PMSD2_l1` = lag(PMSD2, 1),
  `PMSD3_l1` = lag(PMSD3, 1),
  `PMSD4_l1` = lag(PMSD4, 1),
  `PMSD5_l1` = lag(PMSD5, 1),
  `PMSD6_l1` = lag(PMSD6, 1),
  `WMA2_l2` = lag(WMA2,2),
  `WMA3_l2` = lag(WMA3,2),
  `WMA4_l2` = lag(WMA4,2),
  `WMA5_l2` = lag(WMA5,2),
  `WMA6_l2` = lag(WMA6,2),
  `WMSD2_l2` = lag(WMSD2,2),
  `WMSD3_l2` = lag(WMSD3,2),
  `WMSD4_l2` = lag(WMSD4,2),
  `WMSD5_l2` = lag(WMSD5,2),
  `WMSD6_l2` = lag(WMSD6,2),
  `PMA2_l2` = lag(PMA2, 2),
  `PMA3_l2` = lag(PMA3, 2),
  `PMA4_l2` = lag(PMA4, 2),
  `PMA5_l2` = lag(PMA5, 2),
  `PMA6_l2` = lag(PMA6, 2),
  `PMSD2_l2` = lag(PMSD2, 2),
  `PMSD3_l2` = lag(PMSD3, 2),
  `PMSD4_l2` = lag(PMSD4, 2),
  `PMSD5_l2` = lag(PMSD5, 2),
  `PMSD6_l2` = lag(PMSD6, 2),
  `WMA2_l3` = lag(WMA2,3),
  `WMA3_l3` = lag(WMA3,3),
  `WMA4_l3` = lag(WMA4,3),
  `WMA5_l3` = lag(WMA5,3),
  `WMA6_l3` = lag(WMA6,3),
  `WMSD2_l3` = lag(WMSD2,3),
  `WMSD3_l3` = lag(WMSD3,3),
  `WMSD4_l3` = lag(WMSD4,3),
  `WMSD5_l3` = lag(WMSD5,3),
  `WMSD6_l3` = lag(WMSD6,3),
  `PMA2_l3` = lag(PMA2, 3),
  `PMA3_l3` = lag(PMA3, 3),
  `PMA4_l3` = lag(PMA4, 3),
  `PMA5_l3` = lag(PMA5, 3),
  `PMA6_l3` = lag(PMA6, 3),
  `PMSD2_l3` = lag(PMSD2, 3),
  `PMSD3_l3` = lag(PMSD3, 3),
  `PMSD4_l3` = lag(PMSD4, 3),
  `PMSD5_l3` = lag(PMSD5, 3),
  `PMSD6_l3` = lag(PMSD6, 3),
  `WMA2_l4` = lag(WMA2,4),
  `WMA3_l4` = lag(WMA3,4),
  `WMA4_l4` = lag(WMA4,4),
  `WMA5_l4` = lag(WMA5,4),
  `WMA6_l4` = lag(WMA6,4),
  `WMSD2_l4` = lag(WMSD2,4),
  `WMSD3_l4` = lag(WMSD3,4),
  `WMSD4_l4` = lag(WMSD4,4),
  `WMSD5_l4` = lag(WMSD5,4),
  `WMSD6_l4` = lag(WMSD6,4),
  `PMA2_l4` = lag(PMA2, 4),
  `PMA3_l4` = lag(PMA3, 4),
  `PMA4_l4` = lag(PMA4, 4),
  `PMA5_l4` = lag(PMA5, 4),
  `PMA6_l4` = lag(PMA6, 4),
  `PMSD2_l4` = lag(PMSD2, 4),
  `PMSD3_l4` = lag(PMSD3, 4),
  `PMSD4_l4` = lag(PMSD4, 4),
  `PMSD5_l4` = lag(PMSD5, 4),
  `PMSD6_l4` = lag(PMSD6, 4),
  `WMA2_l5` = lag(WMA2,5),
  `WMA3_l5` = lag(WMA3,5),
  `WMA4_l5` = lag(WMA4,5),
  `WMA5_l5` = lag(WMA5,5),
  `WMA6_l5` = lag(WMA6,5),
  `WMSD2_l5` = lag(WMSD2,5),
  `WMSD3_l5` = lag(WMSD3,5),
  `WMSD4_l5` = lag(WMSD4,5),
  `WMSD5_l5` = lag(WMSD5,5),
  `WMSD6_l5` = lag(WMSD6,5),
  `PMA2_l5` = lag(PMA2, 5),
  `PMA3_l5` = lag(PMA3, 5),
  `PMA4_l5` = lag(PMA4, 5),
  `PMA5_l5` = lag(PMA5, 5),
  `PMA6_l5` = lag(PMA6, 5),
  `PMSD2_l5` = lag(PMSD2, 5),
  `PMSD3_l5` = lag(PMSD3, 5),
  `PMSD4_l5` = lag(PMSD4, 5),
  `PMSD5_l5` = lag(PMSD5, 5),
  `PMSD6_l5` = lag(PMSD6, 5)
)

# save .rds file
saveRDS(min10, file = "min10_features.rds")

# load .rds file
min10 <- readRDS(file = "min10_features.rds")

# compute number of entries
N = nrow(min10)/n_keys(min10)

gc()

# set up parallelisation
registerDoParallel(cl <- makeCluster(numThreads))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc10_lr <- foreach(i = seq(ceiling(N*TrainingProportion),N-6,6), .combine = bind_rows, .packages = c("fpp3")) %dopar%
  {
    # training set is from elements 1 up to i, i+1 is 1-step forecast
    # initialize accuracy tibble
    fc_lr <- NULL;
    
    
    # compute fit
    fit_h1 <- min10 %>%
      slice_head(n = i) %>%
      model(mod1 = TSLM(Power ~ WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_power1 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
      reconcile(
        bu_mod1 = bottom_up(mod1),
        td_mod1 = top_down(mod1),
        mo_mod1 = middle_out(mod1),
        ols_mod1 = min_trace(mod1, method = "ols"),
        mint_mod1 = min_trace(mod1, method = "mint_shrink")
      )
    
    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      fitted_h1 <- fit_h1 %>%
        fitted()
      residuals_h1 <- fit_h1 %>%
        residuals()
      saveRDS(fitted_h1, file = "fc10_h1_lr_fitted.rds")
      saveRDS(residuals_h1, file = "fc10_h1_lr_residuals.rds")
    }
    
    # forecast with new data
    fc_lr <- fc_lr %>%
      bind_rows(fit_h1 %>%
                  forecast(new_data = min10 %>%
                             group_by(Group, Subgroup) %>%
                             slice(n = i+1)))
    
    rm(fit_h1)
    gc()
    
    
    fit_h2 <- min10 %>%
      slice_head(n = i) %>%
      model(mod1 = TSLM(Power ~ WMA2_l1 + WMA3_l1 + WMA4_l1 + WMA5_l1 + WMA6_l1 + WMSD2_l1 + WMSD3_l1 + WMSD4_l1 + WMSD5_l1 + WMSD6_l1 + PMA2_l1 + PMA3_l1 + PMA4_l1 + PMA5_l1 + PMA6_l1 + PMSD2_l1 + PMSD3_l1 + PMSD4_l1 + PMSD5_l1 + PMSD6_l1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + lag_power7 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
      reconcile(
        bu_mod1 = bottom_up(mod1),
        td_mod1 = top_down(mod1),
        mo_mod1 = middle_out(mod1),
        ols_mod1 = min_trace(mod1, method = "ols"),
        mint_mod1 = min_trace(mod1, method = "mint_shrink")
      )
    
    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      fitted_h2 <- fit_h2 %>%
        fitted()
      residuals_h2 <- fit_h2 %>%
        residuals()
      saveRDS(fitted_h2, file = "fc10_h2_lr_fitted.rds")
      saveRDS(residuals_h2, file = "fc10_h2_lr_residuals.rds")
    }
    
    fc_lr <- fc_lr %>%
      bind_rows(fit_h2 %>%
                  forecast(new_data = min10 %>%
                             group_by(Group, Subgroup) %>%
                             slice(n = i+2)))
    
    rm(fit_h2)
    gc()
    
    
    fit_h3 <- min10 %>%
      slice_head(n = i) %>%
      model(mod1 = TSLM(Power ~ WMA2_l2 + WMA3_l2 + WMA4_l2 + WMA5_l2 + WMA6_l2 + WMSD2_l2 + WMSD3_l2 + WMSD4_l2 + WMSD5_l2 + WMSD6_l2 + PMA2_l2 + PMA3_l2 + PMA4_l2 + PMA5_l2 + PMA6_l2 + PMSD2_l2 + PMSD3_l2 + PMSD4_l2 + PMSD5_l2 + PMSD6_l2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + lag_power7 + lag_power8 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
      reconcile(
        bu_mod1 = bottom_up(mod1),
        td_mod1 = top_down(mod1),
        mo_mod1 = middle_out(mod1),
        ols_mod1 = min_trace(mod1, method = "ols"),
        mint_mod1 = min_trace(mod1, method = "mint_shrink")
      )
    
    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      fitted_h3 <- fit_h3 %>%
        fitted()
      residuals_h3 <- fit_h3 %>%
        residuals()
      saveRDS(fitted_h3, file = "fc10_h3_lr_fitted.rds")
      saveRDS(residuals_h3, file = "fc10_h3_lr_residuals.rds")
    }
    
    fc_lr <- fc_lr %>%
      bind_rows(fit_h3 %>%
                  forecast(new_data = min10 %>%
                             group_by(Group, Subgroup) %>%
                             slice(n = i+3)))
    
    rm(fit_h3)
    gc()
    
    
    fit_h4 <- min10 %>%
      slice_head(n = i) %>%
      model(mod1 = TSLM(Power ~ WMA2_l3 + WMA3_l3 + WMA4_l3 + WMA5_l3 + WMA6_l3 + WMSD2_l3 + WMSD3_l3 + WMSD4_l3 + WMSD5_l3 + WMSD6_l3 + PMA2_l3 + PMA3_l3 + PMA4_l3 + PMA5_l3 + PMA6_l3 + PMSD2_l3 + PMSD3_l3 + PMSD4_l3 + PMSD5_l3 + PMSD6_l3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_wind9 + lag_power4 + lag_power5 + lag_power6 + lag_power7 + lag_power8 + lag_power9 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
      reconcile(
        bu_mod1 = bottom_up(mod1),
        td_mod1 = top_down(mod1),
        mo_mod1 = middle_out(mod1),
        ols_mod1 = min_trace(mod1, method = "ols"),
        mint_mod1 = min_trace(mod1, method = "mint_shrink")
      )
    
    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      fitted_h4 <- fit_h4 %>%
        fitted()
      residuals_h4 <- fit_h4 %>%
        residuals()
      saveRDS(fitted_h4, file = "fc10_h4_lr_fitted.rds")
      saveRDS(residuals_h4, file = "fc10_h4_lr_residuals.rds")
    }
    
    fc_lr <- fc_lr %>%
      bind_rows(fit_h4 %>%
                  forecast(new_data = min10 %>%
                             group_by(Group, Subgroup) %>%
                             slice(n = i+4)))
    
    rm(fit_h4)
    gc()
    
    
    fit_h5 <- min10 %>%
      slice_head(n = i) %>%
      model(mod1 = TSLM(Power ~ WMA2_l4 + WMA3_l4 + WMA4_l4 + WMA5_l4 + WMA6_l4 + WMSD2_l4 + WMSD3_l4 + WMSD4_l4 + WMSD5_l4 + WMSD6_l4 + PMA2_l4 + PMA3_l4 + PMA4_l4 + PMA5_l4 + PMA6_l4 + PMSD2_l4 + PMSD3_l4 + PMSD4_l4 + PMSD5_l4 + PMSD6_l4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_wind9 + lag_wind10 + lag_power5 + lag_power6 + lag_power7 + lag_power8 + lag_power9 + lag_power10 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
      reconcile(
        bu_mod1 = bottom_up(mod1),
        td_mod1 = top_down(mod1),
        mo_mod1 = middle_out(mod1),
        ols_mod1 = min_trace(mod1, method = "ols"),
        mint_mod1 = min_trace(mod1, method = "mint_shrink")
      )
    
    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      fitted_h5 <- fit_h5 %>%
        fitted()
      residuals_h5 <- fit_h5 %>%
        residuals()
      saveRDS(fitted_h5, file = "fc10_h5_lr_fitted.rds")
      saveRDS(residuals_h5, file = "fc10_h5_lr_residuals.rds")
    }
    
    fc_lr <- fc_lr %>%
      bind_rows(fit_h5 %>%
                  forecast(new_data = min10 %>%
                             group_by(Group, Subgroup) %>%
                             slice(n = i+5)))
    
    rm(fit_h5)
    gc()
    
    
    fit_h6 <- min10 %>%
      slice_head(n = i) %>%
      model(mod1 = TSLM(Power ~ WMA2_l5 + WMA3_l5 + WMA4_l5 + WMA5_l5 + WMA6_l5 + WMSD2_l5 + WMSD3_l5 + WMSD4_l5 + WMSD5_l5 + WMSD6_l5 + PMA2_l5 + PMA3_l5 + PMA4_l5 + PMA5_l5 + PMA6_l5 + PMSD2_l5 + PMSD3_l5 + PMSD4_l5 + PMSD5_l5 + PMSD6_l5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_wind9 + lag_wind10 + lag_wind11 + lag_power6 + lag_power7 + lag_power8 + lag_power9 + lag_power10 + lag_power11 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
      reconcile(
        bu_mod1 = bottom_up(mod1),
        td_mod1 = top_down(mod1),
        mo_mod1 = middle_out(mod1),
        ols_mod1 = min_trace(mod1, method = "ols"),
        mint_mod1 = min_trace(mod1, method = "mint_shrink")
      )

    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      fitted_h6 <- fit_h6 %>%
        fitted()
      residuals_h6 <- fit_h6 %>%
        residuals()
      saveRDS(fitted_h6, file = "fc10_h6_lr_fitted.rds")
      saveRDS(residuals_h6, file = "fc10_h6_lr_residuals.rds")
    }
    
    fc_lr <- fc_lr %>%
      bind_rows(fit_h6 %>%
                  forecast(new_data = min10 %>%
                             group_by(Group, Subgroup) %>%
                             slice(n = i+6)))
    
    rm(fit_h6)
    gc()
    
    return(fc_lr)
  }
stopCluster(cl)

saveRDS(fc10_lr, file = "fc10_lr.rds")

#### Time series linear regression with feature engineering - 20 minutely ####
# again we use TSCV to evaluate the model
min20 <- min20_power
# %>%  filter_index("2021-06-30")

# compute number of entries
N = nrow(min20_power)/n_keys(min20_power)

# initialize accuracy tibble
fc20_lr <- NULL;

# compute features on data set
min20 <- min20 %>% mutate(
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
  `lag_wind7` = lag(Wind_Speed, 7),
  `lag_wind8` = lag(Wind_Speed, 8),
  `lag_power1` = lag(Power, 1),
  `lag_power2` = lag(Power, 2),
  `lag_power3` = lag(Power, 3),
  `lag_power4` = lag(Power, 4),
  `lag_power5` = lag(Power, 5),
  `lag_power6` = lag(Power, 6),
  `lag_power7` = lag(Power, 7),
  `lag_power8` = lag(Power, 8),
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

min20 <- min20 %>% mutate(
  `WMA2_l1` = lag(WMA2,1),
  `WMA3_l1` = lag(WMA3,1),
  `WMA4_l1` = lag(WMA4,1),
  `WMA5_l1` = lag(WMA5,1),
  `WMA6_l1` = lag(WMA6,1),
  `WMSD2_l1` = lag(WMSD2,1),
  `WMSD3_l1` = lag(WMSD3,1),
  `WMSD4_l1` = lag(WMSD4,1),
  `WMSD5_l1` = lag(WMSD5,1),
  `WMSD6_l1` = lag(WMSD6,1),
  `PMA2_l1` = lag(PMA2, 1),
  `PMA3_l1` = lag(PMA3, 1),
  `PMA4_l1` = lag(PMA4, 1),
  `PMA5_l1` = lag(PMA5, 1),
  `PMA6_l1` = lag(PMA6, 1),
  `PMSD2_l1` = lag(PMSD2, 1),
  `PMSD3_l1` = lag(PMSD3, 1),
  `PMSD4_l1` = lag(PMSD4, 1),
  `PMSD5_l1` = lag(PMSD5, 1),
  `PMSD6_l1` = lag(PMSD6, 1),
  `WMA2_l2` = lag(WMA2,2),
  `WMA3_l2` = lag(WMA3,2),
  `WMA4_l2` = lag(WMA4,2),
  `WMA5_l2` = lag(WMA5,2),
  `WMA6_l2` = lag(WMA6,2),
  `WMSD2_l2` = lag(WMSD2,2),
  `WMSD3_l2` = lag(WMSD3,2),
  `WMSD4_l2` = lag(WMSD4,2),
  `WMSD5_l2` = lag(WMSD5,2),
  `WMSD6_l2` = lag(WMSD6,2),
  `PMA2_l2` = lag(PMA2, 2),
  `PMA3_l2` = lag(PMA3, 2),
  `PMA4_l2` = lag(PMA4, 2),
  `PMA5_l2` = lag(PMA5, 2),
  `PMA6_l2` = lag(PMA6, 2),
  `PMSD2_l2` = lag(PMSD2, 2),
  `PMSD3_l2` = lag(PMSD3, 2),
  `PMSD4_l2` = lag(PMSD4, 2),
  `PMSD5_l2` = lag(PMSD5, 2),
  `PMSD6_l2` = lag(PMSD6, 2)
)

# save .rds file
saveRDS(min20, file = "min20_features.rds")

# load .rds file
min20 <- readRDS(file = "min20_features.rds")

# compute number of entries
N = nrow(min20)/n_keys(min20)

# set up parallelisation
registerDoParallel(cl <- makeCluster(numThreads))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc20_lr <- foreach(i = seq(ceiling(N*TrainingProportion),N-3,3), .combine = bind_rows, .packages = c("fpp3")) %dopar%
  {
    gc()
    
    # initialize accuracy tibble
    fc_lr <- NULL;
    
    # compute fit
    fit_h1 <- min20 %>%
      slice_head(n = i) %>%
      model(mod1 = TSLM(Power ~ WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_power1 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
      reconcile(
        bu_mod1 = bottom_up(mod1),
        td_mod1 = top_down(mod1),
        mo_mod1 = middle_out(mod1),
        ols_mod1 = min_trace(mod1, method = "ols"),
        mint_mod1 = min_trace(mod1, method = "mint_shrink")
      )
    
    fit_h2 <- min20 %>%
      slice_head(n = i) %>%
      model(mod1 = TSLM(Power ~ WMA2_l1 + WMA3_l1 + WMA4_l1 + WMA5_l1 + WMA6_l1 + WMSD2_l1 + WMSD3_l1 + WMSD4_l1 + WMSD5_l1 + WMSD6_l1 + PMA2_l1 + PMA3_l1 + PMA4_l1 + PMA5_l1 + PMA6_l1 + PMSD2_l1 + PMSD3_l1 + PMSD4_l1 + PMSD5_l1 + PMSD6_l1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + lag_power7 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
      reconcile(
        bu_mod1 = bottom_up(mod1),
        td_mod1 = top_down(mod1),
        mo_mod1 = middle_out(mod1),
        ols_mod1 = min_trace(mod1, method = "ols"),
        mint_mod1 = min_trace(mod1, method = "mint_shrink")
      )
    
    fit_h3 <- min20 %>%
      slice_head(n = i) %>%
      model(mod1 = TSLM(Power ~ WMA2_l2 + WMA3_l2 + WMA4_l2 + WMA5_l2 + WMA6_l2 + WMSD2_l2 + WMSD3_l2 + WMSD4_l2 + WMSD5_l2 + WMSD6_l2 + PMA2_l2 + PMA3_l2 + PMA4_l2 + PMA5_l2 + PMA6_l2 + PMSD2_l2 + PMSD3_l2 + PMSD4_l2 + PMSD5_l2 + PMSD6_l2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + lag_power7 + lag_power8 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
      reconcile(
        bu_mod1 = bottom_up(mod1),
        td_mod1 = top_down(mod1),
        mo_mod1 = middle_out(mod1),
        ols_mod1 = min_trace(mod1, method = "ols"),
        mint_mod1 = min_trace(mod1, method = "mint_shrink")
      )
    
    # forecast with new data
    fc_lr <- fc_lr %>%
      bind_rows(fit_h1 %>%
                  forecast(new_data = min20 %>%
                             group_by(Group, Subgroup) %>%
                             slice(n = i+1)))
    
    fc_lr <- fc_lr %>%
      bind_rows(fit_h2 %>%
                  forecast(new_data = min20 %>%
                             group_by(Group, Subgroup) %>%
                             slice(n = i+2)))
    
    fc_lr <- fc_lr %>%
      bind_rows(fit_h3 %>%
                  forecast(new_data = min20 %>%
                             group_by(Group, Subgroup) %>%
                             slice(n = i+3)))
    
    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      fitted_h1 <- fit_h1 %>%
        fitted()
      residuals_h1 <- fit_h1 %>%
        residuals()
      fitted_h2 <- fit_h2 %>%
        fitted()
      residuals_h2 <- fit_h2 %>%
        residuals()
      fitted_h3 <- fit_h3 %>%
        fitted()
      residuals_h3 <- fit_h3 %>%
        residuals()
      saveRDS(fitted_h1, file = "fc20_h1_lr_fitted.rds")
      saveRDS(residuals_h1, file = "fc20_h1_lr_residuals.rds")
      saveRDS(fitted_h2, file = "fc20_h2_lr_fitted.rds")
      saveRDS(residuals_h2, file = "fc20_h2_lr_residuals.rds")
      saveRDS(fitted_h3, file = "fc20_h3_lr_fitted.rds")
      saveRDS(residuals_h3, file = "fc20_h3_lr_residuals.rds")
    }
    
    return(fc_lr)
  }
stopCluster(cl)

saveRDS(fc20_lr, file = "fc20_lr.rds")

#### Time series linear regression with feature engineering - 30 minutely ####
# again we use TSCV to evaluate the model
min30 <- min30_power
# %>%  filter_index("2021-06-30")

# compute number of entries
N = nrow(min30_power)/n_keys(min30_power)

# initialize accuracy tibble
fc30_lr <- NULL;

# compute features on data set
min30 <- min30 %>% mutate(
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
  `lag_wind7` = lag(Wind_Speed, 7),
  `lag_power1` = lag(Power, 1),
  `lag_power2` = lag(Power, 2),
  `lag_power3` = lag(Power, 3),
  `lag_power4` = lag(Power, 4),
  `lag_power5` = lag(Power, 5),
  `lag_power6` = lag(Power, 6),
  `lag_power7` = lag(Power, 7),
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

min30 <- min30 %>% mutate(
  `WMA2_l1` = lag(WMA2,1),
  `WMA3_l1` = lag(WMA3,1),
  `WMA4_l1` = lag(WMA4,1),
  `WMA5_l1` = lag(WMA5,1),
  `WMA6_l1` = lag(WMA6,1),
  `WMSD2_l1` = lag(WMSD2,1),
  `WMSD3_l1` = lag(WMSD3,1),
  `WMSD4_l1` = lag(WMSD4,1),
  `WMSD5_l1` = lag(WMSD5,1),
  `WMSD6_l1` = lag(WMSD6,1),
  `PMA2_l1` = lag(PMA2, 1),
  `PMA3_l1` = lag(PMA3, 1),
  `PMA4_l1` = lag(PMA4, 1),
  `PMA5_l1` = lag(PMA5, 1),
  `PMA6_l1` = lag(PMA6, 1),
  `PMSD2_l1` = lag(PMSD2, 1),
  `PMSD3_l1` = lag(PMSD3, 1),
  `PMSD4_l1` = lag(PMSD4, 1),
  `PMSD5_l1` = lag(PMSD5, 1),
  `PMSD6_l1` = lag(PMSD6, 1)
)

# save .rds file
saveRDS(min30, file = "min30_features.rds")

# load .rds file
min30 <- readRDS(file = "min30_features.rds")

# compute number of entries
N = nrow(min30)/n_keys(min30)

# set up parallelisation
registerDoParallel(cl <- makeCluster(numThreads))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc30_lr <- foreach(i = seq(ceiling(N*TrainingProportion),N-1,2), .combine = bind_rows, .packages = c("fpp3")) %dopar%
  {
    gc()
    
    # initialize accuracy tibble
    fc_lr <- NULL;
    
    # compute fit
    fit_h1 <- min30 %>%
      slice_head(n = i) %>%
      model(mod1 = TSLM(Power ~ WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_power1 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
      reconcile(
        bu_mod1 = bottom_up(mod1),
        td_mod1 = top_down(mod1),
        mo_mod1 = middle_out(mod1),
        ols_mod1 = min_trace(mod1, method = "ols"),
        mint_mod1 = min_trace(mod1, method = "mint_shrink")
      )
    
    fit_h2 <- min30 %>%
      slice_head(n = i) %>%
      model(mod1 = TSLM(Power ~ WMA2_l1 + WMA3_l1 + WMA4_l1 + WMA5_l1 + WMA6_l1 + WMSD2_l1 + WMSD3_l1 + WMSD4_l1 + WMSD5_l1 + WMSD6_l1 + PMA2_l1 + PMA3_l1 + PMA4_l1 + PMA5_l1 + PMA6_l1 + PMSD2_l1 + PMSD3_l1 + PMSD4_l1 + PMSD5_l1 + PMSD6_l1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + lag_power7 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
      reconcile(
        bu_mod1 = bottom_up(mod1),
        td_mod1 = top_down(mod1),
        mo_mod1 = middle_out(mod1),
        ols_mod1 = min_trace(mod1, method = "ols"),
        mint_mod1 = min_trace(mod1, method = "mint_shrink")
      )
    
    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      fitted_h1 <- fit_h1 %>%
        fitted()
      residuals_h1 <- fit_h1 %>%
        residuals()
      fitted_h2 <- fit_h2 %>%
        fitted()
      residuals_h2 <- fit_h2 %>%
        residuals()
      saveRDS(fitted_h1, file = "fc30_h1_lr_fitted.rds")
      saveRDS(residuals_h1, file = "fc30_h1_lr_residuals.rds")
      saveRDS(fitted_h2, file = "fc30_h2_lr_fitted.rds")
      saveRDS(residuals_h2, file = "fc30_h2_lr_residuals.rds")
    }
    
    # forecast with new data
    fc_lr <- fc_lr %>%
      bind_rows(fit_h1 %>%
                  forecast(new_data = min30 %>%
                             group_by(Group, Subgroup) %>%
                             dplyr::slice(i+1)))
    fc_lr <- fc_lr %>%
      bind_rows(fit_h2 %>%
                  forecast(new_data = min30 %>%
                             group_by(Group, Subgroup) %>%
                             dplyr::slice(i+2)))
    
    return(fc_lr)
  }
stopCluster(cl)

saveRDS(fc30_lr, file = "fc30_lr.rds")


#### Time series linear regression with feature engineering - 1 hourly ####
# again we use TSCV to evaluate the model
hr1 <- hr1_power

# initialize accuracy tibble
fc1_lr <- NULL;

# compute features on data set
hr1 <- hr1 %>% mutate(
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
  `lag_wind7` = lag(Wind_Speed, 7),
  `lag_power1` = lag(Power, 1),
  `lag_power2` = lag(Power, 2),
  `lag_power3` = lag(Power, 3),
  `lag_power4` = lag(Power, 4),
  `lag_power5` = lag(Power, 5),
  `lag_power6` = lag(Power, 6),
  `lag_power7` = lag(Power, 7),
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

# save .rds file
saveRDS(hr1, file = "hr1_features.rds")

# load .rds file
hr1 <- readRDS(file = "hr1_features.rds")

# compute number of entries
N = nrow(hr1)/n_keys(hr1)

# set up parallelisation
registerDoParallel(cl <- makeCluster(numThreads))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc1_lr <- foreach(i = seq(ceiling(N*TrainingProportion),N-1,1), .combine = bind_rows, .packages = c("fpp3")) %dopar%
  {
    
    # initialize accuracy tibble
    fc_lr <- NULL;
    
    # compute fit
    fit_total <- hr1 %>%
      dplyr::slice_head(n = i) %>%
      model(mod1 = TSLM(Power ~ WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_power1 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
      reconcile(
        bu_mod1 = bottom_up(mod1),
        td_mod1 = top_down(mod1),
        mo_mod1 = middle_out(mod1),
        ols_mod1 = fabletools::min_trace(mod1, method = "ols"),
        mint_mod1 = fabletools::min_trace(mod1, method = "mint_shrink")
      )
    
    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      fitted <- fit_total %>%
        fitted()
      residuals <- fit_total %>%
        residuals()
      saveRDS(fitted, file = "fc1_lr_fitted.rds")
      saveRDS(residuals, file = "fc1_lr_residuals.rds")
    }
    
    # forecast with new data
    fc_lr <- fc_lr %>%
      bind_rows(fit_total %>%
                  forecast(new_data = hr1 %>%
                             group_by(Group, Subgroup) %>%
                             dplyr::slice(i+1)))
    
    return(fc_lr)
  }
stopCluster(cl)

saveRDS(fc1_lr, file = "fc1_lr.rds")


#### Define auxiliary functions for LightGBM regression ####
# function to convert x_data (i.e., all regressors) tsibble into matrix for lightgbm
to_x <- function(data){
  return(data %>%
           ungroup() %>%
           as_tibble() %>%
           select(-c(Group, Subgroup, Time, Power, Wind_Speed)) %>%
           as.matrix())
}

# function to convert y data (i.e., power) tsibble into matrix for lightgbm
to_y <- function(data){
  return(data %>%
           ungroup() %>%
           as_tibble() %>%
           select(Power) %>%
           as.matrix())
}

# function to perform gridsearch to optimize hyperparameters
tuning_para <- function(train_data,test_data, validation_x, validation_y){
  dtrain_b3 <- train_data
  dtest_b3 <- test_data
  valids_b3 = list(test=dtest_b3)
  
  model_b3 <- list()
  perf_b3 <- numeric(nrow(grid_search))
  
  for (i in 1:nrow(grid_search)) {
    model_b3[[i]] <- lightgbm::lgb.train(objective = "regression",
                                         metric = "l2",
                                         max_depth = grid_search[i, "max_depth"],
                                         num_leaves= grid_search[i,"num_leaves"],
                                         learning_rate = grid_search[i,"learning_rate"],
                                         min_data_in_leaf = grid_search[i,"min_data_in_leaf"],
                                         data = dtrain_b3,
                                         valids= valids_b3,
                                         verbose = -1)
    
    fit <- predict(model_b3[[i]], validation_x)
    e <- (validation_y - fit)^2
    
    perf_b3[i] = mean(e)
  }
  
  
  
  # grid_search
  cat("Model ", which.min(perf_b3), " is lowest loss: ", min(perf_b3), sep = "","\n")
  return(perf_b3)
}

#### Optimize hyperparameters 10-minutely ####
# load .rds file
min10 <- readRDS(file = "min10_features.rds")
N = nrow(min10)/n_keys(min10)

errors <- data.frame()

training_percentages = c(0.8,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88)

for (i in seq_along(training_percentages)){
  
  gc()
  
  ind = training_percentages[i]
  
  train_x <- to_x(min10 %>%
                    slice_head(n = ceiling(N*ind)))
  train_y <- to_y(min10 %>%
                    slice_head(n = ceiling(N*ind)))
  test_x <- to_x(min10 %>%
                   dplyr::slice(ceiling(N*ind)+1:floor(N*(ind + 0.1))))
  test_y <- to_y(min10 %>%
                   dplyr::slice(ceiling(N*ind)+1:floor(N*(ind + 0.1))))
  
  validation_x <- to_x(min10 %>%
                         dplyr::slice(floor(N*(ind + 0.1))+1:floor(N*(ind + 0.02))))
  validation_y <- to_y(min10 %>%
                         dplyr::slice(floor(N*(ind + 0.1))+1:floor(N*(ind + 0.02))))
  
  dtrain = lgb.Dataset(train_x, label = train_y)
  dtest = lgb.Dataset.create.valid(dtrain, test_x, label = test_y)
  
  grid_search <- expand.grid(
    num_leaves = c(50,60,70),
    max_depth= c(6,7,8),
    learning_rate= c(0.1),
    min_data_in_leaf = c(110,120,130),
    linear_lambda = c(1,2))
  
  errors <- errors %>% rbind(tuning_para(dtrain, dtest, validation_x, validation_y))
}

optimal <- grid_search[which.min(colMeans(errors)), ]
print(optimal)
saveRDS(optimal, file = "fc10_gb_optimal.rds")


#### Optimize hyperparameters 20-minutely ####
# load .rds file
min20 <- readRDS(file = "min20_features.rds")
N = nrow(min20)/n_keys(min20)

errors <- data.frame()

training_percentages = c(0.8,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88)

for (i in seq_along(training_percentages)){
  ind = training_percentages[i]
  
  train_x <- to_x(min20 %>%
                    slice_head(n = ceiling(N*ind)))
  train_y <- to_y(min20 %>%
                    slice_head(n = ceiling(N*ind)))
  test_x <- to_x(min20 %>%
                   dplyr::slice(ceiling(N*ind)+1:floor(N*(ind + 0.1))))
  test_y <- to_y(min20 %>%
                   dplyr::slice(ceiling(N*ind)+1:floor(N*(ind + 0.1))))
  
  validation_x <- to_x(min20 %>%
                         dplyr::slice(floor(N*(ind + 0.1))+1:floor(N*(ind + 0.02))))
  validation_y <- to_y(min20 %>%
                         dplyr::slice(floor(N*(ind + 0.1))+1:floor(N*(ind + 0.02))))
  
  dtrain = lgb.Dataset(train_x, label = train_y)
  dtest = lgb.Dataset.create.valid(dtrain, test_x, label = test_y)
  
  grid_search <- expand.grid(
    num_leaves = c(50,60,70),
    max_depth= c(5,6,7),
    learning_rate= c(0.1),
    min_data_in_leaf = c(170,180,190),
    linear_lambda = c(1,2,3))
  
  errors <- errors %>% rbind(tuning_para(dtrain, dtest, validation_x, validation_y))
}

optimal <- grid_search[which.min(colMeans(errors)), ]
print(optimal)

#### Optimize hyperparameters 30-minutely ####
# load .rds file
min30 <- readRDS(file = "min30_features.rds")
N = nrow(min30)/n_keys(min30)

errors <- data.frame()

training_percentages = c(0.8,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88)

for (i in seq_along(training_percentages)){
  ind = training_percentages[i]
  
  train_x <- to_x(min30 %>%
                    slice_head(n = ceiling(N*ind)))
  train_y <- to_y(min30 %>%
                    slice_head(n = ceiling(N*ind)))
  test_x <- to_x(min30 %>%
                   dplyr::slice(ceiling(N*ind)+1:floor(N*(ind + 0.1))))
  test_y <- to_y(min30 %>%
                   dplyr::slice(ceiling(N*ind)+1:floor(N*(ind + 0.1))))
  
  validation_x <- to_x(min30 %>%
                         dplyr::slice(floor(N*(ind + 0.1))+1:floor(N*(ind + 0.02))))
  validation_y <- to_y(min30 %>%
                         dplyr::slice(floor(N*(ind + 0.1))+1:floor(N*(ind + 0.02))))
  
  dtrain = lgb.Dataset(train_x, label = train_y)
  dtest = lgb.Dataset.create.valid(dtrain, test_x, label = test_y)
  
  grid_search <- expand.grid(
    num_leaves = c(50,60,70),
    max_depth= c(5,6,7),
    learning_rate= c(0.1),
    min_data_in_leaf = c(150,160,170),
    linear_lambda = c(1,2))
  
  errors <- errors %>% rbind(tuning_para(dtrain, dtest, validation_x, validation_y))
}

optimal <- grid_search[which.min(colMeans(errors)), ]
print(optimal)

#### Optimize hyperparameters 1-hourly ####
# load .rds file
hr1 <- readRDS(file = "hr1_features.rds")
N = nrow(hr1)/n_keys(hr1)

errors <- data.frame()

training_percentages = c(0.8,0.81,0.82,0.83,0.84,0.85,0.86,0.87,0.88)

for (i in seq_along(training_percentages)){
  gc()
  
  ind = training_percentages[i]
  
  train_x <- to_x(hr1 %>%
                    slice_head(n = ceiling(N*ind)))
  train_y <- to_y(hr1 %>%
                    slice_head(n = ceiling(N*ind)))
  test_x <- to_x(hr1 %>%
                   dplyr::slice(ceiling(N*ind)+1:floor(N*(ind + 0.1))))
  test_y <- to_y(hr1 %>%
                   dplyr::slice(ceiling(N*ind)+1:floor(N*(ind + 0.1))))
  
  validation_x <- to_x(hr1 %>%
                         dplyr::slice(floor(N*(ind + 0.1))+1:floor(N*(ind + 0.02))))
  validation_y <- to_y(hr1 %>%
                         dplyr::slice(floor(N*(ind + 0.1))+1:floor(N*(ind + 0.02))))
  
  dtrain = lgb.Dataset(train_x, label = train_y)
  dtest = lgb.Dataset.create.valid(dtrain, test_x, label = test_y)
  
  grid_search <- expand.grid(
    num_leaves = c(40,50,60),
    max_depth= c(6,7,8),
    learning_rate= c(0.1),
    min_data_in_leaf = c(170,180,190),
    linear_lambda = c(1,2))
  
  errors <- errors %>% rbind(tuning_para(dtrain, dtest, validation_x, validation_y))
}

optimal <- grid_search[which.min(colMeans(errors)), ]
print(optimal)

#### Use gradient boosting - 1 hourly ####
# this uses features calculated in the linear regression section

hr1_test <- hr1
# %>% filter_index("2021-06-20" ~ "2021-06-30")

gc()

N = nrow(hr1_test)/n_keys(hr1_test)

fc1_gb <- NULL

# define parameters for 1 hourly
params = list(
  objective = "regression"
  , metric = "l2"
  , learning_rate = 0.1
  , num_leaves = 50
  , max_depth = 7
  , min_data_in_leaf = 180
  , num_threads = 6
  , linear_lambda = 1
)

# do our TSCV manually, starting from 90% of the dataset up to the second last element
for (i in seq(ceiling(N*TrainingProportion),N-1,1)) {
  
  gc()
  
  # compute fit
  fit_total <- hr1_test %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Power ~ hyperparameters(params) + WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_power1 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  # forecast with new data
  fc1_gb <- fc1_gb %>%
    bind_rows(fit_total %>%
                forecast(new_data = hr1_test %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(i+1)))
  
  # extract fitted and residuals
  if (i == ceiling(N*TrainingProportion)){
    fitted <- fit_total %>%
      fitted()
    residuals <- fit_total %>%
      residuals()
    saveRDS(fitted, file = "fc1_gb_fitted.rds")
    saveRDS(residuals, file = "fc1_gb_residuals.rds")
  }
}

saveRDS(fc1_gb, file = "fc1_gb.rds")


#### Use gradient boosting - 30 minutely ####
# this uses features calculated in the linear regression section

min30_test <- min30
# %>% filter_index("2021-06-20" ~ "2021-06-30")

gc()

N = nrow(min30_test)/n_keys(min30_test)

fc30_gb <- NULL

# define parameters for 30-minutely
params = list(
  objective = "regression"
  , metric = "l2"
  , learning_rate = 0.1
  , num_leaves = 70
  , max_depth = 6
  , min_data_in_leaf = 160
  , num_threads = 6
  , linear_lambda = 1
)

# do our TSCV manually, starting from 90% of the dataset up to the second last element
for (i in seq(ceiling(N*TrainingProportion),N-2,2)) {
  
  gc()
  
  # compute fit
  fit_h1 <- min30_test %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Power ~ hyperparameters(params) + WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_power1 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  # compute fit
  fit_h2 <- min30_test %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Power ~ hyperparameters(params) + WMA2_l1 + WMA3_l1 + WMA4_l1 + WMA5_l1 + WMA6_l1 + WMSD2_l1 + WMSD3_l1 + WMSD4_l1 + WMSD5_l1 + WMSD6_l1 + PMA2_l1 + PMA3_l1 + PMA4_l1 + PMA5_l1 + PMA6_l1 + PMSD2_l1 + PMSD3_l1 + PMSD4_l1 + PMSD5_l1 + PMSD6_l1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + lag_power7 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  # forecast with new data
  fc30_gb <- fc30_gb %>%
    bind_rows(fit_h1 %>%
                forecast(new_data = min30_test %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(i+1)))
  fc30_gb <- fc30_gb %>%
    bind_rows(fit_h2 %>%
                forecast(new_data = min30_test %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(i+2)))
  
  # extract fitted and residuals
  if (i == ceiling(N*TrainingProportion)){
    fitted_h1 <- fit_h1 %>%
      fitted()
    residuals_h1 <- fit_h1 %>%
      residuals()
    fitted_h2 <- fit_h2 %>%
      fitted()
    residuals_h2 <- fit_h2 %>%
      residuals()
    saveRDS(fitted_h1, file = "fc30_h1_gb_fitted.rds")
    saveRDS(residuals_h1, file = "fc30_h1_gb_residuals.rds")
    saveRDS(fitted_h2, file = "fc30_h2_gb_fitted.rds")
    saveRDS(residuals_h2, file = "fc30_h2_gb_residuals.rds")
  }
  
}

saveRDS(fc30_gb, file = "fc30_gb.rds")

#### Use gradient boosting - 20 minutely ####
# this uses features calculated in the linear regression section
min20 <- readRDS(file = "min20_features.rds")
min20_test <- min20
# %>% filter_index("2021-06-20" ~ "2021-06-30")

gc()

N = nrow(min20_test)/n_keys(min20_test)

fc20_gb <- NULL

# define parameters for 30-minutely
params = list(
  objective = "regression"
  , metric = "l2"
  , learning_rate = 0.1
  , num_leaves = 60
  , max_depth = 6
  , min_data_in_leaf = 180
  , num_threads = 8
  , linear_lambda = 1
)

# do our TSCV manually, starting from 90% of the dataset up to the second last element
for (i in seq(ceiling(N*TrainingProportion),N-3,3)) {
  
  gc()
  
  # compute fit
  fit_h1 <- min20 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Power ~ hyperparameters(params) + WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_power1 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  fit_h2 <- min20 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Power ~ hyperparameters(params) + WMA2_l1 + WMA3_l1 + WMA4_l1 + WMA5_l1 + WMA6_l1 + WMSD2_l1 + WMSD3_l1 + WMSD4_l1 + WMSD5_l1 + WMSD6_l1 + PMA2_l1 + PMA3_l1 + PMA4_l1 + PMA5_l1 + PMA6_l1 + PMSD2_l1 + PMSD3_l1 + PMSD4_l1 + PMSD5_l1 + PMSD6_l1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + lag_power7 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  fit_h3 <- min20 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Power ~ hyperparameters(params) + WMA2_l2 + WMA3_l2 + WMA4_l2 + WMA5_l2 + WMA6_l2 + WMSD2_l2 + WMSD3_l2 + WMSD4_l2 + WMSD5_l2 + WMSD6_l2 + PMA2_l2 + PMA3_l2 + PMA4_l2 + PMA5_l2 + PMA6_l2 + PMSD2_l2 + PMSD3_l2 + PMSD4_l2 + PMSD5_l2 + PMSD6_l2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + lag_power7 + lag_power8 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  # forecast with new data
  fc20_gb <- fc20_gb %>%
    bind_rows(fit_h1 %>%
                forecast(new_data = min20 %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(n = i+1)))
  
  fc20_gb <- fc20_gb %>%
    bind_rows(fit_h2 %>%
                forecast(new_data = min20 %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(n = i+2)))
  
  fc20_gb <- fc20_gb %>%
    bind_rows(fit_h3 %>%
                forecast(new_data = min20 %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(n = i+3)))
  
  # extract fitted and residuals
  if (i == ceiling(N*TrainingProportion)){
    fitted_h1 <- fit_h1 %>%
      fitted()
    residuals_h1 <- fit_h1 %>%
      residuals()
    fitted_h2 <- fit_h2 %>%
      fitted()
    residuals_h2 <- fit_h2 %>%
      residuals()
    fitted_h3 <- fit_h3 %>%
      fitted()
    residuals_h3 <- fit_h3 %>%
      residuals()
    saveRDS(fitted_h1, file = "fc20_h1_gb_fitted.rds")
    saveRDS(residuals_h1, file = "fc20_h1_gb_residuals.rds")
    saveRDS(fitted_h2, file = "fc20_h2_gb_fitted.rds")
    saveRDS(residuals_h2, file = "fc20_h2_gb_residuals.rds")
    saveRDS(fitted_h3, file = "fc20_h3_gb_fitted.rds")
    saveRDS(residuals_h3, file = "fc20_h3_gb_residuals.rds")
    rm(fitted_h1)
    rm(fitted_h2)
    rm(fitted_h3)
    rm(residuals_h1)
    rm(residuals_h2)
    rm(residuals_h3)
  }
}

saveRDS(fc20_gb, file = "fc20_gb.rds")



#### Use gradient boosting - 10 minutely ####
# this uses features calculated in the linear regression section

min10_test <- min10
# %>% filter_index("2021-06-20" ~ "2021-06-30")

gc()

N = nrow(min10_test)/n_keys(min10_test)

fc10_gb <- NULL

# define parameters for 10-minutely
params = list(
  objective = "regression"
  , metric = "l2"
  , learning_rate = 0.1
  , num_leaves = 60
  , max_depth = 7
  , min_data_in_leaf = 120
  , num_threads = 8
  , linear_lambda = 1
)

# do our TSCV manually, starting from 90% of the dataset up to the second last element
for (i in seq(ceiling(N*TrainingProportion),N-6,6)) {
  
  gc()
  
  # compute fit
  fit_h1 <- min10 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Power ~ hyperparameters(params) + WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_power1 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  # forecast with new data
  fc10_gb <- fc10_gb %>%
    dplyr::bind_rows(fit_h1 %>%
                       forecast(new_data = min10 %>%
                                  group_by(Group, Subgroup) %>%
                                  dplyr::slice(n = i+1)))
  
  # extract fitted and residuals
  if (i == ceiling(N*TrainingProportion)){
    fitted_h1 <- fit_h1 %>%
      fitted()
    residuals_h1 <- fit_h1 %>%
      residuals()
    saveRDS(fitted_h1, file = "fc10_h1_gb_fitted.rds")
    saveRDS(residuals_h1, file = "fc10_h1_gb_residuals.rds")
    rm(fitted_h1)
    rm(residuals_h1)
  }
  
  rm(fit_h1)
  gc()
  
  fit_h2 <- min10 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Power ~ hyperparameters(params) + WMA2_l1 + WMA3_l1 + WMA4_l1 + WMA5_l1 + WMA6_l1 + WMSD2_l1 + WMSD3_l1 + WMSD4_l1 + WMSD5_l1 + WMSD6_l1 + PMA2_l1 + PMA3_l1 + PMA4_l1 + PMA5_l1 + PMA6_l1 + PMSD2_l1 + PMSD3_l1 + PMSD4_l1 + PMSD5_l1 + PMSD6_l1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_power2 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + lag_power7 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  fc10_gb <- fc10_gb %>%
    bind_rows(fit_h2 %>%
                forecast(new_data = min10 %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(n = i+2)))
  
  # extract fitted and residuals
  if (i == ceiling(N*TrainingProportion)){
    fitted_h2 <- fit_h2 %>%
      fitted()
    residuals_h2 <- fit_h2 %>%
      residuals()
    saveRDS(fitted_h2, file = "fc10_h2_gb_fitted.rds")
    saveRDS(residuals_h2, file = "fc10_h2_gb_residuals.rds")
    rm(fitted_h2)
    rm(residuals_h2)
  }
  
  rm(fit_h2)
  gc()
  
  fit_h3 <- min10 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Power ~ hyperparameters(params) + WMA2_l2 + WMA3_l2 + WMA4_l2 + WMA5_l2 + WMA6_l2 + WMSD2_l2 + WMSD3_l2 + WMSD4_l2 + WMSD5_l2 + WMSD6_l2 + PMA2_l2 + PMA3_l2 + PMA4_l2 + PMA5_l2 + PMA6_l2 + PMSD2_l2 + PMSD3_l2 + PMSD4_l2 + PMSD5_l2 + PMSD6_l2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_power3 + lag_power4 + lag_power5 + lag_power6 + lag_power7 + lag_power8 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  fc10_gb <- fc10_gb %>%
    bind_rows(fit_h3 %>%
                forecast(new_data = min10 %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(n = i+3)))
  
  # extract fitted and residuals
  if (i == ceiling(N*TrainingProportion)){
    fitted_h3 <- fit_h3 %>%
      fitted()
    residuals_h3 <- fit_h3 %>%
      residuals()
    saveRDS(fitted_h3, file = "fc10_h3_gb_fitted.rds")
    saveRDS(residuals_h3, file = "fc10_h3_gb_residuals.rds")
    rm(fitted_h3)
    rm(residuals_h3)
  }
  
  rm(fit_h3)
  gc()
  
  
  fit_h4 <- min10 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Power ~ hyperparameters(params) + WMA2_l3 + WMA3_l3 + WMA4_l3 + WMA5_l3 + WMA6_l3 + WMSD2_l3 + WMSD3_l3 + WMSD4_l3 + WMSD5_l3 + WMSD6_l3 + PMA2_l3 + PMA3_l3 + PMA4_l3 + PMA5_l3 + PMA6_l3 + PMSD2_l3 + PMSD3_l3 + PMSD4_l3 + PMSD5_l3 + PMSD6_l3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_wind9 + lag_power4 + lag_power5 + lag_power6 + lag_power7 + lag_power8 + lag_power9 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  fc10_gb <- fc10_gb %>%
    bind_rows(fit_h4 %>%
                forecast(new_data = min10 %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(n = i+4)))
  
  # extract fitted and residuals
  if (i == ceiling(N*TrainingProportion)){
    fitted_h4 <- fit_h4 %>%
      fitted()
    residuals_h4 <- fit_h4 %>%
      residuals()
    saveRDS(fitted_h4, file = "fc10_h4_gb_fitted.rds")
    saveRDS(residuals_h4, file = "fc10_h4_gb_residuals.rds")
    rm(fitted_h4)
    rm(residuals_h4)
  }
  
  rm(fit_h4)
  gc()
  
  fit_h5 <- min10 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Power ~ hyperparameters(params) + WMA2_l4 + WMA3_l4 + WMA4_l4 + WMA5_l4 + WMA6_l4 + WMSD2_l4 + WMSD3_l4 + WMSD4_l4 + WMSD5_l4 + WMSD6_l4 + PMA2_l4 + PMA3_l4 + PMA4_l4 + PMA5_l4 + PMA6_l4 + PMSD2_l4 + PMSD3_l4 + PMSD4_l4 + PMSD5_l4 + PMSD6_l4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_wind9 + lag_wind10 + lag_power5 + lag_power6 + lag_power7 + lag_power8 + lag_power9 + lag_power10 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  fc10_gb <- fc10_gb %>%
    bind_rows(fit_h5 %>%
                forecast(new_data = min10 %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(n = i+5)))
  
  # extract fitted and residuals
  if (i == ceiling(N*TrainingProportion)){
    fitted_h5 <- fit_h5 %>%
      fitted()
    residuals_h5 <- fit_h5 %>%
      residuals()
    saveRDS(fitted_h5, file = "fc10_h5_gb_fitted.rds")
    saveRDS(residuals_h5, file = "fc10_h5_gb_residuals.rds")
    rm(fitted_h5)
    rm(residuals_h5)
  }
  
  rm(fit_h5)
  gc()
  
  fit_h6 <- min10 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Power ~ hyperparameters(params) + WMA2_l5 + WMA3_l5 + WMA4_l5 + WMA5_l5 + WMA6_l5 + WMSD2_l5 + WMSD3_l5 + WMSD4_l5 + WMSD5_l5 + WMSD6_l5 + PMA2_l5 + PMA3_l5 + PMA4_l5 + PMA5_l5 + PMA6_l5 + PMSD2_l5 + PMSD3_l5 + PMSD4_l5 + PMSD5_l5 + PMSD6_l5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_wind9 + lag_wind10 + lag_wind11 + lag_power6 + lag_power7 + lag_power8 + lag_power9 + lag_power10 + lag_power11 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  fc10_gb <- fc10_gb %>%
    bind_rows(fit_h6 %>%
                forecast(new_data = min10 %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(n = i+6)))
  
  # extract fitted and residuals
  if (i == ceiling(N*TrainingProportion)){
    fitted_h6 <- fit_h6 %>%
      fitted()
    residuals_h6 <- fit_h6 %>%
      residuals()
    saveRDS(fitted_h6, file = "fc10_h6_gb_fitted.rds")
    saveRDS(residuals_h6, file = "fc10_h6_gb_residuals.rds")
    rm(fitted_h6)
    rm(residuals_h6)
  }
  
  rm(fit_h6)
  gc()

}

saveRDS(fc10_gb, file = "fc10_gb.rds")