#### Benchmark models - 10 minutely ####
# we use TSCV, forecasting on rolling origin basis

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
library(FoReco)
library(tsutils)

#### Define auxiliary functions ####
# function to convert grouped/keyed tsibble into matrix of ts() time series
# if you get Error in `[<-`(`*tmp*`, , 2, value = (data %>% filter(Group == "A", is_aggregated(Subgroup)))[[variable]]) : subscript out of bounds
# change all n_keys to n_groups or vice versa
to_matrix_for_ts <- function(data, variable){
  
  N = nrow(data)/n_keys(data)
  matrix_tmp <- matrix(NA, nrow = N, ncol = n_keys(data))
  matrix_tmp[,1] <- (data %>% filter(is_aggregated(Group), is_aggregated(Subgroup)))[[variable]]
  matrix_tmp[,2] <- (data %>% filter(Group == "A", is_aggregated(Subgroup)))[[variable]]
  matrix_tmp[,3] <- (data %>% filter(Group == "B", is_aggregated(Subgroup)))[[variable]]
  matrix_tmp[,4] <- (data %>% filter(Group == "A", Subgroup == "A1"))[[variable]]
  matrix_tmp[,5] <- (data %>% filter(Group == "A", Subgroup == "A2"))[[variable]]
  matrix_tmp[,6] <- (data %>% filter(Group == "A", Subgroup == "A4"))[[variable]]
  matrix_tmp[,7] <- (data %>% filter(Group == "A", Subgroup == "A5"))[[variable]]
  matrix_tmp[,8] <- (data %>% filter(Group == "A", Subgroup == "A6"))[[variable]]
  matrix_tmp[,9] <- (data %>% filter(Group == "A", Subgroup == "A7"))[[variable]]
  matrix_tmp[,10] <- (data %>% filter(Group == "A", Subgroup == "A8"))[[variable]]
  matrix_tmp[,11] <- (data %>% filter(Group == "A", Subgroup == "A9"))[[variable]]
  matrix_tmp[,12] <- (data %>% filter(Group == "A", Subgroup == "A10"))[[variable]]
  matrix_tmp[,13] <- (data %>% filter(Group == "A", Subgroup == "A11"))[[variable]]
  matrix_tmp[,14] <- (data %>% filter(Group == "A", Subgroup == "A12"))[[variable]]
  matrix_tmp[,15] <- (data %>% filter(Group == "A", Subgroup == "A13"))[[variable]]
  matrix_tmp[,16] <- (data %>% filter(Group == "A", Subgroup == "A14"))[[variable]]
  matrix_tmp[,17] <- (data %>% filter(Group == "A", Subgroup == "A15"))[[variable]]
  matrix_tmp[,18] <- (data %>% filter(Group == "B", Subgroup == "B1"))[[variable]]
  matrix_tmp[,19] <- (data %>% filter(Group == "B", Subgroup == "B2"))[[variable]]
  matrix_tmp[,20] <- (data %>% filter(Group == "B", Subgroup == "B3"))[[variable]]
  matrix_tmp[,21] <- (data %>% filter(Group == "B", Subgroup == "B4"))[[variable]]
  matrix_tmp[,22] <- (data %>% filter(Group == "B", Subgroup == "B5"))[[variable]]
  matrix_tmp[,23] <- (data %>% filter(Group == "B", Subgroup == "B6"))[[variable]]
  colnames(matrix_tmp) <- c("T", "A", "B", "A1", "A2", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12", "A13", "A14", "A15", "B1", "B2", "B3", "B4", "B5", "B6")
  
  return(matrix_tmp)
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set this to your working directory

# set the proportion of training set
TrainingProportion <- 0.90
# set number of threads
numThreads = 3;

min10 <- readRDS(file = "min10.rds")
min20 <- readRDS(file = "min20.rds")
min30 <- readRDS(file = "min30.rds")
hr1 <- readRDS(file = "hr1.rds")

min10_data <- min10 #%>% filter_index("2021-06-20" ~ "2021-06-30")
min20_data <- min20 #%>% filter_index("2021-06-20" ~ "2021-06-30")
min30_data <- min30 #%>% filter_index("2021-06-20" ~ "2021-06-30")
hr1_data <- hr1 #%>% filter_index("2021-06-20" ~ "2021-06-30")
gc()

# compute number of entries
N = nrow(min10_data)/n_keys(min10_data)

fc_naive <- NULL;

# do our TSCV manually, starting from 90% of the dataset up to the second last element
for (i in seq(ceiling(N*TrainingProportion),N-6,6)) {
    gc()
    
    # Obs is training + test set
    # Test is just test set
    # Tr is training set
    min10_obs <- min10_data %>%
      slice_head(n = i+6)
    min20_obs <- min20_data %>%
      slice_head(n = (i/2)+3)
    min30_obs <- min30_data %>%
      slice_head(n = (i/3)+2)
    hr1_obs <- hr1_data %>%
      slice_head(n = (i/6)+1)
    
    # take training set from elements 1 up to i
    min10_tr_benchmark <- min10_obs %>%
      slice_head(n = i)
    min20_tr_benchmark <- min20_obs %>%
      slice_head(n = i/2)
    min30_tr_benchmark <- min30_obs %>%
      slice_head(n = i/3)
    hr1_tr_benchmark <- hr1_obs %>%
      slice_head(n = i/6)
    
    #take test set
    min10_test <- min10_obs %>%
      slice_tail(n = 6)
    min20_test <- min20_obs %>%
      slice_tail(n = 3)
    min30_test <- min30_obs %>%
      slice_tail(n = 2)
    hr1_test <- hr1_obs %>%
      slice_tail(n = 1)
    
    fc_benchmark <- NULL;
    
    #initialize model (naive benchmark)
    min10_model <- min10_tr_benchmark %>%
      model(naive_model = NAIVE(Energy))
    min20_model <- min20_tr_benchmark %>%
      model(naive_model = NAIVE(Energy))
    min30_model <- min30_tr_benchmark %>%
      model(naive_model = NAIVE(Energy))
    hr1_model <- hr1_tr_benchmark %>%
      model(naive_model = NAIVE(Energy))
    
    # produce forecasts
    fc_10 <- min10_tr_benchmark %>%
      model(naive_model = NAIVE(Energy)) %>%
      forecast(h=6)
    fc_20 <- min20_tr_benchmark %>%
      model(naive_model = NAIVE(Energy)) %>%
      forecast(h=3)
    fc_30 <- min30_tr_benchmark %>%
      model(naive_model = NAIVE(Energy)) %>%
      forecast(h=2)
    fc_1 <- hr1_tr_benchmark %>%
      model(naive_model = NAIVE(Energy)) %>%
      forecast(h=1)    

    lr <- NULL
    
    lr$k1 <- to_matrix_for_ts(fc_10, ".mean")
    lr$k1 <- ts(lr$k1, frequency = 6)
    lr$k2 <- to_matrix_for_ts(fc_20, ".mean")
    lr$k2 <- ts(lr$k2, frequency = 3)
    lr$k3 <- to_matrix_for_ts(fc_30, ".mean")
    lr$k3 <- ts(lr$k3, frequency = 2)
    lr$k6 <- to_matrix_for_ts(fc_1, ".mean")
    lr$k6 <- ts(lr$k6, frequency = 1)
    
    lr <- t(do.call(rbind, rev(lr)))
    
    kset <- c(6, 3, 2, 1)
    h <- 1
    
    colnames(lr) <- paste("k", rep(kset, h * rev(kset)), "_h",
                          do.call("c", as.list(sapply(
                            rev(kset) * h,
                            function(x) seq(1:x)))),
                          sep = "")
    
    fc_naive <- cbind(fc_naive, lr)
}

saveRDS(fc_naive, file = "fc_benchmark.rds")