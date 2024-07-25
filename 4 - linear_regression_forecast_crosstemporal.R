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
library(purrr)

#### Define auxiliary functions ####
# function to convert grouped/keyed tsibble into matrix of ts() time series
# if you get Error in `[<-`(`*tmp*`, , 2, value = (data %>% filter(Group == "A", is_aggregated(Subgroup)))[[variable]]) : subscript out of bounds
# change all n_keys to n_groups or vice versa
to_matrix_for_ts <- function(data, variable){
  
  N = nrow(data)/n_groups(data)
  matrix_tmp <- matrix(NA, nrow = N, ncol = n_groups(data))
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
numThreads = 6;

min10 <- readRDS(file = "min10_wFeatures.rds")
min20 <- readRDS(file = "min20_wFeatures.rds")
min30 <- readRDS(file = "min30_wFeatures.rds")
hr1 <- readRDS(file = "hr1_wFeatures.rds")

min10_data <- min10 #%>% filter_index("2021-06-20" ~ "2021-06-30")
min20_data <- min20 #%>% filter_index("2021-06-20" ~ "2021-06-30")
min30_data <- min30 #%>% filter_index("2021-06-20" ~ "2021-06-30")
hr1_data <- hr1 #%>% filter_index("2021-06-20" ~ "2021-06-30")
rm(min10)
rm(min20)
rm(min30)
rm(hr1)
gc()

# compute number of entries
N = nrow(min10_data)/n_keys(min10_data)

fc_thf <- NULL;
fc_tcs <- NULL;
fc_cst <- NULL;
fc_ite <- NULL;
fc_oct <- NULL;
fc_ctbu <- NULL;
fc_LR <- NULL;
test_set <- NULL;

# do our TSCV manually, starting from 90% of the dataset up to the second last element
for (i in seq(ceiling(N*TrainingProportion),N-6,6)) {
    start.time <- Sys.time()
    # Obs is training + test set
    # Test is just test set
    # Tr is training set
  
    # Perform all 10-minutely forecasts firsts and then free up memory etc
    min10_obs <- min10_data %>%
      slice_head(n = i+6)
    # take training set from elements 1 up to i
    min10_tr <- min10_obs %>%
      slice_head(n = i)
    #take test set
    min10_test <- min10_obs %>%
      slice_tail(n = 6)
    
    #initialize model (LR)
    min10_h1_model <- min10_tr %>%
      model(mod1 = TSLM(Energy ~ WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_Energy1 + lag_Energy2 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22))
    min10_h2_model <- min10_tr %>%
      model(mod1 = TSLM(Energy ~ WMA2_l1 + WMA3_l1 + WMA4_l1 + WMA5_l1 + WMA6_l1 + WMSD2_l1 + WMSD3_l1 + WMSD4_l1 + WMSD5_l1 + WMSD6_l1 + PMA2_l1 + PMA3_l1 + PMA4_l1 + PMA5_l1 + PMA6_l1 + PMSD2_l1 + PMSD3_l1 + PMSD4_l1 + PMSD5_l1 + PMSD6_l1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_Energy2 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + lag_Energy7 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22))
    min10_h3_model <- min10_tr %>%
      model(mod1 = TSLM(Energy ~ WMA2_l2 + WMA3_l2 + WMA4_l2 + WMA5_l2 + WMA6_l2 + WMSD2_l2 + WMSD3_l2 + WMSD4_l2 + WMSD5_l2 + WMSD6_l2 + PMA2_l2 + PMA3_l2 + PMA4_l2 + PMA5_l2 + PMA6_l2 + PMSD2_l2 + PMSD3_l2 + PMSD4_l2 + PMSD5_l2 + PMSD6_l2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + lag_Energy7 + lag_Energy8 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22))
    min10_h4_model <- min10_tr %>%
      model(mod1 = TSLM(Energy ~ WMA2_l3 + WMA3_l3 + WMA4_l3 + WMA5_l3 + WMA6_l3 + WMSD2_l3 + WMSD3_l3 + WMSD4_l3 + WMSD5_l3 + WMSD6_l3 + PMA2_l3 + PMA3_l3 + PMA4_l3 + PMA5_l3 + PMA6_l3 + PMSD2_l3 + PMSD3_l3 + PMSD4_l3 + PMSD5_l3 + PMSD6_l3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_wind9 + lag_Energy4 + lag_Energy5 + lag_Energy6 + lag_Energy7 + lag_Energy8 + lag_Energy9 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22))
    min10_h5_model <- min10_tr %>%
      model(mod1 = TSLM(Energy ~ WMA2_l4 + WMA3_l4 + WMA4_l4 + WMA5_l4 + WMA6_l4 + WMSD2_l4 + WMSD3_l4 + WMSD4_l4 + WMSD5_l4 + WMSD6_l4 + PMA2_l4 + PMA3_l4 + PMA4_l4 + PMA5_l4 + PMA6_l4 + PMSD2_l4 + PMSD3_l4 + PMSD4_l4 + PMSD5_l4 + PMSD6_l4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_wind9 + lag_wind10 + lag_Energy5 + lag_Energy6 + lag_Energy7 + lag_Energy8 + lag_Energy9 + lag_Energy10 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22))
    min10_h6_model <- min10_tr %>%
      model(mod1 = TSLM(Energy ~ WMA2_l5 + WMA3_l5 + WMA4_l5 + WMA5_l5 + WMA6_l5 + WMSD2_l5 + WMSD3_l5 + WMSD4_l5 + WMSD5_l5 + WMSD6_l5 + PMA2_l5 + PMA3_l5 + PMA4_l5 + PMA5_l5 + PMA6_l5 + PMSD2_l5 + PMSD3_l5 + PMSD4_l5 + PMSD5_l5 + PMSD6_l5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_wind9 + lag_wind10 + lag_wind11 + lag_Energy6 + lag_Energy7 + lag_Energy8 + lag_Energy9 + lag_Energy10 + lag_Energy11 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22))
    
    # extract residuals of model, we interpolate 
    resid_10_h1 <- as_tibble(min10_h1_model %>%
                               residuals())
    resid_10_h1 <- resid_10_h1 %>% as.data.frame()
    resid_10_h1 <- resid_10_h1[seq(1, nrow(resid_10_h1), 6), ]
    resid_10_h2 <- as_tibble(min10_h2_model %>%
                               residuals())
    resid_10_h2 <- resid_10_h2 %>% as.data.frame()
    resid_10_h2 <- resid_10_h2[seq(2, nrow(resid_10_h2), 6), ]
    resid_10_h3 <- as_tibble(min10_h3_model %>%
                               residuals())
    resid_10_h3 <- resid_10_h3 %>% as.data.frame()
    resid_10_h3 <- resid_10_h3[seq(3, nrow(resid_10_h3), 6), ]
    resid_10_h4 <- as_tibble(min10_h4_model %>%
                               residuals())
    resid_10_h4 <- resid_10_h4 %>% as.data.frame()
    resid_10_h4 <- resid_10_h4[seq(4, nrow(resid_10_h4), 6), ]
    resid_10_h5 <- as_tibble(min10_h5_model %>%
                               residuals())
    resid_10_h5 <- resid_10_h5 %>% as.data.frame()
    resid_10_h5 <- resid_10_h5[seq(5, nrow(resid_10_h5), 6), ]
    resid_10_h6 <- as_tibble(min10_h6_model %>%
                               residuals())
    resid_10_h6 <- resid_10_h6 %>% as.data.frame()
    resid_10_h6 <- resid_10_h6[seq(6, nrow(resid_10_h6), 6), ]
    
    resid_10 <- bind_rows(resid_10_h1,resid_10_h2,resid_10_h3,resid_10_h4,resid_10_h5,resid_10_h6) %>% group_by(Group, Subgroup, .model)
    n <- nrow(resid_10_h1)
    m <- 6
    interleave_index <- as.vector(t(matrix(1:(n*m), n, m)))
    resid_10 <- resid_10[interleave_index, ]
    
    fc_10 <- NULL;
    # forecast with new data
    fc_10 <- fc_10 %>%
      bind_rows(min10_h1_model %>%
                  forecast(new_data = min10_test %>%
                             group_by(Group, Subgroup) %>%
                             dplyr::slice(1)))
    fc_10 <- fc_10 %>%
      bind_rows(min10_h2_model %>%
                  forecast(new_data = min10_test %>%
                             group_by(Group, Subgroup) %>%
                             dplyr::slice(2)))
    fc_10 <- fc_10 %>%
      bind_rows(min10_h3_model %>%
                  forecast(new_data = min10_test %>%
                             group_by(Group, Subgroup) %>%
                             dplyr::slice(3)))
    fc_10 <- fc_10 %>%
      bind_rows(min10_h4_model %>%
                  forecast(new_data = min10_test %>%
                             group_by(Group, Subgroup) %>%
                             dplyr::slice(4)))
    fc_10 <- fc_10 %>%
      bind_rows(min10_h5_model %>%
                  forecast(new_data = min10_test %>%
                             group_by(Group, Subgroup) %>%
                             dplyr::slice(5)))
    fc_10 <- fc_10 %>%
      bind_rows(min10_h6_model %>%
                  forecast(new_data = min10_test %>%
                             group_by(Group, Subgroup) %>%
                             dplyr::slice(6)))
    fc_10 <- fc_10 %>% group_by_key()
    #lr holds base forecasts
    lr <- NULL
    lr$k1 <- to_matrix_for_ts(fc_10, ".mean")
    lr$k1 <- ts(lr$k1, frequency = 6)
    residuals <- NULL
    residuals$k1 <- to_matrix_for_ts(resid_10, ".resid")
    residuals$k1 <- ts(residuals$k1, frequency = 6)
    obs <- NULL
    obs$k1 <- to_matrix_for_ts(min10_obs, "Energy")
    obs$k1 <- ts(obs$k1, frequency = 6)
    test <- NULL
    test$k1 <- to_matrix_for_ts(min10_test, "Energy")
    test$k1 <- ts(test$k1, frequency = 6)
    #now clear memory of all 10-minutely
    rm(dfs,min10_h1_model,min10_h2_model,min10_h3_model,min10_h4_model,min10_h5_model,min10_h6_model,min10_obs,min10_tr,min10_test)
    
    # Perform all 20-minutely forecasts firsts and then free up memory etc
    min20_obs <- min20_data %>%
      slice_head(n = (i/2)+3)
    min20_tr <- min20_obs %>%
      slice_head(n = i/2)
    min20_test <- min20_obs %>%
      slice_tail(n = 3)
    # train models
    min20_h1_model <- min20_tr %>%
      model(mod1 = TSLM(Energy ~ WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_Energy1 + lag_Energy2 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22))
    min20_h2_model <- min20_tr %>%
      model(mod1 = TSLM(Energy ~ WMA2_l1 + WMA3_l1 + WMA4_l1 + WMA5_l1 + WMA6_l1 + WMSD2_l1 + WMSD3_l1 + WMSD4_l1 + WMSD5_l1 + WMSD6_l1 + PMA2_l1 + PMA3_l1 + PMA4_l1 + PMA5_l1 + PMA6_l1 + PMSD2_l1 + PMSD3_l1 + PMSD4_l1 + PMSD5_l1 + PMSD6_l1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_Energy2 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + lag_Energy7 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22))
    min20_h3_model <- min20_tr %>%
      model(mod1 = TSLM(Energy ~ WMA2_l2 + WMA3_l2 + WMA4_l2 + WMA5_l2 + WMA6_l2 + WMSD2_l2 + WMSD3_l2 + WMSD4_l2 + WMSD5_l2 + WMSD6_l2 + PMA2_l2 + PMA3_l2 + PMA4_l2 + PMA5_l2 + PMA6_l2 + PMSD2_l2 + PMSD3_l2 + PMSD4_l2 + PMSD5_l2 + PMSD6_l2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + lag_Energy7 + lag_Energy8 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22))
    # extract residuals
    resid_20_h1 <- as_tibble(min20_h1_model %>%
                               residuals())
    resid_20_h1 <- resid_20_h1 %>% as.data.frame()
    resid_20_h1 <- resid_20_h1[seq(1, nrow(resid_20_h1), 3), ]
    resid_20_h2 <- as_tibble(min20_h2_model %>%
                               residuals())
    resid_20_h2 <- resid_20_h2 %>% as.data.frame()
    resid_20_h2 <- resid_20_h2[seq(2, nrow(resid_20_h2), 3), ]
    resid_20_h3 <- as_tibble(min20_h3_model %>%
                               residuals())
    resid_20_h3 <- resid_20_h3 %>% as.data.frame()
    resid_20_h3 <- resid_20_h3[seq(3, nrow(resid_20_h3), 3), ]
    
    resid_20 <- bind_rows(resid_20_h1,resid_20_h2,resid_20_h3) %>% group_by(Group, Subgroup, .model)
    n <- nrow(resid_20_h1)
    m <- 3
    interleave_index <- as.vector(t(matrix(1:(n*m), n, m)))
    resid_20 <- resid_20[interleave_index, ]
    
    fc_20 <- NULL;
    # produce forecasts
    fc_20 <- fc_20 %>%
      bind_rows(min20_h1_model %>%
                  forecast(new_data = min20_test %>%
                             group_by(Group, Subgroup) %>%
                             dplyr::slice(1)))
    fc_20 <- fc_20 %>%
      bind_rows(min20_h2_model %>%
                  forecast(new_data = min20_test %>%
                             group_by(Group, Subgroup) %>%
                             dplyr::slice(2)))
    fc_20 <- fc_20 %>%
      bind_rows(min20_h3_model %>%
                  forecast(new_data = min20_test %>%
                             group_by(Group, Subgroup) %>%
                             dplyr::slice(3)))
    fc_20 <- fc_20 %>% group_by_key()
    lr$k2 <- to_matrix_for_ts(fc_20, ".mean")
    lr$k2 <- ts(lr$k2, frequency = 3)
    residuals$k2 <- to_matrix_for_ts(resid_20, ".resid")
    residuals$k2 <- ts(residuals$k2, frequency = 3)
    obs$k2 <- to_matrix_for_ts(min20_obs, "Energy")
    obs$k2 <- ts(obs$k2, frequency = 3)
    test$k2 <- to_matrix_for_ts(min20_test, "Energy")
    test$k2 <- ts(test$k2, frequency = 3)
    rm(dfs,min20_h1_model,min20_h2_model,min20_h3_model,min20_obs,min20_tr,min20_test)
    
    # Perform all 30-minutely forecasts firsts and then free up memory etc
    min30_obs <- min30_data %>%
      slice_head(n = (i/3)+2)  
    min30_tr <- min30_obs %>%
      slice_head(n = i/3)
    min30_test <- min30_obs %>%
      slice_tail(n = 2)
    min30_h1_model <- min30_tr %>%
      model(mod1 = TSLM(Energy ~ WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_Energy1 + lag_Energy2 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22))
    min30_h2_model <- min30_tr %>%
      model(mod1 = TSLM(Energy ~ WMA2_l1 + WMA3_l1 + WMA4_l1 + WMA5_l1 + WMA6_l1 + WMSD2_l1 + WMSD3_l1 + WMSD4_l1 + WMSD5_l1 + WMSD6_l1 + PMA2_l1 + PMA3_l1 + PMA4_l1 + PMA5_l1 + PMA6_l1 + PMSD2_l1 + PMSD3_l1 + PMSD4_l1 + PMSD5_l1 + PMSD6_l1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_Energy2 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + lag_Energy7 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22))
    resid_30_h1 <- as_tibble(min30_h1_model %>%
                               residuals())
    resid_30_h1 <- resid_30_h1 %>% as.data.frame()
    resid_30_h1 <- resid_30_h1[seq(1, nrow(resid_30_h1), 2), ]
    resid_30_h2 <- as_tibble(min30_h2_model %>%
                               residuals())
    resid_30_h2 <- resid_30_h2 %>% as.data.frame()
    resid_30_h2 <- resid_30_h2[seq(2, nrow(resid_30_h2), 2), ]
    
    resid_30 <- bind_rows(resid_30_h1,resid_30_h2) %>% group_by(Group, Subgroup, .model)
    n <- nrow(resid_30_h1)
    m <- 2
    interleave_index <- as.vector(t(matrix(1:(n*m), n, m)))
    resid_30 <- resid_30[interleave_index, ]
    
    fc_30 <- NULL;
    fc_30 <- fc_30 %>%
      bind_rows(min30_h1_model %>%
                  forecast(new_data = min30_test %>%
                             group_by(Group, Subgroup) %>%
                             dplyr::slice(1)))  
    fc_30 <- fc_30 %>%
      bind_rows(min30_h2_model %>%
                  forecast(new_data = min30_test %>%
                             group_by(Group, Subgroup) %>%
                             dplyr::slice(2)))
    fc_30 <- fc_30 %>% group_by_key()
    lr$k3 <- to_matrix_for_ts(fc_30, ".mean")
    lr$k3 <- ts(lr$k3, frequency = 2)
    residuals$k3 <- to_matrix_for_ts(resid_30, ".resid")
    residuals$k3 <- ts(residuals$k3, frequency = 2)
    obs$k3 <- to_matrix_for_ts(min30_obs, "Energy")
    obs$k3 <- ts(obs$k3, frequency = 2)
    test$k3 <- to_matrix_for_ts(min30_test, "Energy")
    test$k3 <- ts(test$k3, frequency = 2)
    rm(dfs,min30_h1_model,min30_h2_model,min30_obs,min30_tr,min30_test)
    
    # Perform 1-hourly forecasts
    hr1_obs <- hr1_data %>%
      slice_head(n = (i/6)+1)
    hr1_tr <- hr1_obs %>%
      slice_head(n = i/6)
    hr1_test <- hr1_obs %>%
      slice_tail(n = 1)
    hr1_model <- hr1_tr %>%
      model(mod1 = TSLM(Energy ~ WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_Energy1 + lag_Energy2 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22))
    resid_1 <- hr1_model %>%
      residuals() %>% group_by(Group, Subgroup, .model)
    fc_1 <- NULL;
    fc_1 <- fc_1 %>%
      bind_rows(hr1_model %>%
                  forecast(new_data = hr1_test %>%
                             group_by(Group, Subgroup)))
    fc_1 <- fc_1 %>% group_by_key()
    lr$k6 <- to_matrix_for_ts(fc_1, ".mean")
    lr$k6 <- ts(lr$k6, frequency = 1)
    residuals$k6 <- to_matrix_for_ts(resid_1, ".resid")
    residuals$k6 <- ts(residuals$k6, frequency = 1)
    obs$k6 <- to_matrix_for_ts(hr1_obs, "Energy")
    obs$k6 <- ts(obs$k6, frequency = 1)
    test$k6 <- to_matrix_for_ts(hr1_test, "Energy")
    test$k6 <- ts(test$k6, frequency = 1)
    rm(hr1_model,hr1_obs,hr1_tr,hr1_test)
    gc()
    
    lr <- t(do.call(rbind, rev(lr)))
    kset <- c(6, 3, 2, 1)
    h <- 1
    colnames(lr) <- paste("k", rep(kset, h * rev(kset)), "_h",
                          do.call("c", as.list(sapply(
                            rev(kset) * h,
                            function(x) seq(1:x)))),
                          sep = "")
    base_lr <- NULL
    base_lr <- lr
    res_lr <- t(do.call(rbind, rev(residuals)))
    
    ## Balanced hierarchy
    #                 T
    #    |--------|-------|-------|
    #    A        B       C       D
    #  |---|    |---|   |---|   |---|
    # A1...A7  B1...B9 C1...C3 D1...D6
    # Names of the bottom level variables
    data_bts <- data.frame(X1 = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B"),
                           X2 = c("1", "2", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "1", "2", "3", "4", "5", "6"),
                           stringsAsFactors = FALSE)
    # Cross-sectional aggregation matrix
    C <- Cmatrix(~ X1 / X2, data_bts, sep = "")
    
    test <- t(do.call(rbind, rev(test)))
    
    colnames(test) <- paste("k", rep(kset, h * rev(kset)), "_h",
                            do.call("c", as.list(sapply(
                              rev(kset) * h,
                              function(x) seq(1:x)))),
                            sep = "")
    
    FoReco_data <- list(base = base_lr,
                        test = test,
                        res = res_lr,
                        C = C,
                        obs = obs)
    
    # Cross-temporal reconciliation
    #### Heuristic first-temporal-then-cross-sectional cross-temporal reconciliation using t-wls + cs-shr (Kourentzes and Athanasopoulos, 2019) ####
    
    tcs_recf <- tcsrec(FoReco_data$base, m = 6, C = FoReco_data$C,
                       thf_comb = "wlsv", hts_comb = "shr",
                       res = FoReco_data$res)$recf
    
    fc_tcs <- cbind(fc_tcs, tcs_recf)
    
    #### Forecast reconciliation through temporal hierarchies for all time series using series-acov (Nystrup et al., 2020) ####
    n <- NROW(FoReco_data$base)
    thf_recf <- matrix(NA, n, NCOL(FoReco_data$base))
    dimnames(thf_recf) <- dimnames(FoReco_data$base)
    for(i in 1:n){
      # ts base forecasts ([lowest_freq' ...  highest_freq']')
      tsbase <- FoReco_data$base[i, ]
      # ts residuals ([lowest_freq' ...  highest_freq']')
      tsres <- FoReco_data$res[i, ]
      thf_recf[i,] <- thfrec(tsbase, m = 6, comb = "acov",
                             res = tsres, keep = "recf")
    }
    
    # have to set this since the FoReco package has a bug that changes column names for some reason
    colnames(thf_recf) <- colnames(tcs_recf)
    
    fc_thf <- cbind(fc_thf, thf_recf)
    
    #### Heuristic first-cross-sectional-then-temporal cross-temporal reconciliation using t-acov + cs-shr (Di Fonzo and Girolimetto, 2020) ####
    
    cst_recf <- as.matrix(cstrec(FoReco_data$base, m = 6, C = FoReco_data$C,
                                 thf_comb = "acov", hts_comb = "shr",
                                 res = FoReco_data$res)$recf)
    
    fc_cst <- cbind(fc_cst, cst_recf)
    
    #### Iterative cross-temporal reconciliation (Di Fonzo and Girolimetto, 2020) ####
    
    ite_recf <- iterec(FoReco_data$base,
                       m = 6, C = FoReco_data$C,
                       thf_comb = "acov", hts_comb = "shr",
                       res = FoReco_data$res, start_rec = "thf")$recf
    
    fc_ite <- cbind(fc_ite, ite_recf)
    
    #### Optimal cross-temporal reconciliation using wlsv (Di Fonzo and Girolimetto, 2020) ####
    oct_recf <- octrec(FoReco_data$base, m = 6, C = FoReco_data$C,
                       comb = "wlsv", res = FoReco_data$res)$recf
    
    fc_oct <- cbind(fc_oct, oct_recf)
    
    #### Bottom up cross-temporal reconciliation ####
    id <- which(simplify2array(strsplit(colnames(FoReco_data$base),
                                        split = "_"))[1, ] == "k1")
    hfbts <- FoReco_data$base[-c(1:3), id]
    obj <- ctbu(Bmat = hfbts, m = 6, C = FoReco_data$C)
    rownames(obj) <- rownames(FoReco_data$base)
    
    fc_ctbu <- cbind(fc_ctbu, obj)
    
    test_set <- cbind(test_set, test)
    
    fc_LR <- cbind(fc_LR, lr)
    
    rm(FoReco_data)
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
}

saveRDS(fc_LR, file = "fc_LR.rds")
saveRDS(fc_cst, file = "fc_LR_cst.rds")
saveRDS(fc_tcs, file = "fc_LR_tcs.rds")
saveRDS(fc_thf, file = "fc_LR_thf.rds")
saveRDS(fc_ite, file = "fc_LR_ite.rds")
saveRDS(fc_oct, file = "fc_LR_oct.rds")
saveRDS(fc_ctbu, file = "fc_LR_ctbu.rds")
saveRDS(test_set, file = "fc_test_set.rds")