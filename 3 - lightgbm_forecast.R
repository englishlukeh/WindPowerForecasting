#### Time series LightGBM with feature engineering - base forecasts ####

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

# set the proportion of training set
TrainingProportion <- 0.90
# set number of threads
numThreads = 3;

min10 <- readRDS(file = "min10_wFeatures.rds")
min20 <- readRDS(file = "min20_wFeatures.rds")
min30 <- readRDS(file = "min30_wFeatures.rds")
hr1 <- readRDS(file = "hr1_wFeatures.rds")

#### Use gradient boosting - 1 hourly ####
# this uses features calculated in the linear regression section

hr1_test <- hr1 %>% filter_index("2021-06-20" ~ "2021-06-30")

gc()

N = nrow(hr1_test)/n_keys(hr1_test)

fc1_gb <- NULL

# define parameters for 1 hourly
params = list(
  objective = "regression"
  , metric = "l2"
  , learning_rate = 0.1
  , num_leaves = 130
  , max_depth = 8
  , min_data_in_leaf = 200
  , num_threads = 8
  , linear_lambda = 2
)

# do our TSCV manually, starting from 90% of the dataset up to the second last element
for (i in seq(ceiling(N*TrainingProportion),N-1,1)) {
  
  gc()
  
  # compute fit
  fit_total <- hr1_test %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Energy ~ hyperparameters(params) + WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_Energy1 + lag_Energy2 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
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

min30_test <- min30 %>% filter_index("2021-06-20" ~ "2021-06-30")

gc()

N = nrow(min30_test)/n_keys(min30_test)

fc30_gb <- NULL

# define parameters for 30-minutely
params = list(
  objective = "regression"
  , metric = "l2"
  , learning_rate = 0.1
  , num_leaves = 80
  , max_depth = 7
  , min_data_in_leaf = 200
  , num_threads = 8
  , linear_lambda = 3
)

# do our TSCV manually, starting from 90% of the dataset up to the second last element
for (i in seq(ceiling(N*TrainingProportion),N-2,2)) {
  
  gc()
  
  # compute fit
  fit_h1 <- min30_test %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Energy ~ hyperparameters(params) + WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_Energy1 + lag_Energy2 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
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
    model(mod1 = lgbm(Energy ~ hyperparameters(params) + WMA2_l1 + WMA3_l1 + WMA4_l1 + WMA5_l1 + WMA6_l1 + WMSD2_l1 + WMSD3_l1 + WMSD4_l1 + WMSD5_l1 + WMSD6_l1 + PMA2_l1 + PMA3_l1 + PMA4_l1 + PMA5_l1 + PMA6_l1 + PMSD2_l1 + PMSD3_l1 + PMSD4_l1 + PMSD5_l1 + PMSD6_l1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_Energy2 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + lag_Energy7 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
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

min20_test <- min20 %>% filter_index("2021-06-20" ~ "2021-06-30")

gc()

N = nrow(min20_test)/n_keys(min20_test)

fc20_gb <- NULL

# define parameters for 30-minutely
params = list(
  objective = "regression"
  , metric = "l2"
  , learning_rate = 0.1
  , num_leaves = 100
  , max_depth = 9
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
    model(mod1 = lgbm(Energy ~ hyperparameters(params) + WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_Energy1 + lag_Energy2 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  fit_h2 <- min20 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Energy ~ hyperparameters(params) + WMA2_l1 + WMA3_l1 + WMA4_l1 + WMA5_l1 + WMA6_l1 + WMSD2_l1 + WMSD3_l1 + WMSD4_l1 + WMSD5_l1 + WMSD6_l1 + PMA2_l1 + PMA3_l1 + PMA4_l1 + PMA5_l1 + PMA6_l1 + PMSD2_l1 + PMSD3_l1 + PMSD4_l1 + PMSD5_l1 + PMSD6_l1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_Energy2 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + lag_Energy7 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  fit_h3 <- min20 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Energy ~ hyperparameters(params) + WMA2_l2 + WMA3_l2 + WMA4_l2 + WMA5_l2 + WMA6_l2 + WMSD2_l2 + WMSD3_l2 + WMSD4_l2 + WMSD5_l2 + WMSD6_l2 + PMA2_l2 + PMA3_l2 + PMA4_l2 + PMA5_l2 + PMA6_l2 + PMSD2_l2 + PMSD3_l2 + PMSD4_l2 + PMSD5_l2 + PMSD6_l2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + lag_Energy7 + lag_Energy8 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
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
                           dplyr::slice(i+1)))
  
  fc20_gb <- fc20_gb %>%
    bind_rows(fit_h2 %>%
                forecast(new_data = min20 %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(i+2)))
  
  fc20_gb <- fc20_gb %>%
    bind_rows(fit_h3 %>%
                forecast(new_data = min20 %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(i+3)))
  
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
  }
}

saveRDS(fc20_gb, file = "fc20_gb.rds")

#### Use gradient boosting - 10 minutely ####
# this uses features calculated in the linear regression section

min10_test <- min10 %>% filter_index("2021-06-20" ~ "2021-06-30")

gc()

N = nrow(min10_test)/n_keys(min10_test)

fc10_gb <- NULL

# define parameters for 10-minutely
params = list(
  objective = "regression"
  , metric = "l2"
  , learning_rate = 0.1
  , num_leaves = 140
  , max_depth = 13
  , min_data_in_leaf = 140
  , num_threads = 8
  , linear_lambda = 1
)

# do our TSCV manually, starting from 90% of the dataset up to the second last element
for (i in seq(ceiling(N*TrainingProportion),N-6,6)) {
  
  gc()
  
  # compute fit
  fit_h1 <- min10 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Energy ~ hyperparameters(params) + WMA2 + WMA3 + WMA4 + WMA5 + WMA6 + WMSD2 + WMSD3 + WMSD4 + WMSD5 + WMSD6 + PMA2 + PMA3 + PMA4 + PMA5 + PMA6 + PMSD2 + PMSD3 + PMSD4 + PMSD5 + PMSD6 + lag_wind1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_Energy1 + lag_Energy2 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  fit_h2 <- min10 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Energy ~ hyperparameters(params) + WMA2_l1 + WMA3_l1 + WMA4_l1 + WMA5_l1 + WMA6_l1 + WMSD2_l1 + WMSD3_l1 + WMSD4_l1 + WMSD5_l1 + WMSD6_l1 + PMA2_l1 + PMA3_l1 + PMA4_l1 + PMA5_l1 + PMA6_l1 + PMSD2_l1 + PMSD3_l1 + PMSD4_l1 + PMSD5_l1 + PMSD6_l1 + lag_wind2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_Energy2 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + lag_Energy7 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  fit_h3 <- min10 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Energy ~ hyperparameters(params) + WMA2_l2 + WMA3_l2 + WMA4_l2 + WMA5_l2 + WMA6_l2 + WMSD2_l2 + WMSD3_l2 + WMSD4_l2 + WMSD5_l2 + WMSD6_l2 + PMA2_l2 + PMA3_l2 + PMA4_l2 + PMA5_l2 + PMA6_l2 + PMSD2_l2 + PMSD3_l2 + PMSD4_l2 + PMSD5_l2 + PMSD6_l2 + lag_wind3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_Energy3 + lag_Energy4 + lag_Energy5 + lag_Energy6 + lag_Energy7 + lag_Energy8 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  fit_h4 <- min10 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Energy ~ hyperparameters(params) + WMA2_l3 + WMA3_l3 + WMA4_l3 + WMA5_l3 + WMA6_l3 + WMSD2_l3 + WMSD3_l3 + WMSD4_l3 + WMSD5_l3 + WMSD6_l3 + PMA2_l3 + PMA3_l3 + PMA4_l3 + PMA5_l3 + PMA6_l3 + PMSD2_l3 + PMSD3_l3 + PMSD4_l3 + PMSD5_l3 + PMSD6_l3 + lag_wind4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_wind9 + lag_Energy4 + lag_Energy5 + lag_Energy6 + lag_Energy7 + lag_Energy8 + lag_Energy9 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  fit_h5 <- min10 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Energy ~ hyperparameters(params) + WMA2_l4 + WMA3_l4 + WMA4_l4 + WMA5_l4 + WMA6_l4 + WMSD2_l4 + WMSD3_l4 + WMSD4_l4 + WMSD5_l4 + WMSD6_l4 + PMA2_l4 + PMA3_l4 + PMA4_l4 + PMA5_l4 + PMA6_l4 + PMSD2_l4 + PMSD3_l4 + PMSD4_l4 + PMSD5_l4 + PMSD6_l4 + lag_wind5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_wind9 + lag_wind10 + lag_Energy5 + lag_Energy6 + lag_Energy7 + lag_Energy8 + lag_Energy9 + lag_Energy10 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
    reconcile(
      bu_mod1 = bottom_up(mod1),
      td_mod1 = top_down(mod1),
      mo_mod1 = middle_out(mod1),
      ols_mod1 = min_trace(mod1, method = "ols"),
      mint_mod1 = min_trace(mod1, method = "mint_shrink")
    )
  
  fit_h6 <- min10 %>%
    slice_head(n = i) %>%
    model(mod1 = lgbm(Energy ~ hyperparameters(params) + WMA2_l5 + WMA3_l5 + WMA4_l5 + WMA5_l5 + WMA6_l5 + WMSD2_l5 + WMSD3_l5 + WMSD4_l5 + WMSD5_l5 + WMSD6_l5 + PMA2_l5 + PMA3_l5 + PMA4_l5 + PMA5_l5 + PMA6_l5 + PMSD2_l5 + PMSD3_l5 + PMSD4_l5 + PMSD5_l5 + PMSD6_l5 + lag_wind6 + lag_wind7 + lag_wind8 + lag_wind9 + lag_wind10 + lag_wind11 + lag_Energy6 + lag_Energy7 + lag_Energy8 + lag_Energy9 + lag_Energy10 + lag_Energy11 + is_q1 + is_q2 + is_q3 + is_00 + is_01 + is_02 + is_03 + is_04 + is_05 + is_06 + is_07 + is_08 + is_09 + is_10 + is_11 + is_12 + is_13 + is_14 + is_15 + is_16 + is_17 + is_18 + is_19 + is_20 + is_21 + is_22)) %>%
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
                                  dplyr::slice(i+1)))
  
  fc10_gb <- fc10_gb %>%
    bind_rows(fit_h2 %>%
                forecast(new_data = min10 %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(i+2)))
  
  fc10_gb <- fc10_gb %>%
    bind_rows(fit_h3 %>%
                forecast(new_data = min10 %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(i+3)))
  
  fc10_gb <- fc10_gb %>%
    bind_rows(fit_h4 %>%
                forecast(new_data = min10 %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(i+4)))
  
  fc10_gb <- fc10_gb %>%
    bind_rows(fit_h5 %>%
                forecast(new_data = min10 %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(i+5)))
  
  fc10_gb <- fc10_gb %>%
    bind_rows(fit_h6 %>%
                forecast(new_data = min10 %>%
                           group_by(Group, Subgroup) %>%
                           dplyr::slice(i+6)))
  
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
    fitted_h4 <- fit_h4 %>%
      fitted()
    residuals_h4 <- fit_h4 %>%
      residuals()
    fitted_h5 <- fit_h5 %>%
      fitted()
    residuals_h5 <- fit_h5 %>%
      residuals()
    fitted_h6 <- fit_h6 %>%
      fitted()
    residuals_h6 <- fit_h6 %>%
      residuals()
    saveRDS(fitted_h1, file = "fc10_h1_gb_fitted.rds")
    saveRDS(residuals_h1, file = "fc10_h1_gb_residuals.rds")
    saveRDS(fitted_h2, file = "fc10_h2_gb_fitted.rds")
    saveRDS(residuals_h2, file = "fc10_h2_gb_residuals.rds")
    saveRDS(fitted_h3, file = "fc10_h3_gb_fitted.rds")
    saveRDS(residuals_h3, file = "fc10_h3_gb_residuals.rds")
    saveRDS(fitted_h4, file = "fc10_h4_gb_fitted.rds")
    saveRDS(residuals_h4, file = "fc10_h4_gb_residuals.rds")
    saveRDS(fitted_h5, file = "fc10_h5_gb_fitted.rds")
    saveRDS(residuals_h5, file = "fc10_h5_gb_residuals.rds")
    saveRDS(fitted_h6, file = "fc10_h6_gb_fitted.rds")
    saveRDS(residuals_h6, file = "fc10_h6_gb_residuals.rds")
  }
}

saveRDS(fc10_gb, file = "fc10_gb.rds")