#### Import packages and set active directory ####

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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set this to your working directory

#### Define auxiliary functions ####
# function to convert grouped/keyed tsibble into matrix of ts() time series
to_matrix_for_ts <- function(data, variable){
  N = nrow(data)/n_keys(data)
  matrix_tmp <- matrix(NA, nrow = N, ncol = n_keys(data))
  matrix_tmp[,1] <- (data %>% filter(is_aggregated(Group), is_aggregated(Subgroup)))[[variable]]
  matrix_tmp[,2] <- (data %>% filter(Group == "A", is_aggregated(Subgroup)))[[variable]]
  matrix_tmp[,3] <- (data %>% filter(Group == "B", is_aggregated(Subgroup)))[[variable]]
  matrix_tmp[,4] <- (data %>% filter(Group == "C", is_aggregated(Subgroup)))[[variable]]
  matrix_tmp[,5] <- (data %>% filter(Group == "D", is_aggregated(Subgroup)))[[variable]]
  matrix_tmp[,5] <- (data %>% filter(Group == "D", is_aggregated(Subgroup)))[[variable]]
  matrix_tmp[,6] <- (data %>% filter(Group == "A", Subgroup == "A1"))[[variable]]
  matrix_tmp[,7] <- (data %>% filter(Group == "A", Subgroup == "A2"))[[variable]]
  matrix_tmp[,8] <- (data %>% filter(Group == "A", Subgroup == "A3"))[[variable]]
  matrix_tmp[,9] <- (data %>% filter(Group == "A", Subgroup == "A4"))[[variable]]
  matrix_tmp[,10] <- (data %>% filter(Group == "A", Subgroup == "A5"))[[variable]]
  matrix_tmp[,11] <- (data %>% filter(Group == "A", Subgroup == "A6"))[[variable]]
  matrix_tmp[,12] <- (data %>% filter(Group == "A", Subgroup == "A7"))[[variable]]
  matrix_tmp[,13] <- (data %>% filter(Group == "B", Subgroup == "B1"))[[variable]]
  matrix_tmp[,14] <- (data %>% filter(Group == "B", Subgroup == "B2"))[[variable]]
  matrix_tmp[,15] <- (data %>% filter(Group == "B", Subgroup == "B3"))[[variable]]
  matrix_tmp[,16] <- (data %>% filter(Group == "B", Subgroup == "B4"))[[variable]]
  matrix_tmp[,17] <- (data %>% filter(Group == "B", Subgroup == "B5"))[[variable]]
  matrix_tmp[,18] <- (data %>% filter(Group == "B", Subgroup == "B6"))[[variable]]
  matrix_tmp[,19] <- (data %>% filter(Group == "B", Subgroup == "B7"))[[variable]]
  matrix_tmp[,20] <- (data %>% filter(Group == "B", Subgroup == "B8"))[[variable]]
  matrix_tmp[,21] <- (data %>% filter(Group == "B", Subgroup == "B9"))[[variable]]
  matrix_tmp[,22] <- (data %>% filter(Group == "C", Subgroup == "C1"))[[variable]]
  matrix_tmp[,23] <- (data %>% filter(Group == "C", Subgroup == "C2"))[[variable]]
  matrix_tmp[,24] <- (data %>% filter(Group == "C", Subgroup == "C3"))[[variable]]
  matrix_tmp[,25] <- (data %>% filter(Group == "D", Subgroup == "D1"))[[variable]]
  matrix_tmp[,26] <- (data %>% filter(Group == "D", Subgroup == "D2"))[[variable]]
  matrix_tmp[,27] <- (data %>% filter(Group == "D", Subgroup == "D3"))[[variable]]
  matrix_tmp[,28] <- (data %>% filter(Group == "D", Subgroup == "D4"))[[variable]]
  matrix_tmp[,29] <- (data %>% filter(Group == "D", Subgroup == "D5"))[[variable]]
  matrix_tmp[,30] <- (data %>% filter(Group == "D", Subgroup == "D6"))[[variable]]
  colnames(matrix_tmp) <- c("T", "A", "B", "C", "D", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "C1", "C2", "C3", "D1", "D2", "D3", "D4", "D5", "D6")

  return(matrix_tmp)
}

# function to convert rMAE of matrix of errors
compute_rMAE <- function(error, naive_error_10, naive_error_20, naive_error_30, naive_error_1){
  # extract mean absolute error for each time series
  id <- which(simplify2array(strsplit(colnames(error), split = "_"))[1, ] == "k1")
  error_10 <- colMeans(t(error[, id]))/naive_error_10
  
  id <- which(simplify2array(strsplit(colnames(error), split = "_"))[1, ] == "k2")
  error_20 <- colMeans(t(error[, id]))/naive_error_20
  
  id <- which(simplify2array(strsplit(colnames(error), split = "_"))[1, ] == "k3")
  error_30 <- colMeans(t(error[, id]))/naive_error_30
  
  id <- which(simplify2array(strsplit(colnames(error), split = "_"))[1, ] == "k6")
  error_1 <- colMeans(t(error[, id]))/naive_error_1
  
  # set up matrix of rMAE
  error <- matrix(NA,4,3)
  colnames(error) <- c(2,1,0)
  rownames(error) <- c(10,20,30,1)
  
  # populate relative error matrix
  # we calculate geometric mean using exp(mean(log(vector))), taken from https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
  error[1,1] <- exp(mean(log(error_10[26:30])))
  error[1,2] <- exp(mean(log(error_10[2:5])))
  error[1,3] <- exp(mean(log(error_10[1])))
  
  error[2,1] <- exp(mean(log(error_20[26:30])))
  error[2,2] <- exp(mean(log(error_20[2:5])))
  error[2,3] <- exp(mean(log(error_20[1])))
  
  error[3,1] <- exp(mean(log(error_30[26:30])))
  error[3,2] <- exp(mean(log(error_30[2:5])))
  error[3,3] <- exp(mean(log(error_30[1])))
  
  error[4,1] <- exp(mean(log(error_1[26:30])))
  error[4,2] <- exp(mean(log(error_1[2:5])))
  error[4,3] <- exp(mean(log(error_1[1])))
  
  # return matrix of rMAE
  return(list(error, error_10, error_20, error_30, error_1))
}

# function to convert rRMSE of matrix of errors
compute_rRMSE <- function(error_squared, naive_error_squared_10, naive_error_squared_20, naive_error_squared_30, naive_error_squared_1){
  # extract mean squared error for each time series
  id <- which(simplify2array(strsplit(colnames(error_squared), split = "_"))[1, ] == "k1")
  error_10_squared <- sqrt(colMeans(t(error_squared[, id])))/sqrt(naive_error_squared_10)
  
  id <- which(simplify2array(strsplit(colnames(error_squared), split = "_"))[1, ] == "k2")
  error_20_squared <- sqrt(colMeans(t(error_squared[, id])))/sqrt(naive_error_squared_20)
  
  id <- which(simplify2array(strsplit(colnames(error_squared), split = "_"))[1, ] == "k3")
  error_30_squared <- sqrt(colMeans(t(error_squared[, id])))/sqrt(naive_error_squared_30)
  
  id <- which(simplify2array(strsplit(colnames(error_squared), split = "_"))[1, ] == "k6")
  error_1_squared <- sqrt(colMeans(t(error_squared[, id])))/sqrt(naive_error_squared_1)

  # set up matrix of rRMSE
  error_squared <- matrix(NA,4,3)
  colnames(error_squared) <- c(2,1,0)
  rownames(error_squared) <- c(10,20,30,1)
  
  # populate relative error matrix
  error_squared[1,1] <- exp(mean(log(error_10_squared[26:30])))
  error_squared[1,2] <- exp(mean(log(error_10_squared[2:5])))
  error_squared[1,3] <- exp(mean(log(error_10_squared[1])))
  
  error_squared[2,1] <- exp(mean(log(error_20_squared[26:30])))
  error_squared[2,2] <- exp(mean(log(error_20_squared[2:5])))
  error_squared[2,3] <- exp(mean(log(error_20_squared[1])))
  
  error_squared[3,1] <- exp(mean(log(error_30_squared[26:30])))
  error_squared[3,2] <- exp(mean(log(error_30_squared[2:5])))
  error_squared[3,3] <- exp(mean(log(error_30_squared[1])))
  
  error_squared[4,1] <- exp(mean(log(error_1_squared[26:30])))
  error_squared[4,2] <- exp(mean(log(error_1_squared[2:5])))
  error_squared[4,3] <- exp(mean(log(error_1_squared[1])))
  
  # return matrix of rRMSE
  return(list(error_squared, error_10_squared, error_20_squared, error_30_squared, error_1_squared))
}



#### LINEAR REGRESSION - extract BU ####
# forecasts linear regression
fc10_lr_bu <- as_tsibble(readRDS(file = "fc10_lr.rds")) %>% filter(.model == "bu_mod1")
fc20_lr_bu <- as_tsibble(readRDS(file = "fc20_lr.rds")) %>% filter(.model == "bu_mod1")
fc30_lr_bu <- as_tsibble(readRDS(file = "fc30_lr.rds")) %>% filter(.model == "bu_mod1")
fc1_lr_bu <- as_tsibble(readRDS(file = "fc1_lr.rds")) %>% filter(.model == "bu_mod1")

fc20_lr_bu <- fc20_lr_bu %>% mutate(.mean = .mean * 2)
fc30_lr_bu <- fc30_lr_bu %>% mutate(.mean = .mean * 3)
fc1_lr_bu <- fc1_lr_bu %>% mutate(.mean = .mean * 6)

lr_bu <- NULL

lr_bu$k1 <- to_matrix_for_ts(fc10_lr_bu, ".mean")
lr_bu$k1 <- ts(lr_bu$k1, frequency = 6, start = c(47304, 1))
lr_bu$k2 <- to_matrix_for_ts(fc20_lr_bu, ".mean")
lr_bu$k2 <- ts(lr_bu$k2, frequency = 3, start = c(47304, 1))
lr_bu$k3 <- to_matrix_for_ts(fc30_lr_bu, ".mean")
lr_bu$k3 <- ts(lr_bu$k3, frequency = 2, start = c(47304, 1))
lr_bu$k6 <- to_matrix_for_ts(fc1_lr_bu, ".mean")
lr_bu$k6 <- ts(lr_bu$k6, frequency = 1, start = c(47304, 1))

lr_bu <- t(do.call(rbind, rev(lr_bu)))


#### LINEAR REGRESSION - extract TD ####
# forecasts linear regression

fc10_lr_td <- as_tsibble(readRDS(file = "fc10_lr.rds")) %>% filter(.model == "td_mod1")
fc20_lr_td <- as_tsibble(readRDS(file = "fc20_lr.rds")) %>% filter(.model == "td_mod1")
fc30_lr_td <- as_tsibble(readRDS(file = "fc30_lr.rds")) %>% filter(.model == "td_mod1")
fc1_lr_td <- as_tsibble(readRDS(file = "fc1_lr.rds")) %>% filter(.model == "td_mod1")

fc20_lr_td <- fc20_lr_td %>% mutate(.mean = .mean * 2)
fc30_lr_td <- fc30_lr_td %>% mutate(.mean = .mean * 3)
fc1_lr_td <- fc1_lr_td %>% mutate(.mean = .mean * 6)

lr_td <- NULL

lr_td$k1 <- to_matrix_for_ts(fc10_lr_td, ".mean")
lr_td$k1 <- ts(lr_td$k1, frequency = 6, start = c(47304, 1))
lr_td$k2 <- to_matrix_for_ts(fc20_lr_td, ".mean")
lr_td$k2 <- ts(lr_td$k2, frequency = 3, start = c(47304, 1))
lr_td$k3 <- to_matrix_for_ts(fc30_lr_td, ".mean")
lr_td$k3 <- ts(lr_td$k3, frequency = 2, start = c(47304, 1))
lr_td$k6 <- to_matrix_for_ts(fc1_lr_td, ".mean")
lr_td$k6 <- ts(lr_td$k6, frequency = 1, start = c(47304, 1))

lr_td <- t(do.call(rbind, rev(lr_td)))


#### LINEAR REGRESSION - extract MO ####
fc10_lr_mo <- as_tsibble(readRDS(file = "fc10_lr.rds")) %>% filter(.model == "mo_mod1")
fc20_lr_mo <- as_tsibble(readRDS(file = "fc20_lr.rds")) %>% filter(.model == "mo_mod1")
fc30_lr_mo <- as_tsibble(readRDS(file = "fc30_lr.rds")) %>% filter(.model == "mo_mod1")
fc1_lr_mo <- as_tsibble(readRDS(file = "fc1_lr.rds")) %>% filter(.model == "mo_mod1")

fc20_lr_mo <- fc20_lr_mo %>% mutate(.mean = .mean * 2)
fc30_lr_mo <- fc30_lr_mo %>% mutate(.mean = .mean * 3)
fc1_lr_mo <- fc1_lr_mo %>% mutate(.mean = .mean * 6)

lr_mo <- NULL

lr_mo$k1 <- to_matrix_for_ts(fc10_lr_mo, ".mean")
lr_mo$k1 <- ts(lr_mo$k1, frequency = 6, start = c(47304, 1))
lr_mo$k2 <- to_matrix_for_ts(fc20_lr_mo, ".mean")
lr_mo$k2 <- ts(lr_mo$k2, frequency = 3, start = c(47304, 1))
lr_mo$k3 <- to_matrix_for_ts(fc30_lr_mo, ".mean")
lr_mo$k3 <- ts(lr_mo$k3, frequency = 2, start = c(47304, 1))
lr_mo$k6 <- to_matrix_for_ts(fc1_lr_mo, ".mean")
lr_mo$k6 <- ts(lr_mo$k6, frequency = 1, start = c(47304, 1))

lr_mo <- t(do.call(rbind, rev(lr_mo)))

#### LINEAR REGRESSION - extract OLS ####
fc10_lr_ols <- as_tsibble(readRDS(file = "fc10_lr.rds")) %>% filter(.model == "ols_mod1")
fc20_lr_ols <- as_tsibble(readRDS(file = "fc20_lr.rds")) %>% filter(.model == "ols_mod1")
fc30_lr_ols <- as_tsibble(readRDS(file = "fc30_lr.rds")) %>% filter(.model == "ols_mod1")
fc1_lr_ols <- as_tsibble(readRDS(file = "fc1_lr.rds")) %>% filter(.model == "ols_mod1")

fc20_lr_ols <- fc20_lr_ols %>% mutate(.mean = .mean * 2)
fc30_lr_ols <- fc30_lr_ols %>% mutate(.mean = .mean * 3)
fc1_lr_ols <- fc1_lr_ols %>% mutate(.mean = .mean * 6)

lr_ols <- NULL

lr_ols$k1 <- to_matrix_for_ts(fc10_lr_ols, ".mean")
lr_ols$k1 <- ts(lr_ols$k1, frequency = 6, start = c(47304, 1))
lr_ols$k2 <- to_matrix_for_ts(fc20_lr_ols, ".mean")
lr_ols$k2 <- ts(lr_ols$k2, frequency = 3, start = c(47304, 1))
lr_ols$k3 <- to_matrix_for_ts(fc30_lr_ols, ".mean")
lr_ols$k3 <- ts(lr_ols$k3, frequency = 2, start = c(47304, 1))
lr_ols$k6 <- to_matrix_for_ts(fc1_lr_ols, ".mean")
lr_ols$k6 <- ts(lr_ols$k6, frequency = 1, start = c(47304, 1))

lr_ols <- t(do.call(rbind, rev(lr_ols)))


#### LINEAR REGRESSION - extract MinT-shr ####
fc10_lr_mint <- as_tsibble(readRDS(file = "fc10_lr.rds")) %>% filter(.model == "mint_mod1")
fc20_lr_mint <- as_tsibble(readRDS(file = "fc20_lr.rds")) %>% filter(.model == "mint_mod1")
fc30_lr_mint <- as_tsibble(readRDS(file = "fc30_lr.rds")) %>% filter(.model == "mint_mod1")
fc1_lr_mint <- as_tsibble(readRDS(file = "fc1_lr.rds")) %>% filter(.model == "mint_mod1")

fc20_lr_mint <- fc20_lr_mint %>% mutate(.mean = .mean * 2)
fc30_lr_mint <- fc30_lr_mint %>% mutate(.mean = .mean * 3)
fc1_lr_mint <- fc1_lr_mint %>% mutate(.mean = .mean * 6)

lr_mint <- NULL

lr_mint$k1 <- to_matrix_for_ts(fc10_lr_mint, ".mean")
lr_mint$k1 <- ts(lr_mint$k1, frequency = 6, start = c(47304, 1))
lr_mint$k2 <- to_matrix_for_ts(fc20_lr_mint, ".mean")
lr_mint$k2 <- ts(lr_mint$k2, frequency = 3, start = c(47304, 1))
lr_mint$k3 <- to_matrix_for_ts(fc30_lr_mint, ".mean")
lr_mint$k3 <- ts(lr_mint$k3, frequency = 2, start = c(47304, 1))
lr_mint$k6 <- to_matrix_for_ts(fc1_lr_mint, ".mean")
lr_mint$k6 <- ts(lr_mint$k6, frequency = 1, start = c(47304, 1))

lr_mint <- t(do.call(rbind, rev(lr_mint)))

#### LINEAR REGRESSION - extract unreconciled base forecasts ####
fc10_lr <- as_tsibble(readRDS(file = "fc10_lr.rds")) %>% filter(.model == "mod1")
fc20_lr <- as_tsibble(readRDS(file = "fc20_lr.rds")) %>% filter(.model == "mod1")
fc30_lr <- as_tsibble(readRDS(file = "fc30_lr.rds")) %>% filter(.model == "mod1")
fc1_lr <- as_tsibble(readRDS(file = "fc1_lr.rds")) %>% filter(.model == "mod1")

fc20_lr <- fc20_lr %>% mutate(.mean = .mean * 2)
fc30_lr <- fc30_lr %>% mutate(.mean = .mean * 3)
fc1_lr <- fc1_lr %>% mutate(.mean = .mean * 6)

lr <- NULL

lr$k1 <- to_matrix_for_ts(fc10_lr, ".mean")
lr$k1 <- ts(lr$k1, frequency = 6, start = c(47304, 1))
lr$k2 <- to_matrix_for_ts(fc20_lr, ".mean")
lr$k2 <- ts(lr$k2, frequency = 3, start = c(47304, 1))
lr$k3 <- to_matrix_for_ts(fc30_lr, ".mean")
lr$k3 <- ts(lr$k3, frequency = 2, start = c(47304, 1))
lr$k6 <- to_matrix_for_ts(fc1_lr, ".mean")
lr$k6 <- ts(lr$k6, frequency = 1, start = c(47304, 1))

lr <- t(do.call(rbind, rev(lr)))

#### LIGHTGBM - extract BU ####
# forecasts linear regression
fc10_gb_bu <- as_tsibble(readRDS(file = "fc10_gb.rds")) %>% filter(.model == "bu_mod1")
fc20_gb_bu <- as_tsibble(readRDS(file = "fc20_gb.rds")) %>% filter(.model == "bu_mod1")
fc30_gb_bu <- as_tsibble(readRDS(file = "fc30_gb.rds")) %>% filter(.model == "bu_mod1")
fc1_gb_bu <- as_tsibble(readRDS(file = "fc1_gb.rds")) %>% filter(.model == "bu_mod1")

fc20_gb_bu <- fc20_gb_bu %>% mutate(.mean = .mean * 2)
fc30_gb_bu <- fc30_gb_bu %>% mutate(.mean = .mean * 3)
fc1_gb_bu <- fc1_gb_bu %>% mutate(.mean = .mean * 6)

gb_bu <- NULL

gb_bu$k1 <- to_matrix_for_ts(fc10_gb_bu, ".mean")
gb_bu$k1 <- ts(gb_bu$k1, frequency = 6, start = c(47304, 1))
gb_bu$k2 <- to_matrix_for_ts(fc20_gb_bu, ".mean")
gb_bu$k2 <- ts(gb_bu$k2, frequency = 3, start = c(47304, 1))
gb_bu$k3 <- to_matrix_for_ts(fc30_gb_bu, ".mean")
gb_bu$k3 <- ts(gb_bu$k3, frequency = 2, start = c(47304, 1))
gb_bu$k6 <- to_matrix_for_ts(fc1_gb_bu, ".mean")
gb_bu$k6 <- ts(gb_bu$k6, frequency = 1, start = c(47304, 1))

gb_bu <- t(do.call(rbind, rev(gb_bu)))


#### LIGHTGBM - extract TD ####
# forecasts linear regression

fc10_gb_td <- as_tsibble(readRDS(file = "fc10_gb.rds")) %>% filter(.model == "td_mod1")
fc20_gb_td <- as_tsibble(readRDS(file = "fc20_gb.rds")) %>% filter(.model == "td_mod1")
fc30_gb_td <- as_tsibble(readRDS(file = "fc30_gb.rds")) %>% filter(.model == "td_mod1")
fc1_gb_td <- as_tsibble(readRDS(file = "fc1_gb.rds")) %>% filter(.model == "td_mod1")

fc20_gb_td <- fc20_gb_td %>% mutate(.mean = .mean * 2)
fc30_gb_td <- fc30_gb_td %>% mutate(.mean = .mean * 3)
fc1_gb_td <- fc1_gb_td %>% mutate(.mean = .mean * 6)

gb_td <- NULL

gb_td$k1 <- to_matrix_for_ts(fc10_gb_td, ".mean")
gb_td$k1 <- ts(gb_td$k1, frequency = 6, start = c(47304, 1))
gb_td$k2 <- to_matrix_for_ts(fc20_gb_td, ".mean")
gb_td$k2 <- ts(gb_td$k2, frequency = 3, start = c(47304, 1))
gb_td$k3 <- to_matrix_for_ts(fc30_gb_td, ".mean")
gb_td$k3 <- ts(gb_td$k3, frequency = 2, start = c(47304, 1))
gb_td$k6 <- to_matrix_for_ts(fc1_gb_td, ".mean")
gb_td$k6 <- ts(gb_td$k6, frequency = 1, start = c(47304, 1))

gb_td <- t(do.call(rbind, rev(gb_td)))


#### LIGHTGBM - extract MO ####
fc10_gb_mo <- as_tsibble(readRDS(file = "fc10_gb.rds")) %>% filter(.model == "mo_mod1")
fc20_gb_mo <- as_tsibble(readRDS(file = "fc20_gb.rds")) %>% filter(.model == "mo_mod1")
fc30_gb_mo <- as_tsibble(readRDS(file = "fc30_gb.rds")) %>% filter(.model == "mo_mod1")
fc1_gb_mo <- as_tsibble(readRDS(file = "fc1_gb.rds")) %>% filter(.model == "mo_mod1")

fc20_gb_mo <- fc20_gb_mo %>% mutate(.mean = .mean * 2)
fc30_gb_mo <- fc30_gb_mo %>% mutate(.mean = .mean * 3)
fc1_gb_mo <- fc1_gb_mo %>% mutate(.mean = .mean * 6)

gb_mo <- NULL

gb_mo$k1 <- to_matrix_for_ts(fc10_gb_mo, ".mean")
gb_mo$k1 <- ts(gb_mo$k1, frequency = 6, start = c(47304, 1))
gb_mo$k2 <- to_matrix_for_ts(fc20_gb_mo, ".mean")
gb_mo$k2 <- ts(gb_mo$k2, frequency = 3, start = c(47304, 1))
gb_mo$k3 <- to_matrix_for_ts(fc30_gb_mo, ".mean")
gb_mo$k3 <- ts(gb_mo$k3, frequency = 2, start = c(47304, 1))
gb_mo$k6 <- to_matrix_for_ts(fc1_gb_mo, ".mean")
gb_mo$k6 <- ts(gb_mo$k6, frequency = 1, start = c(47304, 1))

gb_mo <- t(do.call(rbind, rev(gb_mo)))

#### LIGHTGBM - extract OLS ####
fc10_gb_ols <- as_tsibble(readRDS(file = "fc10_gb.rds")) %>% filter(.model == "ols_mod1")
fc20_gb_ols <- as_tsibble(readRDS(file = "fc20_gb.rds")) %>% filter(.model == "ols_mod1")
fc30_gb_ols <- as_tsibble(readRDS(file = "fc30_gb.rds")) %>% filter(.model == "ols_mod1")
fc1_gb_ols <- as_tsibble(readRDS(file = "fc1_gb.rds")) %>% filter(.model == "ols_mod1")

fc20_gb_ols <- fc20_gb_ols %>% mutate(.mean = .mean * 2)
fc30_gb_ols <- fc30_gb_ols %>% mutate(.mean = .mean * 3)
fc1_gb_ols <- fc1_gb_ols %>% mutate(.mean = .mean * 6)

gb_ols <- NULL

gb_ols$k1 <- to_matrix_for_ts(fc10_gb_ols, ".mean")
gb_ols$k1 <- ts(gb_ols$k1, frequency = 6, start = c(47304, 1))
gb_ols$k2 <- to_matrix_for_ts(fc20_gb_ols, ".mean")
gb_ols$k2 <- ts(gb_ols$k2, frequency = 3, start = c(47304, 1))
gb_ols$k3 <- to_matrix_for_ts(fc30_gb_ols, ".mean")
gb_ols$k3 <- ts(gb_ols$k3, frequency = 2, start = c(47304, 1))
gb_ols$k6 <- to_matrix_for_ts(fc1_gb_ols, ".mean")
gb_ols$k6 <- ts(gb_ols$k6, frequency = 1, start = c(47304, 1))

gb_ols <- t(do.call(rbind, rev(gb_ols)))


#### LIGHTGBM - extract MinT-shr ####
fc10_gb_mint <- as_tsibble(readRDS(file = "fc10_gb.rds")) %>% filter(.model == "mint_mod1")
fc20_gb_mint <- as_tsibble(readRDS(file = "fc20_gb.rds")) %>% filter(.model == "mint_mod1")
fc30_gb_mint <- as_tsibble(readRDS(file = "fc30_gb.rds")) %>% filter(.model == "mint_mod1")
fc1_gb_mint <- as_tsibble(readRDS(file = "fc1_gb.rds")) %>% filter(.model == "mint_mod1")

fc20_gb_mint <- fc20_gb_mint %>% mutate(.mean = .mean * 2)
fc30_gb_mint <- fc30_gb_mint %>% mutate(.mean = .mean * 3)
fc1_gb_mint <- fc1_gb_mint %>% mutate(.mean = .mean * 6)

gb_mint <- NULL

gb_mint$k1 <- to_matrix_for_ts(fc10_gb_mint, ".mean")
gb_mint$k1 <- ts(gb_mint$k1, frequency = 6, start = c(47304, 1))
gb_mint$k2 <- to_matrix_for_ts(fc20_gb_mint, ".mean")
gb_mint$k2 <- ts(gb_mint$k2, frequency = 3, start = c(47304, 1))
gb_mint$k3 <- to_matrix_for_ts(fc30_gb_mint, ".mean")
gb_mint$k3 <- ts(gb_mint$k3, frequency = 2, start = c(47304, 1))
gb_mint$k6 <- to_matrix_for_ts(fc1_gb_mint, ".mean")
gb_mint$k6 <- ts(gb_mint$k6, frequency = 1, start = c(47304, 1))

gb_mint <- t(do.call(rbind, rev(gb_mint)))

#### LIGHTGBM - extract unreconciled base forecasts ####
fc10_gb <- as_tsibble(readRDS(file = "fc10_gb.rds")) %>% filter(.model == "mod1")
fc20_gb <- as_tsibble(readRDS(file = "fc20_gb.rds")) %>% filter(.model == "mod1")
fc30_gb <- as_tsibble(readRDS(file = "fc30_gb.rds")) %>% filter(.model == "mod1")
fc1_gb <- as_tsibble(readRDS(file = "fc1_gb.rds")) %>% filter(.model == "mod1")

fc20_gb <- fc20_gb %>% mutate(.mean = .mean * 2)
fc30_gb <- fc30_gb %>% mutate(.mean = .mean * 3)
fc1_gb <- fc1_gb %>% mutate(.mean = .mean * 6)

gb <- NULL

gb$k1 <- to_matrix_for_ts(fc10_gb, ".mean")
gb$k1 <- ts(gb$k1, frequency = 6, start = c(47304, 1))
gb$k2 <- to_matrix_for_ts(fc20_gb, ".mean")
gb$k2 <- ts(gb$k2, frequency = 3, start = c(47304, 1))
gb$k3 <- to_matrix_for_ts(fc30_gb, ".mean")
gb$k3 <- ts(gb$k3, frequency = 2, start = c(47304, 1))
gb$k6 <- to_matrix_for_ts(fc1_gb, ".mean")
gb$k6 <- ts(gb$k6, frequency = 1, start = c(47304, 1))

gb <- t(do.call(rbind, rev(gb)))

#### NAIVE BENCHMARK ####
fc10_benchmark <- as_tsibble(readRDS(file = "fc10_benchmark.rds"))
fc20_benchmark <- as_tsibble(readRDS(file = "fc20_benchmark.rds"))
fc30_benchmark <- as_tsibble(readRDS(file = "fc30_benchmark.rds"))
fc1_benchmark <- as_tsibble(readRDS(file = "fc1_benchmark.rds"))

fc20_benchmark <- fc20_benchmark %>% mutate(.mean = .mean * 2)
fc30_benchmark <- fc30_benchmark %>% mutate(.mean = .mean * 3)
fc1_benchmark <- fc1_benchmark %>% mutate(.mean = .mean * 6)

benchmark <- NULL

benchmark$k1 <- to_matrix_for_ts(fc10_benchmark, ".mean")
benchmark$k1 <- ts(benchmark$k1, frequency = 6, start = c(47304, 1))
benchmark$k2 <- to_matrix_for_ts(fc20_benchmark, ".mean")
benchmark$k2 <- ts(benchmark$k2, frequency = 3, start = c(47304, 1))
benchmark$k3 <- to_matrix_for_ts(fc30_benchmark, ".mean")
benchmark$k3 <- ts(benchmark$k3, frequency = 2, start = c(47304, 1))
benchmark$k6 <- to_matrix_for_ts(fc1_benchmark, ".mean")
benchmark$k6 <- ts(benchmark$k6, frequency = 1, start = c(47304, 1))

benchmark <- t(do.call(rbind, rev(benchmark)))

#### Import Test Data ####
# observations (for test data)
min10 <- readRDS(file = "min10.rds")
min20 <- readRDS(file = "min20.rds")
min30 <- readRDS(file = "min30.rds")
hr1 <- readRDS(file = "hr1.rds")

min20 <- min20 %>% mutate(Power = Power * 2)
min30 <- min30 %>% mutate(Power = Power * 3)
hr1 <- hr1 %>% mutate(Power = Power * 6)

test <- NULL
values <- NULL

TrainingProportion = 0.9

values$k1 <- to_matrix_for_ts(min10, "Power")
values$k1 <- ts(values$k1, frequency = 6)
values$k2 <- to_matrix_for_ts(min20, "Power")
values$k2 <- ts(values$k2, frequency = 3)
values$k3 <- to_matrix_for_ts(min30, "Power")
values$k3 <- ts(values$k3, frequency = 2)
values$k6 <- to_matrix_for_ts(hr1, "Power")
values$k6 <- ts(values$k6, frequency = 1)

test$k1 <- values$k1[-c(1:ceiling(dim(values$k1)[1]*TrainingProportion)), ]
test$k2 <- values$k2[-c(1:ceiling(dim(values$k2)[1]*TrainingProportion)), ]
test$k3 <- values$k3[-c(1:ceiling(dim(values$k3)[1]*TrainingProportion)), ]
test$k6 <- values$k6[-c(1:ceiling(dim(values$k6)[1]*TrainingProportion)), ]

test <- t(do.call(rbind, rev(test)))
obs <- values

#### Rename columns ####
kset <- c(6, 3, 2, 1)
h <- 876
colnames(benchmark) <- paste("k", rep(kset, h * rev(kset)), "_h",
                        do.call("c", as.list(sapply(
                          rev(kset) * h,
                          function(x) seq(1:x)))),
                        sep = "")


colnames(test) <- paste("k", rep(kset, h * rev(kset)), "_h",
                        do.call("c", as.list(sapply(
                          rev(kset) * h,
                          function(x) seq(1:x)))),
                        sep = "")

colnames(lr) <- paste("k", rep(kset, h * rev(kset)), "_h",
                        do.call("c", as.list(sapply(
                          rev(kset) * h,
                          function(x) seq(1:x)))),
                        sep = "")

colnames(lr_bu) <- paste("k", rep(kset, h * rev(kset)), "_h",
                      do.call("c", as.list(sapply(
                        rev(kset) * h,
                        function(x) seq(1:x)))),
                      sep = "")

colnames(lr_td) <- paste("k", rep(kset, h * rev(kset)), "_h",
                         do.call("c", as.list(sapply(
                           rev(kset) * h,
                           function(x) seq(1:x)))),
                         sep = "")

colnames(lr_mo) <- paste("k", rep(kset, h * rev(kset)), "_h",
                         do.call("c", as.list(sapply(
                           rev(kset) * h,
                           function(x) seq(1:x)))),
                         sep = "")

colnames(lr_ols) <- paste("k", rep(kset, h * rev(kset)), "_h",
                         do.call("c", as.list(sapply(
                           rev(kset) * h,
                           function(x) seq(1:x)))),
                         sep = "")

colnames(lr_mint) <- paste("k", rep(kset, h * rev(kset)), "_h",
                         do.call("c", as.list(sapply(
                           rev(kset) * h,
                           function(x) seq(1:x)))),
                         sep = "")

colnames(gb) <- paste("k", rep(kset, h * rev(kset)), "_h",
                      do.call("c", as.list(sapply(
                        rev(kset) * h,
                        function(x) seq(1:x)))),
                      sep = "")

colnames(gb_bu) <- paste("k", rep(kset, h * rev(kset)), "_h",
                         do.call("c", as.list(sapply(
                           rev(kset) * h,
                           function(x) seq(1:x)))),
                         sep = "")

colnames(gb_td) <- paste("k", rep(kset, h * rev(kset)), "_h",
                         do.call("c", as.list(sapply(
                           rev(kset) * h,
                           function(x) seq(1:x)))),
                         sep = "")

colnames(gb_mo) <- paste("k", rep(kset, h * rev(kset)), "_h",
                         do.call("c", as.list(sapply(
                           rev(kset) * h,
                           function(x) seq(1:x)))),
                         sep = "")

colnames(gb_ols) <- paste("k", rep(kset, h * rev(kset)), "_h",
                          do.call("c", as.list(sapply(
                            rev(kset) * h,
                            function(x) seq(1:x)))),
                          sep = "")

colnames(gb_mint) <- paste("k", rep(kset, h * rev(kset)), "_h",
                           do.call("c", as.list(sapply(
                             rev(kset) * h,
                             function(x) seq(1:x)))),
                           sep = "")

#### Compute errors of benchmark forecast (1 step ahead naive) ####
naive_error <- abs(benchmark - test)
naive_error_squared <- abs(benchmark - test)^2

id <- which(simplify2array(strsplit(colnames(benchmark), split = "_"))[1, ] == "k1")
naive_error_10 <- colMeans(t(naive_error[, id]))
naive_error_10_squared <- colMeans(t(naive_error_squared[, id]))

id <- which(simplify2array(strsplit(colnames(benchmark), split = "_"))[1, ] == "k2")
naive_error_20 <- colMeans(t(naive_error[, id]))
naive_error_20_squared <- colMeans(t(naive_error_squared[, id]))

id <- which(simplify2array(strsplit(colnames(benchmark), split = "_"))[1, ] == "k3")
naive_error_30 <- colMeans(t(naive_error[, id]))
naive_error_30_squared <- colMeans(t(naive_error_squared[, id]))

id <- which(simplify2array(strsplit(colnames(benchmark), split = "_"))[1, ] == "k6")
naive_error_1 <- colMeans(t(naive_error[, id]))
naive_error_1_squared <- colMeans(t(naive_error_squared[, id]))

#### Compute rMAE and rRMSE of unreconciled LR forecast ####
lr_error <- abs(lr - test)
lr_error_squared <- (lr - test)^2

rMAE_lr <- compute_rMAE(lr_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_lr <- compute_rRMSE(lr_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_lr[1],"rMAE_lr.csv", row.names = TRUE)
write.csv(rRMSE_lr[1],"rRMSE_lr.csv", row.names = TRUE)
write.csv(c(rMAE_lr[[2]],rMAE_lr[[3]],rMAE_lr[[4]],rMAE_lr[[5]]), "rMAE_lr_nemenyi.csv")
write.csv(c(rRMSE_lr[[2]],rRMSE_lr[[3]],rRMSE_lr[[4]],rRMSE_lr[[5]]), "rRMSE_lr_nemenyi.csv")

#### Compute rMAE and rRMSE of LR_bu forecast ####
lr_bu_error <- abs(lr_bu - test)
lr_bu_error_squared <- abs(lr_bu - test)^2

rMAE_lr_bu <- compute_rMAE(lr_bu_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_lr_bu <- compute_rRMSE(lr_bu_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_lr_bu[1],"rMAE_lr_bu.csv", row.names = TRUE)
write.csv(rRMSE_lr_bu[1],"rRMSE_lr_bu.csv", row.names = TRUE)
write.csv(c(rMAE_lr_bu[[2]],rMAE_lr_bu[[3]],rMAE_lr_bu[[4]],rMAE_lr_bu[[5]]), "rMAE_lr_bu_nemenyi.csv")
write.csv(c(rRMSE_lr_bu[[2]],rRMSE_lr_bu[[3]],rRMSE_lr_bu[[4]],rRMSE_lr_bu[[5]]), "rRMSE_lr_bu_nemenyi.csv")


#### Compute rMAE and rRMSE of LR_td forecast ####
lr_td_error <- abs(lr_td - test)
lr_td_error_squared <- abs(lr_td - test)^2

rMAE_lr_td <- compute_rMAE(lr_td_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_lr_td <- compute_rRMSE(lr_td_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_lr_td[1],"rMAE_lr_td.csv", row.names = TRUE)
write.csv(rRMSE_lr_td[1],"rRMSE_lr_td.csv", row.names = TRUE)
write.csv(c(rMAE_lr_td[[2]],rMAE_lr_td[[3]],rMAE_lr_td[[4]],rMAE_lr_td[[5]]), "rMAE_lr_td_nemenyi.csv")
write.csv(c(rRMSE_lr_td[[2]],rRMSE_lr_td[[3]],rRMSE_lr_td[[4]],rRMSE_lr_td[[5]]), "rRMSE_lr_td_nemenyi.csv")

#### Compute rMAE and rRMSE of LR_mo forecast ####
lr_mo_error <- abs(lr_mo - test)
lr_mo_error_squared <- abs(lr_mo - test)^2

rMAE_lr_mo <- compute_rMAE(lr_mo_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_lr_mo <- compute_rRMSE(lr_mo_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_lr_mo[1],"rMAE_lr_mo.csv", row.names = TRUE)
write.csv(rRMSE_lr_mo[1],"rRMSE_lr_mo.csv", row.names = TRUE)
write.csv(c(rMAE_lr_mo[[2]],rMAE_lr_mo[[3]],rMAE_lr_mo[[4]],rMAE_lr_mo[[5]]), "rMAE_lr_mo_nemenyi.csv")
write.csv(c(rRMSE_lr_mo[[2]],rRMSE_lr_mo[[3]],rRMSE_lr_mo[[4]],rRMSE_lr_mo[[5]]), "rRMSE_lr_mo_nemenyi.csv")

#### Compute rMAE and rRMSE of LR_ols forecast ####
lr_ols_error <- abs(lr_ols - test)
lr_ols_error_squared <- abs(lr_ols - test)^2

rMAE_lr_ols <- compute_rMAE(lr_ols_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_lr_ols <- compute_rRMSE(lr_ols_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_lr_ols[1],"rMAE_lr_ols.csv", row.names = TRUE)
write.csv(rRMSE_lr_ols[1],"rRMSE_lr_ols.csv", row.names = TRUE)
write.csv(c(rMAE_lr_ols[[2]],rMAE_lr_ols[[3]],rMAE_lr_ols[[4]],rMAE_lr_ols[[5]]), "rMAE_lr_ols_nemenyi.csv")
write.csv(c(rRMSE_lr_ols[[2]],rRMSE_lr_ols[[3]],rRMSE_lr_ols[[4]],rRMSE_lr_ols[[5]]), "rRMSE_lr_ols_nemenyi.csv")

#### Compute rMAE and rRMSE of LR_mint forecast ####
lr_mint_error <- abs(lr_mint - test)
lr_mint_error_squared <- abs(lr_mint - test)^2

rMAE_lr_mint <- compute_rMAE(lr_mint_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_lr_mint <- compute_rRMSE(lr_mint_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_lr_mint[1],"rMAE_lr_mint.csv", row.names = TRUE)
write.csv(rRMSE_lr_mint[1],"rRMSE_lr_mint.csv", row.names = TRUE)
write.csv(c(rMAE_lr_mint[[2]],rMAE_lr_mint[[3]],rMAE_lr_mint[[4]],rMAE_lr_mint[[5]]), "rMAE_lr_mint_nemenyi.csv")
write.csv(c(rRMSE_lr_mint[[2]],rRMSE_lr_mint[[3]],rRMSE_lr_mint[[4]],rRMSE_lr_mint[[5]]), "rRMSE_lr_mint_nemenyi.csv")

#### Compute rMAE and rRMSE of unreconciled gb forecast ####
gb_error <- abs(gb - test)
gb_error_squared <- abs(gb - test)^2

rMAE_gb <- compute_rMAE(gb_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_gb <- compute_rRMSE(gb_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_gb[1],"rMAE_gb.csv", row.names = TRUE)
write.csv(rRMSE_gb[1],"rRMSE_gb.csv", row.names = TRUE)
write.csv(c(rMAE_gb[[2]],rMAE_gb[[3]],rMAE_gb[[4]],rMAE_gb[[5]]), "rMAE_gb_nemenyi.csv")
write.csv(c(rRMSE_gb[[2]],rRMSE_gb[[3]],rRMSE_gb[[4]],rRMSE_gb[[5]]), "rRMSE_gb_nemenyi.csv")


#### Compute rMAE and rRMSE of gb_bu forecast ####
gb_bu_error <- abs(gb_bu - test)
gb_bu_error_squared <- abs(gb_bu - test)^2

rMAE_gb_bu <- compute_rMAE(gb_bu_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_gb_bu <- compute_rRMSE(gb_bu_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_gb_bu[1],"rMAE_gb_bu.csv", row.names = TRUE)
write.csv(rRMSE_gb_bu[1],"rRMSE_gb_bu.csv", row.names = TRUE)
write.csv(c(rMAE_gb_bu[[2]],rMAE_gb_bu[[3]],rMAE_gb_bu[[4]],rMAE_gb_bu[[5]]), "rMAE_gb_bu_nemenyi.csv")
write.csv(c(rRMSE_gb_bu[[2]],rRMSE_gb_bu[[3]],rRMSE_gb_bu[[4]],rRMSE_gb_bu[[5]]), "rRMSE_gb_bu_nemenyi.csv")


#### Compute rMAE and rRMSE of gb_td forecast ####
gb_td_error <- abs(gb_td - test)
gb_td_error_squared <- abs(gb_td - test)^2

rMAE_gb_td <- compute_rMAE(gb_td_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_gb_td <- compute_rRMSE(gb_td_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_gb_td[1],"rMAE_gb_td.csv", row.names = TRUE)
write.csv(rRMSE_gb_td[1],"rRMSE_gb_td.csv", row.names = TRUE)
write.csv(c(rMAE_gb_td[[2]],rMAE_gb_td[[3]],rMAE_gb_td[[4]],rMAE_gb_td[[5]]), "rMAE_gb_td_nemenyi.csv")
write.csv(c(rRMSE_gb_td[[2]],rRMSE_gb_td[[3]],rRMSE_gb_td[[4]],rRMSE_gb_td[[5]]), "rRMSE_gb_td_nemenyi.csv")

#### Compute rMAE and rRMSE of gb_mo forecast ####
gb_mo_error <- abs(gb_mo - test)
gb_mo_error_squared <- abs(gb_mo - test)^2

rMAE_gb_mo <- compute_rMAE(gb_mo_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_gb_mo <- compute_rRMSE(gb_mo_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_gb_mo[1],"rMAE_gb_mo.csv", row.names = TRUE)
write.csv(rRMSE_gb_mo[1],"rRMSE_gb_mo.csv", row.names = TRUE)
write.csv(c(rMAE_gb_mo[[2]],rMAE_gb_mo[[3]],rMAE_gb_mo[[4]],rMAE_gb_mo[[5]]), "rMAE_gb_mo_nemenyi.csv")
write.csv(c(rRMSE_gb_mo[[2]],rRMSE_gb_mo[[3]],rRMSE_gb_mo[[4]],rRMSE_gb_mo[[5]]), "rRMSE_gb_mo_nemenyi.csv")

#### Compute rMAE and rRMSE of gb_ols forecast ####
gb_ols_error <- abs(gb_ols - test)
gb_ols_error_squared <- abs(gb_ols - test)^2

rMAE_gb_ols <- compute_rMAE(gb_ols_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_gb_ols <- compute_rRMSE(gb_ols_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_gb_ols[1],"rMAE_gb_ols.csv", row.names = TRUE)
write.csv(rRMSE_gb_ols[1],"rRMSE_gb_ols.csv", row.names = TRUE)
write.csv(c(rMAE_gb_ols[[2]],rMAE_gb_ols[[3]],rMAE_gb_ols[[4]],rMAE_gb_ols[[5]]), "rMAE_gb_ols_nemenyi.csv")
write.csv(c(rRMSE_gb_ols[[2]],rRMSE_gb_ols[[3]],rRMSE_gb_ols[[4]],rRMSE_gb_ols[[5]]), "rRMSE_gb_ols_nemenyi.csv")

#### Compute rMAE and rRMSE of gb_mint forecast ####
gb_mint_error <- abs(gb_mint - test)
gb_mint_error_squared <- abs(gb_mint - test)^2

rMAE_gb_mint <- compute_rMAE(gb_mint_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_gb_mint <- compute_rRMSE(gb_mint_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_gb_mint[1],"rMAE_gb_mint.csv", row.names = TRUE)
write.csv(rRMSE_gb_mint[1],"rRMSE_gb_mint.csv", row.names = TRUE)
write.csv(c(rMAE_gb_mint[[2]],rMAE_gb_mint[[3]],rMAE_gb_mint[[4]],rMAE_gb_mint[[5]]), "rMAE_gb_mint_nemenyi.csv")
write.csv(c(rRMSE_gb_mint[[2]],rRMSE_gb_mint[[3]],rRMSE_gb_mint[[4]],rRMSE_gb_mint[[5]]), "rRMSE_gb_mint_nemenyi.csv")

#### prep for LR cross-temporal reconciliation ####

# residuals linear regression
fc10_lr_residuals <- readRDS(file = "fc10_lr_residuals.rds") %>% filter(.model == "mod1")
fc20_lr_residuals <- readRDS(file = "fc20_lr_residuals.rds") %>% filter(.model == "mod1")
fc30_lr_residuals <- readRDS(file = "fc30_lr_residuals.rds") %>% filter(.model == "mod1")
fc1_lr_residuals <- readRDS(file = "fc1_lr_residuals.rds") %>% filter(.model == "mod1")

#### change means to sums for temporal hierarchy ####
fc20_lr_residuals <- fc20_lr_residuals %>% mutate(.resid = .resid * 2)
fc30_lr_residuals <- fc30_lr_residuals %>% mutate(.resid = .resid * 3)
fc1_lr_residuals <- fc1_lr_residuals %>% mutate(.resid = .resid * 6)

base_lr <- NULL
residuals <- NULL

residuals$k1 <- to_matrix_for_ts(fc10_lr_residuals, ".resid")
residuals$k1 <- ts(residuals$k1, frequency = 6)
residuals$k2 <- to_matrix_for_ts(fc20_lr_residuals, ".resid")
residuals$k2 <- ts(residuals$k2, frequency = 3)
residuals$k3 <- to_matrix_for_ts(fc30_lr_residuals, ".resid")
residuals$k3 <- ts(residuals$k3, frequency = 2)
residuals$k6 <- to_matrix_for_ts(fc1_lr_residuals, ".resid")
residuals$k6 <- ts(residuals$k6, frequency = 1)

base_lr <- lr
res_lr <- t(do.call(rbind, rev(residuals)))

## Balanced hierarchy
#                 T
#    |--------|-------|-------|
#    A        B       C       D
#  |---|    |---|   |---|   |---|
# A1...A7  B1...B9 C1...C3 D1...D6
# Names of the bottom level variables
data_bts <- data.frame(X1 = c("A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B", "B", "B", "B", "C", "C", "C", "D", "D", "D", "D", "D", "D"),
                       X2 = c("1", "2", "3", "4", "5", "6", "7", "1", "2", "3", "4", "5", "6", "7", "8", "9", "1", "2", "3", "1", "2", "3", "4", "5", "6"),
                       stringsAsFactors = FALSE)
# Cross-sectional aggregation matrix
C <- Cmatrix(~ X1 / X2, data_bts, sep = "")

FoReco_data <- list(base = base_lr,
                    test = test,
                    res = res_lr,
                    C = C,
                    obs = obs)

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

# Compute rMAE and rRMSE
thf_recf_error <- abs(thf_recf - test)
thf_recf_error_squared <- abs(thf_recf - test)^2

rMAE_thf_recf <- compute_rMAE(thf_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_thf_recf <- compute_rRMSE(thf_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_thf_recf[1],"rMAE_thf_recf.csv", row.names = TRUE)
write.csv(rRMSE_thf_recf[1],"rRMSE_thf_recf.csv", row.names = TRUE)
write.csv(c(rMAE_thf_recf[[2]],rMAE_thf_recf[[3]],rMAE_thf_recf[[4]],rMAE_thf_recf[[5]]), "nemenyi_rMAE_thf_recf.csv")
write.csv(c(rRMSE_thf_recf[[2]],rRMSE_thf_recf[[3]],rRMSE_thf_recf[[4]],rRMSE_thf_recf[[5]]), "nemenyi_rRMSE_thf_recf.csv")

#### Heuristic first-temporal-then-cross-sectional cross-temporal reconciliation using t-wls + cs-shr (Kourentzes and Athanasopoulos, 2019) ####

tcs_recf <- tcsrec(FoReco_data$base, m = 6, C = FoReco_data$C,
                   thf_comb = "wlsv", hts_comb = "shr",
                   res = FoReco_data$res)$recf

# have to set this since the FoReco package has a bug that changes column names for some reason
colnames(tcs_recf) <- colnames(thf_recf)

# Compute rMAE and rRMSE
tcs_recf_error <- abs(tcs_recf - test)
tcs_recf_error_squared <- abs(tcs_recf - test)^2

rMAE_tcs_recf <- compute_rMAE(tcs_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_tcs_recf <- compute_rRMSE(tcs_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_tcs_recf[1],"rMAE_tcs_recf.csv", row.names = TRUE)
write.csv(rRMSE_tcs_recf[1],"rRMSE_tcs_recf.csv", row.names = TRUE)
write.csv(c(rMAE_tcs_recf[[2]],rMAE_tcs_recf[[3]],rMAE_tcs_recf[[4]],rMAE_tcs_recf[[5]]), "nemenyi_rMAE_tcs_recf.csv")
write.csv(c(rRMSE_tcs_recf[[2]],rRMSE_tcs_recf[[3]],rRMSE_tcs_recf[[4]],rRMSE_tcs_recf[[5]]), "nemenyi_rRMSE_tcs_recf.csv")

#### Heuristic first-cross-sectional-then-temporal cross-temporal reconciliation using t-acov + cs-shr (Di Fonzo and Girolimetto, 2020) ####

cst_recf <- as.matrix(cstrec(FoReco_data$base, m = 6, C = FoReco_data$C,
                   thf_comb = "acov", hts_comb = "shr",
                   res = FoReco_data$res)$recf)

# have to set this since the FoReco package has a bug that changes column names for some reason
colnames(cst_recf) <- colnames(thf_recf)

# Compute rMAE and rRMSE
cst_recf_error <- abs(cst_recf - test)
cst_recf_error_squared <- abs(cst_recf - test)^2

rMAE_cst_recf <- compute_rMAE(cst_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_cst_recf <- compute_rRMSE(cst_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_cst_recf[1],"rMAE_cst_recf.csv", row.names = TRUE)
write.csv(rRMSE_cst_recf[1],"rRMSE_cst_recf.csv", row.names = TRUE)
write.csv(c(rMAE_cst_recf[[2]],rMAE_cst_recf[[3]],rMAE_cst_recf[[4]],rMAE_cst_recf[[5]]), "nemenyi_rMAE_cst_recf.csv")
write.csv(c(rRMSE_cst_recf[[2]],rRMSE_cst_recf[[3]],rRMSE_cst_recf[[4]],rRMSE_cst_recf[[5]]), "nemenyi_rRMSE_cst_recf.csv")

#### Iterative cross-temporal reconciliation (Di Fonzo and Girolimetto, 2020) ####

ite_recf <- iterec(FoReco_data$base,
                   m = 6, C = FoReco_data$C,
                   thf_comb = "acov", hts_comb = "shr",
                   res = FoReco_data$res, start_rec = "thf")$recf

# have to set this since the FoReco package has a bug that changes column names for some reason
colnames(ite_recf) <- colnames(thf_recf)

# Compute rMAE and rRMSE
ite_recf_error <- abs(ite_recf - test)
ite_recf_error_squared <- abs(ite_recf - test)^2

rMAE_ite_recf <- compute_rMAE(ite_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_ite_recf <- compute_rRMSE(ite_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_ite_recf[1],"rMAE_ite_recf.csv", row.names = TRUE)
write.csv(rRMSE_ite_recf[1],"rRMSE_ite_recf.csv", row.names = TRUE)
write.csv(c(rMAE_ite_recf[[2]],rMAE_ite_recf[[3]],rMAE_ite_recf[[4]],rMAE_ite_recf[[5]]), "nemenyi_rMAE_ite_recf.csv")
write.csv(c(rRMSE_ite_recf[[2]],rRMSE_ite_recf[[3]],rRMSE_ite_recf[[4]],rRMSE_ite_recf[[5]]), "nemenyi_rRMSE_ite_recf.csv")

#### Optimal cross-temporal reconciliation using bdshr (Di Fonzo and Girolimetto, 2020) ####
oct_recf <- octrec(FoReco_data$base, m = 6, C = FoReco_data$C,
                   comb = "bdshr", res = FoReco_data$res)$recf
# have to set this since the FoReco package has a bug that changes column names for some reason
colnames(oct_recf) <- colnames(thf_recf)

# Compute rMAE and rRMSE
oct_recf_error <- abs(oct_recf - test)
oct_recf_error_squared <- abs(oct_recf - test)^2

rMAE_oct_recf <- compute_rMAE(oct_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_oct_recf <- compute_rRMSE(oct_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_oct_recf[1],"rMAE_oct_recf.csv", row.names = TRUE)
write.csv(rRMSE_oct_recf[1],"rRMSE_oct_recf.csv", row.names = TRUE)
write.csv(c(rMAE_oct_recf[[2]],rMAE_oct_recf[[3]],rMAE_oct_recf[[4]],rMAE_oct_recf[[5]]), "nemenyi_rMAE_oct_recf.csv")
write.csv(c(rRMSE_oct_recf[[2]],rRMSE_oct_recf[[3]],rRMSE_oct_recf[[4]],rRMSE_oct_recf[[5]]), "nemenyi_rRMSE_oct_recf.csv")

#### Bottom up cross-temporal reconciliation ####
id <- which(simplify2array(strsplit(colnames(FoReco_data$base),
                                    split = "_"))[1, ] == "k1")
hfbts <- FoReco_data$base[-c(1:5), id]
obj <- ctbu(Bmat = hfbts, m = 6, C = FoReco_data$C)
rownames(obj) <- rownames(FoReco_data$base)
# have to set this since the FoReco package has a bug that changes column names for some reason
colnames(obj) <- colnames(thf_recf)

# Compute rMAE and rRMSE
obj_error <- abs(obj- test)
obj_error_squared <- abs(obj - test)^2

rMAE_bu_ct <- compute_rMAE(obj_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_bu_ct <- compute_rRMSE(obj_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_bu_ct,"rMAE_bu_ct.csv", row.names = TRUE)
write.csv(rRMSE_bu_ct,"rRMSE_bu_ct.csv", row.names = TRUE)

#### prep for LightGBM cross-temporal reconciliation ####

# residuals linear regression
fc10_gb_residuals <- readRDS(file = "fc10_gb_residuals.rds") %>% filter(.model == "mod1")
fc20_gb_residuals <- readRDS(file = "fc20_gb_residuals.rds") %>% filter(.model == "mod1")
fc30_gb_residuals <- readRDS(file = "fc30_gb_residuals.rds") %>% filter(.model == "mod1")
fc1_gb_residuals <- readRDS(file = "fc1_gb_residuals.rds") %>% filter(.model == "mod1")

#### change means to sums for temporal hierarchy ####
fc20_gb_residuals <- fc20_gb_residuals %>% mutate(.resid = .resid * 2)
fc30_gb_residuals <- fc30_gb_residuals %>% mutate(.resid = .resid * 3)
fc1_gb_residuals <- fc1_gb_residuals %>% mutate(.resid = .resid * 6)

base_gb <- NULL
residuals <- NULL

residuals$k1 <- to_matrix_for_ts(fc10_gb_residuals, ".resid")
residuals$k1 <- ts(residuals$k1, frequency = 6)
residuals$k2 <- to_matrix_for_ts(fc20_gb_residuals, ".resid")
residuals$k2 <- ts(residuals$k2, frequency = 3)
residuals$k3 <- to_matrix_for_ts(fc30_gb_residuals, ".resid")
residuals$k3 <- ts(residuals$k3, frequency = 2)
residuals$k6 <- to_matrix_for_ts(fc1_gb_residuals, ".resid")
residuals$k6 <- ts(residuals$k6, frequency = 1)

base_gb <- gb
res_gb <- t(do.call(rbind, rev(residuals)))

## Balanced hierarchy
#                 T
#    |--------|-------|-------|
#    A        B       C       D
#  |---|    |---|   |---|   |---|
# A1...A7  B1...B9 C1...C3 D1...D6
# Names of the bottom level variables
data_bts <- data.frame(X1 = c("A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B", "B", "B", "B", "C", "C", "C", "D", "D", "D", "D", "D", "D"),
                       X2 = c("1", "2", "3", "4", "5", "6", "7", "1", "2", "3", "4", "5", "6", "7", "8", "9", "1", "2", "3", "1", "2", "3", "4", "5", "6"),
                       stringsAsFactors = FALSE)
# Cross-sectional aggregation matrix
C <- Cmatrix(~ X1 / X2, data_bts, sep = "")

FoReco_data <- list(base = base_gb,
                    test = test,
                    res = res_gb,
                    C = C,
                    obs = obs)

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

# Compute rMAE_gb and rRMSE_gb
thf_recf_error <- abs(thf_recf - test)
thf_recf_error_squared <- abs(thf_recf - test)^2

rMAE_gb_thf_recf <- compute_rMAE(thf_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_gb_thf_recf <- compute_rRMSE(thf_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_gb_thf_recf[1],"rMAE_gb_thf_recf.csv", row.names = TRUE)
write.csv(rRMSE_gb_thf_recf[1],"rRMSE_gb_thf_recf.csv", row.names = TRUE)
write.csv(c(rMAE_gb_thf_recf[[2]],rMAE_gb_thf_recf[[3]],rMAE_gb_thf_recf[[4]],rMAE_gb_thf_recf[[5]]), "nemenyi_rMAE_gb_thf_recf.csv")
write.csv(c(rRMSE_gb_thf_recf[[2]],rRMSE_gb_thf_recf[[3]],rRMSE_gb_thf_recf[[4]],rRMSE_gb_thf_recf[[5]]), "nemenyi_rRMSE_gb_thf_recf.csv")

#### Heuristic first-temporal-then-cross-sectional cross-temporal reconciliation using t-wls + cs-shr (Kourentzes and Athanasopoulos, 2019) ####

tcs_recf <- tcsrec(FoReco_data$base, m = 6, C = FoReco_data$C,
                   thf_comb = "wlsv", hts_comb = "shr",
                   res = FoReco_data$res)$recf

# have to set this since the FoReco package has a bug that changes column names for some reason
colnames(tcs_recf) <- colnames(thf_recf)

# Compute rMAE_gb and rRMSE_gb
tcs_recf_error <- abs(tcs_recf - test)
tcs_recf_error_squared <- abs(tcs_recf - test)^2

rMAE_gb_tcs_recf <- compute_rMAE(tcs_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_gb_tcs_recf <- compute_rRMSE(tcs_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_gb_tcs_recf[1],"rMAE_gb_tcs_recf.csv", row.names = TRUE)
write.csv(rRMSE_gb_tcs_recf[1],"rRMSE_gb_tcs_recf.csv", row.names = TRUE)
write.csv(c(rMAE_gb_tcs_recf[[2]],rMAE_gb_tcs_recf[[3]],rMAE_gb_tcs_recf[[4]],rMAE_gb_tcs_recf[[5]]), "nemenyi_rMAE_gb_tcs_recf.csv")
write.csv(c(rRMSE_gb_tcs_recf[[2]],rRMSE_gb_tcs_recf[[3]],rRMSE_gb_tcs_recf[[4]],rRMSE_gb_tcs_recf[[5]]), "nemenyi_rRMSE_gb_tcs_recf.csv")

#### Heuristic first-cross-sectional-then-temporal cross-temporal reconciliation using t-acov + cs-shr (Di Fonzo and Girolimetto, 2020) ####

cst_recf <- as.matrix(cstrec(FoReco_data$base, m = 6, C = FoReco_data$C,
                             thf_comb = "acov", hts_comb = "shr",
                             res = FoReco_data$res)$recf)

# have to set this since the FoReco package has a bug that changes column names for some reason
colnames(cst_recf) <- colnames(thf_recf)

# Compute rMAE_gb and rRMSE_gb
cst_recf_error <- abs(cst_recf - test)
cst_recf_error_squared <- abs(cst_recf - test)^2

rMAE_gb_cst_recf <- compute_rMAE(cst_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_gb_cst_recf <- compute_rRMSE(cst_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_gb_cst_recf[1],"rMAE_gb_cst_recf.csv", row.names = TRUE)
write.csv(rRMSE_gb_cst_recf[1],"rRMSE_gb_cst_recf.csv", row.names = TRUE)
write.csv(c(rMAE_gb_cst_recf[[2]],rMAE_gb_cst_recf[[3]],rMAE_gb_cst_recf[[4]],rMAE_gb_cst_recf[[5]]), "nemenyi_rMAE_gb_cst_recf.csv")
write.csv(c(rRMSE_gb_cst_recf[[2]],rRMSE_gb_cst_recf[[3]],rRMSE_gb_cst_recf[[4]],rRMSE_gb_cst_recf[[5]]), "nemenyi_rRMSE_gb_cst_recf.csv")

#### Iterative cross-temporal reconciliation (Di Fonzo and Girolimetto, 2020) ####

ite_recf <- iterec(FoReco_data$base,
                   m = 6, C = FoReco_data$C,
                   thf_comb = "acov", hts_comb = "shr",
                   res = FoReco_data$res, start_rec = "thf")$recf

# have to set this since the FoReco package has a bug that changes column names for some reason
colnames(ite_recf) <- colnames(thf_recf)

# Compute rMAE_gb and rRMSE_gb
ite_recf_error <- abs(ite_recf - test)
ite_recf_error_squared <- abs(ite_recf - test)^2

rMAE_gb_ite_recf <- compute_rMAE(ite_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_gb_ite_recf <- compute_rRMSE(ite_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_gb_ite_recf[1],"rMAE_gb_ite_recf.csv", row.names = TRUE)
write.csv(rRMSE_gb_ite_recf[1],"rRMSE_gb_ite_recf.csv", row.names = TRUE)
write.csv(c(rMAE_gb_ite_recf[[2]],rMAE_gb_ite_recf[[3]],rMAE_gb_ite_recf[[4]],rMAE_gb_ite_recf[[5]]), "nemenyi_rMAE_gb_ite_recf.csv")
write.csv(c(rRMSE_gb_ite_recf[[2]],rRMSE_gb_ite_recf[[3]],rRMSE_gb_ite_recf[[4]],rRMSE_gb_ite_recf[[5]]), "nemenyi_rRMSE_gb_ite_recf.csv")

#### Optimal cross-temporal reconciliation using bdshr (Di Fonzo and Girolimetto, 2020) ####
oct_recf <- octrec(FoReco_data$base, m = 6, C = FoReco_data$C,
                   comb = "bdshr", res = FoReco_data$res)$recf
# have to set this since the FoReco package has a bug that changes column names for some reason
colnames(oct_recf) <- colnames(thf_recf)

# Compute rMAE_gb and rRMSE_gb
oct_recf_error <- abs(oct_recf - test)
oct_recf_error_squared <- abs(oct_recf - test)^2

rMAE_gb_oct_recf <- compute_rMAE(oct_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_gb_oct_recf <- compute_rRMSE(oct_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_gb_oct_recf[1],"rMAE_gb_oct_recf.csv", row.names = TRUE)
write.csv(rRMSE_gb_oct_recf[1],"rRMSE_gb_oct_recf.csv", row.names = TRUE)
write.csv(c(rMAE_gb_oct_recf[[2]],rMAE_gb_oct_recf[[3]],rMAE_gb_oct_recf[[4]],rMAE_gb_oct_recf[[5]]), "nemenyi_rMAE_gb_oct_recf.csv")
write.csv(c(rRMSE_gb_oct_recf[[2]],rRMSE_gb_oct_recf[[3]],rRMSE_gb_oct_recf[[4]],rRMSE_gb_oct_recf[[5]]), "nemenyi_rRMSE_gb_oct_recf.csv")

#### Bottom up cross-temporal reconciliation ####
id <- which(simplify2array(strsplit(colnames(FoReco_data$base),
                                    split = "_"))[1, ] == "k1")
hfbts <- FoReco_data$base[-c(1:5), id]
obj <- ctbu(Bmat = hfbts, m = 6, C = FoReco_data$C)
rownames(obj) <- rownames(FoReco_data$base)
# have to set this since the FoReco package has a bug that changes column names for some reason
colnames(obj) <- colnames(thf_recf)

# Compute rMAE_gb and rRMSE_gb
obj_error <- abs(obj - test)
obj_error_squared <- abs(obj - test)^2

rMAE_gb_bu_ct <- compute_rMAE(obj_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_gb_bu_ct <- compute_rRMSE(obj_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_gb_bu_ct,"rMAE_gb_bu_ct.csv", row.names = TRUE)
write.csv(rRMSE_gb_bu_ct,"rRMSE_gb_bu_ct.csv", row.names = TRUE)

#### Nemenyi Test ####
library(tsutils)

# cross-sec

nem_cross_sec <- read_excel("Nemenyi-Cross-Sectional.xlsx")


x_rMAE <- as.matrix(nem_cross_sec[,1:12])
x_rRMSE <- as.matrix(nem_cross_sec[,13:24])

colnames(x_rMAE) <- colnames(nem_cross_sec[1:12])
colnames(x_rRMSE) <- colnames(nem_cross_sec[13:24])

x_LR_rMAE <- x_rMAE[,1:6]
x_LR_rRMSE <- x_rRMSE[,1:6]

x_gb_rMAE <- x_rMAE[,7:12]
x_gb_rRMSE <- x_rRMSE[,7:12]

nemenyi(x_LR_rRMSE, plottype = "mcb")

# now cross-temp
nem_cross_temp <- read_excel("Nemenyi-Cross-Temporal.xlsx")

x_rMAE <- as.matrix(nem_cross_temp[,1:12])
x_rRMSE <- as.matrix(nem_cross_temp[,13:24])

colnames(x_rMAE) <- colnames(nem_cross_temp[1:12])
colnames(x_rRMSE) <- colnames(nem_cross_temp[13:24])

x_LR_rMAE <- x_rMAE[,1:6]
x_LR_rRMSE <- x_rRMSE[,1:6]

x_gb_rMAE <- x_rMAE[,7:12]
x_gb_rRMSE <- x_rRMSE[,7:12]

nemenyi(x_LR_rMAE, plottype = "mcb")
