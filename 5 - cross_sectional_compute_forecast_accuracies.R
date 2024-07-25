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
library(tsutils)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set this to your working directory

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
  error[1,1] <- exp(mean(log(error_10[4:23])))
  error[1,2] <- exp(mean(log(error_10[2:3])))
  error[1,3] <- exp(mean(log(error_10[1])))
  
  error[2,1] <- exp(mean(log(error_20[4:23])))
  error[2,2] <- exp(mean(log(error_20[2:3])))
  error[2,3] <- exp(mean(log(error_20[1])))
  
  error[3,1] <- exp(mean(log(error_30[4:23])))
  error[3,2] <- exp(mean(log(error_30[2:3])))
  error[3,3] <- exp(mean(log(error_30[1])))
  
  error[4,1] <- exp(mean(log(error_1[4:23])))
  error[4,2] <- exp(mean(log(error_1[2:3])))
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
  error_squared[1,1] <- exp(mean(log(error_10_squared[4:23])))
  error_squared[1,2] <- exp(mean(log(error_10_squared[2:3])))
  error_squared[1,3] <- exp(mean(log(error_10_squared[1])))
  
  error_squared[2,1] <- exp(mean(log(error_20_squared[4:23])))
  error_squared[2,2] <- exp(mean(log(error_20_squared[2:3])))
  error_squared[2,3] <- exp(mean(log(error_20_squared[1])))
  
  error_squared[3,1] <- exp(mean(log(error_30_squared[4:23])))
  error_squared[3,2] <- exp(mean(log(error_30_squared[2:3])))
  error_squared[3,3] <- exp(mean(log(error_30_squared[1])))
  
  error_squared[4,1] <- exp(mean(log(error_1_squared[4:23])))
  error_squared[4,2] <- exp(mean(log(error_1_squared[2:3])))
  error_squared[4,3] <- exp(mean(log(error_1_squared[1])))
  
  # return matrix of rRMSE
  return(list(error_squared, error_10_squared, error_20_squared, error_30_squared, error_1_squared))
}

#### group by keys ####
fc10_lr <- as_tsibble(readRDS(file = "fc10_lr.rds")) %>% group_by_key()
fc20_lr <- as_tsibble(readRDS(file = "fc20_lr.rds")) %>% group_by_key()
fc30_lr <- as_tsibble(readRDS(file = "fc30_lr.rds")) %>% group_by_key()
fc1_lr <- as_tsibble(readRDS(file = "fc1_lr.rds")) %>% group_by_key()

fc10_gb <- as_tsibble(readRDS(file = "fc10_gb.rds")) %>% group_by_key()
fc20_gb <- as_tsibble(readRDS(file = "fc20_gb.rds")) %>% group_by_key()
fc30_gb <- as_tsibble(readRDS(file = "fc30_gb.rds")) %>% group_by_key()
fc1_gb <- as_tsibble(readRDS(file = "fc1_gb.rds")) %>% group_by_key()

fc10_benchmark <- as_tsibble(readRDS(file = "fc10_benchmark.rds")) %>% group_by_key()
fc20_benchmark <- as_tsibble(readRDS(file = "fc20_benchmark.rds")) %>% group_by_key()
fc30_benchmark <- as_tsibble(readRDS(file = "fc30_benchmark.rds")) %>% group_by_key()
fc1_benchmark <- as_tsibble(readRDS(file = "fc1_benchmark.rds")) %>% group_by_key()

saveRDS(fc10_lr, file = "fc10_lr.rds")
saveRDS(fc20_lr, file = "fc20_lr.rds")
saveRDS(fc30_lr, file = "fc30_lr.rds")
saveRDS(fc1_lr, file = "fc1_lr.rds")

saveRDS(fc10_gb, file = "fc10_gb.rds")
saveRDS(fc20_gb, file = "fc20_gb.rds")
saveRDS(fc30_gb, file = "fc30_gb.rds")
saveRDS(fc1_gb, file = "fc1_gb.rds")

saveRDS(fc10_benchmark, file = "fc10_benchmark.rds")
saveRDS(fc20_benchmark, file = "fc20_benchmark.rds")
saveRDS(fc30_benchmark, file = "fc30_benchmark.rds")
saveRDS(fc1_benchmark, file = "fc1_benchmark.rds")

#### LINEAR REGRESSION - extract BU ####
# forecasts linear regression
fc10_lr_bu <- as_tsibble(readRDS(file = "fc10_lr.rds")) %>% filter(.model == "bu_mod1")
fc20_lr_bu <- as_tsibble(readRDS(file = "fc20_lr.rds")) %>% filter(.model == "bu_mod1")
fc30_lr_bu <- as_tsibble(readRDS(file = "fc30_lr.rds")) %>% filter(.model == "bu_mod1")
fc1_lr_bu <- as_tsibble(readRDS(file = "fc1_lr.rds")) %>% filter(.model == "bu_mod1")

lr_bu <- NULL

lr_bu$k1 <- to_matrix_for_ts(fc10_lr_bu, ".mean")
lr_bu$k1 <- ts(lr_bu$k1, frequency = 6, start = c(47412, 1))
lr_bu$k2 <- to_matrix_for_ts(fc20_lr_bu, ".mean")
lr_bu$k2 <- ts(lr_bu$k2, frequency = 3, start = c(47412, 1))
lr_bu$k3 <- to_matrix_for_ts(fc30_lr_bu, ".mean")
lr_bu$k3 <- ts(lr_bu$k3, frequency = 2, start = c(47412, 1))
lr_bu$k6 <- to_matrix_for_ts(fc1_lr_bu, ".mean")
lr_bu$k6 <- ts(lr_bu$k6, frequency = 1, start = c(47412, 1))

lr_bu <- t(do.call(rbind, rev(lr_bu)))


#### LINEAR REGRESSION - extract TD ####
# forecasts linear regression

fc10_lr_td <- as_tsibble(readRDS(file = "fc10_lr.rds")) %>% filter(.model == "td_mod1")
fc20_lr_td <- as_tsibble(readRDS(file = "fc20_lr.rds")) %>% filter(.model == "td_mod1")
fc30_lr_td <- as_tsibble(readRDS(file = "fc30_lr.rds")) %>% filter(.model == "td_mod1")
fc1_lr_td <- as_tsibble(readRDS(file = "fc1_lr.rds")) %>% filter(.model == "td_mod1")

lr_td <- NULL

lr_td$k1 <- to_matrix_for_ts(fc10_lr_td, ".mean")
lr_td$k1 <- ts(lr_td$k1, frequency = 6, start = c(47412, 1))
lr_td$k2 <- to_matrix_for_ts(fc20_lr_td, ".mean")
lr_td$k2 <- ts(lr_td$k2, frequency = 3, start = c(47412, 1))
lr_td$k3 <- to_matrix_for_ts(fc30_lr_td, ".mean")
lr_td$k3 <- ts(lr_td$k3, frequency = 2, start = c(47412, 1))
lr_td$k6 <- to_matrix_for_ts(fc1_lr_td, ".mean")
lr_td$k6 <- ts(lr_td$k6, frequency = 1, start = c(47412, 1))

lr_td <- t(do.call(rbind, rev(lr_td)))


#### LINEAR REGRESSION - extract MO ####
fc10_lr_mo <- as_tsibble(readRDS(file = "fc10_lr.rds")) %>% filter(.model == "mo_mod1")
fc20_lr_mo <- as_tsibble(readRDS(file = "fc20_lr.rds")) %>% filter(.model == "mo_mod1")
fc30_lr_mo <- as_tsibble(readRDS(file = "fc30_lr.rds")) %>% filter(.model == "mo_mod1")
fc1_lr_mo <- as_tsibble(readRDS(file = "fc1_lr.rds")) %>% filter(.model == "mo_mod1")

lr_mo <- NULL

lr_mo$k1 <- to_matrix_for_ts(fc10_lr_mo, ".mean")
lr_mo$k1 <- ts(lr_mo$k1, frequency = 6, start = c(47412, 1))
lr_mo$k2 <- to_matrix_for_ts(fc20_lr_mo, ".mean")
lr_mo$k2 <- ts(lr_mo$k2, frequency = 3, start = c(47412, 1))
lr_mo$k3 <- to_matrix_for_ts(fc30_lr_mo, ".mean")
lr_mo$k3 <- ts(lr_mo$k3, frequency = 2, start = c(47412, 1))
lr_mo$k6 <- to_matrix_for_ts(fc1_lr_mo, ".mean")
lr_mo$k6 <- ts(lr_mo$k6, frequency = 1, start = c(47412, 1))

lr_mo <- t(do.call(rbind, rev(lr_mo)))

#### LINEAR REGRESSION - extract OLS ####
fc10_lr_ols <- as_tsibble(readRDS(file = "fc10_lr.rds")) %>% filter(.model == "ols_mod1")
fc20_lr_ols <- as_tsibble(readRDS(file = "fc20_lr.rds")) %>% filter(.model == "ols_mod1")
fc30_lr_ols <- as_tsibble(readRDS(file = "fc30_lr.rds")) %>% filter(.model == "ols_mod1")
fc1_lr_ols <- as_tsibble(readRDS(file = "fc1_lr.rds")) %>% filter(.model == "ols_mod1")

lr_ols <- NULL

lr_ols$k1 <- to_matrix_for_ts(fc10_lr_ols, ".mean")
lr_ols$k1 <- ts(lr_ols$k1, frequency = 6, start = c(47412, 1))
lr_ols$k2 <- to_matrix_for_ts(fc20_lr_ols, ".mean")
lr_ols$k2 <- ts(lr_ols$k2, frequency = 3, start = c(47412, 1))
lr_ols$k3 <- to_matrix_for_ts(fc30_lr_ols, ".mean")
lr_ols$k3 <- ts(lr_ols$k3, frequency = 2, start = c(47412, 1))
lr_ols$k6 <- to_matrix_for_ts(fc1_lr_ols, ".mean")
lr_ols$k6 <- ts(lr_ols$k6, frequency = 1, start = c(47412, 1))

lr_ols <- t(do.call(rbind, rev(lr_ols)))


#### LINEAR REGRESSION - extract MinT-shr ####
fc10_lr_mint <- as_tsibble(readRDS(file = "fc10_lr.rds")) %>% filter(.model == "mint_mod1")
fc20_lr_mint <- as_tsibble(readRDS(file = "fc20_lr.rds")) %>% filter(.model == "mint_mod1")
fc30_lr_mint <- as_tsibble(readRDS(file = "fc30_lr.rds")) %>% filter(.model == "mint_mod1")
fc1_lr_mint <- as_tsibble(readRDS(file = "fc1_lr.rds")) %>% filter(.model == "mint_mod1")

lr_mint <- NULL

lr_mint$k1 <- to_matrix_for_ts(fc10_lr_mint, ".mean")
lr_mint$k1 <- ts(lr_mint$k1, frequency = 6, start = c(47412, 1))
lr_mint$k2 <- to_matrix_for_ts(fc20_lr_mint, ".mean")
lr_mint$k2 <- ts(lr_mint$k2, frequency = 3, start = c(47412, 1))
lr_mint$k3 <- to_matrix_for_ts(fc30_lr_mint, ".mean")
lr_mint$k3 <- ts(lr_mint$k3, frequency = 2, start = c(47412, 1))
lr_mint$k6 <- to_matrix_for_ts(fc1_lr_mint, ".mean")
lr_mint$k6 <- ts(lr_mint$k6, frequency = 1, start = c(47412, 1))

lr_mint <- t(do.call(rbind, rev(lr_mint)))

#### LINEAR REGRESSION - extract unreconciled base forecasts ####
fc10_lr <- as_tsibble(readRDS(file = "fc10_lr.rds")) %>% filter(.model == "mod1")
fc20_lr <- as_tsibble(readRDS(file = "fc20_lr.rds")) %>% filter(.model == "mod1")
fc30_lr <- as_tsibble(readRDS(file = "fc30_lr.rds")) %>% filter(.model == "mod1")
fc1_lr <- as_tsibble(readRDS(file = "fc1_lr.rds")) %>% filter(.model == "mod1")

lr <- NULL

lr$k1 <- to_matrix_for_ts(fc10_lr, ".mean")
lr$k1 <- ts(lr$k1, frequency = 6, start = c(47412, 1))
lr$k2 <- to_matrix_for_ts(fc20_lr, ".mean")
lr$k2 <- ts(lr$k2, frequency = 3, start = c(47412, 1))
lr$k3 <- to_matrix_for_ts(fc30_lr, ".mean")
lr$k3 <- ts(lr$k3, frequency = 2, start = c(47412, 1))
lr$k6 <- to_matrix_for_ts(fc1_lr, ".mean")
lr$k6 <- ts(lr$k6, frequency = 1, start = c(47412, 1))

lr <- t(do.call(rbind, rev(lr)))

#### LIGHTGBM - extract BU ####
# forecasts linear regression
fc10_gb_bu <- as_tsibble(readRDS(file = "fc10_gb.rds")) %>% filter(.model == "bu_mod1")
fc20_gb_bu <- as_tsibble(readRDS(file = "fc20_gb.rds")) %>% filter(.model == "bu_mod1")
fc30_gb_bu <- as_tsibble(readRDS(file = "fc30_gb.rds")) %>% filter(.model == "bu_mod1")
fc1_gb_bu <- as_tsibble(readRDS(file = "fc1_gb.rds")) %>% filter(.model == "bu_mod1")

gb_bu <- NULL

gb_bu$k1 <- to_matrix_for_ts(fc10_gb_bu, ".mean")
gb_bu$k1 <- ts(gb_bu$k1, frequency = 6, start = c(47412, 1))
gb_bu$k2 <- to_matrix_for_ts(fc20_gb_bu, ".mean")
gb_bu$k2 <- ts(gb_bu$k2, frequency = 3, start = c(47412, 1))
gb_bu$k3 <- to_matrix_for_ts(fc30_gb_bu, ".mean")
gb_bu$k3 <- ts(gb_bu$k3, frequency = 2, start = c(47412, 1))
gb_bu$k6 <- to_matrix_for_ts(fc1_gb_bu, ".mean")
gb_bu$k6 <- ts(gb_bu$k6, frequency = 1, start = c(47412, 1))

gb_bu <- t(do.call(rbind, rev(gb_bu)))


#### LIGHTGBM - extract TD ####
# forecasts linear regression

fc10_gb_td <- as_tsibble(readRDS(file = "fc10_gb.rds")) %>% filter(.model == "td_mod1")
fc20_gb_td <- as_tsibble(readRDS(file = "fc20_gb.rds")) %>% filter(.model == "td_mod1")
fc30_gb_td <- as_tsibble(readRDS(file = "fc30_gb.rds")) %>% filter(.model == "td_mod1")
fc1_gb_td <- as_tsibble(readRDS(file = "fc1_gb.rds")) %>% filter(.model == "td_mod1")

gb_td <- NULL

gb_td$k1 <- to_matrix_for_ts(fc10_gb_td, ".mean")
gb_td$k1 <- ts(gb_td$k1, frequency = 6, start = c(47412, 1))
gb_td$k2 <- to_matrix_for_ts(fc20_gb_td, ".mean")
gb_td$k2 <- ts(gb_td$k2, frequency = 3, start = c(47412, 1))
gb_td$k3 <- to_matrix_for_ts(fc30_gb_td, ".mean")
gb_td$k3 <- ts(gb_td$k3, frequency = 2, start = c(47412, 1))
gb_td$k6 <- to_matrix_for_ts(fc1_gb_td, ".mean")
gb_td$k6 <- ts(gb_td$k6, frequency = 1, start = c(47412, 1))

gb_td <- t(do.call(rbind, rev(gb_td)))


#### LIGHTGBM - extract MO ####
fc10_gb_mo <- as_tsibble(readRDS(file = "fc10_gb.rds")) %>% filter(.model == "mo_mod1")
fc20_gb_mo <- as_tsibble(readRDS(file = "fc20_gb.rds")) %>% filter(.model == "mo_mod1")
fc30_gb_mo <- as_tsibble(readRDS(file = "fc30_gb.rds")) %>% filter(.model == "mo_mod1")
fc1_gb_mo <- as_tsibble(readRDS(file = "fc1_gb.rds")) %>% filter(.model == "mo_mod1")

gb_mo <- NULL

gb_mo$k1 <- to_matrix_for_ts(fc10_gb_mo, ".mean")
gb_mo$k1 <- ts(gb_mo$k1, frequency = 6, start = c(47412, 1))
gb_mo$k2 <- to_matrix_for_ts(fc20_gb_mo, ".mean")
gb_mo$k2 <- ts(gb_mo$k2, frequency = 3, start = c(47412, 1))
gb_mo$k3 <- to_matrix_for_ts(fc30_gb_mo, ".mean")
gb_mo$k3 <- ts(gb_mo$k3, frequency = 2, start = c(47412, 1))
gb_mo$k6 <- to_matrix_for_ts(fc1_gb_mo, ".mean")
gb_mo$k6 <- ts(gb_mo$k6, frequency = 1, start = c(47412, 1))

gb_mo <- t(do.call(rbind, rev(gb_mo)))

#### LIGHTGBM - extract OLS ####
fc10_gb_ols <- as_tsibble(readRDS(file = "fc10_gb.rds")) %>% filter(.model == "ols_mod1")
fc20_gb_ols <- as_tsibble(readRDS(file = "fc20_gb.rds")) %>% filter(.model == "ols_mod1")
fc30_gb_ols <- as_tsibble(readRDS(file = "fc30_gb.rds")) %>% filter(.model == "ols_mod1")
fc1_gb_ols <- as_tsibble(readRDS(file = "fc1_gb.rds")) %>% filter(.model == "ols_mod1")

gb_ols <- NULL

gb_ols$k1 <- to_matrix_for_ts(fc10_gb_ols, ".mean")
gb_ols$k1 <- ts(gb_ols$k1, frequency = 6, start = c(47412, 1))
gb_ols$k2 <- to_matrix_for_ts(fc20_gb_ols, ".mean")
gb_ols$k2 <- ts(gb_ols$k2, frequency = 3, start = c(47412, 1))
gb_ols$k3 <- to_matrix_for_ts(fc30_gb_ols, ".mean")
gb_ols$k3 <- ts(gb_ols$k3, frequency = 2, start = c(47412, 1))
gb_ols$k6 <- to_matrix_for_ts(fc1_gb_ols, ".mean")
gb_ols$k6 <- ts(gb_ols$k6, frequency = 1, start = c(47412, 1))

gb_ols <- t(do.call(rbind, rev(gb_ols)))


#### LIGHTGBM - extract MinT-shr ####
fc10_gb_mint <- as_tsibble(readRDS(file = "fc10_gb.rds")) %>% filter(.model == "mint_mod1")
fc20_gb_mint <- as_tsibble(readRDS(file = "fc20_gb.rds")) %>% filter(.model == "mint_mod1")
fc30_gb_mint <- as_tsibble(readRDS(file = "fc30_gb.rds")) %>% filter(.model == "mint_mod1")
fc1_gb_mint <- as_tsibble(readRDS(file = "fc1_gb.rds")) %>% filter(.model == "mint_mod1")

gb_mint <- NULL

gb_mint$k1 <- to_matrix_for_ts(fc10_gb_mint, ".mean")
gb_mint$k1 <- ts(gb_mint$k1, frequency = 6, start = c(47412, 1))
gb_mint$k2 <- to_matrix_for_ts(fc20_gb_mint, ".mean")
gb_mint$k2 <- ts(gb_mint$k2, frequency = 3, start = c(47412, 1))
gb_mint$k3 <- to_matrix_for_ts(fc30_gb_mint, ".mean")
gb_mint$k3 <- ts(gb_mint$k3, frequency = 2, start = c(47412, 1))
gb_mint$k6 <- to_matrix_for_ts(fc1_gb_mint, ".mean")
gb_mint$k6 <- ts(gb_mint$k6, frequency = 1, start = c(47412, 1))

gb_mint <- t(do.call(rbind, rev(gb_mint)))

#### LIGHTGBM - extract unreconciled base forecasts ####
fc10_gb <- as_tsibble(readRDS(file = "fc10_gb.rds")) %>% filter(.model == "mod1")
fc20_gb <- as_tsibble(readRDS(file = "fc20_gb.rds")) %>% filter(.model == "mod1")
fc30_gb <- as_tsibble(readRDS(file = "fc30_gb.rds")) %>% filter(.model == "mod1")
fc1_gb <- as_tsibble(readRDS(file = "fc1_gb.rds")) %>% filter(.model == "mod1")

gb <- NULL

gb$k1 <- to_matrix_for_ts(fc10_gb, ".mean")
gb$k1 <- ts(gb$k1, frequency = 6, start = c(47412, 1))
gb$k2 <- to_matrix_for_ts(fc20_gb, ".mean")
gb$k2 <- ts(gb$k2, frequency = 3, start = c(47412, 1))
gb$k3 <- to_matrix_for_ts(fc30_gb, ".mean")
gb$k3 <- ts(gb$k3, frequency = 2, start = c(47412, 1))
gb$k6 <- to_matrix_for_ts(fc1_gb, ".mean")
gb$k6 <- ts(gb$k6, frequency = 1, start = c(47412, 1))

gb <- t(do.call(rbind, rev(gb)))

#### NAIVE BENCHMARK ####
fc10_benchmark <- as_tsibble(readRDS(file = "fc10_benchmark.rds"))
fc20_benchmark <- as_tsibble(readRDS(file = "fc20_benchmark.rds"))
fc30_benchmark <- as_tsibble(readRDS(file = "fc30_benchmark.rds"))
fc1_benchmark <- as_tsibble(readRDS(file = "fc1_benchmark.rds"))

benchmark <- NULL

benchmark$k1 <- to_matrix_for_ts(fc10_benchmark, ".mean")
benchmark$k1 <- ts(benchmark$k1, frequency = 6, start = c(47412, 1))
benchmark$k2 <- to_matrix_for_ts(fc20_benchmark, ".mean")
benchmark$k2 <- ts(benchmark$k2, frequency = 3, start = c(47412, 1))
benchmark$k3 <- to_matrix_for_ts(fc30_benchmark, ".mean")
benchmark$k3 <- ts(benchmark$k3, frequency = 2, start = c(47412, 1))
benchmark$k6 <- to_matrix_for_ts(fc1_benchmark, ".mean")
benchmark$k6 <- ts(benchmark$k6, frequency = 1, start = c(47412, 1))

benchmark <- t(do.call(rbind, rev(benchmark)))

#### Import Test Data ####
# observations (for test data)
min10 <- readRDS(file = "min10.rds")
min20 <- readRDS(file = "min20.rds")
min30 <- readRDS(file = "min30.rds")
hr1 <- readRDS(file = "hr1.rds")

test <- NULL
values <- NULL

TrainingProportion = 0.9

values$k1 <- to_matrix_for_ts(min10, "Energy")
values$k1 <- ts(values$k1, frequency = 6)
values$k2 <- to_matrix_for_ts(min20, "Energy")
values$k2 <- ts(values$k2, frequency = 3)
values$k3 <- to_matrix_for_ts(min30, "Energy")
values$k3 <- ts(values$k3, frequency = 2)
values$k6 <- to_matrix_for_ts(hr1, "Energy")
values$k6 <- ts(values$k6, frequency = 1)

# use this with the entire dataset
test$k1 <- values$k1[-c(1:ceiling(dim(values$k1)[1]*TrainingProportion)), ]
test$k2 <- values$k2[-c(1:ceiling(dim(values$k2)[1]*TrainingProportion)), ]
test$k3 <- values$k3[-c(1:ceiling(dim(values$k3)[1]*TrainingProportion)), ]
test$k6 <- values$k6[-c(1:ceiling(dim(values$k6)[1]*TrainingProportion)), ]

# use this for small subset of data (can adjust as needed)
#test$k1 <- values$k1[-c(1:dim(values$k1)[1])-26, ]
#test$k2 <- values$k2[-c(1:dim(values$k1)[1])-52, ]
#test$k3 <- values$k3[-c(1:dim(values$k1)[1])-78, ]
#test$k6 <- values$k6[-c(1:dim(values$k1)[1])-156, ]

test <- t(do.call(rbind, rev(test)))
obs <- values

#### Rename columns ####
kset <- c(6, 3, 2, 1)
# need to change this number depending on if using limited subset of full data (for speed) - 878 for full dataset, 26 for smaller
h <- 878
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