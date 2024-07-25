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


#### Import benchmark and LR forecasts ####
fc_benchmark <- readRDS(file = "fc_benchmark.rds")
fc_test_set <- readRDS(file = "fc_test_set.rds")
fc_LR_ite <- readRDS(file = "fc_LR_ite.rds")
colnames(fc_LR_ite) <- colnames(fc_benchmark)
fc_LR_oct <- readRDS(file = "fc_LR_oct.rds")
colnames(fc_LR_oct) <- colnames(fc_benchmark)
fc_LR_ctbu <- readRDS(file = "fc_LR_ctbu.rds")
colnames(fc_LR_ctbu) <- colnames(fc_benchmark)
fc_LR <- readRDS(file = "fc_LR.rds")
fc_LR_cst <- readRDS(file = "fc_LR_cst.rds")
colnames(fc_LR_cst) <- colnames(fc_benchmark)
fc_LR_tcs <- readRDS(file = "fc_LR_tcs.rds")
colnames(fc_LR_tcs) <- colnames(fc_benchmark)
fc_LR_thf <- readRDS(file = "fc_LR_thf.rds")
colnames(fc_LR_thf) <- colnames(fc_benchmark)

#### Compute naive errors ####

naive_error <- abs(fc_benchmark - fc_test_set)
naive_error_squared <- abs(fc_benchmark - fc_test_set)^2

id <- which(simplify2array(strsplit(colnames(fc_benchmark), split = "_"))[1, ] == "k1")
naive_error_10 <- colMeans(t(naive_error[, id]))
naive_error_10_squared <- colMeans(t(naive_error_squared[, id]))

id <- which(simplify2array(strsplit(colnames(fc_benchmark), split = "_"))[1, ] == "k2")
naive_error_20 <- colMeans(t(naive_error[, id]))
naive_error_20_squared <- colMeans(t(naive_error_squared[, id]))

id <- which(simplify2array(strsplit(colnames(fc_benchmark), split = "_"))[1, ] == "k3")
naive_error_30 <- colMeans(t(naive_error[, id]))
naive_error_30_squared <- colMeans(t(naive_error_squared[, id]))

id <- which(simplify2array(strsplit(colnames(fc_benchmark), split = "_"))[1, ] == "k6")
naive_error_1 <- colMeans(t(naive_error[, id]))
naive_error_1_squared <- colMeans(t(naive_error_squared[, id]))

#### Compute LR-ITE accuracy ####
ite_recf_error <- abs(fc_LR_ite - fc_test_set)
ite_recf_error_squared <- abs(fc_LR_ite - fc_test_set)^2

rMAE_LGBM_ite_recf <- compute_rMAE(ite_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_LGBM_ite_recf <- compute_rRMSE(ite_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_LGBM_ite_recf[1],"rMAE_LR_ite_recf.csv", row.names = TRUE)
write.csv(rRMSE_LGBM_ite_recf[1],"rRMSE_LR_ite_recf.csv", row.names = TRUE)
write.csv(c(rMAE_LGBM_ite_recf[[2]],rMAE_LGBM_ite_recf[[3]],rMAE_LGBM_ite_recf[[4]],rMAE_LGBM_ite_recf[[5]]), "nemenyi_rMAE_LR_ite_recf.csv")
write.csv(c(rRMSE_LGBM_ite_recf[[2]],rRMSE_LGBM_ite_recf[[3]],rRMSE_LGBM_ite_recf[[4]],rRMSE_LGBM_ite_recf[[5]]), "nemenyi_rRMSE_LR_ite_recf.csv")

#### Compute LR-OCT accuracy ####
oct_recf_error <- abs(fc_LR_oct - fc_test_set)
oct_recf_error_squared <- abs(fc_LR_oct - fc_test_set)^2

rMAE_oct_recf <- compute_rMAE(oct_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_oct_recf <- compute_rRMSE(oct_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_oct_recf[1],"rMAE_LR_oct_recf.csv", row.names = TRUE)
write.csv(rRMSE_oct_recf[1],"rRMSE_LR_oct_recf.csv", row.names = TRUE)
write.csv(c(rMAE_oct_recf[[2]],rMAE_oct_recf[[3]],rMAE_oct_recf[[4]],rMAE_oct_recf[[5]]), "nemenyi_rMAE_LR_oct_recf.csv")
write.csv(c(rRMSE_oct_recf[[2]],rRMSE_oct_recf[[3]],rRMSE_oct_recf[[4]],rRMSE_oct_recf[[5]]), "nemenyi_rRMSE_LR_oct_recf.csv")

#### Compute LR-CTBU accuracy ####
obj_error <- abs(fc_LR_ctbu - fc_test_set)
obj_error_squared <- abs(fc_LR_ctbu - fc_test_set)^2

rMAE_LGBM_bu_ct <- compute_rMAE(obj_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_LGBM_bu_ct <- compute_rRMSE(obj_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_LGBM_bu_ct[1],"rMAE_LR_bu_ct.csv", row.names = TRUE)
write.csv(rRMSE_LGBM_bu_ct[1],"rRMSE_LR_bu_ct.csv", row.names = TRUE)
write.csv(c(rMAE_LGBM_bu_ct[[2]],rMAE_LGBM_bu_ct[[3]],rMAE_LGBM_bu_ct[[4]],rMAE_LGBM_bu_ct[[5]]), "nemenyi_rMAE_LR_bu_ct.csv")
write.csv(c(rRMSE_LGBM_bu_ct[[2]],rRMSE_LGBM_bu_ct[[3]],rRMSE_LGBM_bu_ct[[4]],rRMSE_LGBM_bu_ct[[5]]), "nemenyi_rRMSE_LR_bu_ct.csv")

#### Compute unreconciled LR accuracy ####
lr_error <- abs(fc_LR - fc_test_set)
lr_error_squared <- (fc_LR - fc_test_set)^2

rMAE_lr <- compute_rMAE(lr_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_lr <- compute_rRMSE(lr_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_lr[1],"rMAE_lr.csv", row.names = TRUE)
write.csv(rRMSE_lr[1],"rRMSE_lr.csv", row.names = TRUE)
write.csv(c(rMAE_lr[[2]],rMAE_lr[[3]],rMAE_lr[[4]],rMAE_lr[[5]]), "rMAE_lr_nemenyi.csv")
write.csv(c(rRMSE_lr[[2]],rRMSE_lr[[3]],rRMSE_lr[[4]],rRMSE_lr[[5]]), "rRMSE_lr_nemenyi.csv")

#### Compute LR-TCS accuracy ####
tcs_recf_error <- abs(fc_LR_tcs - fc_test_set)
tcs_recf_error_squared <- abs(fc_LR_tcs - fc_test_set)^2

rMAE_tcs_recf <- compute_rMAE(tcs_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_tcs_recf <- compute_rRMSE(tcs_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_tcs_recf[1],"rMAE_LR_tcs_recf.csv", row.names = TRUE)
write.csv(rRMSE_tcs_recf[1],"rRMSE_LR_tcs_recf.csv", row.names = TRUE)
write.csv(c(rMAE_tcs_recf[[2]],rMAE_tcs_recf[[3]],rMAE_tcs_recf[[4]],rMAE_tcs_recf[[5]]), "nemenyi_rMAE_LR_tcs_recf.csv")
write.csv(c(rRMSE_tcs_recf[[2]],rRMSE_tcs_recf[[3]],rRMSE_tcs_recf[[4]],rRMSE_tcs_recf[[5]]), "nemenyi_rRMSE_LR_tcs_recf.csv")

#### Compute LR-THF accuracy ####
thf_recf_error <- abs(fc_LR_thf - fc_test_set)
thf_recf_error_squared <- abs(fc_LR_thf - fc_test_set)^2

rMAE_thf_recf <- compute_rMAE(thf_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_thf_recf <- compute_rRMSE(thf_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_thf_recf[1],"rMAE_LR_thf_recf.csv", row.names = TRUE)
write.csv(rRMSE_thf_recf[1],"rRMSE_LR_thf_recf.csv", row.names = TRUE)
write.csv(c(rMAE_thf_recf[[2]],rMAE_thf_recf[[3]],rMAE_thf_recf[[4]],rMAE_thf_recf[[5]]), "nemenyi_rMAE_LR_thf_recf.csv")
write.csv(c(rRMSE_thf_recf[[2]],rRMSE_thf_recf[[3]],rRMSE_thf_recf[[4]],rRMSE_thf_recf[[5]]), "nemenyi_rRMSE_LR_thf_recf.csv")

#### Compute LR-CST accuracy ####
cst_recf_error <- abs(fc_LR_cst - fc_test_set)
cst_recf_error_squared <- abs(fc_LR_cst - fc_test_set)^2

rMAE_cst_recf <- compute_rMAE(cst_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_cst_recf <- compute_rRMSE(cst_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_cst_recf[1],"rMAE_LR_cst_recf.csv", row.names = TRUE)
write.csv(rRMSE_cst_recf[1],"rRMSE_LR_cst_recf.csv", row.names = TRUE)
write.csv(c(rMAE_cst_recf[[2]],rMAE_cst_recf[[3]],rMAE_cst_recf[[4]],rMAE_cst_recf[[5]]), "nemenyi_rMAE_LR_cst_recf.csv")
write.csv(c(rRMSE_cst_recf[[2]],rRMSE_cst_recf[[3]],rRMSE_cst_recf[[4]],rRMSE_cst_recf[[5]]), "nemenyi_rRMSE_LR_cst_recf.csv")

#### Import LightGBM forecasts ####
fc_LGBM_ite <- readRDS(file = "fc_LGBM_ite.rds")
colnames(fc_LGBM_ite) <- colnames(fc_benchmark)
fc_LGBM_oct <- readRDS(file = "fc_LGBM_oct.rds")
colnames(fc_LGBM_oct) <- colnames(fc_benchmark)
fc_LGBM_ctbu <- readRDS(file = "fc_LGBM_ctbu.rds")
colnames(fc_LGBM_ctbu) <- colnames(fc_benchmark)
fc_LGBM <- readRDS(file = "fc_LGBM.rds")
fc_LGBM_cst <- readRDS(file = "fc_LGBM_cst.rds")
colnames(fc_LGBM_cst) <- colnames(fc_benchmark)
fc_LGBM_tcs <- readRDS(file = "fc_LGBM_tcs.rds")
colnames(fc_LGBM_tcs) <- colnames(fc_benchmark)
fc_LGBM_thf <- readRDS(file = "fc_LGBM_thf.rds")
colnames(fc_LGBM_thf) <- colnames(fc_benchmark)

#### Compute GB-ITE accuracy ####
ite_recf_error <- abs(fc_LGBM_ite - fc_test_set)
ite_recf_error_squared <- abs(fc_LGBM_ite - fc_test_set)^2

rMAE_LGBM_ite_recf <- compute_rMAE(ite_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_LGBM_ite_recf <- compute_rRMSE(ite_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_LGBM_ite_recf[1],"rMAE_LGBM_ite_recf.csv", row.names = TRUE)
write.csv(rRMSE_LGBM_ite_recf[1],"rRMSE_LGBM_ite_recf.csv", row.names = TRUE)
write.csv(c(rMAE_LGBM_ite_recf[[2]],rMAE_LGBM_ite_recf[[3]],rMAE_LGBM_ite_recf[[4]],rMAE_LGBM_ite_recf[[5]]), "nemenyi_rMAE_LGBM_ite_recf.csv")
write.csv(c(rRMSE_LGBM_ite_recf[[2]],rRMSE_LGBM_ite_recf[[3]],rRMSE_LGBM_ite_recf[[4]],rRMSE_LGBM_ite_recf[[5]]), "nemenyi_rRMSE_LGBM_ite_recf.csv")

#### Compute GB-OCT accuracy ####
oct_recf_error <- abs(fc_LGBM_oct - fc_test_set)
oct_recf_error_squared <- abs(fc_LGBM_oct - fc_test_set)^2

rMAE_oct_recf <- compute_rMAE(oct_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_oct_recf <- compute_rRMSE(oct_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_oct_recf[1],"rMAE_LGBM_oct_recf.csv", row.names = TRUE)
write.csv(rRMSE_oct_recf[1],"rRMSE_LGBM_oct_recf.csv", row.names = TRUE)
write.csv(c(rMAE_oct_recf[[2]],rMAE_oct_recf[[3]],rMAE_oct_recf[[4]],rMAE_oct_recf[[5]]), "nemenyi_rMAE_LGBM_oct_recf.csv")
write.csv(c(rRMSE_oct_recf[[2]],rRMSE_oct_recf[[3]],rRMSE_oct_recf[[4]],rRMSE_oct_recf[[5]]), "nemenyi_rRMSE_LGBM_oct_recf.csv")

#### Compute GB-CTBU accuracy ####
obj_error <- abs(fc_LGBM_ctbu - fc_test_set)
obj_error_squared <- abs(fc_LGBM_ctbu - fc_test_set)^2

rMAE_LGBM_bu_ct <- compute_rMAE(obj_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_LGBM_bu_ct <- compute_rRMSE(obj_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_LGBM_bu_ct[1],"rMAE_LGBM_bu_ct.csv", row.names = TRUE)
write.csv(rRMSE_LGBM_bu_ct[1],"rRMSE_LGBM_bu_ct.csv", row.names = TRUE)
write.csv(c(rMAE_LGBM_bu_ct[[2]],rMAE_LGBM_bu_ct[[3]],rMAE_LGBM_bu_ct[[4]],rMAE_LGBM_bu_ct[[5]]), "nemenyi_rMAE_LGBM_bu_ct.csv")
write.csv(c(rRMSE_LGBM_bu_ct[[2]],rRMSE_LGBM_bu_ct[[3]],rRMSE_LGBM_bu_ct[[4]],rRMSE_LGBM_bu_ct[[5]]), "nemenyi_rRMSE_LGBM_bu_ct.csv")

#### Compute unreconciled LGBM accuracy ####
LGBM_error <- abs(fc_LGBM - fc_test_set)
LGBM_error_squared <- (fc_LGBM - fc_test_set)^2

rMAE_LGBM <- compute_rMAE(LGBM_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_LGBM <- compute_rRMSE(LGBM_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_LGBM[1],"rMAE_LGBM.csv", row.names = TRUE)
write.csv(rRMSE_LGBM[1],"rRMSE_LGBM.csv", row.names = TRUE)
write.csv(c(rMAE_LGBM[[2]],rMAE_LGBM[[3]],rMAE_LGBM[[4]],rMAE_LGBM[[5]]), "rMAE_LGBM_nemenyi.csv")
write.csv(c(rRMSE_LGBM[[2]],rRMSE_LGBM[[3]],rRMSE_LGBM[[4]],rRMSE_LGBM[[5]]), "rRMSE_LGBM_nemenyi.csv")

#### Compute GB-TCS accuracy ####
tcs_recf_error <- abs(fc_LGBM_tcs - fc_test_set)
tcs_recf_error_squared <- abs(fc_LGBM_tcs - fc_test_set)^2

rMAE_tcs_recf <- compute_rMAE(tcs_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_tcs_recf <- compute_rRMSE(tcs_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_tcs_recf[1],"rMAE_LGBM_tcs_recf.csv", row.names = TRUE)
write.csv(rRMSE_tcs_recf[1],"rRMSE_LGBM_tcs_recf.csv", row.names = TRUE)
write.csv(c(rMAE_tcs_recf[[2]],rMAE_tcs_recf[[3]],rMAE_tcs_recf[[4]],rMAE_tcs_recf[[5]]), "nemenyi_rMAE_LGBM_tcs_recf.csv")
write.csv(c(rRMSE_tcs_recf[[2]],rRMSE_tcs_recf[[3]],rRMSE_tcs_recf[[4]],rRMSE_tcs_recf[[5]]), "nemenyi_rRMSE_LGBM_tcs_recf.csv")

#### Compute rMAE and rRMSE ####
thf_recf_error <- abs(fc_LGBM_thf - fc_test_set)
thf_recf_error_squared <- abs(fc_LGBM_thf - fc_test_set)^2

rMAE_thf_recf <- compute_rMAE(thf_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_thf_recf <- compute_rRMSE(thf_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_thf_recf[1],"rMAE_LGBM_thf_recf.csv", row.names = TRUE)
write.csv(rRMSE_thf_recf[1],"rRMSE_LGBM_thf_recf.csv", row.names = TRUE)
write.csv(c(rMAE_thf_recf[[2]],rMAE_thf_recf[[3]],rMAE_thf_recf[[4]],rMAE_thf_recf[[5]]), "nemenyi_rMAE_LGBM_thf_recf.csv")
write.csv(c(rRMSE_thf_recf[[2]],rRMSE_thf_recf[[3]],rRMSE_thf_recf[[4]],rRMSE_thf_recf[[5]]), "nemenyi_rRMSE_LGBM_thf_recf.csv")

#### Compute GB-CST accuracy ####
cst_recf_error <- abs(fc_LGBM_cst - fc_test_set)
cst_recf_error_squared <- abs(fc_LGBM_cst - fc_test_set)^2

rMAE_cst_recf <- compute_rMAE(cst_recf_error, naive_error_10, naive_error_20, naive_error_30, naive_error_1)
rRMSE_cst_recf <- compute_rRMSE(cst_recf_error_squared, naive_error_10_squared, naive_error_20_squared, naive_error_30_squared, naive_error_1_squared)

write.csv(rMAE_cst_recf[1],"rMAE_LGBM_cst_recf.csv", row.names = TRUE)
write.csv(rRMSE_cst_recf[1],"rRMSE_LGBM_cst_recf.csv", row.names = TRUE)
write.csv(c(rMAE_cst_recf[[2]],rMAE_cst_recf[[3]],rMAE_cst_recf[[4]],rMAE_cst_recf[[5]]), "nemenyi_LGBM_rMAE_cst_recf.csv")
write.csv(c(rRMSE_cst_recf[[2]],rRMSE_cst_recf[[3]],rRMSE_cst_recf[[4]],rRMSE_cst_recf[[5]]), "nemenyi_LGBM_rRMSE_cst_recf.csv")
