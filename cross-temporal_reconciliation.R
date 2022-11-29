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
library(FoReco)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set this to your working directory

#### Define auxiliary functions ####
# function to convert grouped/keyed tsibble into matrix of ts() time series
to_ts <- function(data, variable){
  N = nrow(data)/n_keys(data)
  matrix_tmp <- matrix(NA, nrow = N, ncol = n_keys(data))
  matrix_tmp[,1] <- t(as.ts(data %>% filter(is_aggregated(Group), is_aggregated(Subgroup)), value = variable))
  matrix_tmp[,2] <- t(as.ts(data %>% filter(Group == "A", is_aggregated(Subgroup)), value = variable))
  matrix_tmp[,3] <- t(as.ts(data %>% filter(Group == "B", is_aggregated(Subgroup)), value = variable))
  matrix_tmp[,4] <- t(as.ts(data %>% filter(Group == "C", is_aggregated(Subgroup)), value = variable))
  matrix_tmp[,5] <- t(as.ts(data %>% filter(Group == "D", is_aggregated(Subgroup)), value = variable))
  matrix_tmp[,5] <- t(as.ts(data %>% filter(Group == "D", is_aggregated(Subgroup)), value = variable))
  matrix_tmp[,6] <- t(as.ts(data %>% filter(Group == "A", Subgroup == "A1"), value = variable))
  matrix_tmp[,7] <- t(as.ts(data %>% filter(Group == "A", Subgroup == "A2"), value = variable))
  matrix_tmp[,8] <- t(as.ts(data %>% filter(Group == "A", Subgroup == "A3"), value = variable))
  matrix_tmp[,9] <- t(as.ts(data %>% filter(Group == "A", Subgroup == "A4"), value = variable))
  matrix_tmp[,10] <- t(as.ts(data %>% filter(Group == "A", Subgroup == "A5"), value = variable))
  matrix_tmp[,11] <- t(as.ts(data %>% filter(Group == "A", Subgroup == "A6"), value = variable))
  matrix_tmp[,12] <- t(as.ts(data %>% filter(Group == "A", Subgroup == "A7"), value = variable))
  matrix_tmp[,13] <- t(as.ts(data %>% filter(Group == "B", Subgroup == "B1"), value = variable))
  matrix_tmp[,14] <- t(as.ts(data %>% filter(Group == "B", Subgroup == "B2"), value = variable))
  matrix_tmp[,15] <- t(as.ts(data %>% filter(Group == "B", Subgroup == "B3"), value = variable))
  matrix_tmp[,16] <- t(as.ts(data %>% filter(Group == "B", Subgroup == "B4"), value = variable))
  matrix_tmp[,17] <- t(as.ts(data %>% filter(Group == "B", Subgroup == "B5"), value = variable))
  matrix_tmp[,18] <- t(as.ts(data %>% filter(Group == "B", Subgroup == "B6"), value = variable))
  matrix_tmp[,19] <- t(as.ts(data %>% filter(Group == "B", Subgroup == "B7"), value = variable))
  matrix_tmp[,20] <- t(as.ts(data %>% filter(Group == "B", Subgroup == "B8"), value = variable))
  matrix_tmp[,21] <- t(as.ts(data %>% filter(Group == "B", Subgroup == "B9"), value = variable))
  matrix_tmp[,22] <- t(as.ts(data %>% filter(Group == "C", Subgroup == "C1"), value = variable))
  matrix_tmp[,23] <- t(as.ts(data %>% filter(Group == "C", Subgroup == "C2"), value = variable))
  matrix_tmp[,24] <- t(as.ts(data %>% filter(Group == "C", Subgroup == "C3"), value = variable))
  matrix_tmp[,25] <- t(as.ts(data %>% filter(Group == "D", Subgroup == "D1"), value = variable))
  matrix_tmp[,26] <- t(as.ts(data %>% filter(Group == "D", Subgroup == "D2"), value = variable))
  matrix_tmp[,27] <- t(as.ts(data %>% filter(Group == "D", Subgroup == "D3"), value = variable))
  matrix_tmp[,28] <- t(as.ts(data %>% filter(Group == "D", Subgroup == "D4"), value = variable))
  matrix_tmp[,29] <- t(as.ts(data %>% filter(Group == "D", Subgroup == "D5"), value = variable))
  matrix_tmp[,30] <- t(as.ts(data %>% filter(Group == "D", Subgroup == "D6"), value = variable))
  colnames(matrix_tmp) <- c("T", "A", "B", "C", "D", "A1", "A2", "A3", "A4", "A5", "A6", "A7", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "C1", "C2", "C3", "D1", "D2", "D3", "D4", "D5", "D6")

  return(matrix_tmp)
}

#### import all necessary forecasts, fitted, residuals ####

# BENCHMARK
# forecasts benchmark
fc10_benchmark <- as_tsibble(readRDS(file = "fc10_benchmark.rds"))
fc20_benchmark <- as_tsibble(readRDS(file = "fc20_benchmark.rds"))
fc30_benchmark <- as_tsibble(readRDS(file = "fc30_benchmark.rds"))
fc1_benchmark <- as_tsibble(readRDS(file = "fc1_benchmark.rds"))

# fitted benchmark
fc10_benchmark_fitted <- readRDS(file = "fc10_benchmark_fitted.rds")
fc20_benchmark_fitted <- readRDS(file = "fc20_benchmark_fitted.rds")
fc30_benchmark_fitted <- readRDS(file = "fc30_benchmark_fitted.rds")
fc1_benchmark_fitted <- readRDS(file = "fc1_benchmark_fitted.rds")

# residuals benchmark
fc10_benchmark_residuals <- readRDS(file = "fc10_benchmark_residuals.rds")
fc20_benchmark_residuals <- readRDS(file = "fc20_benchmark_residuals.rds")
fc30_benchmark_residuals <- readRDS(file = "fc30_benchmark_residuals.rds")
fc1_benchmark_residuals <- readRDS(file = "fc1_benchmark_residuals.rds")


#LINEAR REGRESSION - we extract the unreconciled forecasts
# forecasts linear regression
fc10_lr <- as_tsibble(readRDS(file = "fc10_lr.rds")) %>% filter(.model == "mod1")
fc20_lr <- as_tsibble(readRDS(file = "fc20_lr.rds")) %>% filter(.model == "mod1")
fc30_lr <- as_tsibble(readRDS(file = "fc30_lr.rds")) %>% filter(.model == "mod1")
fc1_lr <- as_tsibble(readRDS(file = "fc1_lr.rds")) %>% filter(.model == "mod1")

# fitted linear regression
fc10_lr_fitted <- readRDS(file = "fc10_lr_fitted.rds") %>% filter(.model == "mod1")
fc20_lr_fitted <- readRDS(file = "fc20_lr_fitted.rds") %>% filter(.model == "mod1")
fc30_lr_fitted <- readRDS(file = "fc30_lr_fitted.rds") %>% filter(.model == "mod1")
fc1_lr_fitted <- readRDS(file = "fc1_lr_fitted.rds") %>% filter(.model == "mod1")

# residuals linear regression
fc10_lr_residuals <- readRDS(file = "fc10_lr_residuals.rds") %>% filter(.model == "mod1")
fc20_lr_residuals <- readRDS(file = "fc20_lr_residuals.rds") %>% filter(.model == "mod1")
fc30_lr_residuals <- readRDS(file = "fc30_lr_residuals.rds") %>% filter(.model == "mod1")
fc1_lr_residuals <- readRDS(file = "fc1_lr_residuals.rds") %>% filter(.model == "mod1")


#ACTUAL DATA
# observations (for test data)
min10 <- readRDS(file = "min10.rds")
min20 <- readRDS(file = "min20.rds")
min30 <- readRDS(file = "min30.rds")
hr1 <- readRDS(file = "hr1.rds")

#### prep for reconciliation ####

values <- NULL
base <- NULL
residuals <- NULL
test <- NULL

TrainingProportion = 0.9

values$min10 <- to_ts(min10, "Power")
values$min20 <- to_ts(min20, "Power")
values$min30 <- to_ts(min30, "Power")
values$hr1 <- to_ts(hr1, "Power")

base$min10 <- to_ts(fc10_benchmark, ".mean")
base$min20 <- to_ts(fc20_benchmark, ".mean")
base$min30 <- to_ts(fc30_benchmark, ".mean")
base$hr1 <- to_ts(fc1_benchmark, ".mean")

residuals$min10 <- to_ts(fc10_benchmark_residuals, ".resid")
residuals$min20 <- to_ts(fc20_benchmark_residuals, ".resid")
residuals$min30 <- to_ts(fc30_benchmark_residuals, ".resid")
residuals$hr1 <- to_ts(fc1_benchmark_residuals, ".resid")

test$min10 <- values$min10[-c(1:ceiling(dim(values$min10)[1]*TrainingProportion)), ]
test$min20 <- values$min20[-c(1:ceiling(dim(values$min20)[1]*TrainingProportion)), ]
test$min30 <- values$min30[-c(1:ceiling(dim(values$min30)[1]*TrainingProportion)), ]
test$hr1 <- values$hr1[-c(1:ceiling(dim(values$hr1)[1]*TrainingProportion)), ]

base <- t(do.call(rbind, rev(base)))
res <- t(do.call(rbind, rev(residuals)))
test <- t(do.call(rbind, rev(test)))

obs <- values

C <- matrix(c(rep(1,25),
              rep(1,7), rep(0,18),
              rep(0,7), rep(1,9), rep(0,9),
              rep(0,16), rep(1,3), rep(0,6),
              rep(0,19), rep(1,6)), byrow = TRUE, nrow = 5)

# set first NA's to 0 (first observation since using)
base[is.na(base)] <- 0
res[is.na(res)] <- 0
test[is.na(test)] <- 0
obs[is.na(obs)] <- 0

FoReco_data <- list(base = base,
                    test = test,
                    res = res,
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

thf_score <- score_index(recf = thf_recf,
                         base = FoReco_data$base,
                         test = FoReco_data$test, m = 6, nb = 25)

write.csv(thf_score,"thf_score.csv", row.names = TRUE)

#### Heuristic first-temporal-then-cross-sectional cross-temporal reconciliation using t-wls + cs-shr (Kourentzes and Athanasopoulos, 2019) ####

tcs_recf <- tcsrec(FoReco_data$base, m = 6, C = FoReco_data$C,
                   thf_comb = "wlsv", hts_comb = "shr",
                   res = FoReco_data$res)$recf

tcs_score <- score_index(recf = tcs_recf,
                         base = FoReco_data$base,
                         test = FoReco_data$test, m = 6, nb = 25)

write.csv(tcs_score,"tcs_score.csv", row.names = TRUE)

#### Heuristic first-cross-sectional-then-temporal cross-temporal reconciliation using t-acov + cs-shr (Di Fonzo and Girolimetto, 2020) ####

cst_recf <- cstrec(FoReco_data$base, m = 6, C = FoReco_data$C,
                   thf_comb = "acov", hts_comb = "shr",
                   res = FoReco_data$res)$recf

tcs_score <- score_index(recf = cst_recf,
                         base = FoReco_data$base,
                         test = FoReco_data$test, m = 6, nb = 25)

#### Iterative cross-temporal reconciliation (Di Fonzo and Girolimetto, 2020) ####

ite_recf <- iterec(FoReco_data$base,
                   m = 6, C = FoReco_data$C,
                   thf_comb = "acov", hts_comb = "shr",
                   res = FoReco_data$res, start_rec = "thf")$recf

ite_score <- score_index(recf = ite_recf,
                         base = FoReco_data$base,
                         test = FoReco_data$test, m = 6, nb = 25)

write.csv(ite_score,"ite_score.csv", row.names = TRUE)

#### Optimal cross-temporal reconciliation using bdshr (Di Fonzo and Girolimetto, 2020) ####
oct_recf <- octrec(FoReco_data$base, m = 6, C = FoReco_data$C,
                   comb = "bdshr", res = FoReco_data$res)$recf
oct_score <- score_index(recf = oct_recf,
                         base = FoReco_data$base,
                         test = FoReco_data$test, m = 6, nb = 25)

write.csv(oct_score,"oct_score.csv", row.names = TRUE)
