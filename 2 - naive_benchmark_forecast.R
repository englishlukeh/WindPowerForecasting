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


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set this to your working directory

# set the proportion of training set
TrainingProportion <- 0.90
# set number of threads
numThreads = 3;

min10 <- readRDS(file = "min10.rds")
min20 <- readRDS(file = "min20.rds")
min30 <- readRDS(file = "min30.rds")
hr1 <- readRDS(file = "hr1.rds")

min10_test <- min10 #%>% filter_index("2021-06-20" ~ "2021-06-30")

gc()

# compute number of entries
N = nrow(min10_test)/n_keys(min10_test)

# initialize our accuracy tibble
fc10_benchmark <- NULL;

# set up parallelisation
registerDoParallel(cl <- makeCluster(numThreads))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc10_benchmark <- foreach(i = seq(ceiling(N*TrainingProportion),N-6,6), .combine = bind_rows, .packages = c("fpp3")) %dopar%
  {
    gc()
    
    # take training set from elements 1 up to i
    min10_tr_benchmark <- min10_test %>%
      slice_head(n = i)
    
    fc_benchmark <- NULL;
    
    # initialize the accuracy tibble
    fc_benchmark <- fc_benchmark %>%
      bind_rows(min10_tr_benchmark %>%
                  model(naive_model = NAIVE(Energy)) %>%
                  forecast(h=6))
    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      mod_tmp <- min10_tr_benchmark %>%
        model(naive_model = NAIVE(Energy))
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

min20_test <- min20 #%>% filter_index("2021-06-20" ~ "2021-06-30")

gc()

# compute number of entries
N = nrow(min20_test)/n_keys(min20_test)

# initialize our accuracy tibble
fc20_benchmark <- NULL;

# set up parallelisation
registerDoParallel(cl <- makeCluster(numThreads))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc20_benchmark <- foreach(i = seq(ceiling(N*TrainingProportion),N-3,3), .combine = bind_rows, .packages = c("fpp3")) %dopar%
  {
    
    gc()
    
    # take training set from elements 1 up to i
    min20_tr_benchmark <- min20_test %>%
      slice_head(n = i)
    
    fc_benchmark <- NULL;
    
    # initialize the accuracy tibble
    fc_benchmark <- fc_benchmark %>%
      bind_rows(min20_tr_benchmark %>%
                  model(naive_model = NAIVE(Energy)) %>%
                  forecast(h=3))
    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      mod_tmp <- min20_tr_benchmark %>%
        model(naive_model = NAIVE(Energy))
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

min30_test <- min30 #%>% filter_index("2021-06-20" ~ "2021-06-30")

gc()

# compute number of entries
N = nrow(min30_test)/n_keys(min30_test)

# initialize our accuracy tibble
fc30_benchmark <- NULL;

# set up parallelisation
registerDoParallel(cl <- makeCluster(numThreads))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc30_benchmark <- foreach(i = seq(ceiling(N*TrainingProportion),N-2,2), .combine = bind_rows, .packages = c("fpp3")) %dopar%
  {
    gc()
    
    # take training set from elements 1 up to i
    min30_tr_benchmark <- min30_test %>%
      slice_head(n = i)
    
    fc_benchmark <- NULL;
    
    # initialize the accuracy tibble
    fc_benchmark <- fc_benchmark %>%
      bind_rows(min30_tr_benchmark %>%
                  model(naive_model = NAIVE(Energy)) %>%
                  forecast(h=2))
    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      mod_tmp <- min30_tr_benchmark %>%
        model(naive_model = NAIVE(Energy))
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

hr1_test <- hr1 #%>% filter_index("2021-06-20" ~ "2021-06-30")

gc()

# compute number of entries
N = nrow(hr1_test)/n_keys(hr1_test)

# initialize our accuracy tibble
fc1_benchmark <- NULL;

# set up parallelisation
registerDoParallel(cl <- makeCluster(numThreads))

# do our TSCV manually, starting from 90% of the dataset up to the second last element
fc1_benchmark <- foreach(i = seq(ceiling(N*TrainingProportion),N-1,1), .combine = bind_rows, .packages = c("fpp3")) %dopar%
  {
    gc()
    
    # take training set from elements 1 up to i
    hr1_tr_benchmark <- hr1_test %>%
      slice_head(n = i)
    
    fc_benchmark <- NULL;
    
    # initialize the accuracy tibble
    fc_benchmark <- fc_benchmark %>%
      bind_rows(hr1_tr_benchmark %>%
                  model(naive_model = NAIVE(Energy)) %>%
                  forecast(h=1))
    # extract fitted and residuals
    if (i == ceiling(N*TrainingProportion)){
      mod_tmp <- hr1_tr_benchmark %>%
        model(naive_model = NAIVE(Energy))
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