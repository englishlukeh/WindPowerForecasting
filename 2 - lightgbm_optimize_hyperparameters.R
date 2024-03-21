####Optimize the hyperparameters of LightGBM model#####

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
numThreads = 1;

min10 <- readRDS(file = "min10_wFeatures.rds")
min20 <- readRDS(file = "min20_wFeatures.rds")
min30 <- readRDS(file = "min30_wFeatures.rds")
hr1 <- readRDS(file = "hr1_wFeatures.rds")

#### Define auxiliary functions for LightGBM regression ####
# function to convert x_data (i.e., all regressors) tsibble into matrix for lightgbm
to_x <- function(data){
  return(data %>%
           ungroup() %>%
           as_tibble() %>%
           select(-c(Group, Subgroup, Time, Energy, Wind_Speed)) %>%
           as.matrix())
}

# function to convert y data (i.e., Energy) tsibble into matrix for lightgbm
to_y <- function(data){
  return(data %>%
           ungroup() %>%
           as_tibble() %>%
           select(Energy) %>%
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
    params <- list(objective = "regression",
                   metric = "l2",
                   max_depth = grid_search[i, "max_depth"],
                   num_leaves= grid_search[i,"num_leaves"],
                   learning_rate = grid_search[i,"learning_rate"],
                   min_data_in_leaf = grid_search[i,"min_data_in_leaf"])
    model_b3[[i]] <- lightgbm::lgb.train(params = params,
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
N = nrow(min10)/n_keys(min10)

errors <- data.frame()

training_percentages = c(0.8)

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
                         dplyr::slice(floor(N*(ind + 0.1))+1:floor(N*(ind + 0.2))))
  validation_y <- to_y(min10 %>%
                         dplyr::slice(floor(N*(ind + 0.1))+1:floor(N*(ind + 0.2))))
  
  dtrain = lgb.Dataset(train_x, label = train_y)
  dtest = lgb.Dataset.create.valid(dtrain, test_x, label = test_y)
  
  grid_search <- expand.grid(
    num_leaves = c(110,120,130,140,150),
    max_depth= c(7,8,9),
    learning_rate= c(0.75,0.1,0.125),
    min_data_in_leaf = c(175,200,225,250),
    linear_lambda = c(0))
  
  errors <- errors %>% rbind(tuning_para(dtrain, dtest, validation_x, validation_y))
}

optimal <- grid_search[which.min(colMeans(errors)), ]
print(optimal)

#### Optimize hyperparameters 20-minutely ####
N = nrow(min20)/n_keys(min20)

errors <- data.frame()

training_percentages = c(0.8)

for (i in seq_along(training_percentages)){
  gc()
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
                         dplyr::slice(floor(N*(ind + 0.1))+1:floor(N*(ind + 0.2))))
  validation_y <- to_y(min20 %>%
                         dplyr::slice(floor(N*(ind + 0.1))+1:floor(N*(ind + 0.2))))
  
  dtrain = lgb.Dataset(train_x, label = train_y)
  dtest = lgb.Dataset.create.valid(dtrain, test_x, label = test_y)
  
  grid_search <- expand.grid(
    num_leaves = c(80,90,100,110,120),
    max_depth= c(6,7,8,9,10,11,12,13),
    learning_rate= c(0.5,0.75,0.1,0.125,0.15),
    min_data_in_leaf = c(250,275,300,325,350,375,400,425),
    linear_lambda = c(0))
  
  errors <- errors %>% rbind(tuning_para(dtrain, dtest, validation_x, validation_y))
}

optimal <- grid_search[which.min(colMeans(errors)), ]
print(optimal)

#### Optimize hyperparameters 30-minutely ####
N = nrow(min30)/n_keys(min30)

errors <- data.frame()

training_percentages = c(0.8)

for (i in seq_along(training_percentages)){
  gc()
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
                         dplyr::slice(floor(N*(ind + 0.1))+1:floor(N*(ind + 0.2))))
  validation_y <- to_y(min30 %>%
                         dplyr::slice(floor(N*(ind + 0.1))+1:floor(N*(ind + 0.2))))
  
  dtrain = lgb.Dataset(train_x, label = train_y)
  dtest = lgb.Dataset.create.valid(dtrain, test_x, label = test_y)
  
  grid_search <- expand.grid(
    num_leaves = c(50,70,100,130,160,180,200,250),
    max_depth= c(5,6,7,8,9,10,11),
    learning_rate= c(0.1,0.15,0.175,0.2),
    min_data_in_leaf = c(100,150,200,250,300,350,400),
    linear_lambda = c(0))
  
  errors <- errors %>% rbind(tuning_para(dtrain, dtest, validation_x, validation_y))
}

optimal <- grid_search[which.min(colMeans(errors)), ]
print(optimal)

#### Optimize hyperparameters 1-hourly ####
N = nrow(hr1)/n_keys(hr1)

errors <- data.frame()

training_percentages = c(0.8)

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
                         dplyr::slice(floor(N*(ind + 0.1))+1:floor(N*(ind + 0.2))))
  validation_y <- to_y(hr1 %>%
                         dplyr::slice(floor(N*(ind + 0.1))+1:floor(N*(ind + 0.2))))
  
  dtrain = lgb.Dataset(train_x, label = train_y)
  dtest = lgb.Dataset.create.valid(dtrain, test_x, label = test_y)
  
  grid_search <- expand.grid(
    num_leaves = c(60,80,100,120),
    max_depth= c(7,8,9,10),
    learning_rate= c(0.1,0.15,0.175,0.2,0.225,0.25),
    min_data_in_leaf = c(150,175,200,225,250),
    linear_lambda = c(0))
  
  
  errors <- errors %>% rbind(tuning_para(dtrain, dtest, validation_x, validation_y))
}

optimal <- grid_search[which.min(colMeans(errors)), ]
print(optimal)