#### Investigate seasonality ####

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

min10 <- readRDS(file = "min10.rds")
min20 <- readRDS(file = "min20.rds")
min30 <- readRDS(file = "min30.rds")
hr1 <- readRDS(file = "hr1.rds")

min10 %>% filter(!is_aggregated(Group), !is_aggregated(Subgroup)) %>% gg_season(Energy, labels = "both")

min10 %>%
  filter(Group == "A", Subgroup == "A1") %>%
  model(
    STL(Energy ~ season(period = "day") + season(period="week") + season(period="month") + season(period="year"), robust = TRUE)) %>%
  components() %>% autoplot()

min20 %>%
  filter(is_aggregated(Group), is_aggregated(Subgroup)) %>%
  filter_index("2021-06") %>%
  model(
    STL(Energy ~ season(period=3) + season(period=3*24),
        robust = TRUE)) %>%
  components() %>%
  autoplot()

min30 %>%
  filter(is_aggregated(Group), is_aggregated(Subgroup)) %>%
  filter_index("2021-06") %>%
  model(
    STL(Energy ~ season(period=2) + season(period=2*24),
        robust = TRUE)) %>%
  components() %>%
  autoplot()

hr1 %>%
  filter(is_aggregated(Group), is_aggregated(Subgroup)) %>%
  filter_index("2021-06") %>%
  model(
    STL(Energy ~ season(period=24),
        robust = TRUE)) %>%
  components() %>%
  autoplot()