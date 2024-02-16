#### Check correlation of wind and Energy ####

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

#extract turbine names
tmp <- hr1 %>% group_keys() %>% filter(!is_aggregated(Subgroup))
node_names <- as.character(as.list(tmp[2])$Subgroup)

#check correlation for hourly - use 6'x5' for figs
for (val in node_names) {
  tmp <- hr1 %>%
    filter(Subgroup == val)
  
  Filename = paste(val, "_hourly.png", sep = "")
  tmp_plot <- ggplot(tmp, aes(x = Wind_Speed, y = Energy)) +
    geom_point() +
    labs(x = "Wind Speed (m/s)",
         y = "Total Energy (kWh)", title = paste("Hourly Data, Turbine:", val))
  tmp_plot + theme(text=element_text(size=20))
  ggsave(Filename, tmp_plot)
}

#check correlation for 10 minutely
for (val in node_names) {
  tmp <- min10 %>%
    filter(Subgroup == val)
  
  Filename = paste(val, "_10minutely.png", sep = "")
  tmp_plot <- ggplot(tmp, aes(x = Wind_Speed, y = Energy)) +
    geom_point() +
    labs(x = "Wind Speed (m/s)",
         y = "Total Energy (kWh)", title = paste("10-Minutely Data, Turbine:", val))
  tmp_plot + theme(text=element_text(size=20))
  ggsave(Filename, tmp_plot)
}

#check correlation for 20 minutely
for (val in node_names) {
  tmp <- min20 %>%
    filter(Subgroup == val)
  
  Filename = paste(val, "_20minutely.png", sep = "")
  tmp_plot <- ggplot(tmp, aes(x = Wind_Speed, y = Energy)) +
    geom_point() +
    labs(x = "Wind Speed (m/s)",
         y = "Total Energy (kWh)", title = paste("20-Minutely Data, Turbine:", val))
  
  ggsave(Filename, tmp_plot)
}

#check correlation for 30 minutely
for (val in node_names) {
  tmp <- min30 %>%
    filter(Subgroup == val)
  
  Filename = paste(val, "_30minutely.png", sep = "")
  tmp_plot <- ggplot(tmp, aes(x = Wind_Speed, y = Energy)) +
    geom_point() +
    labs(x = "Wind Speed (m/s)",
         y = "Total Energy (kWh)", title = paste("30-Minutely Data, Turbine:", val))
  
  ggsave(Filename, tmp_plot)
}