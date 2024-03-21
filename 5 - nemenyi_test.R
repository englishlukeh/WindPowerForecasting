#### Nemenyi Test ####
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


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set this to your working directory
library(tsutils)

# cross-sec
# figs 6'x6'
nem_cross_sec <- read_excel("Nemenyi-Cross-Sectional.xlsx")


x_rMAE <- as.matrix(nem_cross_sec[,1:12])
x_rRMSE <- as.matrix(nem_cross_sec[,13:24])

colnames(x_rMAE) <- colnames(nem_cross_sec[1:12])
colnames(x_rRMSE) <- colnames(nem_cross_sec[13:24])

x_LR_rMAE <- x_rMAE[,1:6]
x_LR_rRMSE <- x_rRMSE[,1:6]

x_gb_rMAE <- x_rMAE[,7:12]
x_gb_rRMSE <- x_rRMSE[,7:12]

tmp1_rRMSE <- x_LR_rRMSE
tmp1_rMAE <- x_LR_rMAE

nemenyi(x_rMAE, plottype = "mcb")
nemenyi(x_rRMSE, plottype = "mcb", labels = c("LR", "LR-BU", "LR-TD", "LR-MO", "LR-OLS", "LR-MinT-Shr", "GB", "GB-BU", "GB-TD", "GB-MO", "GB-OLS", "GB-MinT-Shr"))
nemenyi(x_LR_rMAE, plottype = "mcb")
nemenyi(x_LR_rRMSE, plottype = "mcb", labels = c("LR", "LR-BU", "LR-TD", "LR-MO", "LR-OLS", "LR-MinT-Shr", "GB", "GB-BU", "GB-TD", "GB-MO", "GB-OLS", "GB-MinT-Shr"))
nemenyi(x_gb_rMAE, plottype = "mcb")
nemenyi(x_gb_rRMSE, plottype = "mcb")

# now cross-temp
nem_cross_temp <- read_excel("Nemenyi-Cross-Temporal.xlsx")

x_rMAE <- as.matrix(nem_cross_temp[,1:14])
x_rRMSE <- as.matrix(nem_cross_temp[,15:28])

colnames(x_rMAE) <- colnames(nem_cross_temp[1:14])
colnames(x_rRMSE) <- colnames(nem_cross_temp[15:28])

x_LR_rMAE <- x_rMAE[,1:7]
x_LR_rRMSE <- x_rRMSE[,1:7]

x_gb_rMAE <- x_rMAE[,8:14]
x_gb_rRMSE <- x_rRMSE[,8:14]

nemenyi(x_rMAE, plottype = "mcb")
nemenyi(x_rRMSE, plottype = "mcb", labels = c("LR", "LR-T-ACOV", "LR-TCS-WLSV-SHR", "LR-CST-SHR-ACOV", "LR-ITE-ACOV-SHR", "LR-OCT-WLSV", "LR-CT(BU)", "GB", "GB-T-ACOV", "GB-TCS-WLSV-SHR", "GB-CST-SHR-ACOV", "GB-ITE-ACOV-SHR", "GB-OCT-WLSV", "GB-CT(BU)"))
nemenyi(x_LR_rMAE, plottype = "mcb")
nemenyi(x_LR_rRMSE, plottype = "mcb", labels = c("LR", "LR-T-ACOV", "LR-TCS-WLSV-SHR", "LR-CST-SHR-ACOV", "LR-ITE-ACOV-SHR", "LR-OCT-WLSV", "LR-CT(BU)"))
nemenyi(x_gb_rMAE, plottype = "mcb")
nemenyi(x_gb_rRMSE, plottype = "mcb")

#### line plot for accuracy metric ####
# cross-sectional
# 10'x5'
AvgRelRMSE_lr = c(0.9660,0.9673,0.9559,0.9634,0.9566,0.9519)
AvgRelRMSE_lgbm = c(0.9724,0.9717,0.9800,0.9767,0.9745,0.9665)
AvgRelRMSE_Y_lr = c(0.979,0.981,0.971,0.974,0.971,0.971)
AvgRelRMSE_Y_lgbm = c(1.033,1.036,1.011,1.029,1.022,1.026)
AvgRelMAE_lr = c(1.039,1.042,1.010,1.030,1.014,1.010)
AvgRelMAE_lgbm = c(1.048,1.049,1.043,1.047,1.038,1.032)

x = c(1,2,3,4,5,6)
recon <- list("Unreconciled", "BU", "TD", "MO", "OLS", "MinT")

plot( 0, type="n", xlim=c(0.5,6.5), ylim=c(0.950,1.06), ylab="AvgRelRMSE", xaxt='n', xlab=element_blank())
axis(1, at=1:6, labels=recon)
lines(x, AvgRelRMSE_lr, col="red")
points(x, AvgRelRMSE_lr, col="red", pch=15)
lines(x, AvgRelRMSE_lgbm, col="blue")
points(x, AvgRelRMSE_lgbm, col="blue", pch=15)
lines(x, AvgRelRMSE_Y_lr, col="green")
points(x, AvgRelRMSE_Y_lr, col="green", pch=15)
lines(x, AvgRelRMSE_Y_lgbm, col="orange")
points(x, AvgRelRMSE_Y_lgbm, col="orange", pch=15)

legend( x="topright", 
        legend=c("LR - Data Set A","GB - Data Set A","LR - Data Set B","GB - Data Set B"),
        col=c("red","blue","green","orange"), lwd=1, lty=c(1,1), 
        pch=c(NA,NA) )

# cross-temporal
AvgRelRMSE_lr = c(0.9660,0.9087,0.9391,0.9100,0.9187,0.9188,0.9091)
AvgRelRMSE_lgbm = c(0.9724,0.9107,0.9379,0.9160,0.9262,0.9255,0.9140)
AvgRelRMSE_Y_lr = c(0.9791,0.9227,0.9543,0.9303,0.9405,0.9402,0.9573)
AvgRelRMSE_Y_lgbm = c(1.0331,0.9911,1.0035,0.9781,0.9870,0.9868,0.9766)
AvgRelMAE_lr = c(1.039,0.970,1.015,0.967,0.980,0.980,0.965)
AvgRelMAE_lgbm = c(1.048,0.978,1.015,0.977,0.991,0.990,0.975)
x = c(1,2,3,4,5,6,7)
recon <- list("Unreconciled", "BU-CT", "THF", "TCS", "CST", "ITE", "OCT")

plot( 0, type="n", xlim=c(0.5,7.5), ylim=c(0.905,1.05), ylab="AvgRelRMSE", xaxt='n', xlab=element_blank())
axis(1, at=1:7, labels=recon)
lines(x, AvgRelRMSE_lr, col="red")
points(x, AvgRelRMSE_lr, col="red", pch=15)
lines(x, AvgRelRMSE_lgbm, col="blue")
points(x, AvgRelRMSE_lgbm, col="blue", pch=15)
lines(x, AvgRelRMSE_Y_lr, col="green")
points(x, AvgRelRMSE_Y_lr, col="green", pch=15)
lines(x, AvgRelRMSE_Y_lgbm, col="orange")
points(x, AvgRelRMSE_Y_lgbm, col="orange", pch=15)

legend( x="topright", 
        legend=c("LR - Data Set A","GB - Data Set A","LR - Data Set B","GB - Data Set B"),
        col=c("red","blue","green","orange"), lwd=1, lty=c(1,1), 
        pch=c(NA,NA) )