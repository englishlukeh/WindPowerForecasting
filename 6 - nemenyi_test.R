#### Nemenyi Test ####
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

#nemenyi(x_rMAE, plottype = "mcb")
nemenyi(x_rRMSE, plottype = "mcb", labels = c("LR", "LR-BU", "LR-TD", "LR-MO", "LR-OLS", "LR-MinT-Shr", "GB", "GB-BU", "GB-TD", "GB-MO", "GB-OLS", "GB-MinT-Shr"))
#nemenyi(x_LR_rMAE, plottype = "mcb")
nemenyi(x_LR_rRMSE, plottype = "mcb", labels = c("LR", "LR-BU", "LR-TD", "LR-MO", "LR-OLS", "LR-MinT-Shr", "GB", "GB-BU", "GB-TD", "GB-MO", "GB-OLS", "GB-MinT-Shr"))
#nemenyi(x_gb_rMAE, plottype = "mcb")
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

#nemenyi(x_rMAE, plottype = "mcb")
nemenyi(x_rRMSE, plottype = "mcb", labels = c("LR", "LR-T-ACOV", "LR-TCS-WLSV-SHR", "LR-CST-SHR-ACOV", "LR-ITE-ACOV-SHR", "LR-OCT-WLSV", "LR-CT(BU)", "GB", "GB-T-ACOV", "GB-TCS-WLSV-SHR", "GB-CST-SHR-ACOV", "GB-ITE-ACOV-SHR", "GB-OCT-WLSV", "GB-CT(BU)"))
#nemenyi(x_LR_rMAE, plottype = "mcb")
nemenyi(x_LR_rRMSE, plottype = "mcb", labels = c("LR", "LR-T-ACOV", "LR-TCS-WLSV-SHR", "LR-CST-SHR-ACOV", "LR-ITE-ACOV-SHR", "LR-OCT-WLSV", "LR-CT(BU)"))
#nemenyi(x_gb_rMAE, plottype = "mcb")
nemenyi(x_gb_rRMSE, plottype = "mcb")

#### line plot for accuracy metric ####
# cross-sectional
# 10'x5'
AvgRelRMSE_lr = c(0.9665,
                  0.9673,
                  0.9559,
                  0.9634,
                  0.9566,
                  0.9519)
AvgRelRMSE_lgbm = c(0.9719,
                    0.9709,
                    0.9809,
                    0.9748,
                    0.9747,
                    0.9718
)
AvgRelRMSE_Y_lr = c(0.9791,
                    0.9806,
                    0.9707,
                    0.9736,
                    0.9711,
                    0.9704
)
AvgRelRMSE_Y_lgbm = c(1.0353,
                      1.0381,
                      1.0134,
                      1.0299,
                      1.0232,
                      1.0267
)
AvgRelMAE_lr = c(1.039,1.042,1.010,1.030,1.014,1.010)
AvgRelMAE_lgbm = c(1.048,1.049,1.043,1.047,1.038,1.032)

x = c(1,2,3,4,5,6)
recon <- list("Unreconciled", "BU", "TD", "MO", "OLS", "MinT-shr")

# dataset A
plot( 0, type="n", xlim=c(0.5,6.5), ylim=c(0.950,0.98), ylab="AvgRelRMSE", xaxt='n', xlab=element_blank())
axis(1, at=1:6, labels=recon)
lines(x, AvgRelRMSE_lr, col="red")
points(x, AvgRelRMSE_lr, col="red", pch=15)
lines(x, AvgRelRMSE_lgbm, col="blue")
points(x, AvgRelRMSE_lgbm, col="blue", pch=15)

legend( x="topright", 
        legend=c("LR","GB"),
        col=c("red","blue"), lwd=1, lty=c(1,1), 
        pch=c(NA,NA) )

# dataset B
plot( 0, type="n", xlim=c(0.5,6.5), ylim=c(0.960,1.04), ylab="AvgRelRMSE", xaxt='n', xlab=element_blank())
axis(1, at=1:6, labels=recon)
lines(x, AvgRelRMSE_Y_lr, col="green")
points(x, AvgRelRMSE_Y_lr, col="green", pch=15)
lines(x, AvgRelRMSE_Y_lgbm, col="orange")
points(x, AvgRelRMSE_Y_lgbm, col="orange", pch=15)

legend( x="topright", 
        legend=c("LR","GB"),
        col=c("green","orange"), lwd=1, lty=c(1,1), 
        pch=c(NA,NA) )


# cross-temporal
AvgRelRMSE_lr = c(0.9665,0.9086,0.9322,0.9009,0.9122,0.9124,0.9186)
AvgRelRMSE_lgbm = c(0.9719,0.9062,0.9294,0.9041,0.9186,0.9179,0.9138)
AvgRelRMSE_Y_lr = c(0.9791,0.9226,0.9495,0.9195,0.9337,0.9332,0.9339)
AvgRelRMSE_Y_lgbm = c(1.0353,0.9918,1.0016,0.9752,0.9868,0.9859,0.9879)
x = c(1,2,3,4,5,6,7)
recon <- list("Unreconciled", "CT(BU)", "T-ACOV", "TCS-WLSV-SHR", "CST-SHR-ACOV", "ITE-ACOV-SHR", "OCT-WLSV")

plot( 0, type="n", xlim=c(0.5,7.5), ylim=c(0.905,0.98), ylab="AvgRelRMSE", xaxt='n', xlab=element_blank())
axis(1, at=1:7, labels=recon, cex.axis=0.7)
lines(x, AvgRelRMSE_lr, col="red")
points(x, AvgRelRMSE_lr, col="red", pch=15)
lines(x, AvgRelRMSE_lgbm, col="blue")
points(x, AvgRelRMSE_lgbm, col="blue", pch=15)


legend( x="topright", 
        legend=c("LR","GB"),
        col=c("red","blue"), lwd=1, lty=c(1,1), 
        pch=c(NA,NA) )

plot( 0, type="n", xlim=c(0.5,7.5), ylim=c(0.93,1.04), ylab="AvgRelRMSE", xaxt='n', xlab=element_blank())
axis(1, at=1:7, labels=recon, cex.axis=0.7)
lines(x, AvgRelRMSE_Y_lr, col="green")
points(x, AvgRelRMSE_Y_lr, col="green", pch=15)
lines(x, AvgRelRMSE_Y_lgbm, col="orange")
points(x, AvgRelRMSE_Y_lgbm, col="orange", pch=15)
legend( x="topright", 
        legend=c("LR","GB"),
        col=c("green","orange"), lwd=1, lty=c(1,1), 
        pch=c(NA,NA) )