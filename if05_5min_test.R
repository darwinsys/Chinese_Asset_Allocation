library(gdata)
library(TTR)
library(xts)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(mclust)
library(mhsmm)
library(rattle)

source("HMM/gmmhmm.R");

data_if05 <- read.csv("HMM/if1509_5min.csv")
dates_if05 <- as.POSIXlt(strptime(data_if05[,1], format("%m/%d/%y %H:%M")))
data_if05 <- as.xts(data_if05[, 2:7], order.by=dates_if05)


ATRindicator <- ATR(data_if05[, 2:4], n = 14)
ATR <- as.xts(ATRindicator[, 2])
CO <- (data_if05[, 4] - data_if05[, 1])/data_if05[, 4]
CC <- (data_if05[, 4] - lag(data_if05[, 4], 1))/data_if05[, 4]
OC <- (data_if05[, 1] - lag(data_if05[, 4], 1))/data_if05[, 4]
VV <- data_if05[, 6] / data_if05[, 5]
 

target <- (CC>0)*1
target1 <- na.omit(target + lag(target, 1))
target2 <- na.omit(target + lag(target,1) + lag(target, 2))

data_learning <- na.omit(cbind(
    target2,
    CC, CO, OC, VV, ATR,
    lag(CC, 1), lag(CO, 1), lag(OC, 1), lag(VV, 1)
))
rattle();



data_test <- na.omit(cbind(CC, lag(CC, 1), lag(CC, 2), 
                           lag(CC,5), lag(CC,10) , 
                           lag(CC,20)));
data_test <- na.omit(cbind(CO, lag(CO, 1), lag(CO, 2), 
                           lag(CO,5), lag(CO,10),
                           lag(CO, 20), lag(CO,100)));
gmm <- gmm_training(data_test[1000:5000]);
hmm <- hmm_training(gmm, data_test[5001:5500]);

charts.PerformanceSummary(hmm$hmm_ret_regime)