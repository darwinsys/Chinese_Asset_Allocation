library(gdata)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(depmixS4)
library(TTR)
library(quantmod)
 ####上海指数
data_shanghai <- read.csv("HMM/index_shanghai.csv")
data_shanghai <- as.xts(data_shanghai[, 2:5], order.by=strptime(data_shanghai[,1], format="%Y-%m-%d"))

ATRindicator <- ATR(data_shanghai[, 2:4], n = 14)
ATR <- as.xts(ATRindicator[, 2])

log_return <- na.omit(Return.calculate(data_shanghai[,4], method="discrete"))
ret_benchmark_weeklys <- na.omit(Return.calculate(benchmark[endpoints(benchmark,on =  "weeks")]), method="discrete")

ret_benchmark_5d <- na.omit(benchmark / lag(benchmark, 5) - 1) 
ret_benchmark_10d <- na.omit(benchmark / lag(benchmark, 10) - 1)

data_test <- na.omit(cbind(log_return, lag(log_return, 1),
                           lag(log_return, 5), ATR))
colnames(data_test) <- c("shanghai", "lag1", "lag2", "ATR")
data_test <- as.xts(data_test, tzone=Sys.getenv("TZ"))


##### HMM
data_test1 <- data_test[4000:6037, ]
data_shanghai1 <- data_shanghai[index(data_test1), 4]
set.seed(1)
HMM <- depmix(list(shanghai~1, lag1~1, lag2~1, ATR~1), data = data_test1, 
              nstates = 5, family=list(gaussian(), gaussian(), gaussian(), gaussian()))
HMMfit <- fit(HMM, verbose=FALSE)
HMMpost <- posterior(HMMfit)

state1 <- as.xts(HMMpost$S1, order.by = index(data_test1), tzone=Sys.getenv("TZ"))

regimes <- as.xts(HMMpost$state, order.by = index(data_test1), tzone=Sys.getenv("TZ"))

plot(data_shanghai1)
points(data_shanghai1 *  (regimes == 1), pch=20,  col="black")
points(data_shanghai1 * (regimes == 2), pch=20, col="green")
points(data_shanghai1 * (regimes == 3), pch=20, col="red")
points(data_shanghai1 * (regimes == 4), pch=20, col="orange")
points(data_shanghai1 * (regimes == 5), pch=20, col="blue")

regimes_ordered <- regimes * 0;
regimes_ordered[regimes == 3] <- 5;
regimes_ordered[regimes == 2] <- 1;
regimes_ordered[regimes == 5] <- 3;
regimes_ordered[regimes == 1] <- 2;
regimes_ordered[regimes == 4] <- 4;

ret_regimes <- na.omit(cbind(
                             data_test1[, 1] * (regimes==1), 
                            data_test1[,1]*(regimes==2),
                             data_test1[, 1] * (regimes==3), 
                             #data_test1[, 1] * (regimes==4), 
                             #data_test1[, 1] * (regimes==5), 
                             data_test1[, 1]))
charts.PerformanceSummary(ret_regimes)



