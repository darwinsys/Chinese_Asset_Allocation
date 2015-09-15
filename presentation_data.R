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
                           lag(log_return, 2), ATR))
colnames(data_test) <- c("shanghai", "lag1", "lag2", "ATR")
data_test <- as.xts(data_test, tzone=Sys.getenv("TZ"))


##### HMM
data_test1 <- data_test[5000:6000, ]
data_shanghai1 <- data_shanghai[index(data_test1), 4]
set.seed(1)
HMM <- depmix(list(shanghai~1, lag1~1, lag2~1, ATR), data = data_test1, 
              nstates = 5, family=list(gaussian(), gaussian(), gaussian(), gaussian()))
HMMfit <- fit(HMM, verbose=FALSE)
HMMpost <- posterior(HMMfit)

state1 <- as.xts(HMMpost$S1, order.by = index(data_test1), tzone=Sys.getenv("TZ"))

regimes <- as.xts(HMMpost$state, order.by = index(data_test1), tzone=Sys.getenv("TZ"))

plot(data_shanghai1)
points(data_shanghai1* (regimes == 1), pch=20,  col="black")
points(data_shanghai1 * (regimes == 2), pch=20, col="green")
points(data_shanghai1 * (regimes == 3), pch=20, col="red")
points(data_shanghai1 * (regimes == 4), pch=20, col="orange")

ret_regimes <- na.omit(cbind(
                             data_test1[, 1] * (regimes==1), 
                               data_test1[,1]*(regimes==2),
                             data_test1[, 1] * (regimes==3), 
                             data_test1[, 1] * (regimes==4), 
                             data_test1[, 1] * (regimes==5), 
                             data_test1[, 1]))
charts.PerformanceSummary(ret_regimes)



plodata_bm <- read.csv("HMM/NAV_5ETFs.csv")
benchmark <- as.xts(data_bm[, 2:5], order.by=strptime(data_bm[,1], format="%m/%d/%y", tz=""))
ret_benchmark <- na.omit(Return.calculate(benchmark, method="discrete"))
ret_benchmark_weeklys <- na.omit(Return.calculate(benchmark[endpoints(benchmark,on =  "weeks")]), method="discrete")

ret_sh <- ret_benchmark[, 1]
cum_ret_sh <- cumprod(1+ret_sh)
nr_win <- sum(ret_sh > 0)
nr_lose <- sum(ret_sh < 0)

macd <- TTR::MACD(benchmark[, 1])
signal_macd <- na.omit(macd$macd > macd$signal)
ret_macd <- na.omit(ret_sh * lag(signal_macd, 1))

sma250 <- TTR::SMA(benchmark[,1], 20)
signal250 <- na.omit(benchmark[, 1] > sma250)
ret_250 <- na.omit(ret_sh * lag(signal250, 1))

ema5 <- TTR::EMA(benchmark[,1] , 20)
signal_ema5 <- benchmark[,1] > ema5;
ret_ema5 <- na.omit(ret_sh * lag(signal_ema5, 1))

ret_c <- na.omit(cbind(ret_250, ret_ema5, ret_sh))
colnames(ret_c) <- c("20MA","20EMA", "Zhongzheng500")
charts.PerformanceSummary(ret_c[1:1580])

sma60 <- TTR::SMA(benchmark[, 1], 60)
signal60 <- na.omit(benchmark[,1] > sma60)
ret_250 <- na.omit(ret_sh * lag(signal, 1))

weight <- ret_sh * 0; weight[1]<- 1;
ret_strategy <- ret_sh * 0; ret_strategy[1] <- ret_sh[1];
entry_point <- 1;
for (i in  2:(nrow(ret_250)-1)) {
  mdd <- PerformanceAnalytics::maxDrawdown(ret_strategy[entry_point:i, ]);
  if (mdd  <  -0.08) {
    weight[i+1] <- 0;
    entry_point <- i+1;
  }else {
    weight[i] <- 1;
    ret_strategy[i] <- ret_sh[i];
  }
  ret_strategy[i] <- ret_sh[i] * weight[i];
  
  
}