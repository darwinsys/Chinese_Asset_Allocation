library(gdata)
library(TTR)
library(xts)
library(fGarch)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

data <- read.csv("NAV_5ETFs.csv")
data$X <- strptime(data$X, format="%m/%d/%y", tz = "")

etfs <- as.xts(data[,2:6], order.by = data$X, tz="")

etfs_weekly <- etfs[endpoints(etfs, on="weeks")]
etfs_monthly <- etfs[endpoints(etfs, on="months")]


data_bm <- read.csv("benchmark.csv")
benchmark <- as.xts(data_bm[, 2:3], order.by=strptime(data_bm$date, format="%Y/%m/%d", tz=""))
ret_benchmark <- Return.calculate(benchmark, method="discrete")
ret_benchmark_weekly <- Return.calculate(benchmark[endpoints(benchmark,on="weeks")])
ret_benchmark_monthly <- Return.calculate(benchmark[endpoints(benchmark,on="months")])
=======
benchmarks <- as.xts(data[,2:6], order.by = data$X, tz="")

benchmarks_weekly <- sectors[endpoints(benchmarks, on="weeks")]
benchmarks_monthly <- sectors[endpoints(benchmarks, on="months")]
>>>>>>> origin/master

### calc rets
ret_bm <- na.omit(Return.calculate(benchmarks, method="discrete"))
ret_bm_weekly <- na.omit(Return.calculate(benchmarks_weekly, method="discrete"))
ret_bm_monthly <- na.omit(Return.calculate(benchmarks_monthly, method="discrete"))


#### HMM
library(mhsmm)
J<-2
initial <- rep(1/J,J)
P <- matrix(c(.3,.5,.7,.5),nrow=J)
b <- list(mu=list( c(-3,0),c(1,2)),sigma=list(diag(2),matrix(c(4,2,2,3), ncol=2)))
model <- hmmspec(init=initial, trans=P, parms.emission=b,dens.emission=dmvnorm.hsmm)

test_data <- na.omit(cbind(ret_benchmark[, 1], lag(ret_benchmark[,1], 1)))
test_data1 <- test_data[1:1000]
test_data2 <- test_data[1001:1600]
h1 <- hmmfit(test_data1, model, mstep = mstep.mvnorm)
yhat <- predict(h1, test_data2)
signal <- as.xts(x = yhat$s, order.by=index(test_data2))


ret_test <- test_data2
ret_s1 <- ret_test[, 1] * lag(signal == 1)
charts.PerformanceSummary(cbind(ret_s1, ret_test[,1]))
ret_s2 <- ret_test[, 1] * lag(signal == 2)
charts.PerformanceSummary(cbind(ret_s2, ret_test[,1]))

### 3 state ZZ
library(mhsmm)
J<-3
initial <- rep(1/J,J)
P <- matrix(c(.1, .1, .1, .3,.5,.7,.5,.2,.1),nrow=J)
b <- list(mu=list(c(-3,0, -1),c(1,2, 3), c(2,3, -3)),
          sigma=list(diag(3),diag(3)*2, matrix(c(4,2,2,2, 3, 1, 2, 1,3), ncol=3)))
model <- hmmspec(init=initial, trans=P, parms.emission=b,dens.emission=dmvnorm.hsmm)

test_data <- na.omit(cbind(ret_benchmark[, 1], lag(ret_benchmark[,1], 20), lag(ret_benchmark[,1], 5)))
test_data1 <- test_data[1:2000]
test_data2 <- test_data[2000:2560]
h1 <- hmmfit(test_data, model, mstep = mstep.mvnorm)
yhat <- predict(h1, test_data2)
signal <- as.xts(x = yhat$s, order.by=index(test_data2), tzone=Sys.getenv("TZ"))


ret_test <- test_data2
ret_s1 <- ret_test[, 1] * lag(signal == 1, 1)
charts.PerformanceSummary(cbind(ret_s1, ret_test[,1]))
ret_s2 <- ret_test[, 1] * lag(signal == 2, 1)
charts.PerformanceSummary(cbind(ret_s2, ret_test[,1]))
ret_s3 <- ret_test[, 1] * lag(signal == 3, 1) 
charts.PerformanceSummary(cbind(ret_s3, ret_test[,1]))
charts.PerformanceSummary(cbind(ret_s2 + ret_s3, ret_s1, ret_s2, ret_s3, ret_test[,1]))

### 3 state ZZ and HS

J<-3
initial <- rep(1/J,J)
P <- matrix(c(.1, .1, .1, .3,.5,.7,.5,.2,.1),nrow=J)
b <- list(mu=list(c(-3,0, -1),c(1,2, 3), c(2,3, -3)),
          sigma=list(diag(3),diag(3)*2, matrix(c(4,2,2,2, 3, 1, 2, 1,3), ncol=3)))
model <- hmmspec(init=initial, trans=P, parms.emission=b,dens.emission=dmvnorm.hsmm)

test_data <- na.omit(cbind(ret_benchmark[, 1], ret_benchmark[,2], lag(ret_benchmark[,1],5)))
test_data1 <- test_data[1:2000]
test_data2 <- test_data[2000:2560]
h1 <- hmmfit(test_data1, model, mstep = mstep.mvnorm)
yhat <- predict(h1, test_data2, method="smoothed")
signal <- as.xts(x = yhat$s, order.by=index(test_data2), tzone=Sys.getenv("TZ"))


ret_test <- test_data2
ret_s1 <- ret_test[, 1] * lag(signal == 1, 1)
charts.PerformanceSummary(cbind(ret_s1, ret_test[,2]))
ret_s2 <- ret_test[, 1] * lag(signal == 2, 1)
charts.PerformanceSummary(cbind(ret_s2, ret_test[,2]))
ret_s3 <- ret_test[, 1] * lag(signal == 3, 1) 
charts.PerformanceSummary(cbind(ret_s3, ret_test[,2]))

ret_strategy <- cbind(ret_s2+ret_s3-ret_s1, ret_s1, ret_s2, ret_s3, ret_test[,1])
charts.PerformanceSummary(ret_strategy)

rbind(table.AnnualizedReturns(ret_strategy), maxDrawdown(ret_strategy), CalmarRatio(ret_strategy))

### 5 states
J<-5
initial <- rep(1/J,J)
P <- matrix(c(.1, .1,  0.2, 0.3, 0.5, .1, .3,.5,.7,.5,.2,.1, .3, .6, .1, .2, .1, .1, .3, .7, .1),nrow=J)
b <- list(mu=list(c(-3,0, -1),c(1,2, 3), c(2,3, -3), c(1, 3, 1), c(0, 3,0)),
          sigma=list(diag(3),diag(3)*2, matrix(c(4,2,2,2, 3, 1, 2, 1,3), ncol=3), matrix(c(4,2,2,2, 3, 1, 2, 1,3), ncol=3),
          matrix(c(4,2,2,2, 3, 1, 2, 1,3), ncol=3)))
model <- hmmspec(init=initial, trans=P, parms.emission=b,dens.emission=dmvnorm.hsmm)

#ret_target <- ret_benchmark_weekly
ret_target <- ret_benchmark
nr_target <- 1
test_data <- na.omit(cbind(ret_target[, nr_target], 
                           lag(ret_target[,nr_target], 1), lag(ret_target[,nr_target],2)))
#test_data1 <- test_data[1:200, ]
#test_data2 <- test_data[200:540, ]
test_data1 <- test_data[1:1500, ]
test_data2 <- test_data[1500:2580, ]

h1 <- hmmfit(test_data1, model, mstep = mstep.mvnorm)
yhat <- predict(h1, test_data2, method="smoothed")
signal <- as.xts(x = yhat$s, order.by=index(test_data2), tzone=Sys.getenv("TZ"))


ret_test <- test_data2
ret_s1 <- ret_test[, 1] * lag(signal == 1, 1)
charts.PerformanceSummary(cbind(ret_s1, ret_test[,1]))
ret_s2 <- ret_test[, 1] * lag(signal == 2, 1)
charts.PerformanceSummary(cbind(ret_s2, ret_test[,1]))
ret_s3 <- ret_test[, 1] * lag(signal == 3, 1) 
charts.PerformanceSummary(cbind(ret_s3, ret_test[,1]))
ret_s4 <- ret_test[, 1] * lag(signal == 4, 1) 
charts.PerformanceSummary(cbind(ret_s4, ret_test[,1]))
ret_s5 <- ret_test[, 1] * lag(signal == 5, 1) 
charts.PerformanceSummary(cbind(ret_s5, ret_test[,1]))

ret_strategy <- cbind(ret_s5+ret_s4, ret_s1, ret_s2, ret_s3, ret_s4, ret_s5, ret_test[,1])
charts.PerformanceSummary(ret_strategy)

rbind(table.AnnualizedReturns(ret_strategy), maxDrawdown(ret_strategy), CalmarRatio(ret_strategy))


