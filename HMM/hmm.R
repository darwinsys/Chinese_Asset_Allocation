library(gdata)
library(TTR)
library(xts)
library(fGarch)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

data_bm <- read.csv("benchmark.csv")
benchmark <- as.xts(data_bm[, 2:3], order.by=strptime(data_bm$date, format="%Y/%m/%d", tz=""))
ret_benchmark <- na.omit(Return.calculate(benchmark, method="discrete"))

dataset <- na.omit(cbind(ret_benchmark[,1], lag(ret_benchmark[, 1], 1), 
                         #lag(ret_benchmark[,1], 2), 
                         lag(ret_benchmark[,1], 10)
                         ))
n <- nrow(dataset)
data_training <- dataset[1:(n/2),]
data_test <- dataset[(n/2+1):n, ]
#### Using Mixture Model to determine the optimal number of regimes and the settings under 
#### each regime
mm_model <- Mclust(data_training)
mm_output <- summary(mm_model)

#### creating the HMM model
J <- mm_output$G
initial <- rep(1/J, J)
P <- matrix(rep(1/J, J*J), nrow=J)
mean <- list()
vcv <- list()
for (i in 1:J){
  mean[[i]] <- mm_output$mean[, i]
  vcv[[i]] <- mm_output$variance[,,i]
}
b <- list()
b$mu <- mean
b$sigma <- vcv
mv_model <- dmvnorm.hsmm;
#### training HMM model
hmm_model <- hmmspec(init=initial, trans=P, parms.emission = b, dens.emission =mv_model)
hmm_fitted <- hmmfit(data_training, hmm_model, mstep = mstep.mvnorm)

#### Predict future regimes
yhat <- predict(hmm_fitted, data_test, method="smoothed")
signal <- as.xts(x = yhat$s, order.by=index(data_test))

############################################################
#### In the training set, the regimes and returns
yhat_train <- as.xts(hmm_fitted$yhat, order.by = index(data_training), tzone=tzone(data_training))
ret_training_regime <- list()
for (i in 1:J) {
  ret_training_regime[[i]] <- data_training[,1] * (yhat_train == i)
}
ret_training_regime <- do.call(cbind, ret_training_regime)

### calculate the risk measures 
sharpe_training_regime_vol <- SharpeRatio(ret_training_regime)[1,]
max_sharpe_regime <- match(max(sharpe_training_regime_vol), sharpe_training_regime_vol)
calmar_training_regime <- CalmarRatio(ret_training_regime)
max_calmar_regime <- match(max(calmar_training_regime), calmar_training_regime)
sortino_training_regime <- SortinoRatio(ret_training_regime)
max_sortino_regime <- match(max(sortino_training_regime), sortino_training_regime)


ret_test_regime1 <- data_test[, 1] * lag(signal == max_sortino_regime, 1)

ret_s1 <- na.omit(cbind(ret_test_regime1, data_test[,1]))
charts.PerformanceSummary(ret_s1)

rbind(table.AnnualizedReturns(ret_s1), maxDrawdown(ret_s1), CalmarRatio(ret_s1))






