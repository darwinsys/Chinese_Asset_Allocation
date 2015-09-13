library(gdata)
library(TTR)
library(xts)
library(fGarch)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(mclust)
library(mhsmm)

data_bm <- read.csv("HMM/index_szsh.csv")
benchmark <- as.xts(data_bm[, 2:3], order.by=strptime(data_bm[,1], format="%m/%d/%y", tz=""))
ret_benchmark <- na.omit(Return.calculate(benchmark, method="discrete"))
ret_benchmark_weeklys <- na.omit(Return.calculate(benchmark[endpoints(benchmark,on =  "weeks")]), method="discrete")


data_bm <- read.csv("benchmark.csv")
benchmark <- as.xts(data_bm[, 2:3], order.by=strptime(data_bm$date, format="%Y/%m/%d", tz=""))
ret_benchmark <- na.omit(Return.calculate(benchmark, method="discrete"))
ret_benchmark_weekly <- na.omit(Return.calculate(benchmark[endpoints(benchmark,on="weeks")]))
ret_benchmark_monthly <- Return.calculate(benchmark[endpoints(benchmark,on="months")])

data_etfs <- read.csv("Data/NAV_5ETFs.csv")
etfs <- as.xts(data_etfs[, 2:6], order.by=strptime(data_etfs[,1], format="%m/%d/%y", tz=""))
ret_etfs <- na.omit(Return.calculate(etfs, method="discrete"))
ret_etfs_weekly <- na.omit(Return.calculate(etfs[endpoints(etfs,on="weeks")]))

##### 目标是上证指数
ret_target <- ret_benchmark[,1] ### shanghai 
#ret_target <- ret_benchmark[,2] ### shenzhen

n <- nrow(dataset)
data_training <- dataset[1:(n*0.9),]

data_test <- dataset[(n*0.7+1):n, ]


signal <- gmm_hmm_strategy(data_training, data_test, ret_target)

ret_test_regime1 <- data_test[, 1] * lag(signal, 1)

ret_s1 <- na.omit(cbind(ret_test_regime1, data_test[,1]))
charts.PerformanceSummary(ret_s1)

rbind(table.AnnualizedReturns(ret_s1), maxDrawdown(ret_s1), CalmarRatio(ret_s1))


data_etfs <- read.csv("Data/NAV_5ETFs.csv")
etfs <- as.xts(data_etfs[, 2:6], order.by=strptime(data_etfs[,1], format="%m/%d/%y", tz=""))
ret_etfs <- na.omit(Return.calculate(etfs, method="discrete"))
ret_etfs_weekly <- na.omit(Return.calculate(etfs[endpoints(etfs,on="weeks")]))


##########################################################################
####### testing rolling window strategy

data_index <- read.csv("HMM/index_shanghai.csv")
index <- as.xts(data_index[, 2:5], order.by=strptime(data_index[,1], format="%Y-%m-%d", tz=Sys.getenv("TZ")))
index <- index[index$high != index$low]

ret_index <- na.omit(Return.calculate(index, method="discrete"))
ret_index_weekly <- na.omit(Return.calculate(index[endpoints(index, on="weeks")], method="discrete"))
bis <- (index[, 4] - index[, 3])/(index[, 2] - index[, 3])




ret_target <- ret_index_weekly[, 4]

dataset <- na.omit(cbind(ret_target, lag(ret_target, 1), lag(ret_target, 5), 
                         lag(ret_target, 10)#, lag(ret_target, 10)#index[,4] - index[,1]# lag(ret_target, 10)
))

#### test 1
ret_target <- ret_etfs[, 2]
ret_target_5d <- (etfs[,2]/lag(etfs[,2], 5) - 1)
ret_target_10d <- (etfs[,2]/lag(etfs[,2], 10) - 1)
dataset <- na.omit(cbind(ret_target, lag(ret_target, 1), lag(ret_target, 10),
                         #ret_target_5d, lag(ret_target_5d, 1)
          ))

#### test 1
ret_target <- ret_etfs[, 2]
ret_target_5d <- (etfs[,2]/lag(etfs[,2], 5) - 1)
ret_target_10d <- (etfs[,2]/lag(etfs[,2], 10) - 1)
dataset <- na.omit(cbind(ret_target, lag(ret_target, 1), lag(ret_target, 10),
                         #ret_target_5d, lag(ret_target_5d, 1)
))

#### test 2
###################
## using hushen 300 ETF as the underlying. I am using gmmhmm functions (but with moving 
## windows as 250 days = 1Year). I used the daily returns of hushen (today, 1 and 5 days ago)
## daily returns of zhongzheng 500 (today, and 1 day ago), and daily reutrns of SPY ETF
#########################################
ret_target <- ret_etfs[, 2]
ret_target_5d <- (etfs[,2]/lag(etfs[,2], 5) - 1)
ret_target_10d <- (etfs[,2]/lag(etfs[,2], 10) - 1)
dataset <- na.omit(cbind(ret_target, lag(ret_target, 1), lag(ret_target, 5),
                         ret_etfs[,1], lag(ret_etfs[,1], 1),ret_etfs[, 3]
                         #ret_target_5d, lag(ret_target_5d, 1)
))
source('HMM/gmmhmm.R')
gmmhmm(data=dataset[800:1592], n_start = 500)


#################################################



##################################################################################

gmm_hmm_strategy <- function(data_training, data_test, ret_target) {
  #### Using Mixture Model to determine the optimal number of regimes and the settings under 
  #### each regime
  mm_model <- Mclust(data_training);
  mm_output <- summary(mm_model);
  
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
  
  return(signal == max_sharpe_regime);
}



