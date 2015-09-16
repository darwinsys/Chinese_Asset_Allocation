library(gdata)
library(TTR)
library(xts)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(mclust)
library(mhsmm)

source("HMM/gmmhmm.R");
 
##################################
## Test 1: 上证和深成指
##################################

data_bm <- read.csv("HMM/index_szsh.csv")
benchmark <- as.xts(data_bm[, 2:3], order.by=strptime(data_bm[,1], format="%m/%d/%y", tz=""))
ret_benchmark <- na.omit(Return.calculate(benchmark, method="discrete"))
ret_benchmark_weeklys <- na.omit(Return.calculate(benchmark[endpoints(benchmark,on =  "weeks")]), method="discrete")

ret_benchmark_5d <- na.omit(benchmark / lag(benchmark, 5) - 1) 
ret_benchmark_10d <- na.omit(benchmark / lag(benchmark, 10) - 1)




############################################################################
data_bm <- read.csv("HMM/index_szsh.csv")
benchmark <- as.xts(data_bm[, 2:3], order.by=strptime(data_bm[,1], format="%m/%d/%y", tz=""))
ret_benchmark <- na.omit(Return.calculate(benchmark, method="discrete"))
ret_benchmark_weeklys <- na.omit(Return.calculate(benchmark[endpoints(benchmark,on =  "weeks")]), method="discrete")

ret_benchmark_5d <- na.omit(benchmark / lag(benchmark, 5) - 1) 
ret_benchmark_10d <- na.omit(benchmark / lag(benchmark, 10) - 1)


source("HMM/gmmhmm.R");
############ 上海指数
ret_target <- na.omit(cbind(ret_benchmark[, 1], lag(ret_benchmark[, 1], 1), 
                            lag(ret_benchmark[, 1], 2), ret_benchmark_5d[, 1],lag(ret_benchmark_5d[,1], 1)))
periods <- c(500, 1500)

n <- nrow(ret_target);
n_start <- 3950;
ret_strategy <- ret_target[,1] * 0;
for (j in n_start:(n-1))
{
  data_training_short <- ret_target[(j-periods[1]):j]
  data_training_long <- ret_target[(j-periods[2]):j]
  
  gmm_short <- gmm_training(data_training_short);
  gmm_long <- gmm_training(data_training_long);
  hmm_short <- hmm_training(gmm_short, data_training_short);
  hmm_long <- hmm_training(gmm_long, data_training_long)
  
  max_sharpe_regime_short <- hmm_short$sharpe_ratio_max_regime
  max_sharpe_short <- hmm_short$sharpe_ratio[max_sharpe_regime_short]
  max_sharpe_regime_long <- hmm_long$sharpe_ratio_max_regime
  max_sharpe_long <- hmm_long$sharpe_ratio[max_sharpe_regime_long]
  
  previous_regime_short <- tail(hmm_short$hmm_yhat, 1)
  previous_regime_long <- tail(hmm_long$hmm_yhat, 1)
  
  previous_transit_prob_short <- hmm_short$hmm$model$transition[previous_regime_short, ]
  predicted_regime_short <- match(max(previous_transit_prob_short), previous_transit_prob_short)
  
  previous_transit_prob_long <- hmm_long$hmm$model$transition[previous_regime_long, ]
  predicted_regime_long <- match(max(previous_transit_prob_long), previous_transit_prob_long)
  
  signal_short <- as.integer(predicted_regime_short == max_sharpe_regime_short 
                             | hmm_short$sharpe_ratio[predicted_regime_short] > 0
                             )
  signal_long <- as.integer(as.integer(predicted_regime_long) == max_sharpe_regime_long
                            | hmm_long$sharpe_ratio[predicted_regime_long] > 0
                            )
  
  #if (hmm_long$sharpe_ratio[predicted_regime_long] > max_sharpe_short)
  #{
  #  print (paste("signal_long", signal_long) )
  #  ret_strategy[j+1] <- ret_target[j+1, 1] * signal_long;
  #}
  
  if (signal_short | signal_long)
  {
    print (paste("signal_long", signal_long) )
    print (paste("signal_short", signal_short) )
    ret_strategy[j+1] <- ret_target[j+1, 1]
    
  }
    
  #if (hmm_short$sharpe_ratio[predicted_regime_short] > max_sharpe_long)
  #{
  #  print(paste("signal_short", signal_short))
   # ret_strategy[j+1] <- ret_target[j+1, 1] * signal_short;
  #}
  
  print(paste("--------------", as.character(index(ret_strategy[j+1]))))
  print(paste("max sharpe short =", max_sharpe_short))
  print(paste("max sharpe regime short = ", max_sharpe_regime_short, ":predicted = ", predicted_regime_short))
  print(paste("max sharpe long =", max_sharpe_long))
  print(paste("max sharpe regime long = ", max_sharpe_regime_long, ":predicted = ", predicted_regime_long))
  print(paste("prev ret = ", ret_target[j, 1], ": current ret= ", ret_target[j+1, 1]))
  print(paste("selected ret =", ret_strategy[j+1], ": cumu ret =", sum(ret_strategy)))
  
  if (j >= (n_start+10)){
    ret_c <- cbind(ret_strategy[(n_start):(j+1)], ret_target[(n_start):(j+1), 1])
    charts.PerformanceSummary(ret_c)
    
  }
  print("--------------------------------------------")
  
}



#############################################################################
############ 上海指数
ret_target <- na.omit(cbind(ret_benchmark[, 1], lag(ret_benchmark[, 1], 1), 
                            lag(ret_benchmark[, 1], 2), ret_benchmark_5d[, 1],lag(ret_benchmark_5d[,1], 1)))
periods <- c(500, 750, 1000, 1200, 1500, 1750, 2000)

n <- nrow(ret_target);
n_start <- 3030;
ret_strategy <- ret_target[,1] * 0;
for (j in n_start:(n+1))
{
  max_sharpe <- list();
  max_sharpe_regime <- list();
  predicted_regime <- list();
  max_sharpe_periods <- list();
  for (i in 1:length(periods)) {
    print(paste("Trying period ", periods[i]))
    training_set = ret_target[(n_start-periods[i]):n_start]
    gmm <- gmm_training(data_training = training_set);
    if (gmm$J < 2) next;
    if (gmm$J >= 5) next;
    
    hmm_output <- hmm_training(gmm, data_training = training_set);
    max_sharpe[[i]] <- hmm_output$sharpe_ratio[hmm_output$sharpe_ratio_max_regime];
    max_sharpe_regime[[i]] <- hmm_output$sharpe_ratio_max_regime
    predicted_regime[[i]] <- hmm_output$hmm_predict_regime
    max_sharpe_periods[[i]] <- i;
  }
  
  max_sharpe <- do.call(rbind, max_sharpe)
  max_sharpe_regime <- do.call(rbind, max_sharpe_regime)
  max_sharpe_periods <- do.call(rbind, max_sharpe_periods)
  predicted_regime <- do.call(rbind, predicted_regime)
  
  selected_max_sharpe <-  sort(max_sharpe, index.return=TRUE, decreasing=TRUE)
  selected_max_sharpe_index = head(selected_max_sharpe$ix, 1)
  selected_max_sharpe_regime <- max_sharpe_regime[selected_max_sharpe_index]
  selected_predicted_regime <- predicted_regime[selected_max_sharpe_index]
  print("yes")
  yesno <- (selected_predicted_regime == selected_max_sharpe_regime)
  ret_strategy[j] <- ret_target[j, 1] * as.integer(yesno)
  
  print(paste("--------------", as.character(index(ret_target[j]))))
  print(paste("best period =", periods[max_sharpe_periods[selected_max_sharpe_index]]))
  print(paste("best regime =", selected_max_sharpe_regime, ": predict regime = ", selected_predicted_regime))
  print(paste("prev ret = ", ret_target[j-1, 1], ": current ret= ", ret_target[j, 1]))
  print(paste("selected ret =", ret_strategy[j], ": cumu ret =", sum(ret_strategy)))
  
  if (j >= (n_start+10)){
    ret_c <- cbind(ret_strategy[(n_start):(j+1)], ret_target[(n_start):(j+1), 1])
    charts.PerformanceSummary(ret_c)
    
  }
  print("--------------------------------------------")
  
}
##############################################

output <- list();
for (i in 1:length(periods)) {
  print(i)
  output[[i]] <- gmmhmm_training(data_training = ret_target[(n_start-periods[i]):n_start])
}

j = 10
OmegaSharpeRatio(output[[j]]$hmm_ret_regime)
SharpeRatio.annualized(output[[j]]$hmm_ret_regime)
charts.PerformanceSummary(output[[j]]$hmm_ret_regime)


charts.PerformanceSummary(na.omit(cbind(
  #  output[[4]]$hmm_ret_regime[, 2],
  #  output[[5]]$hmm_ret_regime[, 2],
  #  output[[6]]$hmm_ret_regime[, 3],
  output[[7]]$hmm_ret_regime[, 2],
  output[[8]]$hmm_ret_regime[, 2],
  output[[9]]$hmm_ret_regime[, 1],
  output[[10]]$hmm_ret_regime[, 2],
  output[[11]]$hmm_ret_regime[, 1],
  ret_benchmark[, 1]
)))

charts.PerformanceSummary(na.omit(cbind(
#  output[[4]]$hmm_ret_regime[, 2],
#  output[[5]]$hmm_ret_regime[, 2],
#  output[[6]]$hmm_ret_regime[, 3],
  output[[7]]$hmm_ret_regime[, 3],
  output[[8]]$hmm_ret_regime[, 3],
  output[[9]]$hmm_ret_regime[, 3],
  output[[10]]$hmm_ret_regime[, 3],
  output[[11]]$hmm_ret_regime[, 1],
    ret_benchmark[, 1]
)))

charts.PerformanceSummary(na.omit(cbind(
  #  output[[4]]$hmm_ret_regime[, 2],
  #  output[[5]]$hmm_ret_regime[, 2],
  #  output[[6]]$hmm_ret_regime[, 3],
  output[[7]]$hmm_ret_regime[, 3],
  output[[8]]$hmm_ret_regime[, 3],
  output[[9]]$hmm_ret_regime[, 3],
  output[[10]]$hmm_ret_regime[, 3],
  output[[11]]$hmm_ret_regime[, 1],
  output[[16]]$hmm_ret_regime[, 2],
  ret_benchmark[, 1]
)))
