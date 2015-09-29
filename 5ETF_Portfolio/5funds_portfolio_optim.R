library(gdata)
library(TTR)
library(xts)
library(fGarch)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

data <- read.csv("Data/NAV_5ETFs.csv")
data$X <- strptime(data$X, format="%m/%d/%y", tz = "")
benchmarks <- as.xts(data[,2:6], order.by = data$X, tz="")

benchmarks_weekly <- benchmarks[endpoints(benchmarks, on="weeks")]
benchmarks_monthly <- benchmarks[endpoints(benchmarks, on="months")]

### calc rets
ret_bm <- na.omit(Return.calculate(benchmarks, method="discrete"))
ret_bm_weekly <- na.omit(Return.calculate(benchmarks_weekly, method="discrete"))
ret_bm_monthly <- na.omit(Return.calculate(benchmarks_monthly, method="discrete"))

### Portfolio
funds <- colnames(benchmarks)
portf <- portfolio.spec(assets=funds)
portf <- add.constraint(portf, type = "long_only")
#portf <- add.constraint(portf, type = "full_investment")

#portf <- add.constraint(portf, type="turnover", turnover_target=0.2)
portf <- add.constraint(portf, type="return", return_target=0.0008)
#portf <- add.objective(portf, type="risk", name="StdDev")

portf <- add.objective(portf, type="risk", name="StdDev")
#                       arguments=list(p=0.925, clean="boudt"))
#portf <- add.objective(portf, type="quadratic_utility", risk_aversion=0.5)

###### 

bt.opt <- optimize.portfolio(ret_bm, portf, 
            optimize_method = "ROI", trace=TRUE)
weights <- extractWeights(bt.opt)
ret_port <- ret_bm[, 1];
for (i in 1:nrow(ret_port)) {
  ret_port[i, 1] <- sum(ret_bm[i, ] * weights)
}

rets <- cbind(ret_port, ret_bm )
rets <- rets[500:1602, ]
charts.PerformanceSummary(rets)
rbind(table.AnnualizedReturns(rets), maxDrawdown(rets), CalmarRatio(rets))

#########################################################################
##### rebalancing out-of-sample test
funds <- colnames(benchmarks)
portf <- portfolio.spec(assets=funds)
portf <- add.constraint(portf, type = "long_only")
#portf <- add.constraint(portf, type = "full_investment")

#portf <- add.constraint(portf, type="turnover", turnover_target=0.2)
portf <- add.constraint(portf, type="return", return_target=0.0005)
#portf <- add.objective(portf, type="risk", name="StdDev")

portf <- add.objective(portf, type="risk", name="ES")
#                       arguments=list(p=0.925, clean="b

bt.opt1 <- optimize.portfolio.rebalancing(ret_bm, portf, 
            optimize_method = "ROI", 
            rebalance_on = "months", 
            training_period = 20, trace = TRUE)

weights <- extractWeights(bt.opt1)
data <- cbind(ret_bm, weights, fill=NA)
data <- na.locf(data, fromLast = TRUE)
weights <- data[, 6:10]

ret_port <- Return.portfolio(ret_bm, weights=weights)
ret_port <- ret_port[500:1602, ]
tzone(ret_port) <- Sys.getenv("TZ")
rets <- cbind(ret_port, ret_bm[,1:2 ])
charts.PerformanceSummary(rets)
rbind(table.AnnualizedReturns(rets), maxDrawdown(rets), CalmarRatio(rets))

data_output <- na.omit(cbind(ret_port, weights))
write.csv(as.data.frame(data_output), "Data/portfolio_5etfs_ret_0.0008_weekly.csv")

#########################################################################
##### using HMM as the regime capture model to optimize te portfolio
#########################################################################
asset_names  <- colnames(benchmarks)
spec <- portfolio.spec(assets = asset_names)
spec <- add.constraint(spec, type = "long_only")
spec <- add.constraint(portf, type = "full_investment")

#spec <- add.constraint(spec, type="turnover", turnover_target=0.2)
spec <- add.constraint(spec, type = "return", return_target = 0.0004)
spec <- add.objective(spec, type="risk", name="ES")

#spec <- add.objective(spec, type = "risk", name = "ES")
#                       arguments=list(p=0.925, clean="b

### sim is the wraper of all the relevant configuration
sim <- list();
sim$training_period = 20;
sim$rebalance_on = "weeks";
sim$trace = TRUE;

#### test case 1: using different return_target
###################################################################################
list_target <- seq(from = 0.0004, to = 0.001, by = 0.0001);
list_output <- list();

### sim is the wraper of all the relevant configuration
sim <- list();
sim$training_period = 20;
sim$rebalance_on = "weeks";
sim$trace = TRUE;

for (i in 1:length(list_target)) {
  spec <- portfolio.spec(assets = asset_names)
  spec <- add.constraint(spec, type = "long_only")
  spec <- add.constraint(portf, type = "full_investment")
   #spec <- add.constraint(spec, type="turnover", turnover_target=0.2)
  spec <- add.constraint(spec, type = "return", return_target = list_target[i])
  spec <- add.objective(spec, type="risk", name="ES");
  
  output <- strategy_5etfs_portfolio(ret_assets, spec, sim)
  
  print(i);
  list_output[[i]] <- output$returns;
}

list_output <- do.call(cbind, list_output)
colnames(list_output) <- list_target
rbind(table.AnnualizedReturns(list_output), maxDrawdown(list_output), CalmarRatio(list_output))

#### functions to call for the strategy
##############################################################
strategy_5etfs_portfolio <- function(ret_assets, spec, sim) {
  

  bt.opt <- optimize.portfolio.rebalancing(ret_assets, spec, 
                optimize_method = "ROI", 
                rebalance_on = sim$rebalance_on, training_period = sim$training_period,
                trace = sim$trace);
  
  weights <- extractWeights(bt.opt)
  data <- cbind(ret_assets, weights, fill = NA)
  data <- na.locf(data, fromLast = TRUE)
  weights <- data[, 6:10]
  
  ret_port <- Return.portfolio(ret_assets, weights = weights)
  tzone(ret_port) <- Sys.getenv("TZ")
  
  
  output <- list();
  output$weights <- weights;
  output$returns <- ret_port;
  output$bt.opt <- bt.opt;
  
  return(output);
  
}