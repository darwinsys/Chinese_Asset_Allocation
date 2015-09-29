require(gdata)
require(TTR)
require(xts)
require(fGarch)
require(PerformanceAnalytics)
require(PortfolioAnalytics)


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

#portf <- add.objective(portf, type="risk", name="StdDev")
portf <- add.objective(portf, type="risk", name="ES")
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

#### 投资资产和数据
data <- read.csv("Data/NAV_5ETFs.csv")
data$X <- strptime(data$X, format="%m/%d/%y", tz = "")

benchmarks <- as.xts(data[,2:6], order.by = data$X, tz="")
benchmarks_weekly <- benchmarks[endpoints(benchmarks, on="weeks")]
benchmarks_monthly <- benchmarks[endpoints(benchmarks, on="months")]

ret_bm <- na.omit(Return.calculate(benchmarks, method="discrete"))


##### 参数设置
training_period = 20; ### 回望窗口， 大小直接与投资组合响应市场动态变化相关
rebalance_on = "weeks"; ### 周调仓， 可以改为days，quarters
return_target = 0.0008; ### 回报目标 ＝ 日回报＊250 （0.0008 *250 = 20%)


##### 构建优化投资组合条件
funds <- c("zz.500", "hs.300", "gold", "spx", "hengsheng")
portf <- portfolio.spec(assets=funds) ### 指定投资工具
portf <- add.constraint(portf, type = "long_only")
portf <- add.constraint(portf, type="return", return_target)

portf <- add.objective(portf, type="risk", name="ES") ### 优化目标：最小化投资组合的Expected Shortfall

#### 数据：需要用来生成投资组合的历史数据
# ret <- ### 从数据库里取出最新的数据， 长度为回望窗口training_period



#### 生成投资组合
bt.opt1 <- optimize.portfolio.rebalancing(ret_bm, portf, 
                                          optimize_method = "ROI", 
                                          rebalance_on = "weeks", 
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



