library(gdata)
library(TTR)
library(xts)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

data <- read.csv("NAV_5ETFs.csv")
data$X <- strptime(data$X, format="%m/%d/%y", tz = "")
benchmarks <- as.xts(data[,2:6], order.by = data$X, tz="")

benchmarks_weekly <- sectors[endpoints(benchmarks, on="weeks")]
benchmarks_monthly <- sectors[endpoints(benchmarks, on="months")]

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
portf <- add.constraint(portf, type="return", return_target=0.0005)
#portf <- add.objective(portf, type="risk", name="StdDev")

portf <- add.objective(portf, type="risk", name="ES")

bt.opt1 <- optimize.portfolio.rebalancing(ret_bm, portf, 
            optimize_method = "ROI", 
            rebalance_on = "weeks", 
            training_period = 5)

weights <- extractWeights(bt.opt1)
data <- cbind(ret_bm, weights, fill=NA)
data <- na.locf(data, fromLast = TRUE)
weights <- data[, 6:10]

ret_port <- Return.portfolio(ret_bm, weights=weights)
tzone(ret_port) <- Sys.getenv("TZ")
rets <- cbind(ret_port, ret_bm)
charts.PerformanceSummary(rets)
rbind(table.AnnualizedReturns(rets), maxDrawdown(rets), CalmarRatio(rets))

