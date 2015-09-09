library(gdata)
library(TTR)
library(xts)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

data1 <- read.xls("zz500.xlsx")
data2 <- read.xls("hs300.xlsx")

#### 创建中证500和沪深300的数据结构
bm_zz <- as.xts(data1[, 5], order.by = strptime(data1$日期, format="%Y/%m/%d"), tz="")
bm_hs <- as.xts(data2[, 5], order.by = strptime(data2$日期, format="%Y/%m/%d"), tz="")
bm <- na.omit(cbind(bm_zz, bm_hs))
colnames(bm) <- c("zz", "hs")

#### 读取ETF基金价格信息
data3 <- read.xls("5funds.xlsx")
etfs <- as.xts(data3[, 2:3], order.by=strptime(data3$X, format="%Y-%m-%d"), tz="")


#### 周回报
bm_rets <- na.omit(Return.calculate(bm), method=c("discrete"))
bm_weekly <- bm[endpoints(bm, on = "weeks")]
bm_rets_weekly <- na.omit(Return.calculate(bm_weekly, method = c("discrete")))

etfs_weekly <- etfs[endpoints(etfs, on="weeks")]
etfs_rets_weekly <- na.omit(Return.calculate(etfs_weekly, method = c("discrete")))

### 每4周对比两个指数的回报， 
bm_zz_4weeks <- na.omit(SMA(bm_rets_weekly[, 1], n= 4))
bm_hs_4weeks <- na.omit(SMA(bm_rets_weekly[, 2], n=4))
signal1 <- bm_zz_4weeks > bm_hs_4weeks
signal2 <- 1- signal1

ret_strategy <- na.omit(bm_rets_weekly[,1] * lag(signal1, 1) + bm_rets_weekly[,2] * lag(signal2, 1))
colnames(ret_strategy) <- c("strategy0")
ret_etf <- na.omit(etfs_rets_weekly[,1] * lag(signal1, 1) + etfs_rets_weekly[, 2] * lag(signal2, 1))
colnames(ret_etf) <- c("etf")
tzone(ret_strategy) <- Sys.getenv("TZ")
charts.PerformanceSummary(na.omit(cbind(ret_strategy, ret_etf, bm_rets_weekly)))

signal1 <- (bm_zz_4weeks > bm_hs_4weeks & bm_zz_4weeks > 0)
signal2 <- (bm_hs_4weeks > bm_zz_4weeks & bm_hs_4weeks > 0)
ret_strategy <- na.omit(bm_rets_weekly[,1] * lag(signal1, 1) + bm_rets_weekly[,2] * lag(signal2, 1))
colnames(ret_strategy) <- c("strategy")
tzone(ret_strategy) <- Sys.getenv("TZ")
charts.PerformanceSummary(cbind(ret_strategy, bm_rets_weekly))

signal3 <- (bm_zz_4weeks > 0)
signal4 <- (bm_hs_4weeks > 0)
ret_strategy1 <- na.omit(bm_rets_weekly[,1]*lag(signal3, 1))
ret_strategy2 <- na.omit(bm_rets_weekly[,2]*lag(signal4, 1))
colnames(ret_strategy1) <- c("strategy1")
colnames(ret_strategy2) <- c("strategy2")
tzone(ret_strategy1) <- Sys.getenv("TZ")
charts.PerformanceSummary(cbind(ret_strategy, ret_strategy1,ret_strategy2,  bm_rets_weekly))

#### 用移动beta来寻找最佳换仓点
zz_betas <- apply.rolling(bm_rets_weekly[,1], width = 30, FUN = CAPM.beta, Rb = bm_rets_weekly[,2])
zz_alphas <- apply.rolling(bm_rets_weekly[,1], width = 30, FUN = CAPM.alpha, Rb = bm_rets_weekly[,2])

cor(na.omit(cbind(zz_betas, bm_rets[, 1])))
cor(na.omit(bm_rets ))
