library(gdata)
library(TTR)
library(xts)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

filename <- paste(getwd(), "shenwan", "agg.csv", sep="/")
data <- read.csv(filename)

data$date <- strptime(data$date, format="%m/%d/%y")
sectors <- as.xts(data[,2:29], order.by = data$date, tz="")
sectors_weekly <- sectors[endpoints(sectors, on="weeks")]

data_bm <- read.csv("benchmark.csv")
benchmark <- as.xts(data_bm[, 2:3], order.by=strptime(data_bm$date, format="%Y/%m/%d", tz=""))
benchmark_weekly <- benchmark[endpoints(benchmark, on="weeks")]

#### calculate rets
ret_sectors <- na.omit(Return.calculate(sectors, method="discrete"))
ret_benchmark <- na.omit(Return.calculate(benchmark, method="discrete"))

ret_sectors_weekly <- na.omit(Return.calculate(sectors_weekly, method="discrete"))
ret_benchmark_weekly <- na.omit(Return.calculate(benchmark_weekly, method="discrete"))

################################################################
##### strategy 1: zz vs. HS. 
################################################################
zz_betas <- apply.rolling(ret_benchmark_weekly[,1], width = 150, FUN = CAPM.beta, Rb = ret_benchmark_weekly[,2])
zz_alphas <- apply.rolling(ret_benchmark_weekly[,1], width = 150, FUN = CAPM.alpha, Rb = ret_benchmark_weekly[,2])
zz_betas <- na.omit(zz_betas)
zz_alphas <- na.omit(zz_alphas)
tzone(zz_betas) <- tzone(zz_alphas) <- Sys.getenv("TZ")

#### Plot alpha /betas returns in cumulative NAVs
ret_alpha <- zz_alphas
ret_beta <- zz_betas * ret_benchmark_weekly[,2]
colnames(ret_alpha) <- c("Alpha Ret")
colnames(ret_beta) <- c("Beta Ret")
charts.PerformanceSummary(cbind(ret_alpha, ret_beta, ret_benchmark_weekly[,2]))

ret_lag_beta <- lag(zz_betas, 1) * ret_benchmark_weekly[,2]
ret_lag_alpha <- ret_benchmark_weekly[,1] -  ret_lag_beta
colnames(ret_lag_alpha) <- c("Alpha Ret")
colnames(ret_lag_beta) <- c("Beta Ret")

ret_lag <- na.omit(cbind(ret_lag_alpha, ret_lag_beta, ret_benchmark_weekly[,2]))
charts.PerformanceSummary(ret_lag)
rbind(table.AnnualizedReturns(ret_lag), maxDrawdown(ret_lag), CalmarRatio(ret_lag))

####### using beta ma, the result is not convincing
zz_betas_ma <- EMA(zz_betas, 2);
ret_lag_beta <- lag(zz_betas_ma, 1) * ret_benchmark_weekly[,2]
ret_lag_alpha <- ret_benchmark_weekly[,1] -  ret_lag_beta
colnames(ret_lag_alpha) <- c("Alpha Ret")
colnames(ret_lag_beta) <- c("Beta Ret")


ret_lag <- na.omit(cbind(ret_lag_alpha, ret_lag_beta, ret_benchmark_weekly[,2]))
charts.PerformanceSummary(ret_lag)
rbind(table.AnnualizedReturns(ret_lag), maxDrawdown(ret_lag), CalmarRatio(ret_lag))

###### Test1: using alpha and beta returns to determine the turns, result no good!!!!!
charts.PerformanceSummary(cbind(ret_alpha, ret_beta, ret_benchmark_weekly[,2]))
signal_t1 <- (ret_alpha < ret_beta) * 2 - 1
ret_t1 <- na.omit(ret_benchmark_weekly[,2] * lag( signal_t1, 1))
plot(cumprod(1+ret_t1))

#### Test2: it seems that zz_alphas (alphas returns) can indicate the turns of the market
#### we try to use EMA(5) and alpha returns itself to indicate the turns
zz_alphas_ma <- SMA(zz_alphas, n=2)
alphas_macd <- na.omit(MACD(ret_alpha))
alphas_macd_signal <- (alphas_macd$macd > alphas_macd$signal)*1
ret_t2 <- ret_benchmark_weekly[,2] * lag(alphas_macd_signal, 1)
ret_t2 <- ret_strategy * lag(ret_benchmark_weekly[,2] > 0, 1)
tzone(ret_t2) <- Sys.getenv("TZ")

charts.PerformanceSummary(na.omit(cbind(ret_t2, ret_benchmark_weekly[,2])))

#### Test 3: testing various sectors
roll_window = 100
ret_temp <- na.omit(cbind(ret_benchmark_weekly[,2], ret_sectors_weekly))
ret_delta_sectors <- list();
sectors_betas <- list();
sectors_alphas <- list();
for (i in 1:ncol(ret_sectors)) {
  print(paste("i=", i))
  sectors_betas[[i]] <- apply.rolling(ret_temp[,i+1], width = roll_window, 
                                      FUN = CAPM.beta, Rb=ret_temp[, 1]) 
  sectors_alphas[[i]] <- apply.rolling(ret_temp[,i+1], width = roll_window, 
                                      FUN = CAPM.alpha, Rb=ret_temp[,1]) 
}
sectors_betas <- do.call(cbind, sectors_betas)
sectors_alphas <- do.call(cbind, sectors_alphas)
tzone(sectors_alphas) <- Sys.getenv("TZ")
colnames(sectors_alphas) <- colnames(sectors)
colnames(sectors_betas) <- colnames(sectors)
charts.PerformanceSummary(sectors_alphas[,11:15])
rbind(table.AnnualizedReturns(sectors_alphas), maxDrawdown(sectors_alphas), CalmarRatio(sectors_alphas))

sectors_lag_alphas <- list()
for (j in 1:28) {
  sectors_lag_alphas[[j]] <- na.omit(ret_sectors[, j] - ret_temp[,1]*lag(sectors_betas[,j], 1))
}
sectors_lag_alphas <- do.call(cbind, sectors_lag_alphas)
charts.PerformanceSummary(sectors_lag_alphas)
rbind(table.AnnualizedReturns(sectors_lag_alphas), maxDrawdown(sectors_lag_alphas), CalmarRatio(sectors_lag_alphas))
