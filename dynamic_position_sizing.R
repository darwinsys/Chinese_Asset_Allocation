require(gdata)
require(xts)
require(TTR)
require(PortfolioAnalytics)
require(PerformanceAnalytics)
require(quantmod)

library(ggplot2)

### loading data
data <- read.xls('5funds.xlsx')
colnames(data) <- c("date", "zz500", "hs300", "gold", "spx", "hsh")
data$date <- strptime(data$date, format="%Y-%m-%d")

prices <- xts(data[, 2:6], order.by = data[, 1])
returns <- na.omit(Return.calculate(prices))
ret_zz500 <- returns[,1]
ret_hs300 <- returns[,2]
ret_gold <- returns[,3]
ret_spx <- returns[,4]
ret_hsh <- returns[,5]



source("DPStrategy.R")

#### Good performing strategy
ret_test1 <- DPStrategy(ret_benchmark, lookback_short = 10, lookback_long = 50,
                        riskmeasure = "ES", cutoff=0);


#### MA Strategy 沪深300
ret_benchmark <- ret_hs300 #沪深300作为指标
ret_mv_test1 <- MAStrategy(ret_benchmark, riskmeasure = "SMA", ma_period = 200);
ret_mv_test2 <- MAStrategy(ret_benchmark, riskmeasure = "EMA", ma_period = 5);
ret_mv_test3 <- MAStrategy(ret_benchmark, riskmeasure = "EMA", ma_period = 20);

ret_mv <- cbind(ret_mv_test3, ret_mv_test2, 0.5*(ret_mv_test2 + ret_mv_test1), ret_benchmark);
tzone(ret_mv) <- Sys.getenv("TZ")
charts.PerformanceSummary(ret_mv)
rbind(table.AnnualizedReturns(ret_mv), maxDrawdown(ret_mv), CalmarRatio(ret_mv))

ret_mv_hs_best <- ret_mv_test3
colnames(ret_mv_hs_best) <- c("ema_20_hs300")

#### MA Strategy 中证500
ret_benchmark <- ret_zz500 #中证500作为指标
ret_mv_test1 <- MAStrategy(ret_benchmark, riskmeasure = "SMA", ma_period = 200);
ret_mv_test2 <- MAStrategy(ret_benchmark, riskmeasure = "EMA", ma_period = 5);
ret_mv_test3 <- MAStrategy(ret_benchmark, riskmeasure = "EMA", ma_period = 20);

ret_mv <- cbind(ret_mv_test1, ret_mv_test2, ret_mv_test3, ret_benchmark);
tzone(ret_mv) <- Sys.getenv("TZ")
charts.PerformanceSummary(ret_mv)
rbind(table.AnnualizedReturns(ret_mv), maxDrawdown(ret_mv), CalmarRatio(ret_mv))

ret_mv_zz_best <- ret_mv_test2
colnames(ret_mv_zz_best) <- c("ema_20_zz500")

#### 投资组合
ret_port1 <- 1/3 * ret_hs300 + 1/3 * ret_zz500 + 1/3 * ret_gold
ret_port2 <- 1/5 * (ret_hs300 + ret_zz500 + ret_gold + ret_spx + ret_hsh)
#ret_port3 <- 0.3 * ret_spx + 0.6 * ret_zz500 + 0.1 * ret_gold

ret_port3 <- 0.3028* ret_spx + 0.6093 * ret_zz500 + 0.0879 * ret_gold
#ret_port3 <- ret_zz500
ret_port3_ma10 <- MAStrategy(ret_port3, riskmeasure = "EMA", ma_period =  20)

macd <- MACD(cumsum(ret_port3), maType="EMA")
weight_port3_macd <- macd[,2];
weight_port3_macd[macd[,1] < macd[,2]] <- 0;
weight_port3_macd[macd[,1] > macd[,2]] <- 1;
ret_port3_macd <- na.omit(ret_port3 * lag(weight_port3_macd))


ret_port3_combi <- 0.8*ret_port3_ma10 + 0.2*ret_port3_macd

ret_port_test <- cbind(ret_port1, ret_port2, ret_port3, 
                       ret_port3_ma10,ret_port3_macd, ret_port3_combi,
                       ret_zz500, ret_hs300, ret_gold)
colnames(ret_port_test) <- c("3assets", "5assets", "opt", 
                        "opt_ma10", "opt_macd","opt_combi", 
                        "zz500", "hs300", "gold")
tzone(ret_port_test) <- Sys.getenv("TZ")
charts.PerformanceSummary(ret_port_test)
rbind(table.AnnualizedReturns(ret_port_test), maxDrawdown(ret_port_test), CalmarRatio(ret_port_test))



ret_port_test1 <- cbind(ret_port3, ret_port3_combi, ret_spx, ret_zz500, ret_hs300)
colnames(ret_port_test1) <- c("opt", "opt_combi", "spx", "zz500", "hs300")
tzone(ret_port_test1) <- Sys.getenv("TZ")
charts.PerformanceSummary(ret_port_test1)
rbind(table.AnnualizedReturns(ret_port_test1), maxDrawdown(ret_port_test1), CalmarRatio(ret_port_test1))



#### combind MA and ES strategy
ret_combi1 <- cbind(ret_mv_best, ret_test1, 0.5*(ret_mv_best + ret_test1), ret_benchmark)
tzone(ret_combi1) <- Sys.getenv("TZ")
charts.PerformanceSummary(ret_combi1)

#### RSI strategy
rsi30 <- RSI(cumsum(ret_benchmark), n = 14)
ret_rsi <- ret_benchmark;
ret_rsi[rsi30 > 80 | rsi30 < 20] <- 0
ret_combi2 <- cbind(ret_mv_best, ret_rsi, 0.5*(ret_mv_best + ret_benchmark), ret_benchmark)
colnames(ret_combi2) <- c("MA", "RSI", "combi", "Benchmark")
tzone(ret_combi2) <- Sys.getenv("TZ")
charts.PerformanceSummary(ret_combi2)


macd <- MACD(cumsum(ret_benchmark), maType="EMA")
ret_macd <- ret_benchmark;
ret_macd[macd[,1] < macd[,2]] <- 0;

macd_port <- MACD(cumsum(ret_portfolio), maType="EMA")
ret_macd_port <- ret_portfolio;
ret_macd_port[macd_port[,1] < macd_port[,2]] <- 0;

ret_combi3 <- cbind(ret_mv_best, ret_macd, ret_port_mv_best, ret_macd_port, ret_portfolio, 0.4*ret_mv_best + 0.6*ret_macd, 0.4*ret_port_mv_best + 0.6*ret_macd_port, ret_benchmark)
colnames(ret_combi3) <- c("MA", "MACD", "port_opt", "MACD_port", "port", "combi_macd_ma", "combi_macd_ma_port", "benchmark")
tzone(ret_combi3) <- Sys.getenv("TZ")
charts.PerformanceSummary(ret_combi3)

rbind(table.AnnualizedReturns(ret_combi3), maxDrawdown(ret_combi3), CalmarRatio(ret_combi3))




##### Portfolio Moving Average Strategy
ret_portfolio <- (ret_zz500 + ret_hs300 + ret_gold) / 3;
ret_mv_test1 <- MAStrategy(ret_portfolio, riskmeasure = "SMA", ma_period = 200);
ret_mv_test2 <- MAStrategy(ret_portfolio, riskmeasure = "EMA", ma_period = 10);
ret_mv_test3 <- MAStrategy(ret_portfolio, riskmeasure = "EMA", ma_period = 20);

ret_port_mv_best <- ret_mv_test2;


ret_mv <- cbind(ret_port_mv_best, ret_mv_best,  ret_benchmark);
colnames(ret_mv) <- c("ema_20_port", "ema_10_port", "hs300")
tzone(ret_mv) <- Sys.getenv("TZ")
charts.PerformanceSummary(ret_mv)
rbind(table.AnnualizedReturns(ret_mv), maxDrawdown(ret_mv), CalmarRatio(ret_mv))

################
## Apply moving average strategy on portfolio performing better, than applying on single index or asset.
## The reason is that portfolio NAV curve is smoother than the single index, using moving average
## to predict the trend turning is more reliable than to predict the zigzag curves. 

###### combine the best moving average strategy + stdev strategy
ret_mv_best <- MAStrategy(ret_benchmark, riskmeasure = "EMA", ma_period = 20);
ret_sd_best <- DPStrategy(ret_benchmark, lookback_short = 10, lookback_long = 50, ma_period = 2,
                                       riskmeasure = "MDD", cutoff=0);
ret_combi <- cbind(ret_mv_best, ret_sd_best, (0.6*ret_mv_best+0.4*ret_sd_best), ret_benchmark);


charts.PerformanceSummary(ret_combi);
rbind(table.AnnualizedReturns(ret_combi), maxDrawdown(ret_combi), CalmarRatio(ret_combi))


#### HMM
msp <- depmix(hs300~1, nstates = 5, data=ret_benchmark)
set.seed(1)
fmsp <-fit(msp)

xts_state <- posterior(fmsp)[,1]
ret_state1 <- ret_benchmark * (xts_state == 1)
ret_state2 <- ret_benchmark * (xts_state == 2)
ret_state3 <- ret_benchmark * (xts_state == 3)
ret_state4 <- ret_benchmark * (xts_state == 4)
ret_state5 <- ret_benchmark * (xts_state == 5)


ret_hmm <- na.omit(cbind(ret_state1, ret_state2, ret_state3,ret_state4,ret_state5,ret_benchmark))
colnames(ret_hmm) <- c("s1", "s2" ,"s3", "s4", "s5", "Benchmark")
tzone(ret_hmm) <- tzone(ret_combi)
charts.PerformanceSummary(ret_hmm);

rbind(table.AnnualizedReturns(ret_hmm), maxDrawdown(ret_hmm), CalmarRatio(ret_hmm))

#### rolling HMM
lookback = 1550;
last_states <- list();
last_avg_rets <- list();
last_avg_vols <- list();
for (i in lookback:length(ret_benchmark)) {
  msp <- depmix(hs300~1, data=ret_benchmark, nstates = 3)
  set.seed(1)
  fmsp <- fit(msp)
  states <- posterior(fmsp);
  last_state[[i]] <- tail(states, 1)[,1]
  last_sharpe[[i]] <- 
}


 #### Stdev strategy 
ret_test1 <- DPStrategy(ret_zz500, lookback_short = 10, lookback_long = 50, ma_period  = 200,
                    riskmeasure = "STDEV.D", cutoff=0);

ret_test2 <- DPStrategy(ret_zz500, lookback_short = 10, lookback_long = 50, ma_period = 200,
                    riskmeasure = "MDD", cutoff=0);

ret_test3 <- DPStrategy(ret_zz500, lookback_short = 10, lookback_long = 50, ma_period = 200,
                    riskmeasure = "ES", cutoff=0)

ret_1 <- cbind(ret_test1, ret_test2,  ret_test3, ret_zz500)

tzone(ret_1) <- Sys.getenv("TZ")

charts.PerformanceSummary(ret_1)

rbind(table.AnnualizedReturns(ret_1), maxDrawdown(ret_1), CalmarRatio(ret_1))



