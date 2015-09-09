require(quantmod)
require(PerformanceAnalytics)

symbols <- c("MDY", "TLT", "EEM", "ILF", "EPP", "FEZ")
getSymbols(symbols, from="1990-01-01")
getSymbols("SPY", from="1990-01-01")
prices <- list()
for(i in 1:length(symbols)) {
  prices[[i]] <- Ad(get(symbols[i]))
}

prices <- do.call(cbind, prices)
colnames(prices) <- gsub("\\.[A-z]*", "", colnames(prices))
returns <- Return.calculate(prices)
returns <- na.omit(returns)


source("logicInvestGMR.R")


lookbacks = c(2, 5, 10, 15, 20, 25, 30, 40, 50, 60)
investperiods = c(1, 2, 5, 10, 15, 20, 25, 30, 40, 50, 70, 100)
avg_ret <- list();
avg_sigma <- list();
avg_sharpe <- list();
avg_md <- list();


c <- 0
for (l in lookbacks) {
  for (i in investperiods) {
    c <- c+1;
    gmr <- logicInvestGMR(returns, lookback = l, investperiod = i);
    avg_ret[c] <- Return.annualized(gmr)
    avg_sigma[c] <- StdDev.annualized(gmr)
    avg_sharpe[c] <- SharpeRatio.annualized(gmr)
    avg_md[c] <- maxDrawdown(gmr);
  }
}

avg_ret <-  do.call(rbind, avg_ret)
avg_sigma <- do.call(rbind, avg_sigma)
avg_sharpe <- do.call(rbind, avg_sharpe)
avg_md <- do.call(rbind, avg_md)

dim(avg_sharpe) <- c(12, 10);
colnames(avg_sharpe) <- lookbacks
rownames(avg_sharpe) <- investperiods

sharpes <- data.frame(
  score = as.numeric(avg_sharpe),
  investperiod = as.numeric(floor(investperiods)),
  lookback = as.numeric(rep(lookbacks, each=length(investperiod)))
  
)

p <- ggplot(sharpes, aes(x=lookback, y=investperiod, size=score)) 

p +  geom_point(shape=21, colour="black", fill="cornsilk")

gmr <- logicInvestGMR(returns, 15, 1)
compare <- na.omit(cbind(gmr, Return.calculate(Ad(SPY))))
charts.PerformanceSummary(compare)
rbind(table.AnnualizedReturns(compare), maxDrawdown(compare), CalmarRatio(compare))

### Market Timing Test
MarketTiming(gmr, Return.calculate(Ad(SPY)), Rf = 0, method = "HM")


                                