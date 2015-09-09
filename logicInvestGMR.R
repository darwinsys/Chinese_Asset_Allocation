## lookback interval = weeks
## Investment period = weeks
logicInvestGMR <- function(returns, lookback = 12, investperiod = 4) {
  ep <- endpoints(returns, on = "weeks");
  weights <- list()
  for (i in seq(2, (length(ep) - lookback), investperiod)) {
    retSubset <- returns[ep[i]:ep[i+lookback-1],]
    cumRets <- Return.cumulative(retSubset)
    rankCum <- rank(cumRets)
    weight <- rep(0, ncol(retSubset))
    weight[which.max(cumRets)] <- 1
    weight <- xts(t(weight), order.by=index(last(retSubset)))
    weights[[i]] <- weight
  }
  weights <- do.call(rbind, weights)
  stratRets <- Return.portfolio(R = returns, weights = na.omit(lag(weights, 1)))
  return(stratRets)
}

MarketTiming()