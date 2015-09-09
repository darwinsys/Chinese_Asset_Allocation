require(PerformanceAnalytics);
require(TTR)

DPStrategy <- function(returns, rf_returns = NULL, lookback_short = 20, lookback_long = 200, 
                       riskmeasure = c("STDEV", "STDEV.D", "ES", "MDD", "ADD"), 
                       rebalance = c("days", "weeks", "months"),
                       cutoff = 0.5)
{
  if (is.null(rf_returns)) {
    time_index = index(returns)
    rf_returns = xts(x=rep(0, length(time_index)), order.by = index(returns))
  }
  
  price = na.omit(cumsum(returns));
  
  if (riskmeasure == "STDEV") {
    rm_short = apply.rolling(returns, width = lookback_short, FUN = StdDev.annualized);
    rm_long = apply.rolling(returns, width = lookback_long, FUN = StdDev.annualized);
  }
  else if (riskmeasure == "STDEV.D") {
    rm_short = apply.rolling(returns, width = lookback_short, FUN = SemiDeviation);
    rm_long = apply.rolling(returns, width = lookback_long, FUN = SemiDeviation);
  }
  else if (riskmeasure == "ES") {
    rm_short = apply.rolling(returns, width = lookback_short, FUN = ES);
    rm_long = apply.rolling(returns, width = lookback_long, FUN = ES);
  }
  else if (riskmeasure == "MDD") {
    rm_short = apply.rolling(returns, width = lookback_short, FUN = maxDrawdown, MAR=0.005);
    rm_long = apply.rolling(returns, width = lookback_long, FUN = maxDrawdown, MAR=0.005);
  }
  else if (riskmeasure == "ADD") {
    rm_short = apply.rolling(returns, width = lookback_short, FUN = AverageDrawdown, MAR=0.005);
    rm_long = apply.rolling(returns, width = lookback_long, FUN = AverageDrawdown, MAR=0.005);
  }
  else if (riskmeasure == "CDD") {
    rm_short = apply.rolling(returns, width = lookback_short, FUN = CDD, p=0.95);
    rm_long = apply.rolling(returns, width = lookback_long, FUN = CDD, p=0.95);
  }
  
               
  if (riskmeasure == "STDEV") {
    ratio_stdev = rm_long / rm_short;
    pos_stdev = ratio_stdev * (signal_ma); ## reset position when price < moving average
    pos_stdev[pos_stdev > 1] = 1;
    pos_stdev[pos_stdev < cutoff] = 0;
    
    weights <- pos_stdev;
  }
  if (riskmeasure == "STDEV.D") {
    ratio_stdev = rm_long / rm_short;
    pos_stdev = ratio_stdev * (signal_ma); ## reset position when price < moving average
    pos_stdev[pos_stdev > 1] = 1;
    pos_stdev[pos_stdev < cutoff] = 0;
    
    weights <- pos_stdev;
  }
  else if (riskmeasure == "ES") {
    ratio_es = rm_long / rm_short;
    pos_es = ratio_es; ## reset position when price < moving average
    pos_es[pos_es > 1] = 1;
    pos_es[pos_es < cutoff] = 0;
    
    weights <- pos_es;
  }
  else if (riskmeasure == "MDD") {
    sigma <- apply.rolling(returns, width = lookback_short, FUN = StdDev);
    ratio_mdd  = 1 -  abs(rm_short)  / sigma
    pos_mdd = ratio_mdd * signal_ma; ## reset position when price < moving average
    #pos_mdd[pos_mdd > 1] = 1;
    pos_mdd[pos_mdd < cutoff] = 0;
    
    weights <- pos_mdd;
  }
  else if (riskmeasure == "ADD") {
    ratio_mdd = 1- rm_short / rm_long;
    pos_mdd = ratio_mdd * signal_ma; ## reset position when price < moving average
    pos_mdd[pos_mdd > 1] = 1;
    pos_mdd[pos_mdd < cutoff] = 0;
    
    weights <- pos_mdd;
  }
  else if (riskmeasure == "CDD") {
    ratio_mdd = 1- rm_short / rm_long;
    pos_mdd = ratio_mdd * signal_ma; ## reset position when price < moving average
    pos_mdd[pos_mdd > 1] = 1;
    pos_mdd[pos_mdd < cutoff] = 0;
    
    weights <- pos_mdd;
  }

  asset_returns <- na.omit(cbind(returns, rf_returns));
  asset_weights <- na.omit(cbind(weights, 1 - weights));
  
  tzone(asset_returns) <- Sys.getenv("TZ")
  tzone(asset_weights) <- Sys.getenv("TZ")
  
  port_returns <- Return.portfolio(asset_returns, asset_weights);
  colnames(port_returns) <- paste(riskmeasure, "_",c(lookback_short), "_", c(lookback_long),
                    "_ma_",ma_period,  "cutoff_", cutoff, sep = "")
  tzone(port_returns) <- Sys.getenv("TZ")
  return(port_returns);
               
}


MAStrategy <- function(returns, rf_returns = NULL, ma_period = 200,
              riskmeasure = c("SMA", "EMA")) {
  if (is.null(rf_returns)) {
    time_index = index(returns)
    rf_returns = xts(x=rep(0, length(time_index)), order.by = index(returns))
  }
  
  price = na.omit(cumsum(returns));
  weights <- xts(x=rep(1, length(returns)), order.by = index(returns));
  
  if (riskmeasure == "SMA") {
    signal_ma = (price > SMA(price, n = ma_period ));
    weights <- weights * signal_ma;
  }
  else if (riskmeasure == "EMA") {
    signal_ma = (price > EMA(price, n = ma_period ));
    weights <- weights * signal_ma;
  }
  
  asset_returns <- na.omit(cbind(returns, rf_returns));
  asset_weights <- na.omit(cbind(weights, 1 - weights));
  
  tzone(asset_returns) <- Sys.getenv("TZ")
  tzone(asset_weights) <- Sys.getenv("TZ")
  
  port_returns <- Return.portfolio(asset_returns, asset_weights);
  colnames(port_returns) <- paste(riskmeasure, 
                                  "_ma_",ma_period,   sep = "")
  tzone(port_returns) <- Sys.getenv("TZ")
  return(port_returns);
}


