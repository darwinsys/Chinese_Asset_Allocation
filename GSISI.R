library(gdata)
library(TTR)
library(xts)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

filename <- paste(getwd(), "shenwan", "agg.csv", sep="/")
data <- read.csv(filename)

data$date <- strptime(data$date, format="%m/%d/%y")
sectors <- as.xts(data[,2:29], order.by = data$date, tz="")


sectors <- sectors[683:3524, ]


sectors_weekly <- sectors[endpoints(x=sectors, on = "weeks"), k=1 ]

#### read benchmark
b_data <- read.xls('5fund.xlsx') 
colnames(b_data) <- c("date", "zz500", "hs300", "gold", "spx", "hsh")
b_data$date <- strptime(b_data$date, format="%Y-%m-%d")

benchmark <- as.xts(b_data[, 2:6], order.by = b_data$date, tz="")
benchmark_weekly <- benchmark_hs300[endpoints(x=benchmark_hs300, on = "weeks")]

# 沪深300
b_data <- read.xls('hs300.xlsx')
b_data_dates <- strptime(b_data[,1], format="%Y/%m/%d")
benchmark <- as.xts(b_data[, 5], order.by = b_data_dates, tz="")
benchmark_weekly <- benchmark[endpoints(x=benchmark, on = "weeks")]
colnames(benchmark_weekly) <- c("HS300")
#### combine
agg_data <- na.omit(cbind(benchmark_weekly, sectors_weekly))
agg_rets <- na.omit(Return.calculate(agg_data, method="log"))
tzone(agg_rets) <- Sys.getenv("TZ")

benchmark_rets <- agg_rets[,1]
sector_rets <- agg_rets[, 2:ncol(agg_rets)]

### moving betas
roll_window <- 100;
sector_betas <- list();
sector_alphas <- list();
sector_betas_bull <- list();
sector_betas_bear <- list();
sector_avg_rets <- list();
for (i in 1:ncol(sector_rets)){
  print(paste("i = ", i))
  sector_betas[[i]] <-apply.rolling(sector_rets[,i], width = roll_window, FUN = CAPM.beta, Rb=benchmark_rets) 
  sector_alphas[[i]] <-apply.rolling(sector_rets[,i], width = roll_window, FUN = CAPM.alpha, Rb=benchmark_rets) 
  
  sector_betas_bull[[i]] <-apply.rolling(sector_rets[,i], width = roll_window, FUN = CAPM.beta.bull, Rb=benchmark_rets) 
  
  sector_betas_bear[[i]] <-apply.rolling(sector_rets[,i], width = roll_window, FUN = CAPM.beta.bear, Rb=benchmark_rets) 
  sector_avg_rets[[i]] <- SMA(sector_rets[,i], n = roll_window)
}

sector_betas <- do.call(cbind, sector_betas) 
sector_alphas <- do.call(cbind, sector_alphas)
sector_betas_bull <- do.call(cbind, sector_betas_bull)
sector_betas_bear <- do.call(cbind, sector_betas_bear)
sector_avg_rets <- do.call(cbind, sector_avg_rets)
tzone(sector_betas) <- Sys.getenv("TZ")



#### spearman rho
rhos <- list();
clean_sector_betas <- na.omit(sector_betas);
clean_sector_rets <- sector_rets[index(clean_sector_betas)];
clean_sector_avg_rets <- sector_avg_rets[index(clean_sector_betas)]

for (j in 1:nrow(clean_sector_betas)) {
  #rhos[[j]] <- cor(as.vector(clean_sector_avg_rets[j, ]), as.vector(clean_sector_betas[j,]), method='spearman')
  rhos[[j]] <- spearman(as.vector(clean_sector_rets[j,]), as.vector(clean_sector_betas[j,]))
 }
rhos <- do.call(rbind, rhos)
rhos <- as.xts(rhos, order.by = index(clean_sector_betas), tz="")
#rhos <- SMA(rhos, n=2)
#### signals
threshold = 31.7
signal <- rhos;
bull_count = 0;
bear_count = 0;
current_state = 0;



signal_bull = (rhos * 100 > threshold) * 1
signal_bear = (rhos * 100 < -threshold) * -1
signal = na.omit(signal_bull + signal_bear)

prev_trans = 0;
signal_trade = signal*0;
count = 0;

for (k in 1:nrow(signal)) {
  count = count + as.vector(signal[k]);
  if(prev_trans == 0) {
    if (count == 2) {
      signal_trade[k] <- 1;
      prev_trans <- 1;
    }
    if (count == -2) {
      signal_trade[k] <- -1;
      prev_trans <- -1;
    }
  }
  if (prev_trans == 1) {
    if (count > 2) count <- 2;
    if (count == 0) {
      signal_trade[k] <- -1;
      prev_trans <- -1;
      count = -2;
    }
  }
  if (prev_trans == -1) {
    if (count < -2) count <- -2;
    if (count == 0) {
      signal_trade[k] <- 1;
      prev_trans <- 1;
      count = 2;
    }
  }
}

pos <- signal_trade;
last_trade <- 0;
for (a in 1:nrow(signal_trade)) {
  if (signal_trade[a] == 0) pos[a] <- last_trade;
  if (signal_trade[a] != 0) {
    pos[a] <- signal_trade[a];
    last_trade <- signal_trade[a];
  }
  
}
#pos[pos==-1] <- 0;
ret_strategy <- benchmark_rets * lag(pos, 1) 
ret_strategy1 <- benchmark_rets * pos
ret_strategy2 <- na.omit(benchmark_rets * lag(pos, 1))
tzone(ret_strategy) <- Sys.getenv("TZ")
charts.PerformanceSummary(na.omit(cbind(ret_strategy1, benchmark_rets)))

### apply with daily returns;

  
agg_data_daily <- na.omit(cbind(benchmark, sectors))
agg_rets_daily <- Return.calculate(agg_data, method="log")
tzone(agg_rets_daily) <- Sys.getenv("TZ")
signal_trade

