require(gdata)
require(xts)
require(TTR)

#colnames(data) <- c("date", "zz500", "hs300", "gold", "spx", "hengsheng")
#data$date <- strptime(data$date, format="%Y-%m-%d")

data <- read.xls('3assets.xlsx')
colnames(data) <- c("date", "shzh", "gold", "cur")
data$date <- strptime(data$date, format="%Y-%m-%d")

#### calculate return 
xts_shzh <- xts(x=data$shzh, order.by = data$date)
xts_gold <- xts(x=data$gold, order.by = data$date)
xts_cur <- xts(x=data$cur, order.by = data$date)y
xdata <- cbind(xts_shzh, xts_gold, xts_cur)

### lag = 1
nr_lag = 3
xdata_r <- log(xdata / lag(xdata, n=1))
xdata_r[1, ] <- 0 

xts_r <- log(xdata / lag(xdata, n=1))
xts_r[1, ] <- 0
xts_nav <- 1 + cumsum(xts_r); colnames(xts_nav) <-  c("shzh", "gold", "cur")

### portfolio 
xts_r_port1 <- 0.5*xts_r[,1] + 0.5*xts_r[,2]
xts_r_port2 <- 0.33*xts_r[,1] + 0.33*xts_r[,2] + 0.33*xts_r[,3]
xts_nav_port1 <- 1 + cumsum(xts_r_port1)
xts_nav_port2 <- 1 + cumsum(xts_r_port2)

## Plot
plot(xts_nav[,1]);
lines(xts_nav[,2], col=2);
lines(xts_nav_port1, col=3);
lines(xts_nav_port2, col=4);


### moving average strategy
lag1 = 20
price = xts_nav[,1]
mv = SMA(price, n=lag1)
signal = (price > mv) * 1
signal[1:(lag1-1)] = 0

xts_r_mv_shzh <- xts_r_shzh * lag(signal, n =1)
xts_r_mv_shzh[1,] = 0
xts_nav_mv_shzh <- 1 + cumsum(xts_r_mv_shzh)



### portfolio 1
signal_port1 <- (xts_nav_port1 > SMA(xts_nav_port1, n=lag1))* 1
signal_port1[1:(lag1-1)] = 0;
xts_r_mv_port1 <- xts_r_port1 * lag(signal_port1, 1)
xts_r_mv_port1[1,] = 0
xts_nav_mv_port1 <- 1+cumsum(xts_r_mv_port1)

plot(xts_nav_shzh, ylim=c(0.5, 2.0));
lines(xts_nav_mv_shzh, col=2)
lines(xts_nav_port1, col=3)
lines(xts_nav_mv_port1, col=4)

### portfolio 2
signal_port2 <- (xts_nav_port2 > SMA(xts_nav_port2, n=lag1))* 1
signal_port2[1:(lag1-1)] = 0;
xts_r_mv_port2 <- xts_r_port2 * lag(signal_port2, 1)
xts_r_mv_port2[1,] = 0
xts_nav_mv_port2 <- 1+cumsum(xts_r_mv_port2)

lines(xts_nav_port2, col=5)
lines(xts_nav_mv_port2, col=6)
