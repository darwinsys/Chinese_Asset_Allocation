library(gdata)
library(TTR)
library(xts)
library(RMySQL)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

##### read data from csv
data <- read.csv("5ETF_Portfolio/NAV_5ETFs.csv")
data$X <- strptime(data$X, format="%m/%d/%y", tz = "")
benchmarks <- as.xts(data[,2:6], order.by = data$X, tz="")

##### insert data into the database table
db <- list();
db$name <- "stocks"
db$table <- "etf_daily_data"
db$username <- "root"

#### construct insert query
query_temp <- paste("INSERT into `stocks`.`etf_daily_data` (`fund_name`, `date`, `price`) ")
values <- paste("VALUES")

fund_names <- colnames(benchmarks)
for (i in 1:nrow(benchmarks)) {
  data <- benchmarks[i]
  date <- as.character(index(data))
  values <- paste("VALUES")
  for (j in 1:length(fund_names)) {
    value1 <- paste("'", fund_names[j], "', '", date, "', '",  data[, j], "'", sep = "")
  
    values <- paste(values, "(", value1, ")")
    if (j != length(fund_names)) {
      values <- paste(values, ",")
    }
  }
  query <- paste(query_temp, values)
  rs <- dbSendQuery(con, query)
}


dbCommit(con)
dbDisconnect(con)

