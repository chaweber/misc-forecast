# wrapper

source("R/config.R")
source("R/fetch-data.R")
source("R/desc-stats.R")

# read data # ----
data <- fetch_data(tickers = tickers, 
                   tickernames = tickernames,
                   startdate = startdate,
                   enddate = enddate)

daily_stocks <- data$daily_stocks
monthly_stocks <- data$monthly_stocks

desc_stats(data = monthly_stocks, by = "log_return", stocktypes = c("index", "bottom"))
desc_stats(data = monthly_stocks, by = "adjusted", stocktypes = c("index", "bottom"))

desc_stats(data = daily_stocks, by = "log_return", stocktypes = c("index", "bottom"))
desc_stats(data = monthly_stocks, by = "adjusted", stocktypes = c("index", "bottom"))

