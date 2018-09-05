library(tibbletime)
library(tidyquant)
library(tseries)
library(stats)

# come with tidyverse
library(dplyr)
library(purrr)

source("R/01-config.R")
source("R/03-helpers-transform.R")
source("R/04-fetch-data.R")
source("R/05-desc-stats.R")
source("R/06-plots.R")

# read data # ----

# get daily stocks

stocks <- fetch_data(tickers = tickers, 
                   tickernames = tickernames,
                   startdate = startdate,
                   enddate = enddate,
                   period = "daily")

# investigate log_returns

by <- "log_return"
stocks <- add_lags(daily_stocks, 1:10, by = by)

desc_stats(data = stocks, by = by, stocktypes = c("index", "bottom"))


