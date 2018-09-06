library(tibbletime)
library(tidyquant)
library(tseries)
library(stats)
library(dplyr)
library(purrr)
library(ggplot2)
library(wesanderson)

source("R/01-config.R")
source("R/02-helpers.R")
source("R/03-helpers-transform.R")
source("R/04-helpers-plot.R")
source("R/05-fetch-data.R")
source("R/06-prepare-ts.R")
source("R/07-prepare-ml.R")
source("R/08-desc-stats.R")
source("R/09-estimate-arima-garch.R")
source("R/10-estimate-ml.R")

forecast <- function(period, lag){

  # get  data ----
  print("Fetching Data...")
  stocks <- fetch_data(tickers = tickers,
                       tickernames = tickernames,
                       startdate = startdate,
                       enddate = enddate,
                       period = period)

  # get descriptive statistics for prices and returns----
  print("Obtaining Information about Distribution...")
  stats_returns <- desc_stats(stockdata = stocks, by = "log_return")
  stats_prices <- desc_stats(stockdata = stocks, by = "adjusted")

  # estimate arima-garch for returns monthly and daily----
  print("Estimating ARIMA-GARCH...")
  econ_forecast <- arima_garch(by = "log_return", period = "daily", stockdata = stocks)

  # estimate ml----
  ifelse((period == "daily"), lags <- 1:30, lags <- 1:48)
  ml_forecast <- estimate_ml(stockdata = stocks, lags = lags, by = "log_return")

  # return ----
  return(list(stats = list(returns = stats_returns,
                           prices = stats_prices),
              econ_acc = econ_forecast,
              ml_acc = ml_forecast))

}



