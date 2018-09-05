#' Function to prepare data and split into train, test & validation set
#'
#' @param tickers chr list. ticker of assets to fetch data for
#' @param tickernames chr list. ticker as spec. above, w/out any special characters
#' @param startdate chr specifying beginning of sample period. Must be in format "YYY-mm-dd"
#' @param enddate chr specifying end of sample period. Must be in format "YYY-mm-dd". Defaults to current date.
#' @param period chr specifying periodicity. Should be either "daily" or "monthly". Defaults to "daily".
#' @param by chr specifying the target variable to investigate. Defaults to "log_return".
#' Must be one of "log_return", "rel_return", "adjusted", "open", "close", "high", "low".
#' @param train int giving the share of train sample, defaults to 0.8.
#' @param test int giving the share of train sample, defaults to 0.1.
#' 
#' @return a tibble that can be grouped by train, test & validation sample.
#' Includes lagged values of target. If target variable is one of prices, they will be normalised.
#'
#' @import tbl2xts
#'
#' @source R/02-helpers.R
#' @source R/03-helpers-transform.R
#' @source R/04-fetch-data.R


source("R/02-helpers.R")
source("R/03-helpers-transform.R")
source("R/04-fetch-data.R")

prepare_ts_data <- function(tickers, 
                         tickernames, 
                         startdate, 
                         enddate = Sys.Date(), 
                         period = "daily", 
                         by = "log_return"){

  if (by %notin% c("log_return", "rel_return", "adjusted", "open", "close", "high", "low")){
    stop("Invalid target variable.")
  }
  
  # read data ----
  data <- fetch_data(tickers = tickers, 
                     tickernames = tickernames,
                     startdate = startdate,
                     enddate = enddate,
                     period = period)
  
  if (by %notin% c("log_return", "rel_return")){
    
    # mean-normalise prices ----
    # when all tickers are included, the period may not cover dates before any of the company's IPO!
    data <- normalise_prices(calc_period = c(startdate, enddate), stockdata = data, ticker = tickernames, by = "mean")
    
  }
  
  # bring to wide format and convert to xts ----
  
  data <- data %>%
    as.tibble() %>%
    tbl2xts::tbl_xts(cols_to_xts = by, spread_by = "symbol") %>%
    na.omit()
  
  return(data)
  
}