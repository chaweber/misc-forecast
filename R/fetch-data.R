#' Function to pull & format all data
#'
#' @param tickers a vector specifying the tickers of the desired stocks to pull. 
#' Must contain valid tickers.
#' @param tickernames a vector specifying the tickers of the desired stocks to pull.
#' @param startdate chr in "YYYY-mm-dd" date format. Defaults to "1950-01-01".
#' @param enddate chr in "YYYY-mm-dd" date format. Defaults to current date.
#'
#' @return a list including two tbbl_time elements (in long format), "dailiy_stocks" and "monthly_stocks".
#' Each tbbl consists of the following columns: 
#' date, symbols (set by tickernames), open, high, low, close, adjusted, log_returns, rel_returns, type.
#' Type categorises the assets into either "index", "top" or "bottom" and is hard-coded for all tickers in config.
#'
#' @requires library(tibbletime)
#' @requires library(tidyquant)
#' @requires library(dplyr)

library(tibbletime)
library(tidyquant)
library(dplyr)

source("R/helpers-transform.R")

fetch_data <- function(tickers, 
                       tickernames, 
                       startdate = "1950-01-01", 
                       enddate = Sys.Date()){
  
  # fetch data
  
  stocks <- tickers %>%
    tq_get(get = "stock.prices", from = startdate, to = enddate) %>%
    as_tbl_time(., index= date)
  
  # re-set stock names & categorise into types
  
  for (x in seq(length(tickers))) {
    
    stocks <- stocks %>%
      mutate(symbol = replace(symbol, symbol==tickers[x], tickernames[x]))
    
  }
  
  stocktypes <- sapply(stocks$symbol, function(x){
    if (x %in% c("FTSE", "N225", "GSPC")){
      type <- "index"
    } else if (x %in% c("AAPL", "AMZN", "FB", "MSFT", "BRK",  "JPM", "GOOGL", "JNJ", "XOM", "BAC")){
      type <- "top"
    } else if (x %in% c("UAA", "DISCA","PWR", "NWSA", "SCG", "MAT", "SRCL", "HRB", "COTY", "EVHC")){
      type <- "bottom"
    } else {
      type <- NA
      warning("Selected tickers are not within pre-defined range and will not be classified.")
    }
  })
  
  stocks$stocktype <- stocktypes
  
  # add returns on a monthly / daily base, removes any NAs

  stocks_month <- stocks %>%
    group_by(symbol) %>%
    as_period(., period = "monthly") %>%
    ungroup() %>%
    get_returns(stockdata = ., period = "monthly", type = "log", colname = "log_return") %>%
    get_returns(stockdata = ., period = "monthly", type = "arithmetic", colname = "rel_return") 
  
  stocks <- stocks %>%
    get_returns(stockdata = .) %>%
    get_returns(stockdata = ., type = "arithmetic", colname = "rel_return") 
  
  
  return(list(daily_stocks = stocks, 
              monthly_stocks = stocks_month))
  
  }