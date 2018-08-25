# function to pull & format all data

library(tibbletime)
library(tidyquant)
library(dplyr)
library(ggplot2)
library(wesanderson)
library(tseries)

# ----

# pick the ones with voting rights (class a), except for brk b
# FB, COTY (only a)
# GOOGL (c: GOOG), under armour (c: UA), NWSA (b: NWS)

# brighthouse financial (BHF) weird incline 10-17 /7/2017?
# > On August 4, 2017, Brighthouse Financial completed its separation from 
# MetLife and began trading on the Nasdaq stock exchange on August 7

tickers <- c("^GSPC", "^N225", "^FTLC", 
             "NWSA", "UAA", "DISCA","PWR", #BHF
             "SCG", "MAT", "SRCL", "HRB", "COTY", "EVHC", 
             "AAPL", "MSFT", "AMZN", "FB",  "BRK-B",
             "JPM", "GOOGL", "JNJ", "XOM", "BAC")
tickernames <- c("GSPC", "N225", "FTLC", 
                 "NWSA", "UAA", "DISCA","PWR", "SCG", 
                 "MAT", "SRCL", "HRB", "COTY", "EVHC", 
                 "AAPL", "MSFT", "AMZN", "FB",  "BRK",
                 "JPM", "GOOGL", "JNJ", "XOM", "BAC")

# ----

fetch_data <- function(tickers, 
                       tickernames, 
                       startdate = "1950-01-01", 
                       enddate = Sys.Date())
  {
  
  # fetch data
  ## ----
  
  stocks <- tickers %>%
    tq_get(get = "stock.prices", from = startdate, to = enddate) %>%
    as_tbl_time(., index= date)
  
  # re-set stock names & categorise into types
  ## ----
  
  for (x in seq(length(tickers))) {
    
    stocks <- stocks %>%
      mutate(symbol = replace(symbol, symbol==tickers[x], tickernames[x]))
    
  }
  
  stocktypes <- sapply(stocks$symbol, function(x){
    
    if (x %in% c("FTLC", "N225", "GSPC")){
      type <- "index"
    } else if (x %in% c("AAPL", "AMZN", "FB", "MSFT", "BRK",  "JPM", "GOOGL", "JNJ", "XOM", "BAC")){
      type <- "top"
    } else {
      type <- "bottom"
    }  
    
  })
  
  stocks$stocktype <- stocktypes
  
  # add returns on a monthly / daily base
  ## ----
  
  stocks_month <- stocks %>%
    group_by(symbol) %>%
    as_period(., period = "monthly") %>%
    ungroup() %>%
    get_returns(stockdata = ., period = "monthly", type = "log", colname = "log_return") %>%
    get_returns(stockdata = ., period = "monthly", type = "arithmetic", colname = "rel_return") 
  
  stocks <- stocks %>%
    get_returns(stockdata = .) %>%
    get_returns(stockdata = ., type = "arithmetic", colname = "rel_return") 
  
  # return
  ### ----
  return(list(daily_stocks = stocks, 
              monthly_stocks = stocks_month))
  
  }