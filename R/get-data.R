# function to source all specified stock price data from yahoo finance api

get_all_data <- function(ticker, startdate, enddate = Sys.Date()){

  # Get quantmod
  if (!require("quantmod")) {
    install.packages("quantmod")
    library(quantmod)
  }
  

  start <- as.Date(startdate)
  end <- as.Date(enddate)
  
  data <- getSymbols.yahoo(Symbols = ticker,
                           from = start, 
                           to = end, 
                           periodicity = "daily",
                           env = .GlobalEnv,
                           auto.assign = FALSE)
  data <- na.omit(data)
  
}
  