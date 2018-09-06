#' Function to bring data to wide xts format
#'
#' @param startdate chr specifying beginning of sample period. Must be in format "YYY-mm-dd"
#' @param enddate chr specifying end of sample period. Must be in format "YYY-mm-dd". Defaults to current date.
#' @param normalise_by chr specifying the reference value for normalising,
#'  which will be done if 'by' is an absolute price value
#' 
#' @return an xts in wide format, spread by assets containing either log returns or adjusted prices 
#' If target variable is one of prices, they will be normalised.
#'
#' @import tbl2xts
#'
#' @source R/02-helpers.R
#' @source R/03-helpers-transform.R

prepare_ts_data <- function(stockdata,
                         startdate, 
                         enddate = Sys.Date(), 
                         normalise_by = "mean",
                         by){

  if (by %notin% c("log_return", "rel_return", "adjusted", "open", "close", "high", "low")){
    stop("Invalid target variable.")
  }
  
  if (by %notin% c("log_return", "rel_return")){
    
    # mean-normalise prices ----
    # when all tickers are included, the period may not cover dates before any of the company's IPO!
    data <- normalise_prices(calc_period = c(startdate, enddate), 
                             stockdata = stockdata, 
                             ticker = tickernames, 
                             normalise_by = normalise_by)
    
  }
  
  # bring to wide format and convert to xts ----
  
  data <- stockdata %>%
    as.tibble() %>%
    tbl2xts::tbl_xts(cols_to_xts = by, spread_by = "symbol") %>%
    na.omit()
  
  return(data)
  
}