# helper functions to transform data

# Normalise ----

#' Function to add Col with normalised prices to tbbl
#' 
#' @param calc_period A chr vector, containing the start and end date for a subsetted period.
#' Must be in order ("start", "end") and in format "YYYY-mm-dd". 
#' @param stockdata a tbbl_time containing the stock prices to adjust.
#' @param ticker a chr vector containing the tickers of all assets that should be included.
#' @param by a chr, must be either "mean" or "first". Argument to normalise series by.
#' 
#' @return returns modified tbbl_time with new column "first_norm" / "mean_norm"  

normalise_prices <- function(calc_period = c("start", "end"), stockdata, ticker, by){
  
  data <- NULL
  start <- calc_period[1]
  end <- calc_period[2]
    
  if (by == "first"){
    
    for (x in seq(length(ticker))) {
      tmp <- stockdata %>%
        filter(symbol == ticker[x]) %>%
        filter_time(time_formula = start ~ end) %>%
        arrange(date) %>%
        mutate(first_norm = (adjusted/first(adjusted))-1)
      
      data <- bind_rows(data, tmp)
    }
  
  } else if (by == "mean") {
    
    for (x in seq(length(ticker))) {
      tmp <- stockdata %>%
        filter(symbol == ticker[x]) %>%
        filter_time(time_formula = start ~ end) %>%
        arrange(date) %>%
        mutate(mean_norm = (adjusted/mean(adjusted))-1)
      
      data <- bind_rows(data, tmp)
    }  
    
  } else {
      stop("Error: Argument 'by' out of range. Choose either 'mean' or 'first'")
  }
    
  data <- data %>% as_tbl_time(., index = date)
  return(data)
  
}


# Get Returns ----
#' Function to add column with returns based on adjusted prices to tbbl_time.
#' Defaults to daily log returns.
#'
#' @param stockdata 
#' @param period chr, specifying the period. Must be either "daily" or "monthly".  
#' Defaults to "daily".
#' @param type chr, specifying the type of returns calculated. 
#' Select "log" for logarithmic returns or "arithmetic" for relative returns.
#' Defaults to "log".
#' @param colname a chr setting the new column's name. Defaults to "log_return".
#'
#' @return modified tbbl_time with new column including the returns.

get_returns <- function(stockdata, period = "daily", type = "log", colname = "log_return"){
  
  returns <- stockdata %>%
    filter(!is.na(adjusted)) %>%
    group_by(symbol) %>%
    tq_mutate(select = adjusted, 
              mutate_fun = periodReturn, 
              period = period, 
              type = type, 
              col_rename = colname) %>%
    ungroup()
  
  return(returns)
  
}