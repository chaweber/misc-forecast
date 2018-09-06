# helper functions to transform data

# Normalise ----

#' Function to add Col with normalised prices to tbbl
#' 
#' @param calc_period A chr vector, containing the start and end date for a subsetted period.
#' Must be in order ("start", "end") and in format "YYYY-mm-dd". 
#' @param stockdata a tbbl_time containing the stock prices to adjust.
#' @param ticker a chr vector containing the tickers of all assets that should be included.
#' @param normalise_by a chr, must be either "mean" or "first". Argument to normalise series by.
#' 
#' @return returns modified tbbl_time with new column "first_norm" / "mean_norm"  

normalise_prices <- function(calc_period = c("start", "end"), stockdata, ticker, normalise_by){

  normal_data <- NULL
  start <- calc_period[1]
  end <- calc_period[2]
    
  if (normalise_by == "first"){
    
    for (x in seq(length(ticker))) {
      tmp <- stockdata %>%
        filter(symbol == ticker[x]) %>%
        filter_time(time_formula = start ~ end) %>%
        arrange(date) %>%
        mutate(first_norm = (adjusted/first(adjusted))-1)
      
      normal_data <- bind_rows(normal_data, tmp)
      name <- "first_norm"
    }
  
  } else if (normalise_by == "mean") {
    
    for (x in seq(length(ticker))) {
      tmp <- stockdata %>%
        filter(symbol == ticker[x]) %>%
        filter_time(time_formula = start ~ end) %>%
        arrange(date) %>%
        mutate(mean_norm = (adjusted/mean(adjusted))-1)
      
      normal_data <- bind_rows(normal_data, tmp)
      name <- "mean_norm"
    }  
    
  } else {
      stop("Error: Argument 'normalise_by' out of range. Choose either 'mean' or 'first'")
  }
    
  normal_data <- normal_data %>% 
    as_tbl_time(., index = date) %>%
    mutate(adjusted = eval(as.name(name))) %>%
    drop_col(paste(name))
  
  return(normal_data)
  
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
# Add Lagged Values (log_returns or prices) ----

add_lags <- function(stockdata, lag, by){

  col_names <- paste0("lag_", lag)
  
  if (by == "log_return"){
    lagged_data <- stockdata %>%
      group_by(symbol) %>%
      tq_mutate(select = log_return,
                mutate_fun = lag.xts,
                k = lag,
                col_rename = col_names) %>%
      ungroup() 
  } else {
    lagged_data <- stockdata %>%
      group_by(symbol) %>%
      tq_mutate(select = adjusted,
                mutate_fun = lag.xts,
                k = lag,
                col_rename = col_names) %>%
      ungroup() 
  }
  return(lagged_data)
}

# Split Data into Train, Validation & Test Set for a given Ticker ----
splitframe <- function(data, trainshare, testshare){
    
    data <- data %>%
      arrange(date)
    
    trainrow <- round(nrow(data)*trainshare)
    train <- data[1:trainrow,]
    train <- add_column(train, "group" := "train")
    
    if (trainshare + testshare > 1){
      
      stop("Error: Invalid Shares")
      
    } else if ((trainshare + testshare) == 1) {
      
      test <- data[trainrow:nrow(data),]
      test <- add_column(test, "group" := "test")
      
      valid <- NULL
      
    } else {
      validshare <- 1- (trainshare + testshare)
      
      validrow <- trainrow + round(nrow(data)*validshare)
      
      valid <- data[(trainrow+1):validrow,]
      valid <- add_column(valid, "group" := "valid")
      
      test <- data[(validrow+1):nrow(data), ] 
      test <- add_column(test, "group" := "test")
      
    }
    
    splitdata <- bind_rows(train, valid, test) %>%
      as_tbl_time(index = date)
    
    return(splitdata)
}
