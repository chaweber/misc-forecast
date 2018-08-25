# helper functions to transform data

# two functions to add row to tbbl with normalised prices over a given period.
# take as arguments:
# a period (a character vector c("startdate", "enddate"))
# a tbbl_time
# and a list of the  tickers in the stockdata

normalise_by_first <- function(calc_period, stockdata, ticker){
  
  data <- NULL
  start <- calc_period[1]
  end <- calc_period[2]
  
  for (x in seq(length(ticker))) {
    
    tmp <- stockdata %>%
      filter(symbol == ticker[x]) %>%
      filter_time(time_formula = start ~ end) %>%
      arrange(date) %>%
      mutate(first_norm = (adjusted/first(adjusted))-1)
    
    data <- bind_rows(data, tmp)
  }
  
  data <- data %>% as_tbl_time(., index = date)
  
  return(data)
  
}


normalise_by_mean <- function(calc_period, stockdata, ticker){
  
  data <- NULL
  start <- calc_period[1]
  end <- calc_period[2]
  
  for (x in seq(length(ticker))) {
    
    tmp <- stockdata %>%
      filter(symbol == ticker[x]) %>%
      filter_time(time_formula = start ~ end) %>%
      arrange(date) %>%
      mutate(mean_norm = (adjusted/mean(adjusted))-1)
    
    data <- bind_rows(data, tmp)
  }
  
  data <- data %>% as_tbl_time(., index = date)
  
  return(data)
  
}


# function to add column with returns 
# can be either "daily" or "monthly"
# "log" or "arithmetic"
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