# Helper Functions

# Drop columns in tibble by Names ----
# name is a character vector including the columns to drop
drop_col <- function(df, name){
  df <- df %>% select(-one_of(name))
}

# Negate %in% operator ----

"%notin%" <- Negate("%in%")

# Function for Hypothesis Test ----
# requires package(tseries)

test_stats <- function(fun_name, stockdata, by){
  
  if (fun_name == "JB"){
    
    FUN <- jarque.bera.test

    print(paste("The Jarque-Bera test tests the null that a sample follows a normal distribution."))
    
    text_1 <- paste("For the following assets, the series might follow a normal distribution:")
    text_2 <- paste("The series is NOT normally distributed for any of the given assets (at a 99% confidence level).")

  } else if (fun_name == "LB"){
    
    FUN <- Box.test

    print(paste("The Ljung-Box test tests the null of independence of observations in a time series."))
    
    text_1 <- paste("For the following assets, the series might be independent:")
    text_2 <- paste("The series is NOT independent for all of the given assets (at a 99% confidence level).")

  } else if (fun_name == "ADF"){
    
    FUN <- adf.test
    
    print(paste("The Augmented Dickey Fuller test tests for the null that the series has a unit root."))
    #p-value >0.1
    text_1 <- paste("For the following assets, the series might be have a unit root:")
    #p-value < 0.1
    text_2 <- paste("For all given assets, the series does NOT have a unit root (at a 99% confidence level).")
    
  } else {
    stop("Error: Invalid Test Argument. Try 'JB', 'LB' or 'ADF'")
  }
  
  stocks_ts <- stockdata %>%
    as.tibble() %>%
    tbl2xts::tbl_xts(cols_to_xts = by, spread_by = "symbol")

  p_list <- NULL
  
  for (i in seq(ncol(stocks_ts)-1)){
    i <- i+1  
    test <- FUN(na.remove(stocks_ts[, i]))
    p_list$pvalue[i-1] <- test$p.value
    p_list$stock[i-1] <- colnames(stocks_ts)[i]
  }
  
  if (any(p_list$pvalue>=0.01)){
    data <- tibble(stocks = p_list$stock,
                   pvalues = p_list$pvalue) %>%
      filter(pvalues >= 0.01)
    
    print(paste(text_1))
    return(data)
    
  } else {
    print(paste(text_2))
  }
  
}
  

# Get ACF----  
  get_acf <- function(stockdata, by){
    
    acf_data <- stockdata %>%
      nest(-symbol) %>%
      mutate(acf_results = map(data, ~ acf(.x[[by]], plot = F)),
             acf_values = map(acf_results, ~ drop(.x$acf))) %>%
      unnest(acf_values) %>%
      group_by(symbol) %>%
      mutate(lag = seq(0, n()-1),
             cutoff_upper = -qnorm(0.1/2)/(n())^0.5,
             cutoff_lower = qnorm(0.1/2)/(n())^0.5)
    
    return(acf_data)

  }
  
# get correlations between assets ----
  get_corr <- function(stockdata, 
                       stocktypes = c("index", "bottom", "top"), 
                       variable = "rel_return")
    {
    
    corr_data <- stockdata %>%
      filter(stocktype %in% stocktypes) %>%
      select(symbol, date, variable) %>%
      spread(symbol, variable) %>%
      drop_col(., name = "date") %>%
      corrr::correlate(use = "pairwise.complete.obs", quiet = TRUE) %>%    
      corrr::rearrange(method = "HC", absolute = FALSE)  %>% 
      corrr::shave()
    
    return(corr_data)
    
  }