#' Function to  prepare data and split into train, test & validation set
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
#' @param lags int specifying which lags of target to include.
#' 
#' @return a tibble that can be grouped by train, test & validation sample.
#' Includes lagged values of target. If target variable is one of prices, they will be normalised.
#'
#' @source R/02-helpers.R
#' @source R/03-helpers-transform.R
#' @source R/04-fetch-data.R

source("R/02-helpers.R")
source("R/03-helpers-transform.R")
source("R/04-fetch-data.R")

prepare_ml_data <- function(tickers, 
                         tickernames, 
                         startdate, 
                         enddate = Sys.Date(), 
                         period = "daily", 
                         by = "log_return",
                         train = 0.8,
                         test = 0.1,
                         lags = 1:10){

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
  
   # add lagged values ---- IS HARD-CODED FOR EITHER "log_return" OR "adjusted"!!!
  data <- add_lags(data, lag = lags, by = by)
  

  #  split data ----
  data <- data %>%
    nest(-symbol) %>%
    mutate(split =  map(data, splitframe,
                        trainshare = train,
                        testshare = test)) %>%
    unnest(split)
  
  # train <- data  %>% filter(group == "train")
  # test <- data  %>% filter(group == "test")
  # valid <- data  %>% filter(group == "valid")
  
  # plot train test & validation period -----
  # daily_stocks %>%
  #   filter(symbol == "COTY") %>%
  #   ggplot(aes(date, adjusted)) +
  #   geom_rect(xmin = as.numeric(valid$date[1]), 
  #             xmax = as.numeric(tail(valid$date, n=1)),
  #             ymin = 0, ymax = Inf, alpha = 0.1,
  #             fill = palette_light()[[5]]) +
  #   geom_rect(xmin = as.numeric(test$date[1]), 
  #             xmax = as.numeric(tail(test$date, n=1)),
  #             ymin = 0, ymax = Inf, alpha = 0.1,
  #             fill = palette_light()[[3]]) +
  #   annotate("text", x = train$date[nrow(train)/2], y = 0,
  #            colour = palette_light()[[1]], label = "Train Sample") +
  #   annotate("text", x = valid$date[nrow(valid)/2], y = 0,
  #            colour = palette_light()[[1]], label = "Validation") +  
  #   annotate("text", x = test$date[nrow(test)/2], y = 0,
  #            colour = palette_light()[[1]], label = "Test") +
  #   geom_line(color = "darkgrey") +
  #   geom_point(size = 0.5) +
  #   # geom_ma(ma_fun = SMA, n = 28, size = 0.5, linetype = 1, colour = "red") +
  #   theme_tq() +
  #   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  #   labs(title = "Sample Train, Validation, and Test Period",
  #        x = "Date",
  #        y = "Price",
  #        subtitle = "COTY Daily Adjusted Closing Price") 
  
  return(data)
  
}
