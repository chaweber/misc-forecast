# function to:
# return descriptive parameters of distribution 
# including tests for normality & seasonality
# and plot time series

library(tibbletime)
library(tidyquant)
library(dplyr)
library(ggplot2)
library(wesanderson)

source("R/get-data.R")
source("R/helper.R")

# set parameters 

startdate <- "1950-01-01"
enddate <- Sys.Date()
tickers <- c("^GSPC", "^N225", "^FTLC", 
             "NWSA", "UAA", "DISCA","PWR", #BHF
             "SCG", "MAT", "SRCL", "HRB", "COTY", "EVHC", 
             "AAPL", "MSFT", "AMZN", "FB",  "BRK-B",
             "JPM", "GOOGL", "JNJ", "XOM", "BAC")

# pick the ones with voting rights (class a), except for brk b
# FB, COTY (only a)
# GOOGL (c: GOOG), under armour (c: UA), NWSA (b: NWS)

# brighthouse financial (BHF) weird incline 10-17 /7/2017?
# > On August 4, 2017, Brighthouse Financial completed its separation from 
# MetLife and began trading on the Nasdaq stock exchange on August 7

tickernames <- c("GSPC", "N225", "FTLC", 
             "NWSA", "UAA", "DISCA","PWR", "SCG", 
             "MAT", "SRCL", "HRB", "COTY", "EVHC", 
             "AAPL", "MSFT", "AMZN", "FB",  "BRK",
             "JPM", "GOOGL", "JNJ", "XOM", "BAC")

desc_stats <- function(tickers, tickernames, startdate, enddate){
  
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
    
    if (x %in% c("FTLC", "N225", "GSPC")){
      type <- "index"
    } else if (x %in% c("AAPL", "AMZN", "FB", "MSFT", "BRK",  "JPM", "GOOGL", "JNJ", "XOM", "BAC")){
      type <- "top"
    } else {
      type <- "bottom"
    }  
    
  })
  
  stocks$stocktype <- stocktypes
  
  
  # calculate normalised adjusted value (beginning 2014)
  
  calc_period <- c("2014-01-01", "end")
  
  data <- stocks %>% normalise_by_first(calc_period = calc_period, 
                             ticker = tickernames,
                             stockdata = .) %>%
    normalise_by_mean(calc_period = calc_period, 
                            ticker = tickernames,
                            stockdata = .)


  ### ----
  
  #plot grid with all 
  
  data %>%
    ggplot(., aes(x = date, y = first_norm, color = symbol)) +
    geom_line(size = 0.5) +
    scale_color_manual(values = wes_palette(n=23, name="Rushmore1", type = "continuous")) +
    facet_grid(stocktype ~.) +
    labs(title = "Price Movement largest vs. smallest S&P 500 Companies", 
         y = "Normalised Prices", 
         x = "Date",
         caption = "(based on daily adjusted closing prices, normalised resp. 01-01-2014)")
  
  
  #plot index against top / bottom
  
  print(plot_base(ticker = c("NWSA", "UAA", "DISCA")))
  

   
  # ---- 
   
   
  window <- "2013/"
  
  mytheme <- chart_theme()
  mytheme$col$line.col <- "darkgreen"
  chart_Series(normalise_series(close[window, 1]) - 1, theme = mytheme, lty = 1, name = "Normalised Indices")
  add_TA(normalise_series(close[window, 2]) - 1, on = 1, col = "red", lty = 1)
  add_TA(normalise_series(close[window, 3]) - 1, on = 1, col = "blue", lty =1)
  legend(x = 'top', col=c("black", "darkred"), legend = c("TCG", "FTSE 100"), lty = 1, lwd = 1)
  

  par(mfrow = c(2,1))
  chartSeries(close[, 1], theme="white", name="S&P 500", subset='last 52 weeks')
  chartSeries(close[, 2], theme="white", name="NIKKEI 225", subset='last 52 weeks')
  
  
  plot(close[, 1], main = "S&P 500", ylab="Price")
  plot(close[, 2], main = "NIKKEI 225", ylab="Price")
  plot(close[, 3], main = "FTSE 350", ylab="Price")
  
  plot(close[, 1:3])
  plot(close[, 4:8])
  plot(close[, 9:13])
  
  # misc
  hist(log_r, prob=TRUE, breaks = 100, col="lightgrey", border="darkgrey", main="", xlab="Logarithmic Returns")
  curve(dnorm(x, mean=mu, sd=std), col="red", lty = 2, lwd=1, add=TRUE)
  curve(dcauchy(x, location=mu, scale=std), col="darkgreen", lty = 2, lwd=1, add=TRUE)
  curve(dlaplace(x, location=mu, scale=std), col="darkblue", lty = 2, lwd=1, add=TRUE)
  legend(x = 'topright', col=c("red", "darkgreen", "darkblue"), legend = c("Normal Distribution", "Cauchy Distribution", "Laplace Distribution"), lty = 2, lwd = 1)
  
  par(mfrow = c(1,1))
  qqnorm(log_r)
  qqline(log_r, distribution = qnorm, col="red", lwd=1)
  
  p <- ppoints(100)    # 100 equally spaced points on (0,1), excluding endpoints
  q <- quantile(log_r,p=p) # percentiles of the sample distribution
  plot(qlaplace(p) ,q, main="Laplace Q-Q Plot",
       xlab="Theoretical Quantiles",ylab="Sample Quantiles")
  qqline(log_r, distribution=qlaplace ,col="red", lty=2, lwd=1)
  
  ### ----
  
  # # get returns
  # # relative returns
  # rel_ret <- (diff(close) / close[1:(nrow(close)-1), 1])
  # # log returns:
  # log_r <- na.omit(diff(log(close)))

  ### ----
  
  # desc. stats
  print(summary(log_r))
  mu <- mean(log_r)
  std <- sd(log_r)^2
  skewness(log_r)
  kurtosis(log_r)
  
  # tests for normality 
  jarque.bera.test(close)
  # p-value < 2.2e-16
  # the null (normality can be rejected at any lvl) 
  # --> returns are not normally distributed
  
  # shapiro.test(head(log_r) - requires ts
  
  ks.test(log_r, pnorm, mean=mu, sd=std)
  ks.test(log_r, pcauchy, location=mu)
  ks.test(log_r, rlaplace)
  
}
