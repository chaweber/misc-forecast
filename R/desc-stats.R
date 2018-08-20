# function to:
# return descriptive parameters of distribution 
# including tests for normality & seasonality
# and plot time series

source("R/get-data.R")
source("R/helper.R")

desc-stats <- function(){
  
  # set parameters 
  startdate <- "1950-01-01"
  tickers <- c("^GSPC", "^N225", "^FTLC", "BHF", 
            "PWR", "DISCA", "UAA", "NWS", "AAPL", 
            "MSFT", "AMZN", "FB",  "BRK-B")
  
  ### ----
  
  # fetch data
  data <- NULL
  close <- NULL
  
  for (ticker in tickers){
    tmp <- get_all_data(ticker = ticker, startdate = startdate)
    close <- cbind(close, tmp[, 6])
    data <- cbind(data, tmp)
  }
  
  close_full <- na.omit(close)
  data_full <- na.omit(data)
  
  # re-name
  ticker <- c("GSPC", "N225", "FTLC", "BHF", 
               "PWR", "DISCA", "UAA", "NWS", "AAPL", 
               "MSFT", "AMZN", "FB",  "BRK")
  colnames(close) <- ticker
  colnames(close_full) <- ticker

  
  ### ----
  # plot 
  
  normalise_series <- function(x){
    x / coredata(x)[1]
  }
  
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
