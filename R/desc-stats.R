library(corrr)
library(purrr)
library(tseries)
library(stats)

source("R/helpers.R")
source("R/helpers-transform.R")

# function to get desc stats 
# by takes in either "prices" or "returns"

desc_stats <- function(data, by = "log_return", stocktypes = c("index", "bottom")){
  
  ifelse((by == "log_returns"), name <- "Logartihmic Returns", name <- "Adjusted Closing Prices")
  
  period <- monthly_stocks$date[1:2] %>% diff.Date() %>% as.integer()
  ifelse((period > 5), period <- "monthly", period <- "daily")
  
  
  # (1) correlations between prices/returns of diff. assets, incl. corr heatmap ----
  
  # get correlations
  corr <- get_corr(stockdata = data, 
                   stocktypes = stocktypes,
                   variable = by)
  
  # plot correlation heatmap and save to file
  
  jpeg(filename = paste0("heatmap_", period, "_", by, ".jpg"), width = 1500, units = "px")
  rplot(corr, shape = 20) +
    geom_point() + 
    labs(title = paste("Correlations between", period, name, "of Assets"), 
       subtitle = paste("Indices and S&P 500 Companies"))
  dev.off()

  
  # (2) investigate distribution of returns ----
  
  # summary of distribution parameters by asset
  summary <- data %>%
    group_by(symbol) %>%
    summarise(aver = mean(eval(as.name(by))),
              var = var(eval(as.name(by))),
              skew = skewness(eval(as.name(by))),
              kurt = kurtosis(eval(as.name(by))))
  
  # test for normality
  jb <- test_stats(FUN = jarque.bera.test,
             fun_name = "JB",
             stockdata = data,
             by = by,
             text = name)
  print(jb)
  
  
  # (3) investigate stationarity ----
  
  box <- test_stats(FUN = Box.test,
             fun_name = "LB",
             stockdata = data,
             by = by,
             text = name)
  print(box)
  
  adf <- test_stats(FUN = adf.test,
             fun_name = "ADF",
             stockdata = data,
             by = by,
             text = name)
  print(adf)
  
  # get acf
  data_acf <- get_acf(data, by = by)
  
  #### check again -------
  # plot acf
  jpeg(filename = paste0("acf_", period, "_", by, ".jpg"), width = 1500, units = "px")
  
  acf_daily_returns %>%
    filter(symbol == "GSPC") %>%
    ggplot(aes(x = lag, y = acf_values, color = symbol, group = symbol)) +
    theme_bw() +
    geom_hline(yintercept = 0) +
    geom_line(aes(y = cutoff_upper), color = "blue", linetype = 2) +
    geom_line(aes(y = cutoff_lower), color = "blue", linetype = 2) +
    geom_segment(aes(xend = lag, yend = 0), size = 0.8) +
    facet_wrap(~symbol, ncol = 3) +
    expand_limits(y = c(-1, 1)) +
    labs(title = "ACF Plot", x = "Lag") +
    theme(legend.position = "none")
  
  dev.off()
  
  # return results ---- 
  return(list(correlations = corr,
              distribution = summary,
              acf_function = data_acf,
              jb = jb,
              adf = adf,
              lb = box))
  
}