#' function to get correlations between prices/returns of diff. assets (incl. corr heatmap)
#' summary of distribution of returns and test for normality
#'
#' @param data 
#' @param by chr either "prices" or "returns"
#' @param stocktypes 
#'
#' @return
#' 
#' @import library(corrr)
#' @import library(purrr)
#' @import library(tseries)
#' @import library(stats)
#' 
#' @source R/02-helpers.R
#' @source R/03-helpers-transform.R
#' @source R/04-helpers-plot.R


desc_stats <- function(stockdata, by = "log_return"){
  
  ifelse((by == "log_return"), name <- "Logartihmic Returns", name <- "Adjusted Closing Prices")
  
  period <- stockdata$date[1:2] %>% diff.Date() %>% as.integer()
  ifelse((period > 5), period <- "monthly", period <- "daily")
  
  
  # get chart and distribution plots ----
  
  types <- c("index", "bottom", "top")
  
  for(type in types){
    
    jpeg(filename = paste0("chartplot_", period, "_", by, "_", type, ".jpg"), 
         height = 800, 
         units = "px")
    plot(plot_base(stockdata = stockdata, type = type, by = by))
    dev.off()
    
    plotdist <- plot_dist(stockdata = stockdata, type = type, by = by)
    
    jpeg(filename = paste0("dist_", period, "_", by, "_", type, ".jpg"), 
         height = 800, 
         units = "px")
    plot(plotdist$dist)
    dev.off()
    
    jpeg(filename = paste0("qqnorm", period, "_", by, "_", type, ".jpg"), 
         width = 1000, 
         units = "px")
    plot(plotdist$qq)
    dev.off()
  }
  
  # (1) correlations between prices/returns of diff. assets, incl. corr heatmap ----
  
  # get correlations
  corr <- get_corr(stockdata = stockdata, 
                   stocktypes = c("index", "top"),
                   variable = by)
  
  # plot correlation heatmap and save to file
  jpeg(filename = paste0("heatmap_top_", period, "_", by, ".jpg"), 
       width = 500, 
       units = "px")
  corrr::rplot(corr, shape = 20) +
    geom_point() + 
    labs(title = paste("Correlations between", period, name, "of Assets"), 
       subtitle = paste("Indices and largest S&P 500 Companies"))
  dev.off()
  
  # get correlations
  corr <- get_corr(stockdata = stockdata, 
                   stocktypes = c("index", "bottom"),
                   variable = by)
  
  # plot correlation heatmap and save to file
  jpeg(filename = paste0("heatmap_bottom_", period, "_", by, ".jpg"), 
       width = 500, 
       units = "px")
  corrr::rplot(corr, shape = 20) +
    geom_point() + 
    labs(title = paste("Correlations between", period, name, "of Assets"), 
         subtitle = paste("Indices and smallest S&P 500 Companies"))
  dev.off()
  

  
  # (2) investigate distribution of returns ----
  
  # summary of distribution parameters by asset
  summary <- stockdata %>%
    group_by(symbol) %>%
    summarise(aver = mean(eval(as.name(by))),
              var = var(eval(as.name(by))),
              skew = skewness(eval(as.name(by))),
              kurt = kurtosis(eval(as.name(by))))
  
  # test for normality
  jb <- test_stats(fun_name = "JB",
             stockdata = stockdata,
             by = by)
  print(jb)
  
  
  # (3) investigate stationarity ----
  
  box <- test_stats(fun_name = "LB",
             stockdata = stockdata,
             by = by)
  print(box)
  
  adf <- test_stats(fun_name = "ADF",
             stockdata = stockdata,
             by = by)
  print(adf)
  
  # get acf
  data_acf <- get_acf(stockdata, by = by)
  
  # plot acf
  for (ticker in tickernames){
    plot <- data_acf %>%
      filter(symbol == ticker) %>%
      ggplot(aes(x = lag, y = acf_values, colour = symbol, group = symbol)) +
      theme_bw() +
      geom_hline(yintercept = 0) +
      geom_line(aes(y = cutoff_upper), colour = "blue", linetype = 2) +
      geom_line(aes(y = cutoff_lower), colour = "blue", linetype = 2) +
      geom_segment(aes(xend = lag, yend = 0), size = 0.8) +
      facet_wrap(~symbol, ncol = 3) +
      expand_limits(y = c(-1, 1)) +
      labs(title = paste("ACF Plot for", ticker),
           x = "Lag") +
      theme(legend.position = "none")
    
    jpeg(filename = paste0(period, "_acf_", ticker, "_", by, ".jpg"), width = 1000, units = "px")
    plot(plot)
    dev.off()
    
  }
  
  # return results ---- 
  
  results <- list(correlations = corr,
                  distribution = summary,
                  acf_function = data_acf,
                  jb = jb,
                  adf = adf,
                  lb = box)
  
  out <- capture.output(results)
  cat(title = paste("Statistics of", period, by),
      out, 
      file=paste0("stats_", period, "_", by, ".txt"), 
      sep="n", 
      append = TRUE)
  
  return(results)
  
}