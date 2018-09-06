#' Function to model volatility using an ARIMA-GARCH 
#'
#' @param by chr specifying the target variable to investigate. Defaults to "log_return".
#' Must be one of either "log_return" or "adjusted". Defaults to "log_return"
#' @param period chr giving the periodicity of series. must be either "daily" or "monthly". 
#' Defaults to "daily".
#' @param test_split int ratio of test sample. Defaults to 0.2
#'
#' @return tibble listing ME, RMSE, MAE, MPE, MAPE of ARIMA-GARCH for all stocks
#'
#' @import forecast
#' @import rugarch
#' 
#' @source R/01-config.R
#' @source R/econometrics/01-prepare-ts.R

arima_garch <- function(stockdata, by = "log_return", period = "daily", test_split = 0.2){

  # read data ----
  data <- prepare_ts_data(stockdata = stockdata,
                          startdate = startdate, 
                          enddate = enddate,
                          by = by)
  
  performance <- NULL
  
  for (x in tickernames){

    # estimate ARIMA ----
    
    arima <- forecast::auto.arima(data[, x], 
                                  trace = FALSE, 
                                  ic = "bic", 
                                  test = "adf",
                                  approximation= FALSE,
                                  allowmean = TRUE,
                                  allowdrift = TRUE)
    
    print(paste("Summary of ARIMA", x, ":"))
    print(summary(arima))
    
    # save model summary to file
    
    out <- capture.output(summary(arima))
    cat(title = paste("ARIMA of", x),
        out, 
        file=paste0("arima_", x, ".txt"), 
        sep="n", 
        append = TRUE)
    
    # model diagnostics
    
    jpeg(filename = paste0("arima_residuals_", x, ".jpg"), width = 1000, units = "px")
    plot.ts(arima$residuals,
         main = paste("Residuals from the ARIMA model for", x), 
         xlab = "Time", 
         ylab = "Residuals")
    dev.off()
    
    arima_box <- Box.test(arima$residuals)
    
    test_sample <- round(test_split * length(data[, x]))
    n <- length(data[, x])

    if (arima_box$p.value >= 0.1){
      
      print(paste("For", x, ", the NULL of independent distribution of the ARIMA residuals cannot be rejected. The residuals are only randomly correlated. Our job here is done."))
      
      
      # forecast with arima
      arima_predict <- forecast::forecast(arima)
      
      accuracy <- forecast::accuracy(f = as.vector(arima_predict$model$fitted), 
                                     x = data[, x][(n-test_sample):n])
      
      accuracy <- accuracy %>%
        as.tibble() %>%
        mutate(name = x)
      
      performance <- rbind(performance, accuracy)
      
    } else {

      print(paste("The null hypothesis of independence of the ARIMA residuals can be rejected for",
                  x, "at a 90% level of confidence. Let's add a GARCH."))
      
      
      # estimate ARIMA-GARCH ----
      
      # set parameters
      ar <- forecast::arimaorder(arima)[1]
      ma <- forecast::arimaorder(arima)[3]
      
      model_spec <- rugarch::ugarchspec(variance.model = list(garchOrder = c(1,1)),
                                        mean.model= list(armaOrder = c(ar, ma)),
                                        distribution.model = "norm")
      
      # estimate
      garch <- rugarch::ugarchfit(data = data[, x], 
                                  spec = model_spec, 
                                  out.sample = test_sample,  
                                  solver = "solnp")
      
      # save summary to file 
      out <- capture.output(summary(garch))
      cat(title = paste("GARCH of", x),
          out, 
          file=paste0("garch_", x, ".txt"), 
          sep="n", 
          append = TRUE)
      
      # plot model diagnostics
      jpeg(filename = paste0("garch_sq_resid_", x, ".jpg"), width = 1000, units = "px")
      plot((garch@fit$residuals)^2, 
           type = "l", 
           main = paste("Conditional Variance and Squared Residuals of", x), 
           xlab = "Time", 
           ylab = "")
      lines(garch@fit$var, col = "red")
      dev.off()
      
      jpeg(filename = paste0("garch_qqnorm_", x, ".jpg"), width = 1000, units = "px")
      qqnorm(garch@fit$residuals, 
             main = paste("Normal Q-Q Plot of ARIMA-GARCH Residuals for", x))
      qqline(garch@fit$residuals, col="red")
      dev.off()
      
      # -----
      garch_box <- Box.test(garch@fit$residuals)
      garch_kpss <- kpss.test(garch@fit$residuals)
      
      
      # get predictions ---- 
      garch_predict <- rugarch::ugarchforecast(garch, 
                                               n.ahead = 1,
                                               n.roll = test_sample,
                                               out.sample = test_sample)
      
      # get performance measures
      accuracy <- forecast::accuracy(f = as.vector(garch_predict@forecast$seriesFor), 
                         x = data[, x][(n-test_sample):n])
      
      accuracy <- accuracy %>%
        as.tibble() %>%
        mutate(name = x)
        
      cat(title = paste("Model Diagnostics of", x, "on Test Set"),
          capture.output(accuracy), 
          file=paste0("accuracy_garch_", x, ".txt"), 
          sep="n", 
          append = TRUE)
      
      # plot
      jpeg(filename = paste0("garch_forecast_", x, ".jpg"), width = 1000, units = "px")
      spd::plot(garch_predict, which = 2)
      dev.off()
  
      performance <- rbind(performance, accuracy)
    }
  }
  
  return(performance)
  
}
