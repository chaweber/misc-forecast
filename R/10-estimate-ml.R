# hard-coded for log return

estimate_ml <- function(stockdata, lags, by = "log_return"){

  # format data ----
  print("Formating Data to h2o Frame...")
  data <- prepare_ml_data(stockdata = stockdata, 
                          tickernames = tickernames, 
                          startdate = startdate, 
                          enddate = enddate,
                          lags = lags,
                          train = 0.8,
                          test = 0.1,
                          by = by)
  
  # fix training parameters ----
  params <- list(ntrees = c(5, 80), 
                 max_depth = c(2, 20), 
                 learn_rate = c(0.001, 0.3))
  
  search_criteria <- list(strategy = "RandomDiscrete", 
                          max_runtime_secs = 900, 
                          stopping_metric = "MAE", 
                          stopping_tolerance = 0.0001, 
                          stopping_rounds = 6, 
                          seed = 40)
  
  y <- by
  x <- paste0("lag_", lags)
  
  performance <- NULL
  
  # init h2o ----
  h2o::h2o.init(ip = "localhost", min_mem_size = "2G", max_mem_size = "5G")
  
  for(ticker in tickernames){
    
    train <- data  %>% filter(symbol == ticker) %>% filter(group == "train") %>% h2o::as.h2o()
    test <- data %>% filter(symbol == ticker) %>% filter(group == "test") %>% h2o::as.h2o()
    
    if (nrow(data  %>% filter(group == "valid")) > 0){
      valid <- data  %>% filter(symbol == ticker) %>% filter(group == "valid") %>% h2o::as.h2o()
    }
    
    # perform grid search----
    
    # GBM
    print("Training GBM...")
    gbm <- h2o::h2o.grid("gbm",
                         y = y, 
                         x = x, 
                         training_frame = train,
                         validation_frame = valid, 
                         hyper_params = params,
                         search_criteria = search_criteria,
                         grid_id = paste0("gbm_", ticker))
    
    # get best model ----
    print("Done. Getting Predictions...")

    gbm_grid <- h2o::h2o.getGrid(grid_id = paste0("gbm_", ticker), sort_by = "MSE", decreasing = TRUE)
    gbm_best <- h2o::h2o.getModel(gbm_grid@model_ids[[1]])
    
    cat(title = paste("GBM of", ticker),
        capture.output(summary(gbm_best)), 
        file=paste0("gbm_", ticker, ".txt"), 
        sep="n", 
        append = TRUE)
    
    # investigate performance of best model on test set ----
    
    gbm_performance <- h2o::h2o.performance(model = gbm_best, newdata = test)
    
    # Get Predictions on Test Set and merge to Data
    
    gbm_predictions <- h2o::h2o.predict(gbm_best, test) %>% as.vector
    
    test_predict <- data %>%
      filter(symbol == ticker) %>%
      filter(group == "test") %>% 
      select(symbol, group, stocktype, date, log_return) %>%
      add_column(., "prediction_gbm" := gbm_predictions)
      
    # get accuracy on test set
    accuracy <- forecast::accuracy(f = test_predict %>% select(prediction_gbm) %>% as.ts(),
                                   x = test_predict %>% select(log_return) %>% as.ts())

    accuracy <- accuracy %>%
      as.tibble() %>%
      mutate(name = ticker)

    performance <- rbind(performance, accuracy)
    
    # plot ----
    
    data_predict <- data %>%
      filter(symbol == ticker) %>%
      filter(group %in% c("train", "valid")) %>% 
      select(symbol, group, stocktype, date, log_return) %>% 
      add_column(., "prediction_gbm" := as.numeric(NA))
    
    data_predict <- bind_rows(data_predict, test_predict) %>%
      as_tbl_time(index = date) %>%
      arrange(date)
    
    jpeg(filename = paste0("gbm_forecast_", ticker, ".jpg"), width = 1000, units = "px")
    data_predict %>%
      filter_time(time_formula = "2017-01-01" ~ "end") %>%
      ggplot(aes(x = date)) +
      theme_tq() +
      geom_line(aes(y = (log_return)^2, colour = symbol), colour = "black", size = 0.8, alpha = 0.8) +
      geom_line(aes(y = (prediction_gbm)^2, colour = symbol), colour = "red", size = 0.8) +
      labs(title = "GBM One-Day Ahead Forecast of Volatility",
           subtitle = paste(ticker),
           x = "Date",
           y = "Value")
    dev.off()
    
    jpeg(filename = paste0("gbm_varimp_", ticker, ".jpg"), width = 1000, units = "px")
    h2o.varimp_plot(gbm_best)
    dev.off()
  }
  
  # return ----
  h2o.shutdown(prompt = FALSE)
  return(performance)
}