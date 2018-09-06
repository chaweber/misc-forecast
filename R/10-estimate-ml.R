# for log return

estimate_ml <- function(stockdata, lags, by){
  
  # read data ----
  data <- prepare_ml_data(stockdata, 
                          tickernames = tickernames, 
                          startdate = startdate, 
                          enddate = enddate,
                          lags = lags,
                          train = 0.8,
                          test = 0.1,
                          by = by)
  
  for(ticker in tickernames){
    
    # init h2o ----
    h2o::h2o.init(ip = "localhost", min_mem_size = "2G", max_mem_size = "5G")
    
    train <- data  %>% filter(symbol = ticker) %>% filter(group == "train") %>% h2o::as.h2o()
    test <- data %>% filter(symbol = ticker) %>% filter(group == "test") %>% h2o::as.h2o()
    
    if (nrow(data  %>% filter(group == "valid")) > 0){
      valid <- data  %>% filter(symbol = ticker) %>% filter(group == "valid") %>% h2o::as.h2o()
    }
    
    # fix training parameters ----
    params <- list(forest = list(ntrees = c(20, 80), 
                                 max_depth = c(5, 50)),
                   boost = list(ntrees = c(20, 80), 
                                max_depth = c(2, 20), 
                                learn_rate = c(0.01, 0.3)))
    
    search_criteria <- list(strategy = "RandomDiscrete", 
                            max_runtime_secs = 900, 
                            stopping_metric = "MSE", 
                            stopping_tolerance = 0.001, 
                            stopping_rounds = 6, 
                            seed = 40)
    
    # set variables
    y <- by
    x <- paste0("lag_", lags)
    
    # perform grid search----
    
    # Regression Forest
    rf <- h2o::h2o.grid("randomForest",
                        y = y, 
                        x = x, 
                        training_frame = train,
                        validation_frame = valid, 
                        hyper_params = params$forest,
                        search_criteria = search_criteria,
                        grid_id = "rf")
    
    # GBM
    gbm <- h2o::h2o.grid("gbm",
                         y = y, 
                         x = x, 
                         training_frame = train,
                         validation_frame = valid, 
                         hyper_params = params$gbm,
                         search_criteria = search_criteria,
                         grid_id = "gbm")
    
    # get best model of each grid ----
    rf_grid <- h2o::h2o.getGrid(grid_id = "rf", sort_by = "RMSE", decreasing = TRUE)
    rf_best <- h2o::h2o.getModel(rf_grid@model_ids[[1]])
    
    cat(title = paste("RF of", ticker),
        capture.output(summary(rf_best)), 
        file=paste0("rf_", ticker, ".txt"), 
        sep="n", 
        append = TRUE)
    
    gbm_grid <- h2o::h2o.getGrid(grid_id = "gbm", sort_by = "RMSE", decreasing = TRUE)
    gbm_best <- h2o::h2o.getModel(gbm_grid@model_ids[[1]])
    
    cat(title = paste("RF of", ticker),
        capture.output(summary(rf_best)), 
        file=paste0("rf_", ticker, ".txt"), 
        sep="n", 
        append = TRUE)
    
    # investigate performance of best model on test set ----
    
    rf_performance <- h2o::h2o.performance(model = rf_best, newdata = test)
    gbm_performance <- h2o::h2o.performance(model = gbm_best, newdata = test)
    
    # Get Predictions on Test Set and merge to Data
    
    rf_predictions <- h2o::h2o.predict(rf_best, test) %>% as.vector
    gbm_predictions <- h2o::h2o.predict(gbm_best, test) %>% as.vector
    
    test_predict <- data %>%
      filter(symbol = ticker) %>%
      filter(group == "test") %>% 
      select(symbol, group, stocktype, date, log_return) %>%
      add_column(., "prediction_gbm" := gbm_predictions) %>%
      add_column(., "prediction_rf" := rf_predictions)
    
    accuracy <- forecast::accuracy(f = as.vector(test_predict$prediction_rf), 
                                   x = test_predict %>% select(log_return))
    
    data_predict <- data %>%
      filter(symbol = ticker) %>%
      filter(group %in% c("train", "valid")) %>% 
      select(symbol, group, stocktype, date, log_return) %>% 
      add_column(., "prediction_gbm" := as.numeric(NA)) %>%
      add_column(., "prediction_rf" := as.numeric(NA))
    
    data_predict <- union(data_predict, test_predict) %>%
      as_tbl_time(index = date) %>%
      arrange(date) %>%
      group_by(symbol)
    
    
  }
  
}