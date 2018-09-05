library(h2o)

source("R/01-config.R")
source("R/machine-learning/01-prepare-ml.R")

# read data ----

lags <- 1:30
data <- prepare_data(tickers = tickers, 
                     tickernames = tickernames, 
                     startdate = startdate, 
                     enddate = enddate,
                     lags = lags,
                     test = 0.1,
                     by = "adjusted")

# init h2o ----
h2o.init(ip = "localhost", min_mem_size = "2G", max_mem_size = "5G")

train <- data  %>% filter(group == "train") %>% as.h2o()
test <- data  %>% filter(group == "test") %>% as.h2o()

if (nrow(data  %>% filter(group == "valid")) > 0){
  valid <- data  %>% filter(group == "valid") %>% as.h2o()
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

# 1-day forecast, based on lagged values ----

# set variables
y <- "adjusted"
x <- paste0("lag_", lags)

# perform grid search----

#Regression Forest
rf <- h2o.grid("randomForest",
               y = y, 
               x = x, 
               training_frame = train,
               validation_frame = valid, 
               hyper_params = params$forest,
               search_criteria = search_criteria,
               grid_id = "rf")

# GBM
gbm <- h2o.grid("gbm",
               y = y, 
               x = x, 
               training_frame = train,
               validation_frame = valid, 
               hyper_params = params$gbm,
               search_criteria = search_criteria,
               grid_id = "gbm")

# glm <- h2o.automl("glm",
#                 y = y, 
#                 x = x,
#                 family = "gaussian",
#                nfolds = 0,
#                 training_frame = train,
#                 validation_frame = valid, 
#                 model_id = "glm",
#                seed = 334)



# get best model of each grid and investigate performance statistics on test set ----
rf_grid <- h2o.getGrid(grid_id = "rf", sort_by = "RMSE", decreasing = TRUE)
rf_best <- h2o.getModel(rf_grid@model_ids[[1]])
rf_performance <- h2o.performance(model = rf_best, newdata = test)

gbm_grid <- h2o.getGrid(grid_id = "gbm", sort_by = "RMSE", decreasing = TRUE)
gbm_best <- h2o.getModel(gbm_grid@model_ids[[1]])
gbm_performance <- h2o.performance(model = gbm_best, newdata = test)


# Get Predictions on Test Set 
test_predict <- data  %>% 
  filter(group == "test") %>% 
  select(symbol, group, stocktype, date, adjusted) 

gbm_predictions <- h2o.predict(gbm_best, test) %>% as.vector

test_predict <- test_predict %>%
  add_column(., "prediction_gbm" := gbm_predictions)

data_predict <- data %>%
  filter(group %in% c("train", "valid")) %>% 
  select(symbol, group, stocktype, date, adjusted) %>% 
  add_column(., "prediction_gbm" := as.numeric(NA))

data_predict <- union(data_predict, test_predict) %>%
  as_tbl_time(index = date) %>%
  arrange(date) %>%
  group_by(symbol)

