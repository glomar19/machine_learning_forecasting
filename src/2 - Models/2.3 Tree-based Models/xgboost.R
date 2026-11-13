# Clean environment
rm(list = ls())

# Load required libraries
library(xgboost)
library(tidyverse)

# Load data
train <- read.csv("./data/gold/sales_train_normalized.csv")
val <- read.csv("./data/gold/sales_val_normalized.csv")
test <- read.csv("./data/gold/sales_test_normalized.csv")

# Prepare data for XGBoost
# Remove sales_y and product_id as they are not predictors
train_matrix <- xgb.DMatrix(
  data = as.matrix(train[, setdiff(names(train), c("sales_y", "product_id"))]),
  label = train$sales_y
)
val_matrix <- xgb.DMatrix(
  data = as.matrix(val[, setdiff(names(val), c("sales_y", "product_id"))]),
  label = val$sales_y
)

# Define hyperparameter grid
xgb_grid <- expand.grid(
  max_depth = c(3, 5, 7), 
  eta = c(0.01, 0.05, 0.1), 
  nrounds = c(100, 200, 300), 
  subsample = c(0.7, 0.8, 1.0), 
  colsample_bytree = c(0.7, 0.8, 1.0)
)

# Grid search with cross-validation
set.seed(1)
xgb_tune_results <- list()
for (i in 1:nrow(xgb_grid)) {
  params <- as.list(xgb_grid[i,])
  
  # Train the model
  xgb_model <- xgb.train(
    params = list(
      max_depth = params$max_depth, 
      eta = params$eta,             
      subsample = params$subsample,
      colsample_bytree = params$colsample_bytree,
      objective = "reg:squarederror"
    ),
    data = train_matrix,
    nrounds = params$nrounds,
    watchlist = list(val = val_matrix),
    print_every_n = 50,
    early_stopping_rounds = 20
  )
  
  # Store results
  val_preds_xgb <- predict(xgb_model, newdata = val_matrix)
  xgb_rmse <- sqrt(mean((val_preds_xgb - val$sales_y)^2))
  
  xgb_tune_results[[i]] <- list(params = params, rmse = xgb_rmse, model = xgb_model)
}

# Select the best model
best_xgb <- xgb_tune_results[[which.min(sapply(xgb_tune_results, `[[`, "rmse"))]]
best_xgb$rmse                     # 8.49
best_xgb$params$max_depth         # 3
best_xgb$params$eta               # .05
best_xgb$params$nrounds           # 200
best_xgb$params$subsample         # .7
best_xgb$params$colsample_bytree  # .8

# Feature importance
xgb_importance <- xgb.importance(feature_names = colnames(train_matrix), model = best_xgb$model)
xgb.plot.importance(xgb_importance)

# Predict on the test set
test_matrix <- xgb.DMatrix(
  data = as.matrix(test[, setdiff(names(test), "product_id")])
)
test_preds_xgb <- predict(best_xgb$model, newdata = test_matrix)
head(test_preds_xgb)
length(test_preds_xgb)

# right format
xgb_preds_df <- data.frame(product_id = test$product_id,
                               sales_volume_2021 = test_preds_xgb)
head(xgb_preds_df)
summary(xgb_preds_df) # no negative predictions

# Write away
write.csv(xgb_preds_df, "./data/results/xgb_predictions_7dec.csv", row.names = FALSE)
