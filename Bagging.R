# Clean environment
rm(list=ls())

# Load libraries
library(randomForest)

# set seed
set.seed(1)

# load data
train <- read.csv("./data/gold/sales_train_normalized.csv")
val <- read.csv("./data/gold/sales_val_normalized.csv")
test <- read.csv("./data/gold/sales_test_normalized.csv")
sample_submission <- read.csv("./data/bronze/sample_submission.csv")

# simple bagging model
ncol(train) #29
bag_simple <- randomForest(sales_y ~ . - product_id - recency - average_delivery, data = train, mtry = 25, importance = TRUE)
bag_simple

# hyperparameter tuning
mtry_fixed <- 25

# List of values for ntree and nodesize to tune
ntree_values <- c(100, 300, 500, 800, 1000)
nodesize_values <- c(5, 10, 15)

# Initialize best_rmse to store the best result
best_rmse <- Inf
best_ntree <- NULL
best_nodesize <- NULL

# Grid search for ntree and nodesize
for (ntree_val in ntree_values) {
  for (nodesize_val in nodesize_values) {
    
    # Train the model using the current combination of ntree and nodesize
    rf_model <- randomForest(
      sales_y ~ . - product_id - recency - average_delivery,  
      data = train,  
      mtry = mtry_fixed,  
      ntree = ntree_val,  
      nodesize = nodesize_val,  
      importance = TRUE  
    )
    
    # Predict on the validation set
    preds_val <- predict(rf_model, newdata = val)
    
    # Calculate RMSE
    mse <- mean((preds_val - val$sales_y)^2)
    rmse <- sqrt(mse)
    
    # Update best model if the current RMSE is better
    if (rmse < best_rmse) {
      best_rmse <- rmse
      best_ntree <- ntree_val
      best_nodesize <- nodesize_val
    }
    
    # Print current RMSE
    cat("Current RMSE for ntree =", ntree_val, ", nodesize =", nodesize_val, "is", rmse, "\n")
  }
}

# Output the best hyperparameters
cat("Best RMSE:", best_rmse, "\n") # 9.02
cat("Best ntree:", best_ntree, "\n") # 800
cat("Best nodesize:", best_nodesize, "\n") # 10

# Final model with best parameters
final_bagging_model <- randomForest(
  sales_y ~ . - product_id - recency - average_delivery,
  data = train,
  mtry = mtry_fixed,  
  ntree = best_ntree,  
  nodesize = best_nodesize,  
  importance = TRUE  
)

# Predict on the test set
final_preds <- predict(final_bagging_model, newdata = test)
final_preds <- ifelse(final_preds < 0, 0, final_preds)  # Ensure no negative predictions

# Save predictions
final_preds_df <- data.frame(
  product_id = sample_submission$product_id,
  sales_volume_2021 = final_preds
)
# Save predictions to CSV
write.csv(final_preds_df, "./data/results/bagging_predictions_tuned.csv", row.names = FALSE)
