# Clean environment
rm(list=ls())

# Load libraries
library(randomForest)
library(dplyr)

# Load data
train <- read.csv("./data/gold/sales_train_normalized.csv")
val <- read.csv("./data/gold/sales_val_normalized.csv")
test <- read.csv("./data/gold/sales_test_normalized.csv")

# Define mtry tuning grid
# heuristic for mtry p/3, -2 to correct for sales_y and product_id
(ncol(train) - 2)/3 # 9

# Define hyperparameter grid
grid <- expand.grid(
  mtry = c(7, 9, 11),         
  ntree = c(100, 300, 500, 1000),    
  nodesize = c(5, 10, 15)            
)

# Initialize variables to store the results
results <- list()

# Perform grid search
set.seed(1)
for (i in seq_len(nrow(grid))) {
  params <- grid[i, ]
  
  # Train random forest model with the current combination of hyperparameters
  rf_model <- randomForest(
    sales_y ~ . - product_id, 
    data = train, 
    mtry = params$mtry,
    ntree = params$ntree,
    nodesize = params$nodesize,
    importance = TRUE
  )
  
  # Predict on the validation set
  val_preds <- predict(rf_model, newdata = val)
  
  # Compute RMSE for validation predictions
  val_rmse <- sqrt(mean((val_preds - val$sales_y)^2))
  
  # Store the results
  results[[i]] <- list(
    mtry = params$mtry,
    ntree = params$ntree,
    nodesize = params$nodesize,
    rmse = val_rmse,
    model = rf_model
  )
  
  # Print progress
  cat("Completed combination", i, "of", nrow(grid), "\n")
}

# Find the best combination of hyperparameters
best_model <- results[[which.min(sapply(results, `[[`, "rmse"))]]

# Display the best hyperparameters
cat("Best hyperparameters:\n")
print(best_model[c("mtry", "ntree", "nodesize")]) # mtry = 7, ntree = 300, nodesize = 10
cat("Validation RMSE:", best_model$rmse, "\n") # 8.62

random_forest_model <- best_model$model
varImpPlot(random_forest_model)

# Use the best model to predict on the test set
test_preds <- predict(best_model$model, newdata = test)
test_preds <- ifelse(test_preds < 0, 0, test_preds)  # Ensure no negative predictions
summary(test_preds)

# Format predictions for submission
test_preds_df <- data.frame(
  product_id = test$product_id,
  sales_volume_2021 = test_preds
)

# Save predictions to CSV
write.csv(test_preds_df, "./data/results/rf_predictions_tuned.csv", row.names = FALSE)
