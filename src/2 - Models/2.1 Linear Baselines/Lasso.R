################### Lasso Regression ###################
# Clear history
rm(list = ls())

# Load library
library(caret)
library(glmnet)
library(dplyr)

# Load the required datasets
sales_test <- read.csv("data/gold/sales_test_normalized.csv")  # Test set without target variable
train <- read.csv("data/gold/sales_train_normalized.csv")      # Training set
val <- read.csv("data/gold/sales_val_normalized.csv")          # Validation set

# Prepare data
x_train <- model.matrix(sales_y ~ . , data = train)[, -1]  # Drop intercept
y_train <- train$sales_y
x_val <- model.matrix(sales_y ~ . , data = val)[, -1]
y_val <- val$sales_y

# Define 10-Fold Cross-Validation
set.seed(1)
k_folds <- createFolds(y_train, k = 10, list = TRUE, returnTrain = TRUE)

# Initialize to store RMSE for each fold
lasso_cv_rmse <- numeric(length(k_folds))

for (i in seq_along(k_folds)) {
  # Split data into training and validation sets
  train_idx <- k_folds[[i]]
  x_train_cv <- x_train[train_idx, ]
  y_train_cv <- y_train[train_idx]
  
  x_val_cv <- x_train[-train_idx, ]
  y_val_cv <- y_train[-train_idx]
  
  # Fit Lasso model for current fold
  lasso_model <- cv.glmnet(x_train_cv, y_train_cv, alpha = 1, lambda = 10^seq(-4, 2, length = 100))
  
  # Use the best lambda for prediction
  best_lambda <- lasso_model$lambda.min
  
  # Predict on validation data
  predictions <- predict(lasso_model, newx = x_val_cv, s = best_lambda)
  
  # Calculate RMSE for the fold
  lasso_cv_rmse[i] <- sqrt(mean((predictions - y_val_cv)^2))
}

# Average RMSE across all folds
mean_rmse <- mean(lasso_cv_rmse)
print(paste("Average RMSE across 10 folds:", round(mean_rmse, 2)))
# "Average RMSE across 10 folds: 8.83"

# Fit final Lasso model on the full training data
set.seed(1)
final_lasso <- cv.glmnet(x_train, y_train, alpha = 1, lambda = 10^seq(-4, 2, length = 100))
best_lambda_lasso <- final_lasso$lambda.min
print(paste("Best Lambda for Final Lasso:", best_lambda_lasso))
# Best Lambda for Final Lasso: 0.0403701725859655"


# Validation Predictions
lasso_val_predictions <- predict(final_lasso, s = best_lambda_lasso, newx = x_val)
lasso_val_rmse <- sqrt(mean((lasso_val_predictions - y_val)^2))
print(paste("Validation RMSE:", round(lasso_val_rmse, 2)))
#Validation RMSE: 8.7"

# Extract Coefficients
lasso_coefficients <- as.data.frame(as.matrix(coef(final_lasso, s = best_lambda_lasso)))
names(lasso_coefficients) <- c("Coefficient")
lasso_coefficients <- lasso_coefficients[lasso_coefficients$Coefficient != 0, , drop = FALSE]  # Keep non-zero coefficients
print("Non-zero coefficients used by Lasso:")
print(lasso_coefficients)

# Test Set Predictions
x_test <- model.matrix(~ . , data = sales_test)[, -1]  # Prepare test data
lasso_test_predictions <- predict(final_lasso, s = best_lambda_lasso, newx = x_test)
summary(lasso_test_predictions)

# set negative predictions to zero
lasso_test_predictions <- ifelse(lasso_test_predictions < 0, 0, lasso_test_predictions)

# correct format
lasso_test_predictions = data.frame(
  product_id = sales_test$product_id,
  sales_volume_2021 = lasso_test_predictions
)

# Save predictions to CSV
write.csv(
  data.frame(product_id = sales_test$product_id, sales_volume_2021 = lasso_test_predictions$s1),
  "data/results/lasso_predictions.csv",
  row.names = FALSE
)

