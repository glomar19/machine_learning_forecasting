# Clear history
rm(list = ls())

# load library
library(caret)
library(glmnet)
library(dplyr)
library(car)

#Read the sales_base
sales_test <- read.csv("data/gold/sales_test_normalized.csv")
train <- read.csv("data/gold/sales_train_normalized.csv")
val <- read.csv("data/gold/sales_val_normalized.csv")

################ Ridge Regression ###########

# Prepare data for glmnet
x_train <- model.matrix(sales_y ~ . , data = train)[, -1]  # Drop intercept
y_train <- train$sales_y
x_val <- model.matrix(sales_y ~ . , data = val)[, -1]
y_val <- val$sales_y

x_test <- model.matrix(~ ., data = sales_test)[, -1]  # Test set doesn't include `sales_y`

# Cross-Validation to find the best lambda
set.seed(1)
k_folds <- 10  # Number of folds
folds <- createFolds(y_train, k = k_folds, list = TRUE, returnTrain = TRUE)

ridge_cv_rmse <- numeric(k_folds)  # To store RMSE for each fold
best_lambdas <- numeric(k_folds)  # To store the best lambda for each fold

for (i in seq_along(folds)) {
  train_idx <- folds[[i]]
  x_train_cv <- as.matrix(x_train[train_idx, ])
  y_train_cv <- y_train[train_idx]
  
  x_val_cv <- as.matrix(x_train[-train_idx, ])
  y_val_cv <- y_train[-train_idx]
  
  # Fit Ridge Model
  ridge_model <- cv.glmnet(x_train_cv, y_train_cv, alpha = 0)
  best_lambda <- ridge_model$lambda.min
  best_lambdas[i] <- best_lambda
  
  # Predict and calculate RMSE
  predictions <- predict(ridge_model, newx = x_val_cv, s = best_lambda)
  ridge_cv_rmse[i] <- sqrt(mean((predictions - y_val_cv)^2))
}

# Average RMSE across folds
avg_rmse <- mean(ridge_cv_rmse)
cat("Average Cross-Validation RMSE:", round(avg_rmse, 2), "\n")
# Average Cross-Validation RMSE: 8.87 

# Use the best lambda across folds
final_best_lambda <- mean(best_lambdas)
cat("Final Best Lambda (Average from CV):", final_best_lambda, "\n")
# Final Best Lambda (Average from CV): 2.313419 

# Fit Ridge model with the entire training set and the best lambda
final_ridge <- glmnet(as.matrix(x_train), y_train, alpha = 0, lambda = final_best_lambda)
ridge_val_predictions <- predict(final_ridge, s = final_best_lambda, newx = as.matrix(x_val))

# Validation RMSE
ridge_rmse <- sqrt(mean((ridge_val_predictions - y_val)^2))
cat("Final Ridge Validation RMSE:", round(ridge_rmse, 2), "\n")
#Final Ridge Validation RMSE: 8.84 

# Predict on Test Set
ridge_test_predictions <- predict(final_ridge, s = final_best_lambda, newx = as.matrix(x_test))
summary(ridge_test_predictions)

# set negative predictions to zero
ridge_test_predictions <- ifelse(ridge_test_predictions < 0, 0, ridge_test_predictions)

# correct format
ridge_test_predictions = data.frame(
  product_id = sales_test$product_id,
  sales_volume_2021 = ridge_test_predictions
)
ridge_test_predictions = data.frame(
  product_id = sales_test$product_id,
  sales_volume_2021 = ridge_test_predictions$s1
)
head(ridge_test_predictions)

# Save Test Predictions
write.csv(ridge_test_predictions,
          "data/results/ridge_predictions_8dec.csv", row.names = FALSE)


