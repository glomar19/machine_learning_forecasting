# Clear history
rm(list = ls())

# Load library's
library(caret)
library(glmnet)
library(dplyr)
library(car)

# Load the required datasets
sales_test <- read.csv("data/gold/sales_test_normalized.csv")  # Test set without target variable
train <- read.csv("data/gold/sales_train_normalized.csv")      # Training set
val <- read.csv("data/gold/sales_val_normalized.csv")          # Validation set

# Fit an initial linear regression model using all predictors
linear_model <- lm(sales_y ~ ., data = train)

# View the summary of the initial model
summary(linear_model)

# Plot diagnostics for the linear model
par(mfrow = c(2, 2))  # Plot all diagnostic plots together
plot(linear_model)

# Check for multicollinearity using VIF (Variance Inflation Factor)
vif_values <- vif(linear_model)
print(vif_values)

# Check for aliasing (linear dependencies between predictors)
alias_structure <- alias(linear_model)
print(alias_structure)

# Model Interpretation:
# - The model explains 88.09% of the variance in sales_y (Adjusted R-squared = 0.8809).
# - Key predictors:
#   * lag_1, lag_2, lag_3: Strong positive predictors of sales.
#   * volatility: Negative impact on sales, indicating inconsistent historical sales perform poorly.

# Refine the model by keeping only significant predictors
refined_model <- lm(
  sales_y ~ lag_3 + lag_2 + lag_1 + volatility + 
    supplier_country_NA +
    product_lineOutdoors + product_lineSports +
    pct_change_lag_1 + pct_change_lag_2,
  data = train
)

# View the summary of the refined model
summary(refined_model)

# Predict sales on the validation set
val_predictions <- predict(refined_model, newdata = val)

# Calculate RMSE on the validation set
val_rmse <- sqrt(mean((val_predictions - val$sales_y)^2))
cat("Validation RMSE:", round(val_rmse, 2), "\n")
# Result: RMSE = 8.96
# Interpretation: On average, the model's predictions deviate from the actual sales by ~9 units.

# Create a data frame for actual vs. predicted values
val_predictions_df <- data.frame(Actual = val$sales_y, Predicted = val_predictions)

# Plot actual vs. predicted sales
plot(val_predictions_df$Actual, val_predictions_df$Predicted,
     xlab = "Actual Sales", ylab = "Predicted Sales",
     main = "Actual vs. Predicted Sales",
     pch = 19, col = "blue", cex = 0.6)
abline(0, 1, col = "red", lwd = 2, lty = 2)  # Reference line (y = x)

# Predict sales on the test set using the initial linear model
test_predictions <- predict(linear_model, newdata = sales_test)

# Ensure predictions are non-negative
test_predictions <- pmax(0, test_predictions)

# Save the test set predictions to a CSV file
write.csv(
  data.frame(
    product_id = sales_test$product_id,
    sales_volume_2021 = test_predictions
  ),
  "data/results/sales_linear.csv",
  row.names = FALSE
)