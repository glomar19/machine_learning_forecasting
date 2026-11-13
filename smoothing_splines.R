# Clear history
rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(mgcv)
library(Metrics)

# Load the data
train <- read.csv("./data/gold/sales_train_normalized.csv")
val <- read.csv("./data/gold/sales_val_normalized.csv")
test <- read.csv("./data/gold/sales_test_normalized.csv")

# Check data
summary(train)

# Visualize relationships for continuous features
pairs(train[, c("sales_y", "lag_1", "lag_2", "lag_3", "recency", "volatility", "average_delivery", "pct_change_lag_2", "pct_change_lag_1")])

# Fit a GAM model using all 8 continuous predictors
gam_model_all <- gam(sales_y ~ 
                       s(lag_1) + s(lag_2) + s(lag_3) + 
                       s(recency) + s(volatility) + 
                       s(average_delivery) + 
                       s(pct_change_lag_2) + s(pct_change_lag_1),
                     data = train
                     )

# Display
summary(gam_model_all)
#                      edf Ref.df      F  p-value
# s(lag_1)            8.993  9.000 54.986  < 2e-16 ***
# s(lag_2)            8.962  8.998 36.703  < 2e-16 ***
# s(lag_3)            8.951  8.998 24.131  < 2e-16 ***
# s(recency)          1.000  1.000  0.094  0.75976
# s(volatility)       1.001  1.001 17.158 3.62e-05 ***
# s(average_delivery) 1.000  1.001  0.016  0.90110    
# s(pct_change_lag_2) 5.848  6.319  5.014  0.00805 **
# s(pct_change_lag_1) 3.603  3.862  0.592  0.74747
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# fit different GAM models

gam1 <- gam(sales_y ~ s(lag_1) + s(lag_2) + s(lag_3) + s(volatility) + s(pct_change_lag_2), data = train)
gam2 <- gam(sales_y ~ s(lag_1) + s(lag_2) + s(lag_3) + s(volatility) + s(pct_change_lag_2) + product_lineSports, data = train)
gam3 <- gam(sales_y ~ s(lag_1) + s(lag_2) + s(lag_3) + s(volatility) + s(pct_change_lag_2) + product_lineSports + product_lineOutdoors, data = train)
gam4 <- gam(sales_y ~ s(lag_1) + s(lag_2) + s(lag_3) + s(volatility) + s(pct_change_lag_2) + product_lineSports + product_lineOutdoors + product_category, data = train)

# Which model is best
anova(gam1, gam2, gam3, gam4, test = "F")

gam5 <- gam(sales_y ~ s(lag_1) + s(lag_2) + s(lag_3) + s(volatility) + s(pct_change_lag_2) + s(pct_change_lag_1), data = train)
gam6 <- gam(sales_y ~ s(lag_1) + s(lag_2) + s(lag_3) + s(volatility) + s(pct_change_lag_2) + s(pct_change_lag_1) + product_category_Team.Sports + product_lineOutdoors, data = train)
anova(gam1, gam5, gam6)

# adding predictors doesn't significantly improve model so we stick with gam1
pred_gam1 <- predict(gam1, newdata = val)
pred_gam3 <- predict(gam3, newdata = val)
pred_gam5 <- predict(gam5, newdata = val)
pred_gam6 <- predict(gam6, newdata = val)
pred_gam_model_all <- predict(gam_model_all, newdata = val)
rmse(val$sales_y, pred_gam1) # 13.22
rmse(val$sales_y, pred_gam3) # 14.11
rmse(val$sales_y, pred_gam5) # 13.76
rmse(val$sales_y, pred_gam6) # 15.74
rmse(val$sales_y, pred_gam_model_all) # 13.59

# look at hyperparameters model gam5
summary(gam1)
# Approximate significance of smooth terms:
#                       edf Ref.df      F  p-value
# s(lag_1)            9.000  9.000 57.501  < 2e-16 ***
# s(lag_2)            8.932  8.995 36.861  < 2e-16 ***
# s(lag_3)            8.954  8.998 24.186  < 2e-16 ***
# s(volatility)       1.000  1.000 16.604 4.78e-05 ***
# s(pct_change_lag_2) 7.462  8.429  7.178  < 2e-16 ***
# ---

#predict new data
prediction <- predict(gam1, newdata = test)
summary(prediction)
head(prediction)
prediction <- ifelse(prediction < 0, 0, prediction)

# combine to csv file
smoothing_spline_preds = data.frame(product_id = test$product_id, sales_volume_2021 = prediction)
head(smoothing_spline_preds)

# write away
write.csv(smoothing_spline_preds, "./data/results/sales_test_predictions_gam.csv", row.names = FALSE)
