# Clean environment
rm(list=ls())

# Load required libraries
library(e1071)  
library(dplyr)  

# load data
train_scale <- read.csv("./data/gold/sales_train_normalized.csv")
val_scale <- read.csv("./data/gold/sales_val_normalized.csv")
test_scale <- read.csv("./data/gold/sales_test_normalized.csv")
sample_submission <- read.csv("./data/bronze/sample_submission.csv")


# Linear kernel 
cols_for_model <- c('sales_y' ,'lag_1','lag_2' , 'lag_3', 'volatility','average_delivery',
                'product_lineOutdoors' , 
                'product_category_Team.Sports','pct_change_lag_1','pct_change_lag_2')
# Tune does cross validation 
set.seed(1)
svm_lin <- tune(e1071::svm, sales_y ~ lag_1 + lag_2 + lag_3 + volatility + product_lineOutdoors + average_delivery
                + pct_change_lag_1 + product_category_Team.Sports
                + pct_change_lag_2,
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)),
                scale = FALSE, data = train_scale[,cols_for_model], kernel = 'linear')

# Now calculate rmse on the validation set 
svm_lin_preds <- predict(svm_lin$best.model, newdata = val_scale[,cols_for_model])
rmse_lin <- sqrt(mean((svm_lin_preds - val_scale[,cols_for_model]$sales_y)^2))
rmse_lin #8.71

# Predict on the train + val 
lin_cost <- svm_lin$best.model$cost
lin_gamma <- svm_lin$best.model$gamma


# Model using the best hyperparameters 
svm_lin_best <- svm(sales_y ~ lag_1 + lag_2 + lag_3 + volatility 
                    + pct_change_lag_1 + pct_change_lag_2 + product_category_Team.Sports + product_lineOutdoors, 
                    scale = FALSE, data = rbind(train_scale[cols_for_model],val_scale[cols_for_model])
                  ,  kernel = 'linear', cost = lin_cost, gamma = lin_gamma)

svm_lin_preds <- predict(svm_lin_best, newdata = test_scale) # Predictions on test set 
svm_lin_preds <- ifelse(svm_lin_preds < 0, 0, svm_lin_preds) # Turn negative predictions to 0, this can only improve rmse 

# combine to csv file
svm_lin_preds = data.frame(product_id = test_scale$product_id, sales_volume_2021 = svm_lin_preds)
head(svm_lin_preds)

# write away
write.csv(svm_lin_preds, "./data/results/svm_lin_preds.csv", row.names = FALSE)


# Polynomial kernel 

# Now Polynomial kernel 
set.seed(1)
svm_poly <- tune(e1071::svm, sales_y ~  lag_1 + lag_2 + lag_3 + volatility + product_lineOutdoors + product_category_Team.Sports
                
                 + pct_change_lag_1 + pct_change_lag_2,
                 ranges = list(cost = c(0.01, 0.1, 1, 10, 100), degree = c(2,3,4,5)),
                 scale = FALSE, data = train_scale[,cols_for_model], kernel = 'polynomial')

svm_poly_preds <- predict(svm_poly$best.model, newdata = val_scale[,cols_for_model])
svm_poly_preds <- ifelse(svm_poly_preds < 0, 0, svm_poly_preds)
rmse_poly <- sqrt(mean((svm_poly_preds - val_scale[,cols_for_model]$sales_y)^2))
rmse_poly  # 11.20

gamma_best <- svm_poly$best.model$gamma
degree_best <- svm_poly$best.model$degree
cost_best <- svm_poly$best.model$cost

# Retrain best model on train + val 
svm_poly_best <- svm(sales_y ~ lag_1 + lag_2 + lag_3 + volatility + 
                     + product_lineOutdoors + product_category_Team.Sports
   
                     + pct_change_lag_1 + pct_change_lag_2,data = rbind(train_scale[,cols_for_model], val_scale[,cols_for_model]), cost = cost_best, degree = degree_best, gamma = gamma_best , kernel = 'polynomial')

preds_poly <- predict(svm_poly_best, newdata = test_scale)
preds_poly <- ifelse(preds_poly < 0, 0, preds_poly)

# combine to csv file
preds_poly = data.frame(product_id = test_scale$product_id, sales_volume_2021 = preds_poly)
head(preds_poly)

# write away
write.csv(svm_lin_preds, "./data/results/svm_poly_preds.csv", row.names = FALSE)
