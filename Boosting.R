# Clean environment
rm(list=ls())

# Load libraries
library(gbm)
library(caret)

#set seed
set.seed(1)

# load date
train <- read.csv("./data/gold/sales_train_normalized.csv")
val <- read.csv("./data/gold/sales_val_normalized.csv")
test <- read.csv("./data/gold/sales_test_normalized.csv")
sample_submission <- read.csv("./data/bronze/sample_submission.csv")

# Define the grid of hyperparameters to search
grid <- expand.grid(
  n.trees = c(1000, 2000, 3000, 5000),          # Number of trees
  interaction.depth = c(2, 4, 6),               # Tree depth
  shrinkage = c(0.01, 0.05, 0.1),               # Learning rate
  n.minobsinnode = c(5, 10, 20)                 # Minimum obs per node
)

# Set up cross-validation
train_control <- trainControl(
  method = "cv",    # k-fold cross-validation
  number = 5        # Number of folds
)

# Train the model using caret's train function
tuned_model <- train(
  sales_y ~ . - product_id - recency - average_delivery 
  - product_category_Winter.Sports
  - product_category_Assorted.Sports.Articles
  - product_category_Children.Sports
  - product_category_Golf 
  - product_category_Outdoors
  - product_category_Racket.Sports
  - product_category_Running...Jogging 
  - product_category_Shoes - product_category_Swim.Sports
  - product_category_Team.Sports,
  data = train,
  method = "gbm",
  trControl = train_control,
  tuneGrid = grid,
  distribution = "gaussian",
  verbose = FALSE
)

#
set.seed(1)
boost.opt <- gbm(sales_y ~ .
           - product_id - recency - average_delivery 
           - product_category_Winter.Sports
           - product_category_Assorted.Sports.Articles
           - product_category_Children.Sports
           - product_category_Golf 
           - product_category_Outdoors
           - product_category_Racket.Sports
           - product_category_Running...Jogging 
           - product_category_Shoes - product_category_Swim.Sports
           - product_category_Team.Sports,
           data = train, 
           distribution = "gaussian", 
           n.trees = tuned_model$bestTune$n.trees, 
           interaction.depth = tuned_model$bestTune$interaction.depth, 
           shrinkage = tuned_model$bestTune$shrinkage,
           n.minobsinnode = tuned_model$bestTune$n.minobsinnode
)

#hyperparameters
tuned_model$bestTune$n.trees #1000
tuned_model$bestTune$interaction.depth #2
tuned_model$bestTune$shrinkage # 0.01
tuned_model$bestTune$n.minobsinnode # 5

# performance on validation set
preds_val = predict(boost.opt,newdata = val)
rmse = sqrt(mean((preds_val - val$sales_y)^2))
rmse # 8.91

# to submit on kaggle
preds = predict(boost.opt, newdata=test)
preds <- ifelse(preds < 0, 0, preds) 
preds_boosting_opt_5dec <- data.frame(product_id = sample_submission$product_id,
                                                 sales_volume_2021 = preds)
write.csv(preds_boosting_opt_5dec, file = "./data/results/boosting_opt_8dec.csv", row.names = FALSE)
