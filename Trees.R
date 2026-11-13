# Clean environment
rm(list=ls())

# Load libraries
library(MASS)
library(tree)

#set seed
set.seed(1)

# load data
train <- read.csv("./data/gold/sales_train_normalized.csv")
val <- read.csv("./data/gold/sales_val_normalized.csv")
test <- read.csv("./data/gold/sales_test_normalized.csv")
sample_submission <- read.csv("./data/bronze/sample_submission.csv")

# A simple tree
tree_simple <- tree(sales_y ~ . - product_id , data = train)
summary(tree_simple)
plot(tree_simple)
text(tree_simple, pretty = 0)
preds_simple = predict(tree_simple, newdata = test) 

# Pruned tree

## Use cross validation to determine best number of partition regions
tree_cv <- cv.tree(tree_simple)
plot(tree_cv$size, tree_cv$dev, type = 'b')
index = which.min(tree_cv$dev) 
tree_cv$size[index] # 8 is best size, 8 terminal nodes
tree_pruned <- prune.tree(tree_simple, best = tree_cv$size[index])
plot(tree_pruned)
text(tree_pruned, pretty = 0)

# Performance
preds_val = predict(tree_pruned,newdata = val)
preds_val <- ifelse(preds_val < 0, 0, preds_val) 
mse = mean((preds_val - val$sales_y)^2)
rmse = sqrt(mse)
rmse # 10.84

# prediction on test set
preds_pruned = predict(tree_pruned, newdata = test)
summary(preds_pruned) # no negative predictions

# right format
tree_pruned_preds_df <- data.frame(product_id = test$product_id,
                               sales_volume_2021 = preds_pruned)
head(tree_pruned_preds_df)

# Write away
write.csv(tree_pruned_preds_df, "./data/results/tree_pruned_predictions.csv", row.names = FALSE)
