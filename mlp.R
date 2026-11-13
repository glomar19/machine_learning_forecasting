# Clean environment
rm(list=ls())

# Load required libraries
library(dplyr)
library(keras)

# Load data
train_scale <- read.csv("./data/gold/sales_train_normalized.csv")
val_scale <- read.csv("./data/gold/sales_val_normalized.csv")
test_scale <- read.csv("./data/gold/sales_test_normalized.csv")
sample_submission <- read.csv("./data/bronze/sample_submission.csv")




# Neural networks 
# Select features needed for model 
cols_for_model <- c('sales_y' ,'lag_1','lag_2' , 'lag_3', 'volatility',
                    'product_lineOutdoors' , 
                    'product_category_Team.Sports','pct_change_lag_1','pct_change_lag_2')



train_y <- as.numeric(train_scale$sales_y)  # Convert to numeric if it's a data frame
val_y <- as.numeric(val_scale$sales_y) 


cols_for_model <- setdiff(cols_for_model, "sales_y")

# Subset train_scale to create train_x
train_x <- train_scale[, cols_for_model]
val_x <- val_scale[, cols_for_model]
# Turn dummies to numeric 
train_x$product_category_Team.Sports <- as.numeric(train_x$product_category_Team.Sports)
val_x$product_category_Team.Sports <- as.numeric(val_x$product_category_Team.Sports)

train_x$product_lineOutdoors <- as.numeric(train_x$product_lineOutdoors)
val_x$product_lineOutdoors <- as.numeric(val_x$product_lineOutdoors)

train_x <- as.matrix(train_x)
val_x <- as.matrix(val_x)


# Create second validation set and train, we will not perferm 10-fold cross validation here, that's why we need another validation set
set.seed(1)
# Creation of second train set and second validation set from train 
train2.idx <- sample(1:nrow(train_x), 0.80*nrow(train_x))

train2_x <- train_x[train2.idx, ]
train2_y <- train_y[train2.idx]

val2_x <- train_x[-train2.idx, ]
val2_y <- train_y[-train2.idx]


# Make model 

train_model <- function(hidden_units, num_hidden_layers, activation_function, optimizer_type, batch_size) {
  # Create the model
  model <- keras_model_sequential()
  
  # Input layer + first hidden layer
  model %>% layer_dense(units = hidden_units, activation = activation_function, input_shape = c(8)) # Input shape is 8 since we use 8 predictors for this model 
  
  # Add the rest of the hidden layers
  for (i in 2:num_hidden_layers) {
    model %>% layer_dense(units = hidden_units, activation = activation_function)
  }
  
  # Output layer
  model %>% layer_dense(units = 1)  # Output layer
  
  # Compile the model with specified optimizer
  optimizer <- switch(optimizer_type,
                      rmsprop = optimizer_rmsprop(),
                      adam = optimizer_adam(),
                      sgd = optimizer_sgd())
  
  model %>% compile(
    loss = 'mean_squared_error',
    optimizer = optimizer,
    metrics = c('mean_absolute_error')
  )
  
  # Fit the model
  history <- model %>% fit(train2_x, train2_y, 
                           epochs = 50, 
                           batch_size = batch_size, 
                           validation_data = list(val2_x, val2_y),
                           verbose = 0) # Suppress detailed output for clarity
  
  # Return model history for further analysis if needed
  return(history)
}



# Define combinations of parameters
hidden_units_list <- c(128, 256)  # Number of units per layer
num_hidden_layers_list <- c(3, 4, 5)  # Number of hidden layers
activation_functions <- c('relu','sigmoid')
optimizers <- c('rmsprop', 'adam')
batch_sizes <- c(32, 64)

# Initialize results storage
results <- list()
mean_val_losses <- data.frame()

# Loop through all parameter combinations
for (hidden_units in hidden_units_list) {
  for (num_hidden_layers in num_hidden_layers_list) {
    for (activation_function in activation_functions) {
      for (optimizer_type in optimizers) {
        for (batch_size in batch_sizes) {
          cat("Training model with hidden_units =", hidden_units,
              "num_hidden_layers =", num_hidden_layers,
              "activation_function =", activation_function,
              "optimizer =", optimizer_type,
              "batch_size =", batch_size, "\n")
          
          # Train the model
          history <- train_model(hidden_units, num_hidden_layers, 
                                 activation_function, optimizer_type, batch_size)
          
          # Extract validation loss
          val_loss <- history$metrics$val_loss
          
          # Calculate the mean validation loss
          mean_val_loss <- mean(val_loss)
          
          # Store the results
          mean_val_losses <- rbind(mean_val_losses, 
                                   data.frame(
                                     hidden_units = hidden_units,
                                     num_hidden_layers = num_hidden_layers,
                                     activation_function = activation_function,
                                     optimizer = optimizer_type,
                                     batch_size = batch_size,
                                     mean_val_loss = mean_val_loss
                                   ))
        }
      }
    }
  }
}

# Display results and find the best combination
print(mean_val_losses)
best_combination <- mean_val_losses[which.min(mean_val_losses$mean_val_loss), ]
cat("Best combination of parameters:\n")
print(best_combination)
# Best combination: 128 hidden units, 4 hidden layers, relu activation function, 32 batch size

# Now train the best model, and predict on the hold out validation set 

# Extract the best parameters
best_hidden_units <- best_combination$hidden_units
best_num_hidden_layers <- best_combination$num_hidden_layers
best_activation_function <- best_combination$activation_function
best_optimizer <- best_combination$optimizer
best_batch_size <- best_combination$batch_size

cat("Rebuilding and training the model with the best parameters:\n")
print(best_combination)

# Train the best model
# The best amount of hidden layers is 3

best_model <- keras_model_sequential() %>%
  layer_dense(units = best_hidden_units, activation = best_activation_function, input_shape = c(8)) %>%
  layer_dense(units = best_hidden_units, activation = best_activation_function) %>%
  layer_dense(units = best_hidden_units, activation = best_activation_function) %>%
  layer_dense(units = 1)  # Output layer

# Compile the model with specified optimizer
optimizer <- switch(best_optimizer,
                    rmsprop = optimizer_rmsprop(),
                    adam = optimizer_adam(),
                    sgd = optimizer_sgd())

best_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer,
  metrics = c('mean_absolute_error')
)


best_model %>% fit(train_x, train_y, 
                   epochs = 50, 
                   batch_size = best_batch_size, 
                   validation_data = list(val_x, val_y),
                   verbose = 0)






best_model_history <- train_model(
  best_hidden_units,
  best_num_hidden_layers,
  best_activation_function,
  best_optimizer,
  best_batch_size
)

# Evaluate the best model on validation data
preds_val <- predict(best_model, val_x)
rmse_val <- sqrt(mean((preds_val - val_y)^2))
cat("Validation RMSE:", rmse_val, "\n") 
# The rmse on the hold out validation set is 9.18


# Now build the final model 


final_model <- keras_model_sequential() %>%
  layer_dense(units = best_hidden_units, activation = best_activation_function, input_shape = c(8)) %>%
  layer_dense(units = best_hidden_units, activation = best_activation_function) %>%
  layer_dense(units = best_hidden_units, activation = best_activation_function) %>%
  layer_dense(units = 1)  # Output layer

# Compile the model with specified optimizer
optimizer <- switch(best_optimizer,
                    rmsprop = optimizer_rmsprop(),
                    adam = optimizer_adam(),
                    sgd = optimizer_sgd())

final_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer,
  metrics = c('mean_absolute_error')
)







# Predictions on test set 
test_scale <- test_scale[,cols_for_model]
test_scale$product_category_Team.Sports <- as.numeric(test_scale$product_category_Team.Sports)
test_scale$product_lineOutdoors <- as.numeric(test_scale$product_lineOutdoors)
test_scale <- as.matrix(test_scale)



# The final model will be trained on train + val 
# We have to predict on the entire train + val set 
full_train <- rbind(train_scale, val_scale)
full_train_y <- full_train$sales_y


full_train <- full_train[,cols_for_model] # Select only the necessairy cols
full_train$product_category_Team.Sports <- as.numeric(full_train$product_category_Team.Sports) # Ensure that it is numeric 
full_train$product_lineOutdoors <- as.numeric(full_train$product_lineOutdoors)
# Turn it into a matrix 
full_train <- as.matrix(full_train)



final_model %>% fit(full_train, full_train_y, 
                    epochs = 50, 
                    batch_size = best_batch_size, 
                    validation_data = list(val_x, val_y), 
                    
                    verbose = 0)




preds_nn <- predict(final_model, test_scale)
preds_nn <- ifelse(preds_nn < 0,0,  preds_nn)

preds_nn_6dec <- data.frame(product_id = sample_submission$product_id,
                             sales_volume_2021 = preds_nn)

write.csv(preds_nn_6dec, file = "./nn_6dec.csv", row.names = FALSE)


# Plot the learning convergence 
plot(best_model_history, main = 'Learning convergence MLP')

