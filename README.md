# InClass Kaggle competition: Sales Volume Prediction for Online Retail Products

## Project description

This project aims to develop a predictive model to forecast the sales volume of products for the year 2021 based on historical sales data from 2017 to 2020. The goal is to predict the number of units sold for each product in 2021, leveraging customer and product data along with historical sales performance.

## Coding practices 

The coding rules we applied to all teammembers were:
* Use the Snake Case naming convention for all features ex; sales_2017
* The naming of the Sales features is done as follows:
  * For training: lag_3 = sales_2017, lag_2 = sales_2018, and lag_1 = sales_2019
  * For testing: lag_3 = sales_2018, lag_2 = sales_2019, and lag_1 = sales_2020
* Use a seed value of 1 in case of randomization
* Try to avoid copy pasting code. Instead use functions. 

## Methodology
### Data Preprocessing
1) Data Cleaning: The data was first cleaned to handle missing values and clean the formating ensuring that only relevant and consistent data points were used for training, validation, and testing.

2) Data Splitting: The data from 2017 to 2019 was split into a train set (80%) and a validation set (20%). The test set consists of data from 2018-2020, which is used to make predictions for the year 2021. This split ensures that the model is evaluated on data it hasn't been trained on.

3) Feature Engineering: Relevant features were derived from the raw data, and additional meaningful attributes were created to better capture the relationships between the predictors and the target variable (sales volume). 

4) Feature Scaling: The features were scaled to ensure that all input variables were on a similar scale. 

## Model Building
The following models were built to predict the sales volume for 2021:

1. Linear Baselines:
Linear Regression,
Ridge Regression,
Lasso Regression
2. Generalized Additive Models (GAMs):
Smoothing Splines
3. Tree-based Models:
Decision Tree,
Bagging,
Random Forest,
Gradient Boosting,
XGboost
4. Support Vector Regression (SVR):
SVR with a Linear Kernel,
SVR with a Non-linear Kernel
5. Neural Networks:
Multi-layer Perceptron (MLP)

## Model Estimation
Hyperparameter Tuning: For each model (except for MLP), we performed hyperparameter tuning on the train set using cross-validation (CV). This step was done to find the optimal set of hyperparameters for each model, ensuring that the model is well-tuned and not overfitting to the training data.

Re-training: After determining the best hyperparameters, we retrained each model using the entire training set (80% of the data from 2017-2019) and then used the hold-out validation set (20% of the data from 2017-2019) to evaluate the model performance.

Model Selection: We chose the best-performing model based on its performance on the validation set. The model with the lowest error (RMSE) on the validation set was selected.

Retraining on Combined Dataset: Once the best model was chosen, we retrained it on the combined train and validation sets (the full 2017-2019 dataset) to ensure the model had the most data possible to learn from before making final predictions.

## Model Deployment
Predictions on Test Set: Using the best-performing model, we made predictions on the test set. These predictions represent the expected sales volume for 2021.

## Managerial Insights
After predicting, we looked which relevant insights we could give to the managerial audience of the online retailers.
