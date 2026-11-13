# Clear history
rm(list = ls())

# Load libraries
library(dplyr)
library(tidyr)
library(dummy)

# set working directory
# setwd("C:\\Users\\Arthu\\OneDrive - UGent\\Bureaublad\\Machine learning")

# LOAD DATA
sales_2017_2020_cleaned <- read.csv("./data/silver/sales_2017_2020_cleaned.csv")
products_cleaned <- read.csv("./data/silver/products_cleaned.csv")
sample_submission <- read.csv("./data/bronze/sample_submission.csv")

# Repeat the data partitioning (Make sure you run the set.seed(1))
# We use a 80-20 train-val split
set.seed(1)
unique_product_id <- unique(sales_2017_2020_cleaned$product_id)
train.idx <- sample(1:length(unique_product_id),size = 0.80*length(unique_product_id))

# The products in the train, val and test sets
train_id <- unique_product_id[train.idx] 
val_id <- unique_product_id[-train.idx] 
test_id <- sample_submission$product_id # These are all the product ids from the test set. 

# Create base_table 
sales_base <- sales_2017_2020_cleaned %>%
  group_by(product_id, order_year) %>% # Groups sales by product_id and year
  summarise(total_sales = sum(quantity, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(product_id, order_year, fill = list(total_sales = 0)) %>% # When a product has no sales in a given year, it is set to 0. 
  pivot_wider(names_from = order_year, values_from = total_sales, names_prefix = "sales_")

sales_test <- sales_base[,-2] # For the test set we delete the sales from 2017. 
# In the test set, we must add those products that have are only present in the sample_submission 
missing_products <- sample_submission$product_id[!(sample_submission$product_id %in% sales_2017_2020_cleaned$product_id)]

# Create df for the products with no info
missing_sales <- data.frame(
  product_id = missing_products,
  sales_2018 = 0,
  sales_2019 = 0,
  sales_2020 = 0
)
sales_test <- bind_rows(sales_test, missing_sales)

# Sort by product_id
sales_test <- sales_test %>% arrange(product_id)

colnames(sales_base)[1:5] <- c("product_id", "lag_3" ,"lag_2", "lag_1" ,"sales_y")
colnames(sales_test) <-  c("product_id", "lag_3" ,"lag_2", "lag_1")
head(sales_base)
head(sales_test)

# Now we subset sales_base into sales_train and sales_val 
sales_train <- sales_base[sales_base$product_id %in% train_id,]
sales_val <- sales_base[sales_base$product_id %in% val_id,]

# Add recency feature: recency is calcuated as days between 1/1 next year and last order_date of product
# get last porduct_id and order_date from sales
sales_2017_2019 <- sales_2017_2020_cleaned %>%
  mutate(order_date = as.Date(order_date, format = "%Y-%m-%d")) %>%
  filter(order_date <= as.Date("2019-12-31")) %>%
  dplyr::select(product_id, order_date)

# inspect
head(sales_2017_2019)
tail(sales_2017_2019)

#function for creating recency
calculate_recency <- function(input_date, data) {
  # Ensure input_date is in Date format
  input_date <- as.Date(input_date)
  
  # Process the dataframe
  result <- data %>%
    mutate(order_date = as.Date(order_date, format = "%Y-%m-%d")) %>%
    group_by(product_id) %>%
    slice_max(order_date, n = 1, with_ties = FALSE) %>% # Keep only the last order_date per product_id
    ungroup() %>%
    mutate(recency = as.numeric(input_date - order_date)) # Calculate recency in days
  
  return(result)
}

recency_2017_2019 <- calculate_recency('2020-01-01', sales_2017_2019)
head(recency_2017_2019)
recency_2017_2020 <- calculate_recency('2021-01-01', sales_2017_2020_cleaned)
head(recency_2017_2020$recency)

# add to basetable function
left_join_select <- function(left_df, right_df, cols_to_keep) {
  result <- left_df %>%
    left_join(
      right_df %>% dplyr::select(all_of(c("product_id", cols_to_keep))),
      by = "product_id"
    )
  return(result)
}

sales_train <- left_join_select(sales_train, recency_2017_2019, "recency")  
sales_val <- left_join_select(sales_val, recency_2017_2019, "recency") 
sales_test <- left_join_select(sales_test, recency_2017_2020, "recency")

# Average delivery: Here be careful with data leakage between train and val. 
# First merge the sales df with products, to get the supplier_id 
average_delivery <- function(sales, year, id ){
  sales_2017_2020_cleaned <- sales_2017_2020_cleaned[sales_2017_2020_cleaned$product_id %in% id & sales_2017_2020_cleaned$order_year!=year, ] # Subset the sales df (only retain train, val or test products)
  
  
  sales_merged <- merge(sales_2017_2020_cleaned, products_cleaned[c('supplier_id','product_id')], on = 'product_id')
  sales_merged$delivery_time <-as.numeric(as.Date(sales_merged$delivery_date) - as.Date(sales_merged$order_date)) # Delivery time in number of days 
  for (i in unique(sales_merged$supplier_id)) { # We look at average_delivery per supplier 
    sales_supplier <- sales_merged[sales_merged$supplier_id==i & sales_merged$order_year != year,] # The sales for that supplier 
    average_supplier <- mean(sales_supplier$delivery_time, na.rm = TRUE) # The average delivery time of that supplier. 
    # Now put those averages into sales_base or sales_test 
    for (p in unique(sales_supplier$product_id)){
      sales[sales$product_id == p, "average_delivery"] <- average_supplier
    }
  }
  return(sales)
}
sales_train <- average_delivery(sales_train, 2020, train_id)
sales_val <- average_delivery(sales_val, 2020, val_id)
sales_test <- average_delivery(sales_test, 2017, test_id)


# Volatility
# Will use relative volatility so volatility/mean sales , otherwise products with higher sales will always have higher volatility
# For the train and val set, you use year = 2020, because you only want to take into account years 2017-2019
# For test set you use year : 2017, because you only want to take into account years 2018-2020
# This feature is done at the product level 
sales_2017_2020_cleaned$order_month <- as.integer(substr(sales_2017_2020_cleaned$order_date,6,7))
sales_volatility <- function(sales, year) {
  for (i in unique(sales$product_id)) {
    sales_for_product <- sales_2017_2020_cleaned %>%
      filter(product_id == i & order_year != year)
    monthly_sales <- sales_for_product %>%
      group_by(order_year, order_month) %>%
      summarize(monthly_sales = sum(quantity), .groups = "drop")
    
    # Calculate the mean and standard deviation of monthly sales
    sales_mean <- mean(monthly_sales$monthly_sales, na.rm = TRUE)
    sales_vol <- sd(monthly_sales$monthly_sales, na.rm = TRUE)
    
    sales[sales$product_id == i, "volatility"] <- sales_vol/sales_mean # Relative volatility
  }
  return(sales)
}
sales_train <- sales_volatility(sales_train, 2020)
sales_val <- sales_volatility(sales_val, 2020)
sales_test <- sales_volatility(sales_test, 2017)

# Preprocessing of the supplier_country column
# Reduce the amount of needed dummies by converting the country names to their respective continent name
products_cleaned <- products_cleaned %>%
  mutate(supplier_country = case_when(
    supplier_country == "US" ~ "NA",
    supplier_country == "BE" ~ "EU",
    supplier_country == "CA" ~ "NA",
    supplier_country == "DE" ~ "EU",
    supplier_country == "DK" ~ "EU",
    supplier_country == "ES" ~ "EU",
    supplier_country == "FR" ~ "EU",
    supplier_country == "GB" ~ "EU",
    supplier_country == "NL" ~ "EU",
    supplier_country == "NO" ~ "EU",
    supplier_country == "PT" ~ "EU",
    supplier_country == "SE" ~ "EU",
    TRUE ~ supplier_country # Keep other values unchanged
  ))

cats <- categories(products_cleaned["supplier_country"]) # get categories and dummies 
dummies <- dummy(products_cleaned["supplier_country"], object = cats) 
dummies <- subset(dummies, select = -c(supplier_country_EU)) # Europe as a reference category
products_cleaned <- cbind(products_cleaned, dummies)
sales_train <- merge(sales_train, products_cleaned[, c("product_id", colnames(dummies))], by = "product_id", all.x = TRUE)
sales_val <- merge(sales_val, products_cleaned[, c("product_id", colnames(dummies))], by = "product_id", all.x = TRUE)
sales_test <- merge(sales_test, products_cleaned[, c("product_id", colnames(dummies))], by = "product_id", all.x = TRUE)


### Adding dummies for product category
# I would not include these in fitting, and try to engineer a sales level feature
cats <- categories(products_cleaned["product_category"]) # get categories and dummies 
dummies <- dummy(products_cleaned["product_category"], object = cats) 
dummies <- subset(dummies, select = -c(product_category_Clothes)) # Clothes as a reference category
products_cleaned <- cbind(products_cleaned, dummies)
sales_train <- merge(sales_train, products_cleaned[, c("product_id", colnames(dummies))], by = "product_id", all.x = TRUE)
sales_val <- merge(sales_val, products_cleaned[, c("product_id", colnames(dummies))], by = "product_id", all.x = TRUE)
sales_test <- merge(sales_test, products_cleaned[, c("product_id", colnames(dummies))], by = "product_id", all.x = TRUE)

# Adding dummies for product_line 
products_cleaned$product_line <- as.factor(products_cleaned$product_line)
dummies <- as.data.frame(model.matrix(~ product_line - 1, data = products_cleaned))
dummies <- dummies[, !(colnames(dummies) %in% "product_lineClothes & Shoes")] # Set Clothes & Shoes as reference category
products_cleaned <- cbind(products_cleaned, dummies) # Put the dummies into products 
sales_train <- merge(sales_train, products_cleaned[, c("product_id", colnames(dummies))], by = "product_id", all.x = TRUE)
sales_val <- merge(sales_val, products_cleaned[, c("product_id", colnames(dummies))], by = "product_id", all.x = TRUE)
sales_test <- merge(sales_test, products_cleaned[, c("product_id", colnames(dummies))], by = "product_id", all.x = TRUE)


# Function to calculate customer status ratios for each product and relevant years
calculate_status_ratios <- function(base_data, exclude_year) {
  # Filter the base data to exclude the target year
  filtered_data <- base_data %>%
    filter(order_year != exclude_year)
  
  # Group by product_id and customer_status to calculate total quantity sold
  sales_summary <- filtered_data %>%
    group_by(product_id, customer_status) %>%
    summarise(total_sales = sum(quantity, na.rm = TRUE), .groups = "drop")
  
  # Calculate the overall quantity sold per product
  total_product_sales <- sales_summary %>%
    group_by(product_id) %>%
    summarise(overall_sales = sum(total_sales, na.rm = TRUE), .groups = "drop")
  
  # Merge overall quantity back to the status summary
  sales_summary <- sales_summary %>%
    left_join(total_product_sales, by = "product_id")
  
  # Calculate the percentage of sales for each customer status
  status_ratios <- sales_summary %>%
    mutate(status_ratio = (total_sales / overall_sales) * 100) %>%
    dplyr::select(product_id, customer_status, status_ratio)
  
  # Pivot the data to create separate columns for each customer status
  status_ratios_wide <- status_ratios %>%
    pivot_wider(
      names_from = customer_status,
      values_from = status_ratio,
      names_prefix = "ratio_"
    ) %>%
    # Replace missing ratios with 0
    replace_na(list(
      ratio_GOLD = 0,
      ratio_PLATINUM = 0,
      ratio_SILVER = 0,
      ratio_NEW = 0
    )) %>%
    # Remove the NEW status column to use it as a reference category
    dplyr::select(-ratio_NEW)
  
  # Return the wide format with calculated ratios
  return(status_ratios_wide)
}

#Calculate ratios for train, validation, and test
status_ratios_train <- calculate_status_ratios(sales_2017_2020_cleaned, exclude_year = 2020)
status_ratios_val <- calculate_status_ratios(sales_2017_2020_cleaned, exclude_year = 2020)
status_ratios_test <- calculate_status_ratios(sales_2017_2020_cleaned, exclude_year = 2017)

#Merge the calculated ratios into the respective datasets
sales_train <- sales_train %>%
  left_join(status_ratios_train, by = "product_id")
sales_val <- sales_val %>%
  left_join(status_ratios_val, by = "product_id")
sales_test <- sales_test %>%
  left_join(status_ratios_test, by = "product_id")

#impute missing values
ratio_columns <- c("ratio_GOLD", "ratio_SILVER", "ratio_PLATINUM")
# Impute missing values for ratio columns
sales_train[ratio_columns] <- sales_train[ratio_columns] %>%
  replace_na(list(ratio_GOLD = 0, ratio_SILVER = 0, ratio_PLATINUM = 0))
sales_val[ratio_columns] <- sales_val[ratio_columns] %>%
  replace_na(list(ratio_GOLD = 0, ratio_SILVER = 0, ratio_PLATINUM = 0))
sales_test[ratio_columns] <- sales_test[ratio_columns] %>%
  replace_na(list(ratio_GOLD = 0, ratio_SILVER = 0, ratio_PLATINUM = 0))


# Product category feature 
# These features aim to grasp the trends for each product category 
# It looks at the relative sales share of product categories
category_change <- function(sales){ # Takes as input train, val or test set 
  # Merge product category into the sales data
  sales <- merge(sales, products_cleaned[c('product_id','product_category')], by = 'product_id')
  
  # Step 1: Calculate total sales across all categories for each lag
  total_sales_per_lag <- sales %>%
    summarise(
      total_sales_lag_3 = sum(lag_3, na.rm = TRUE),
      total_sales_lag_2 = sum(lag_2, na.rm = TRUE),
      total_sales_lag_1 = sum(lag_1, na.rm = TRUE)
    )
  
  # Step 2: Group by product_category and calculate total sales for each lag
  # The total sales for each product_category for each lag (year)
  category_sales <- sales %>%
    group_by(product_category) %>%
    summarise(
      category_sales_lag_3 = sum(lag_3, na.rm = TRUE),
      category_sales_lag_2 = sum(lag_2, na.rm = TRUE),
      category_sales_lag_1 = sum(lag_1, na.rm = TRUE)
    )
  
  # Step 3: Calculate the relative share of sales for each category
  # So for example, in 2018 product category x accounted for 15% of the total sales
  category_sales <- category_sales %>%
    mutate(
      share_lag_3 = category_sales_lag_3 / total_sales_per_lag$total_sales_lag_3 * 100, # This is the sales share for each product category. 
      share_lag_2 = category_sales_lag_2 / total_sales_per_lag$total_sales_lag_2 * 100,
      share_lag_1 = category_sales_lag_1 / total_sales_per_lag$total_sales_lag_1 * 100
    )
  
  # Step 4: Calculate percentage change in relative share between lags
  category_sales <- category_sales %>%
    mutate(
      pct_change_lag_2 = (share_lag_2 - share_lag_3) / share_lag_3 * 100,
      pct_change_lag_1 = (share_lag_1 - share_lag_2) / share_lag_2 * 100
    )
  
  # Step 5: Merge the percentage change back into the original sales data
  sales <- sales %>%
    left_join(category_sales %>% dplyr::select(product_category, pct_change_lag_2, pct_change_lag_1), 
              by = "product_category")
  
  return(sales)
}

# Apply the function to sales_train, sales_val and sales_test datasets
sales_train <- category_change(sales_train)
sales_val <- category_change(sales_val)
sales_test <- category_change(sales_test)
head(sales_train)


# Code to transform sales_2020
# First calculate the mean growth_rate from 2017-2019
growth <- (sum(sales_base$lag_2)/sum(sales_base$lag_3) + sum(sales_base$lag_1)/sum(sales_base$lag_2))/2 
growth_2 <- (growth -1)*0.5 + 1 # This is the growth rate of the previous years divided by 2. 
projection_2020_train <- sum(sales_train$lag_1)*growth_2 # This is the projected sales of 2020 (of the train set) if the average growth (divided by 2) was followed 
projection_2020_val <- sum(sales_val$lag_1)*growth_2  # Projected sales of 2020 (val set) if average growth (divided by 2) was followed
# Now transform sales_y from train and val, and lag_1 from test set 
sales_train$sales_y <- sales_train$sales_y * (projection_2020_train/sum(sales_train$sales_y)) # This multiplies every product sale with a constant
sales_val$sales_y <- sales_val$sales_y * (projection_2020_val/sum(sales_val$sales_y)) # This multiplies every product sale with a constant
sales_test$lag_1 <- sales_test$lag_1 *((projection_2020_train + projection_2020_val)/sum(sales_test$lag_1))

# Check if sum sales from lag_1 (2020) test is equal to sum sales_y from train and val 
sum(sales_train$sales_y) # 50768.7
sum(sales_val$sales_y) # 11227.45
sum(sales_test$lag_1) # 61996.14

# Impute missing values using sales_train 
# These will be df's we will impute
train <- sales_train  
val <- sales_val 
test <- sales_test

# Imputing missing values (Can maybe make a function to do this more efficiently)
# volatility
train$volatility[is.na(train$volatility)] <- mean(train$volatility, na.rm = TRUE) # Always impute using info from train
val$volatility[is.na(val$volatility)] <- mean(train$volatility, na.rm = TRUE)
sales_test$volatility[is.na(sales_test$volatility)] <- mean(train$volatility, na.rm = TRUE)

# Average_delivery 
train$average_delivery[is.na(train$average_delivery)] <- mean(train$average_delivery, na.rm = TRUE) # Always impute using info from train
val$average_delivery[is.na(val$average_delivery)] <- mean(train$average_delivery, na.rm = TRUE)
sales_test$average_delivery[is.na(sales_test$average_delivery)] <- mean(train$average_delivery, na.rm = TRUE)

# recency 
train$recency[is.na(train$recency)] <- mean(train$recency, na.rm = TRUE) # Always impute using info from train
val$recency[is.na(val$recency)] <- mean(train$recency, na.rm = TRUE)
sales_test$recency[is.na(sales_test$recency)] <- mean(train$recency, na.rm = TRUE)

# check NA
colSums(is.na(train))
colSums(is.na(val))
colSums(is.na(sales_test)) 

# Scale continuos features
set.seed(1)
scale_cols <- c('lag_3','lag_2','lag_1','recency','volatility','average_delivery','pct_change_lag_1','pct_change_lag_2',
                'ratio_GOLD', 'ratio_SILVER','ratio_PLATINUM')

# Log transformation for ratio_PLATINUM (shifting to handle non-positive values)
log_transform <- function(x) {
  min_val <- min(x, na.rm = TRUE)
  if (min_val <= 0) {
    x <- log(x - min_val + 1)  # Shift values to be non-negative before log transformation
  } else {
    x <- log(x)
  }
  return(x)
}

# Apply log transformation to ratio_PLATINUM in all datasets
train$ratio_PLATINUM <- log_transform(train$ratio_PLATINUM)
val$ratio_PLATINUM <- log_transform(val$ratio_PLATINUM)
sales_test$ratio_PLATINUM <- log_transform(sales_test$ratio_PLATINUM)

mean_train <- colMeans(train[,scale_cols])
sd_train <- sapply(train[,scale_cols],sd)

train_scale <- train
val_scale <- val
test_scale <- sales_test

train_scale[,scale_cols] <- scale(train[,scale_cols],center = TRUE, scale = TRUE)
test_scale[,scale_cols] <- scale(sales_test[,scale_cols], center = mean_train, scale = sd_train)
val_scale[,scale_cols] <- scale(val[,scale_cols], center = mean_train, scale = sd_train)

# Remove the product_category column
train_scale <- train_scale %>% dplyr::select(-product_category)
val_scale <- val_scale %>% dplyr::select(-product_category)
test_scale <- test_scale %>% dplyr::select(-product_category)

# SAVE DATA
write_csv(train_scale, "./data/gold/sales_train_normalized.csv")
write_csv(val_scale, "./data/gold/sales_val_normalized.csv")
write_csv(test_scale, "./data/gold/sales_test_normalized.csv")

