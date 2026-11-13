# Load libraries
library(readr)
library(dplyr)

# Set directory
# setwd("C:\\Users\\Arthu\\OneDrive - UGent\\Bureaublad\\Machine learning")

# LOAD DATA
sample_submission <- read.csv('data/bronze/sample_submission.csv')
sales_2017_2020 <- read.csv("data/bronze/sales_2017_2020.csv")
products <- read.csv("data/bronze/products.csv")

# check data classes
sapply(sales_2017_2020,class)
sapply(products,class)

# Data split
# We use a 80-20 train-val split
set.seed(1)
unique_productid <- unique(sales_2017_2020$product_id)
train.idx <- sample(1:length(unique_productid),size = 0.80*length(unique_productid))

# The products in the train, val and test sets
train_id <- unique_productid[train.idx] 
val_id <- unique_productid[-train.idx] 
test_id <- sample_submission$product_id # These are all the product ids from the test set. 

# This removes products in the df that are not in sample_submission
products <- products[products$product_id %in% sample_submission$product_id,] # This removes products in the df that are not in sample_submission

# DATA CLEAN UP FOR EACH COLUMN IN PRODUCTS
# clean the country column
products$supplier_country = ifelse(products$supplier_country == "country not enclosed", NA ,products$supplier_country) # if not enclosed, change to NA
products$supplier_country <- ifelse(is.na(products$supplier_country), NA, sub(".*\\|\\s*", "", products$supplier_country)) # if not NA, only retain the last 2 letters

# Impute missing values in supplier_country using the first non-missing value within the same supplier_id group
products <- products %>% 
  group_by(supplier_id) %>% 
  mutate(supplier_country = ifelse(is.na(supplier_country), 
                                   first(na.omit(supplier_country)), 
                                   supplier_country)) %>% 
  ungroup()

products = data.frame(products)

# clean product_name column
# set missing names to NA
products$product_name <- replace(products$product_name, products$product_name == "", NA) 

# Clean product_category column
products$product_category[products$product_category == ""] <- NA # replace empty values with NA
# Impute product_category based on the first non-missing value within each product_group
products <- products %>%
  group_by(product_group) %>%
  mutate(product_category = ifelse(is.na(product_category), 
                                   first(na.omit(product_category)), 
                                   product_category)) %>%
  ungroup()
sum(is.na(products$product_category)) # 0 NA's left

# Clean product_line column
# Clean product_line column and fill missing values by product_category
products <- products %>%
  mutate(
    # Remove text within square brackets from product_line
    product_line = gsub("\\[.*?\\]", "", product_line),  # Remove text in square brackets
    
    # Replace "NA" strings or empty strings with NA
    product_line = ifelse(product_line == "NA" | product_line == "", NA_character_, product_line)
  ) %>%
  group_by(product_category) %>%  # Group by product_category
  mutate(
    # Fill missing values in product_line with the first non-NA value in the group
    product_line = ifelse(is.na(product_line), first(na.omit(product_line)), product_line)
  ) %>%
  ungroup() 
sum(is.na(products$product_line)) # 0 NA's left

# Clean supplier_name column
# Clean and impute supplier_name column
products <- products %>%
  mutate(
    # Replace missing or "NO NAME" in supplier_name with NA
    supplier_name = ifelse(is.na(supplier_name) | supplier_name == "NO NAME", NA_character_, supplier_name)
  ) %>%
  group_by(supplier_id) %>%  # Group by supplier_id
  mutate(
    # Impute missing supplier_name by looking at other products with the same supplier_id
    supplier_name = ifelse(is.na(supplier_name), first(na.omit(supplier_name)), supplier_name)
  ) %>%
  ungroup()
sum(is.na(products$supplier_name)) # 0 NA's left

# DATA CLEAN UP FOR EACH COLUMN IN SALES_2017_2020

# Clean order_date column
# transform order_date from string in this format: "(dd)-----?(mm); year is (yyyy)" to a Date object
convert_to_date_order <- function(date_strings) {
  idx_regexec <- regexec("(\\d{2})-----?(\\d{2}); year is (\\d{4})", date_strings)
  match_dates <- regmatches(date_strings, idx_regexec)
  day <- sapply(match_dates, `[`, 2)
  month <- sapply(match_dates, `[`, 3)
  year <- sapply(match_dates, `[`, 4)
  formatted_dates <- as.Date(paste(year, month, day, sep = "-"))
  return(formatted_dates)
}
sales_2017_2020$order_date <- convert_to_date_order(sales_2017_2020$order_date)

# create an order_year column 
sales_2017_2020$order_year <- as.integer(substr(sales_2017_2020$order_date,1,4))

# Clean customer_status column
idx_regexec_try <- regexec("\\[status::](\\w+)",sales_2017_2020$customer_status) # This removes status 
match_try <- regmatches(sales_2017_2020$customer_status,idx_regexec_try)
sales_2017_2020$customer_status <- sapply(match_try, `[`, 2) # This selects Silver, Gold or platinum
sales_2017_2020$customer_status <- toupper(sales_2017_2020$customer_status) # convert to upper to have concistency

# New imputations customer_status 
impute_customer_status <- function(id) { # train_id or val_id
  # Filter the relevant part of sales_2017_2020 for processing
  sales_subset <- sales_2017_2020 %>%
    filter(order_year < 2020, product_id %in% id) # Subset for products in id and data before 2020
  
  # Identify customers with NA customer_status
  customers_with_na_status <- sales_subset %>%
    filter(is.na(customer_status)) %>%
    distinct(customer_id)
  
  # Loop through customers with NA status
  # If the first customer status of a customer is NA I assign NEW, else I look at the previous value of customer status for that customer. 
  for (i in customers_with_na_status$customer_id) {
    # Identify rows for the current customer
    rows <- which(sales_subset$customer_id == i)
    for (r in 1:length(rows)) {
      if (is.na(sales_subset$customer_status[rows[r]])) {
        if (r == 1) {
          # Assign 'NEW' for the first record
          sales_subset$customer_status[rows[r]] <- 'NEW'
        } else {
          # Propagate the previous row's status
          sales_subset$customer_status[rows[r]] <- sales_subset$customer_status[rows[r - 1]]
        }
      }
    }
  }
  
  # Write the imputed data back into the original sales_2017_2020
  sales_2017_2020 <- sales_2017_2020 %>%
    left_join(sales_subset %>% select(order_id, customer_status), by = "order_id") %>%
    mutate(customer_status = coalesce(customer_status.y, customer_status.x)) %>%
    select(-customer_status.x, -customer_status.y)
  
  return(sales_2017_2020)
}

sales_2017_2020 <- impute_customer_status(train_id)
sales_2017_2020 <- impute_customer_status(val_id)

# There are still some missing values left, all from order_year 2020
# For these we impute using the mode from the train_set 
sales_no_2020 <- sales_2017_2020 %>% 
  filter(order_year < 2020, product_id %in% train_id) # Use only data from train_id and don't use data from 2020. 

overall_mode <- names(sort(table(sales_no_2020$customer_status), decreasing = TRUE))[1]
sales_2017_2020$customer_status[is.na(sales_2017_2020$customer_status)] <- overall_mode # overall_mode is silver


# Clean delivery_date column
# transform delivery_date from string to Date object
convert_to_date_delivery <- function(date_strings){
  idx_regexec <- regexec("(\\d{2})----month: (\\w+), year: (\\d{4})",date_strings)
  match_dates <- regmatches(date_strings, idx_regexec)
  day <- sapply(match_dates, `[`, 2)
  month <- sapply(match_dates, `[`, 3)
  year <- sapply(match_dates, `[`, 4)
  month <- match(month, month.abb) # Changes Jan to 01 for example
  formatted_dates <- as.Date(paste(year, month, day, sep = "-"))
  return(formatted_dates)
}
sales_2017_2020$delivery_date <- convert_to_date_delivery(sales_2017_2020$delivery_date)

# Clean cost_price_unit and total_retail_price columns
# Convert Price columns to double, this removes the dollar sign 
convert_to_double <- function(prices){
  idx_regexec <- regexec("\\$\\s*(.*)",prices) # Removes the dollar sign 
  match_price <- regmatches(prices, idx_regexec)
  price <- as.double(as.numeric(sapply(match_price,`[`, 2)))
  return(price)
}
sales_2017_2020$cost_price_unit <- convert_to_double(sales_2017_2020$cost_price_unit)
sales_2017_2020$total_retail_price <- convert_to_double(sales_2017_2020$total_retail_price)



# New imputations
impute_missing_retail_price <- function(sales, id, price_col) { # We impute using info from training set 
  # Ensure the price column is numeric and calculate per-unit price
  sales[[price_col]] <- as.numeric(sales[[price_col]]) / sales[['quantity']] # First set it to price per unit
  
  # Step 1: Filter data on train or val 
  data <- sales %>% filter(product_id %in% id & order_year != 2020) # Don't use info from 2020 to avoid data leakage
  
  # Step 2: Compute mean price per product_id from the training set
  product_mean_prices <- data %>%
    group_by(product_id) %>%
    summarize(mean_price = mean(get(price_col), na.rm = TRUE), .groups = "drop")
  
  # Step 3: Impute missing values in the price column using the mean from the training set
  sales <- sales %>%
    left_join(product_mean_prices, by = "product_id") %>%
    mutate(
      imputed_price = ifelse(is.na(get(price_col)), mean_price, get(price_col)),
      total_retail_price = imputed_price * quantity # Calculate total price
    ) %>%
    select(-mean_price, -imputed_price) # Drop helper columns if necessary
  
  return(sales)
}
sales_2017_2020 <- impute_missing_retail_price(sales_2017_2020, train_id, 'total_retail_price')  # Impute train prices
sales_2017_2020 <- impute_missing_retail_price(sales_2017_2020, val_id, 'total_retail_price') # Impute val prices 
sales_2017_2020$retail_price <- sales_2017_2020$total_retail_price/sales_2017_2020$quantity

# Impute remaining few NA values with mean from train set 

train_mean_price <- sales_2017_2020 %>%
  filter(product_id %in% train_id) %>%  # Use only train set data
  summarize(mean_price = mean(retail_price, na.rm = TRUE)) %>%
  pull(mean_price)

sales_2017_2020$retail_price[is.na(sales_2017_2020$retail_price)] <- train_mean_price
sales_2017_2020$total_retail_price <- sales_2017_2020$retail_price*sales_2017_2020$quantity


# cost_price_unit 
# Replace not known with NA
sales_2017_2020$cost_price_unit <- replace(sales_2017_2020$cost_price_unit, sales_2017_2020$cost_price_unit == "not known", NA)

impute_missing_cost_price <- function(id, price) { # id are the product'ids from either train or val set
  sales <- sales_2017_2020
  # Loop through each unique product ID
  for (i in id) { # Only take products into account from either train or val 
    product_prices <- sales[sales$product_id == i & sales$order_year != 2020, ][[price]] # Don't use data from 2020
    if (all(is.na(product_prices))) {
      next  # If all the prices of this product id, the price remains NA
    }
    # Find the mean price for the current product ID (ignoring NA values)
    impute_value <- mean(sales[sales$product_id == i, ][[price]], na.rm = TRUE)
    # Replace NA prices with the imputed value
    sales[sales$product_id == i & is.na(sales[[price]]), price] <- impute_value
  }
  return(sales[[price]])
}

# Do the imputations seperately for train and val 
sales_2017_2020$cost_price_unit <- impute_missing_cost_price(train_id, 'cost_price_unit') 
sales_2017_2020$cost_price_unit <- impute_missing_cost_price(val_id, 'cost_price_unit') 

# Impute the remaining few missing values 
# Mean cost_price_unit
train_mean_cost_price <- sales_2017_2020 %>%
  filter(product_id %in% train_id) %>%  # Use only train set data
  summarize(mean_price = mean(cost_price_unit, na.rm = TRUE)) %>%
  pull(mean_price)

sales_2017_2020$cost_price_unit[is.na(sales_2017_2020$cost_price_unit)] <- train_mean_cost_price

# SAVE DATA 
write_csv(sales_2017_2020, "./data/silver/sales_2017_2020_cleaned.csv") 
write_csv(products, "./data/silver/products_cleaned.csv")