# Libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load Data
pred_2021_df <- read.csv("./data/results/svm_lin_26nov.csv")
sales_2017_2020 <- read.csv("./data/silver/sales_2017_2020_cleaned.csv")
products <- read.csv("./data/silver/products_cleaned.csv")

# Merge DataFrames
sales_latest <- sales_2017_2020 %>%
  group_by(product_id) %>%
  filter(row_number(desc(order_date)) == 1) %>%
  select(product_id, cost_price_unit, retail_price)

final_df <- pred_2021_df %>%
  left_join(sales_latest, by = "product_id") %>%
  left_join(products %>%
              select(product_id, product_name, product_category),
            by = "product_id") %>%
  mutate(
    profit_margin = ((retail_price - cost_price_unit) / retail_price) * 100,
    total_profit = (retail_price - cost_price_unit) * sales_volume_2021
  )

# Product Insights
top_products_by_margin <- final_df %>%
  arrange(desc(profit_margin)) %>%
  slice_head(n = 10)
top_products_by_margin

top_products_by_profit <- final_df %>%
  arrange(desc(total_profit)) %>%
  slice_head(n = 10)
top_products_by_profit

# Visualizations
ggplot(top_products_by_profit, aes(x = reorder(product_name, total_profit), y = total_profit)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  coord_flip() +
  labs(title = "Top 10 Products by Total Profit", x = "Product Name", y = "Total Profit (€)") +
  theme_minimal()

ggplot(final_df, aes(x = profit_margin)) +
  geom_histogram(binwidth = 5, fill = "seagreen", color = "black") +
  labs(title = "Distribution of Profit Margins", x = "Profit Margin (%)", y = "Frequency") +
  theme_minimal()
