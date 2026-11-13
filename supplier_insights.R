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
              select(product_id, supplier_name, supplier_country),
            by = "product_id") %>%
  mutate(
    total_profit = (retail_price - cost_price_unit) * sales_volume_2021
  )

# Supplier Insights
volume_per_supplier <- final_df %>%
  group_by(supplier_name) %>%
  summarize(total_volume = sum(sales_volume_2021, na.rm = TRUE))

revenue_per_supplier <- final_df %>%
  group_by(supplier_name) %>%
  summarize(total_revenue = sum(retail_price * sales_volume_2021, na.rm = TRUE))

profit_per_supplier <- final_df %>%
  group_by(supplier_name) %>%
  summarize(total_profit = sum(total_profit, na.rm = TRUE))

# Visualizations
ggplot(volume_per_supplier, aes(x = reorder(supplier_name, total_volume), y = total_volume)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Total Sales Volume per Supplier", x = "Supplier Name", y = "Total Volume") +
  theme_minimal()

ggplot(revenue_per_supplier, aes(x = reorder(supplier_name, total_revenue), y = total_revenue)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Total Revenue per Supplier", x = "Supplier Name", y = "Total Revenue (€)") +
  theme_minimal()
