source("Grupp_1_Dataförståelse.R")


# Välj relevanta kolumner
relevant_data <- raw_data[, c(
  "product_category",
  "city",
  "quantity",
  "unit_price",
  "discount_pct",
  "shipping_days",
  "returned",
  "customer_segment",
  "customer_type",
  "order_date"
)]

# Rensa och förbered data
sales_clean <- relevant_data %>%
  mutate(
    # Textkolumner
    product_category = str_to_title(str_trim(product_category)),
    customer_segment = str_trim(customer_segment),
    customer_type = str_trim(customer_type),
    city = str_to_title(str_trim(city)),
    city = str_replace_all(city, c("Goteborg" = "Göteborg", "Malmo" = "Malmö")),
    
    # Returkolumn
    returned = str_to_lower(str_trim(returned)),
    returned = if_else(returned == "yes", TRUE, FALSE),
    
    # Datum
    order_date = as.Date(order_date),
    
    # Numeriska kolumner
    discount_pct = as.numeric(discount_pct),
    shipping_days = as.numeric(shipping_days),
    
    # Hantera missing rabatt
    discount_pct = if_else(is.na(discount_pct), 0, discount_pct),
    
    # Hantera missing quantity
    quantity = if_else(
      is.na(quantity),
      as.integer(round(median(quantity, na.rm = TRUE))),
      quantity
    ),
    quantity = as.integer(quantity)
  )

# Hantera missing city
sales_clean <- sales_clean %>%
  mutate(city = if_else(is.na(city), "Okänd", city))

# Hantera missing shipping_days
median_shipping <- median(sales_clean$shipping_days, na.rm = TRUE)
sales_clean <- sales_clean %>%
  mutate(shipping_days = if_else(is.na(shipping_days), median_shipping, shipping_days))

# Skapa order_value, leveranskategori och rabattgrupp
sales_clean <- sales_clean %>%
  mutate(
    order_value = quantity * unit_price * (1 - discount_pct),
    shipping_group = case_when(
      shipping_days <= 3 ~ "Snabb",
      shipping_days <= 7 ~ "Medel",
      shipping_days > 7  ~ "Lång"
    ),
    discount_group = case_when(
      discount_pct == 0 ~ "Ingen",
      discount_pct <= 10 ~ "Låg",
      discount_pct <= 20 ~ "Medel",
      discount_pct > 20 ~ "Hög"
    )
  )

# Kontrollera att inga NA finns kvar
colSums(is.na(sales_clean))