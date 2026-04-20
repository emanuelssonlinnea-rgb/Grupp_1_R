library(tidyverse)

source("data_städning_förberedesle.R")


# Fråga 1: Total försäljning inklusive andelar
sales_category <- sales_clean %>%
  group_by(product_category) %>%
  summarise(
    total_forsaljning = sum(order_value, na.rm = TRUE),
    medel_ordervarde = mean(order_value, na.rm = TRUE),
    median_ordervarde = median(order_value, na.rm = TRUE),
    antal = n(),
    .groups = "drop"
  ) %>%
  mutate(
    andel_forsaljning = total_forsaljning / sum(total_forsaljning) 
  ) %>%
  arrange(desc(total_forsaljning))

  print(sales_category, width = Inf)


# Fråga 2: Leveranstid för returnerade/icke returnerade beställningar
shipping_return <- sales_clean %>%
  group_by(returned) %>%
  summarise(
    antal = n(),
    medel_leveranstid = mean(shipping_days, na.rm = TRUE),
    median_leveranstid = median(shipping_days, na.rm = TRUE),
    sd_leveranstid = sd(shipping_days, na.rm = TRUE),
    .groups = "drop"
  )
  
print(shipping_return)

# t-test för att undersöka om det finns skillnad i genomsnittlig leveranstid.
# mellan beställningar som har returnerats och de som inte har returnerats. 
shipping_return_test <- t.test(shipping_days ~ returned, data = sales_clean)

shipping_return_test$p.value # Resultat: Signifikant skillnad p<0.05
shipping_return_test$conf.int # Resultat: Innehåller ej 0 -> statistisk signifikant. Negativ skillnad (-0.7 och -0.15)
shipping_return_test$estimate # Resultat: Beställningar som inte returneras (FALSE) har i genomsnitt kortare lev.tid än de som returneras (TRUE)

# Fråga 3 Hur skiljer sig kundtyper bland olika kundsegment? (Sorterad efter segment och total försäljning
tabell_segment_typ <- sales_clean %>%
  group_by(customer_segment, customer_type) %>%
  summarise(
    total_forsaljning = sum(order_value, na.rm = TRUE),
    medel_ordervarde = mean(order_value, na.rm = TRUE),
    antal_kop = n(),
    .groups = "drop"
  ) %>%
  group_by(customer_segment) %>%
  mutate(
    segment_total = sum(total_forsaljning),
    andel_inom_segment = total_forsaljning / segment_total
  ) %>%
  ungroup() %>%
  mutate(
    andel_total = total_forsaljning / sum(total_forsaljning)
  ) %>%
  arrange(desc(segment_total), desc(total_forsaljning)) %>%
  mutate(
    customer_segment = factor(customer_segment, levels = unique(customer_segment))
  )

