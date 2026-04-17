
### DATAFÖRSTÅELSE ###

library(tidyverse)
library(scales)



raw_data <- read_csv("ecommerce_orders.csv")



# visa datasetets storlek 
glimpse(raw_data)

# beskriva vilka typer av variabler som finns
summary(raw_data)

# identifiera saknade värden
colSums(is.na(raw_data))


# Snabb överblick av olika delar av datan
# försäljning bland olika kundsegment

raw_data %>%
  group_by(customer_segment) %>%
  summarise(
    avg_spend    = mean(quantity * unit_price, na.rm = TRUE),
    median_spend = median(quantity * unit_price, na.rm = TRUE),
    sd_spend     = sd(quantity * unit_price, na.rm = TRUE),
    n            = n(),
    .groups = "drop"
  )

# Kund överblick -> segments & types ---------------------------------------------------

plot_data <- raw_data %>%
  count(customer_segment, customer_type) %>%
  group_by(customer_segment) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(plot_data, aes(
  x = customer_segment,
  y = prop,
  fill = customer_type
)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = percent(prop)),
    position = position_dodge(0.9),
    vjust = -0.5,
    size = 2
  ) +
  scale_y_continuous(
    labels = percent,
    expand = expansion(mult = c(0, 0.15))          
  )+ 
  scale_fill_brewer(palette = "Dark2")+
  labs(title = "Kundsegment överblick", 
       y = "Kundtyp, %", x="Kundsegment")  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(b = 20)
  )



# antal kunder per region----------------------------------------------------------------------------

ggplot(raw_data, aes(x = region, fill = customer_type)) +
  geom_bar() + 
  scale_fill_brewer(palette = "Dark2")+
  labs(
    title = "Antal kunder per region",
    x = "Region",
    y = "Antal kunder"
  )

#Samband mellan betalningsmetod och retur av varor ---------------------------------------------------

unique(raw_data$payment_method) 
#cleaning up payment method so we can look at the covariance between the two unimpeded

raw_data <- raw_data %>%
  mutate(
    payment_method = str_trim(payment_method),
    payment_method = str_to_title(payment_method)
  )


plot_data <- raw_data %>%
  count(payment_method, returned) %>%
  group_by(payment_method) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()


ggplot(plot_data, aes(
  x = payment_method,
  y = prop,
  fill = returned
)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = scales::percent(prop, accuracy = 0.1)),
    position = position_fill(vjust = 0.5),
    size = 2
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Returer enligt betalningsmetod",
    y = "Returproportion",
    x = "Betalningsmetod"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(b = 20)
  )

# Samband mellan kunder och returnerade varor ---------------------------------------------------

plot_data <- raw_data %>%
  count(customer_segment, returned) %>%
  group_by(customer_segment) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(plot_data, aes(
  x = customer_segment,
  y = prop,
  fill = returned
)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = scales::percent(prop, accuracy = 0.1)),
    position = position_fill(vjust = 0.5),
    size = 2
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Returer per kundsegment",
    y = "Returproportion",
    x = "Kundsegment"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(b = 20)
  )

# beskriva kort vilka delar av datan som verkar viktigast för er analys 
# baserat på dem frågeställningarna vi valt: 
# Vilka produktkategorier verkar driva högst försäljning?
# Finns det tecken på att längre leveranstid hänger ihop med fler returer?
# egen frågeställning: Hur skiljer sig kundtyper bland olika kundsegment?
# viktigaste delar av datan är då: 
# customer_segment, customer_type, product_category,  
# quantity, unit_price, shipping_days, returned
# mindre viktiga men intressant för jämförelse: region, city

