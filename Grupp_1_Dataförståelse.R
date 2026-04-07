
### DATAFÖRSTÅELSE ###

library(tidyverse)
library(scales)
install.packages("usethis")
usethis::use_git_config(user.name="emanuelssonlinnea-rgb", user.email="emanuelsson.linnea@gmail.com")


raw_data <- read_csv("ecommerce_orders.csv")



# visa datasetets storlek 
glimpse(raw_data)

# beskriva vilka typer av variabler som finns
summary(raw_data)

# identifiera saknade värden
colSums(is.na(raw_data))


# Snabb överblick av olika delar av datan

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
  labs(title = "Customer segment overview", 
       y = "Customer type, %", x="Customer segment")  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(b = 20)
  )


#Samband mellan betalningsmetod och retur av varor ---------------------------------------------------

unique(raw_data$payment_method) 
#cleaning up payment method so we can look at the covariance between the two unimpeded
#Earlier payment method = "Card", NA, "PayPal", "Invoice", "Gift Card", "Swish", "card", "Paypal"   
#Now = "Card",NA,"Paypal","Invoice","Gift Card","Swish"

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
    title = "Return rate by payment method",
    y = "Return proportion",
    x = "Payment Method"
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
    title = "Return rate by customer segment",
    y = "Return proportion",
    x = "Customer segment"
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
# customer_segment, customer_type, product_category, product_subcategory, 
# quantity, unit_price, shipping_days, returned
# mindre viktiga men intressant för jämförelse: region, city

