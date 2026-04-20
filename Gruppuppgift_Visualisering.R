# Visualisering: 
library(tidyverse)

source("Grupp_1_Dataförståelse.R")
source("data_städning_förberedesle.R")
source("Statistiska_sammanfattningar.R")

# FÖRSÄLJNING PER PRODUKTKATEGORI:

ggplot(sales_category, aes(
  x = reorder(product_category, total_forsaljning),
  y = total_forsaljning
)) + 
  geom_col(fill = "lightgreen") + 
  coord_flip() +                                # horisontellt
  scale_y_continuous(labels = scales::comma) +  # formaterar siffrorna mer läsbart med comma
  labs(
    title = "Total försäljning per produktkategori",
    x = "Produktkategori",
    y = "Total försäljning"
  )

# Electronics står för största andel av den totala försäljningen, alltså viktigaste intäktsdrivare.
# Beauty och Fashion betydligt mindre, kan tyda på lägre efterfrågan och stor potential inom dessa.





# LEVERANSTID VS RETURER:

ggplot(sales_clean, aes(
  x = returned,
  y = shipping_days,
  fill = returned
)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Set2") + 
  labs(
    title = "Leveranstid för returnerade vs icke returnerade beställningar",
    x = "Returnerad (TRUE = ja, FALSE = nej)",
    y = "Leveranstid (dagar)"
  )

# Skillnaden är inte jättestor visuellt
# Returnerade beställningar har i genomsnitt lite längre leveranstid, vilket tyder på ett samband mellan leveranstid och returer
# Spridning i båda grupperna relativt liknande, så variationen i leveranstid är jämförbar oavsett returstatus





# KUNDSEGMENT & KUNDTYP:

ggplot(tabell_segment_typ, aes(
  x = customer_segment,
  y = andel_inom_segment,
  fill = customer_type
)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Fördelning av kundtyper inom kundsegment",
    x = "Kundsegment",
    y = "Andel",
    fill = "Kundtyp"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Kundtyp Returning dominerar i samtliga kundsegment (många återkommande kunder)
# Segment Consumer har större andel av VIP-kunder än dem andra 
# Small Business saknar nästan helt VIP-kunder
# Kundsegmenten har olika kundstruktur och beteenden
# Andelen nya kunder (New) är relativt likartad mellan segmenten, dvs. jämnt fördelad
# Skillnader mellan segmenten är viktiga för marknadsföring, tex. Consumer kan rikta sig mot VIP-kunder som verkar lojala just där






# TESTAR ÄVEN RABATT VS ORDERVÄRDE (BONUS)

ggplot(sales_clean, aes(
  x = discount_group,
  y = order_value
)) +
  geom_boxplot(fill = "orange") +
  labs(
    title = "Ordervärde per rabattnivå",
    x = "Rabattnivå",
    y = "Ordervärde"
  )

# Denna extra figur visar att ordervärdet varierar inom båda rabattnivåerna
# Medianvärdet är liknande mellan "Ingen" och "Låg rabatt" så lägre rabatter inte har någon tydlig påverkan på ordervärde
# Det finns många höga outliers, särskilt i gruppen "låg rabatt"- kan indikera på att vissa kunder gör stora köp oavsett rabattnivån
# Andra faktorer såsom kundtyp eller produktkategori också kan påverka ordervärde
