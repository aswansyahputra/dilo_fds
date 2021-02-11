# Load packages -----------------------------------------------------------

library(readxl)
library(tidyverse)
library(cowplot)
library(arules)
library(arulesViz)

# Pre-processing data -----------------------------------------------------

invoice_raw <- 
  read_excel("data-raw/online-retail.xlsx",
             .name_repair = janitor::make_clean_names)

glimpse(invoice_raw)

invoice <- 
  invoice_raw %>% 
  drop_na() %>% 
  filter(!str_detect(description, "\\?+")) %>% 
  filter(!str_detect(description, "!+"))

glimpse(invoice)

invoice %>% 
  select(invoice = invoice_no, 
         item = description) %>% 
  write_csv("data-raw/invoice.csv")

# Data exploration --------------------------------------------------------

invoice %>% 
  count(description, sort = TRUE) %>% 
  top_n(15, wt = n) %>% 
  mutate(description = fct_reorder(description, n)) %>% 
  ggplot(aes(description, n)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    x = NULL,
    y = NULL,
    title = "Top 15 items purchased via online"
  ) +
  coord_flip() +
  theme_minimal_vgrid(font_size = 12, font_family = "Roboto")

invoice %>% 
  filter(country != "United Kingdom") %>% 
  count(country, sort = TRUE) %>% 
  mutate(country = fct_reorder(country, n)) %>% 
  top_n(10, wt = n) %>% 
  ggplot(aes(country, n)) +
  geom_col(fill = "firebrick") +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    x = NULL,
    y = "Number of items",
    title = "Top 10 country outside UK to purchase item via online"
  ) +
  coord_flip() +
  theme_minimal_vgrid(font_size = 12, font_family = "Roboto")

invoice %>% 
  mutate(
    hour = lubridate::hour(invoice_date)
  ) %>% 
  ggplot(aes(x = hour)) +
  geom_histogram(fill = "seagreen", bins = 15) +
  labs(
    x = "Time (hour)",
    y = "Number of transaction",
    title = "What time do people often purchase online?"
  ) +
  theme_minimal_grid(font_size = 12, font_family = "Roboto")

invoice %>% 
  count(
    day = lubridate::wday(invoice_date, label = TRUE),
    month = lubridate::month(invoice_date, label = TRUE),
    wt = unit_price,
    name = "price"
  ) %>% 
  ggplot(aes(day, month, fill = price)) +
  geom_tile() +
  scale_fill_viridis_c(option = "inferno", direction = -1, labels = scales::label_dollar()) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Values of transaction by time"
  ) +
  coord_equal() +
  theme_minimal_grid()

# Import transaction data (single-format) ---------------------------------

transaction <-
  read.transactions(
    "data-raw/invoice.csv",
    format = "single",
    header = TRUE,
    sep = ",",
    cols = c("invoice", "item"),
    quote = ""
  )

transaction

summary(transaction)

# Explore transaction data ------------------------------------------------

inspect(transaction[1:10])

itemFrequencyPlot(transaction,
                  topN = 15,
                  type = "absolute",
                  main = "Absolute Item Frequency Plot")

# Create association rules ------------------------------------------------

ar_transaction <-
  apriori(transaction, parameter = list(
    supp = 0.001,
    conf = 0.8,
    maxlen = 10
  ))

summary(ar_transaction)

inspect(ar_transaction[1:10])

ar_transaction <- sort(ar_transaction, by = "lift", decreasing = TRUE)

inspect(ar_transaction[1:10])

# Plot rules --------------------------------------------------------------

plot(ar_transaction[1:20], method = "graph",  engine = "htmlwidget")
