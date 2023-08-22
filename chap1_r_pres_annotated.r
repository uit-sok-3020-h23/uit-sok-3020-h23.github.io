# Clear the workspace by removing all objects
rm(list=ls())

# Load necessary libraries
library(tidyverse)
library(priceR)

#browseURL("https://github.com/stevecondylios/priceR")

# Display available currencies 
currencies()

# Argentine Peso  ARS
# Russian Ruble  RUB
# Turkish Lira  TRY

# Define start date and get current system date
start <- "2022-01-01"
now <- Sys.Date()

# Get historical exchange rates for ARS, RUB, and TRY against USD from 'start' to 'now'
ars <- historical_exchange_rates("ARS", to = "USD", start_date = start, end_date = now)
rub <- historical_exchange_rates("RUB", to = "USD", start_date = start, end_date = now)
try <- historical_exchange_rates("TRY", to = "USD", start_date = start, end_date = now)

# Combine all the dataframes by date
cur <- ars %>% left_join(rub, by = "date") %>% left_join(try, by = "date")

# Display the first and last 6 rows of combined data
head(cur)
tail(cur)

# Display the structure of the combined data
str(cur)

# Load additional libraries for plotting and data manipulation
library(ggthemes)
library(ggrepel)
library(lubridate)

# Rename columns to have clearer names
cur <- cur %>%
  as_tibble() %>% 
  rename(ars_usd = one_ARS_equivalent_to_x_USD,
         rub_usd = one_RUB_equivalent_to_x_USD,
         try_usd = one_TRY_equivalent_to_x_USD) 

# Advanced plotting: Plot the historical exchange rates with custom styles
cur %>% 
  pivot_longer(-date, names_to = "name", values_to = "value") %>% 
  ggplot(aes(x=date, y = value, colour=name)) +
  geom_line(size=1.5) + 
  scale_color_manual(
    breaks = c("ars_usd", "rub_usd","try_usd"), # Sets order in legend
    labels = c( "Argentine Peso", "Russian Ruble", "Turkish Lira"), # Pretty names in legend
    values = c("#02506A", "#03A5DC", "black") # Sets line/legend colours
  ) + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 month") +
  scale_y_continuous(
    expand = c(0, 0), 
    limits = c(0, 0.08)
  ) +
  labs(
    title = "Inflation prone currencies/countries compared to USD",
    y = "Exchange Rate"
  ) +
  theme_economist() + 
  theme(
    plot.title = element_text(size = 18, margin=margin(0,0,8,0)),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.title.y = element_text(vjust = 3.5),
    legend.position="bottom",
    legend.title = element_blank()
  ) 


# Simple plotting: Plot the historical exchange rates with a basic style
cur %>%
  pivot_longer(-date, names_to = "name", values_to = "value") %>% 
  ggplot(aes(x=date, y = value, colour=name)) +
  geom_line() + 
  labs(title = "Inflation prone currencies/countries compared to USD",
       y = "Exchange Rate") +
  theme_bw()  


# Normalizing exchange rates to Jan 1, 2022 values (100 base)
basis <- cur[1,]

cur %>%
  mutate(ars=100*ars_usd/basis$ars_usd,
         rub=100*rub_usd/basis$rub_usd,
         try=100*try_usd/basis$try_usd) %>% 
  select(-contains("_")) %>% 
  pivot_longer(-date, names_to = "name", values_to = "value") %>% 
  ggplot(aes(x=date, y = value, colour=name)) +
  geom_line(size=1.5) + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 month") +
  labs(title = "Inflation prone currencies/countries compared to USD",
       subtitle = "Jan 1 2022=100",
       y = "Exchange Rate") +
  theme_bw()


# Exploring the relationship between the exchange rate and inflation using Turkey's data
# Load web scraping library
library(rvest)

# Consumer Price Index (2003=100) (TURKSTAT)
#browseURL("https://www.tcmb.gov.tr/wps/wcm/connect/EN/TCMB+EN/Main+Menu/Statistics/Inflation+Data/")

# URL for the Consumer Price Index data
url <- "https://www.tcmb.gov.tr/wps/wcm/connect/EN/TCMB+EN/Main+Menu/Statistics/Inflation+Data/"

# The data:
# date
# CPI (Year to Year % Changes)	
# CPI (Month to Month  % Changes)


# Scrape the table data from the web page
web_content <- read_html(url)
table_data <- web_content %>%
  html_node("table[summary='Consumer Price Index (2003=100) (TURKSTAT)']") %>%
  html_table()
names(table_data) <- c("date","cpi","cpi_perc")

table_data <- 
  table_data %>% 
  mutate(date= dmy(paste0("01-", date))) %>% 
  arrange(date)

start_erdogan <- "2014-08-28"

# Retrieve TRY to EUR exchange rates during Erdogan presidency
tryeur <- historical_exchange_rates("TRY", to = "EUR",
                                 start_date = start_erdogan, end_date = now)
str(tryeur)
	
# Plotting exchange rate (TRY to EUR) over days
tryeur %>% 
  ggplot(aes(x=date,y=one_TRY_equivalent_to_x_EUR)) + geom_line()


# Creating a monthly average of TRY to EUR exchange rates
tryeur_mon <-
  tryeur %>% 
  as_tibble() %>% 
  mutate(year_month= floor_date(date, unit = "month")) %>% 
  group_by(year_month) %>% 
  summarise(try_eur=mean(one_TRY_equivalent_to_x_EUR))

# Rename
tryeur_mon <-
  tryeur_mon %>% 
  rename(date=year_month)

# Join CPI data with exchange rate data and remove NA
cpi_eur <- table_data %>% left_join(tryeur_mon)
cpi_eur <- cpi_eur[complete.cases(cpi_eur), ]

# Scatterplot of exchange rates against CPI percentage changes
cpi_eur %>% 
  ggplot(aes(x=try_eur,y=cpi_perc)) + geom_point() + 
  geom_smooth(method = "loess", se = FALSE) +
  theme_clean()

# Scatterplot of percentage change in exchange rates against CPI percentage changes
cpi_eur %>% 
  mutate(dlneur=100*(log(try_eur)-lag(log(try_eur)))) %>% 
    ggplot(aes(x=dlneur,y=cpi_perc)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE, col="red") +
  theme_bw()

