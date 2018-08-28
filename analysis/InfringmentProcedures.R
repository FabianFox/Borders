# EU Infringment Procedures: Asylum

# Data from: 
# https://ec.europa.eu/home-affairs/what-is-new/eu-law-and-monitoring/

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, rvest, janitor)

## ------------------------------------------------------------------------------------------------------------ ##

# Border Management and Schengen
borderURL <- "https://ec.europa.eu/home-affairs/what-is-new/eu-law-and-monitoring/infringements_en?country=All&field_infringement_policy_tid=1625&field_infringement_number_title="

# Asylum
asylumURL <- "https://ec.europa.eu/home-affairs/what-is-new/eu-law-and-monitoring/infringements_en?country=All&field_infringement_policy_tid=1598&field_infringement_number_title="

# Return policy
returnURL <- "https://ec.europa.eu/home-affairs/what-is-new/eu-law-and-monitoring/infringements_en?country=All&field_infringement_policy_tid=1631&field_infringement_number_title="

# Function that extracts the respective tables
ext_table <- function(x) {
  read_html(x) %>%
    html_table(".table", header = T) %>%
    .[[1]]
}

# Scrape
urls <- list(borderURL, asylumURL, returnURL)

# Scrape over urls and combine df
infringment.df <- map(urls, ext_table) %>%
  bind_rows() %>%
  as_tibble() %>%
  clean_names() %>%
  remove_empty(c("rows", "cols"))

# Explorative plot 
inf.count <- infringment.df %>%
  mutate(year = str_extract(decision_date, "[:digit:]{4}(?= )")) %>%
  group_by(country, year) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = year, y = count)) +
  geom_bar(stat = "identity") +
  facet_wrap(~country)
