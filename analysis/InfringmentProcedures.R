# EU Infringment Procedures: Asylum

# Data from: 
# https://ec.europa.eu/home-affairs/what-is-new/eu-law-and-monitoring/

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, rvest, janitor, qdap, eurostat, countrycode)

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

# Add additional data:
# - number of asylum applications (eurostat)
# ID: tps00191: Asylum and first time asylum applicants - annual aggregated data (rounded)
# Metadata: http://ec.europa.eu/eurostat/cache/metadata/en/migr_asyapp_esms.htm
# Further information: https://bit.ly/2LXrsKB and http://dd.eionet.europa.eu/vocabulary/eurostat/asyl_app

# Clean eurostat-cache from time to time
# clean_eurostat_cache(cache_dir = NULL)

# Yearly applications of 2016 
asylum.df <- get_eurostat("tps00191", time_format = "num", stringsAsFactors = FALSE,
                       filters = list(asyl_app = "ASY_APP", time = 2016)) %>%
  select(country = geo, applicants = values) %>%
  filter(country != "EU28") %>%
  mutate(country = countrycode(country, "eurostat", "iso3c"))

# Scrape over urls and combine df
infringment.df <- map(urls, ext_table) %>%
  bind_rows() %>%
  as_tibble() %>%
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>%
  mutate_if(is.character, scrubber) %>%
  mutate(country = countrycode(country, "country.name.en", "iso3c"))

# Join asylum data to the infringment.df
infringment.df <- infringment.df %>%
  left_join(asylum.df)

# Grouped by country/year - individual plots by policy
inf.count <- infringment.df %>%
  mutate(year = as.numeric(str_extract(decision_date, "(?<=/)[:digit:]{4}"))) %>%
  group_by(country, year, policy) %>%
  summarize(count = n()) %>%
  group_by(policy) %>%
  nest() %>%
  mutate(plot = map2(data, policy, ~ggplot(data = .x) +
                       geom_bar(aes(x = year, y = count), stat = "identity") +
                       facet_wrap(~country) +
                       ggtitle(.y) +
                       ylab("") +
                       xlab("") +
                       scale_x_continuous(breaks = seq(2005, 2015, 5)) +
                       scale_y_continuous(labels = function(x) round(x, digits = 0)) +
                       theme_minimal() +
                       theme(panel.grid.minor.x = element_blank(),
                             panel.grid.major.x = element_blank(),
                             text = element_text(size = 14),
                             axis.ticks.x = element_line(size = .5))))

# Total infringment procedures by policy field
inf.total <- infringment.df %>%
  mutate(year = as.numeric(str_extract(decision_date, "(?<=/)[:digit:]{4}"))) %>%
  group_by(year, policy) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(year, count)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~policy) +
  ylab("") +
  xlab("") +
  scale_x_continuous(breaks = seq(2005, 2015, 5)) +
  scale_y_continuous(labels = function(x) round(x, digits = 0)) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))