# Asylum Data

# Data from: 
# https://ec.europa.eu/home-affairs/what-is-new/eu-law-and-monitoring/

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, rvest, janitor, qdap, eurostat, countrycode, wbstats)

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

# Yearly applications from 2015 - 2017
asylum.df <- get_eurostat("tps00191", time_format = "num", stringsAsFactors = FALSE,
                       filters = list(asyl_app = "ASY_APP", time = 2015:2017)) %>%
  select(country = geo, applicants = values, year = time) %>%
  filter(country != "EU28") %>%
  mutate(country = countrycode(country, "eurostat", "iso3c")) 

# Population in 2016 (World Bank Indicators)
# World Bank 
# (1)
# Download data (mrv = newest available)
wb.info <- wb(country = asylum.df$country,
              indicator = "SP.POP.TOTL", 
              startdate = 2015, enddate = 2017,
              return_wide = TRUE) %>%
  select(iso3c, population = SP.POP.TOTL, year = date) %>%
  mutate(year = as.numeric(year))

# (2) Match to base data
asylum.df <- asylum.df %>%
  left_join(wb.info, by = c("country" = "iso3c", "year")) %>%
  mutate(applicants_perc = applicants / (population / 1000)) 

# Scrape over urls and combine df
infringement.df <- map(urls, ext_table) %>%
  bind_rows() %>%
  as_tibble() %>%
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>%
  mutate_if(is.character, scrubber) %>%
  mutate(country = countrycode(country, "country.name.en", "iso3c"),
         year = as.numeric(str_extract(decision_date, "(?<=/)[:digit:]{4}")))

# Infringment cleaned for join (policy field is disregarded here, i.e. doesn't matter whether infringement is due to 
# Border Management, Asylum or Return Policies)
infringement.join <- infringement.df %>%
  select(country, year) %>%
  group_by(country, year) %>%
  summarize(lawsuits = n())

# Join asylum data to the infringement.df
asylum.df<- asylum.df %>%
  left_join(infringement.join)

# Visualizations using the "full" infringement data set
# Grouped by country/year - individual plots by policy
inf.count <- infringement.df %>%
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
inf.total <- infringement.df %>%
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

# Save
ggsave(filename = "./FRAN-reports/AsylumInfringmentProceduresFig.tiff", plot = inf.count$plot[[1]], device = "tiff", dpi = 600)
