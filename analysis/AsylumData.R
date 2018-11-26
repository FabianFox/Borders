# Asylum Data

# Load/install packages
### ------------------------------------------------------------------------###

if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, rvest, janitor, qdap, eurostat, countrycode, wbstats, 
       lubridate, countrycode, ggrepel, tidyr, stringr)

# Asylum data
### ------------------------------------------------------------------------###

# - number of asylum applications (eurostat)
# ID: tps00191: Asylum and first time asylum applicants - annual aggregated data 
#     (rounded)
# Metadata: http://ec.europa.eu/eurostat/cache/metadata/en/migr_asyapp_esms.htm
# Further information: https://bit.ly/2LXrsKB and 
#                      http://dd.eionet.europa.eu/vocabulary/eurostat/asyl_app

# Clean eurostat-cache from time to time
# clean_eurostat_cache(cache_dir = NULL)

# Yearly applications from 2015 - 2017
asylum.df <- get_eurostat("tps00191", time_format = "num", 
                          stringsAsFactors = FALSE,
                          filters = list(asyl_app = "ASY_APP", 
                                      time = 2015:2017)) %>%
  select(country = geo, applicants = values, year = time) %>%
  filter(country != "EU28") %>%
  mutate(country = countrycode(country, "eurostat", "iso3c")) 

# First instance decisions: Recognition rates (migr_asydcfsta, accessed: 16.11.2018)
asylum_decisions.df <- read_csv("./FRAN-reports/migr_asydcfsta_1_Data.csv") %>%
  rename_all(tolower) %>%
  rename(year = time) %>%
  filter(year >= 2015, 
         citizen == "Total",
         sex == "Total",
         age == "Total",
         decision %in% c("Total", "Total positive decisions", "Rejected"),
         !geo %in% c("Total", "European Union (current composition)", "Turkey")) %>%
  mutate(country = countrycode(geo, origin = "country.name.en", destination = "iso3c"),
         value = as.numeric(str_replace_all(value, ",", ""))) %>%
  select(-unit, -citizen, -sex, -age, -geo) %>%
  spread(decision, value) %>%
  clean_names() %>%
  mutate(rejection_rate = (total - total_positive_decisions) / total * 100,
         recognition_rate = (total - rejected) / total * 100)

# Put Eurostat asylum statistics together
asylum.df <- asylum.df %>%
  left_join(asylum_decisions.df, by = c("country", "year"))
  
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
  mutate(applicants_per1000 = round(applicants / (population / 1000), 2)) 


# Infringement procedures 
### ------------------------------------------------------------------------###

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
infringement.df <- map(urls, ext_table) %>%
  bind_rows() %>%
  as_tibble() %>%
  clean_names() %>%
  remove_empty(c("rows", "cols")) %>%
  mutate_if(is.character, scrubber) %>%
  mutate(country = countrycode(country, "country.name.en", "iso3c"),
         year = as.numeric(str_extract(decision_date, "(?<=/)[:digit:]{4}")))

# Infringment cleaned for join (policy field is disregarded here, 
# i.e. doesn't matter whether infringement is due to 
# Border Management, Asylum or Return Policies)
infringement.join <- infringement.df %>%
  select(country, year) %>%
  group_by(country, year) %>%
  summarize(lawsuits = n())

# Join asylum data to the infringement.df
asylum.df <- asylum.df %>%
  left_join(infringement.join) %>%
  mutate(lawsuits = ifelse(is.na(lawsuits), 0, lawsuits))


# ParlGov Data
### ------------------------------------------------------------------------###

# ParlGov data (http://www.parlgov.org/, accessed: 15.11.2018)
election.df <- import("./FRAN-reports/view_election.csv") %>%
  mutate(election_date = as_date(election_date)) %>%
  filter(between(year(election_date), 2010, 2017), 
         country_name_short %in% unique(asylum.df$country),
         election_type == "parliament") %>%
  group_by(country_name_short, election_date) %>%
  summarize(right_share = sum(vote_share[left_right > 8], na.rm = T)) %>%
  mutate(round_year = year(round_date(election_date))) %>%
  group_by(country_name_short, round_year) %>%      
  filter(duplicated(country_name_short) | n() == 1) %>% # Need to figure out how this works.
  select(-election_date)
  
# Matching procedure
# (1) Create a "full"-DB with all possible combinations of country-years;
#     Fill missing values with earlier occurrence
asyl_election_full.df <- asylum.df %>%
  full_join(election.df, 
            by = c("country" = "country_name_short", "year" = "round_year")) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  fill(right_share, .direction = "down") %>%
  select(country, year, right_share)

# (2) Join right_share to asylum.df
asylum.df <- asylum.df %>%
  left_join(asyl_election_full.df, by = c("country", "year")) %>%
  mutate(region = countrycode(country, "iso3c", "region"),
         EU28 = countrycode(country, "iso3c", "eu28")) %>%
  filter(EU28 == "EU")


# Temporary border controls Data
### ------------------------------------------------------------------------###
# Data on temporarily reinstated border controls in the Schengen Area

# Source: https://ec.europa.eu/home-affairs/what-we-do/policies/borders-and-visas/schengen/reintroduction-border-control_en
# pdf: https://ec.europa.eu/home-affairs/sites/homeaffairs/files/what-we-do/policies/borders-and-visas/schengen/reintroduction-border-control/docs/ms_notifications_-_reintroduction_of_border_control_en.pdf

# Location of file
loc <- "./FRAN-reports/Member States' Notifications of Reintroduction of border control.xlsx"

# Load the data
bcontrol.df <- import(loc)[2:96,] %>%
  setNames(.[1,]) %>%
  .[-1,]

# Seperate dates 
year_duration <- str_extract_all(bcontrol.df$Duration, "[:digit:]{4}")

# Add Begin/End of border control to data frame
bcontrol.df <- bcontrol.df %>%
  mutate(Begin = map(year_duration, 1),
         End = map(year_duration, 2),
         End = if_else(lengths(year_duration) == 1, Begin, End),
         Begin = unlist(Begin),
         End = unlist(End))

# Correct a parsing problem 
bcontrol.df[which(bcontrol.df$Begin == "4008"), c("Begin", "End")] <- c("2009", "2009")

# By now excluding some events concerning security issues
bcontrol.df <- bcontrol.df %>%
  mutate(
    migration = case_when(
      str_detect(tolower(`Reasons/Scope`), "migration|migrant|migratory|influx|recommendation|movements") == TRUE ~ "Migration",
      str_detect(tolower(`Reasons/Scope`), "migration|migrant|migratory|influx|recommendation|movements") == FALSE ~ "Other"),
    country = countrycode(`Member State`, "country.name.en", "iso3c"),
    Begin = as.numeric(Begin),
    End = as.numeric(End)) %>%
  filter(migration == "Migration") %>%
  select(country, Begin) %>%
  group_by(country, Begin) %>%
  summarize(checks = n()) %>%
  as_tibble()

asylum.df <- asylum.df %>%
  left_join(bcontrol.df, by = c("country", "year" = "Begin")) %>%
  mutate(checks = ifelse(is.na(checks), 0, checks))


# Country groups
### ------------------------------------------------------------------------###

# Create the grouping variable based on 2015 data
grouping.df <- asylum.df %>%
  filter(year == 2015) %>%
  select(country, region, applicants_per1000) %>%
  mutate(group = case_when(
      region %in% c("Northern Europe", "Western Europe") & 
      applicants_per1000 > 3.5 ~ "Host states",
    country %in% c("MLT", "CYP", "ITA", "GRC") ~ "Frontline states",
    country %in% c("CZE", "HUN", "POL", "SVK") ~ "Visegrád Group",
    TRUE ~ "Other"
  )) %>%
  select(country, group)

# Join grouping variable and carry forward
asylum.df <- asylum.df %>%
  left_join(grouping.df) %>%
  mutate(checks = ifelse(is.na(checks), 0, checks))


# Tables
### ------------------------------------------------------------------------###

# Table 1. Country, application_per1000, region in 2015
table1 <- asylum.df %>%
  filter(year == 2015, EU28 == "EU", group != "Other") %>%
  select(country, region, applicants_per1000, group) %>%
  arrange(factor(group, levels = c("Visegrád Group", "Host states", 
                                   "Frontline states", "Other")),
          region,
          country,
          desc(applicants_per1000))

# To word
write.table(table1, file = "./output/table1.txt", sep = ",", quote = FALSE, 
            row.names = F)


# Exploratory visualizations of the data
### ------------------------------------------------------------------------###

### Theoretical convergence
convergence.df <- asylum.df %>%
  select(country, group, year) %>%
  filter(group != "Other")

paths <- tibble(
  group = c(rep("Visegrad Group", 3), rep("Host states", 3), rep("Frontline states", 3)),
  year = rep(2015:2017, 3),
  approach = jitter(c(2:4, 1:3, c(1, 1, 2.5)), amount = 0.7)
)

convergence.df <- convergence.df %>%
  left_join(paths)

(convergence.fig <- convergence.df %>%
  ggplot(aes(x = year, y = approach, group = group, colour = group)) +
  stat_smooth(method="lm", span = 0.1, se = TRUE, aes(fill=group), alpha=0.3,
              linetype = 0) +
  ylab("Asylum policies") +
  xlab("Time") +
  scale_x_continuous(breaks = seq(2015, 2017, 1)) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5),
        legend.title = element_blank(),
        axis.text.x = element_blank()))

### Convergence over time (intra group)
intra.rejection.fig <- asylum.df %>%
  filter(group != "Other") %>%
  ggplot(aes(x = year, y = rejection_rate, group = country,
             label = country)) +
  geom_line() +
  geom_text_repel(data = subset(asylum.df, year == 2015 & group != "Other")) +
  facet_wrap(~group) +
  ylab("") +
  xlab("") +
  scale_x_continuous(breaks = seq(2015, 2017, 1)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))

### Convergence over time (mean group)
inter.rejection.df <- asylum.df %>%
  filter(group != "Other") %>%
  group_by(group, year) %>%
  summarize(mean_rejection_rate = mean(rejection_rate, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_rejection_rate, colour = group)) +
  geom_line() +
  ylab("") +
  xlab("") +
  scale_x_continuous(breaks = seq(2015, 2017, 1)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))

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
                       scale_y_continuous(labels = function(x) round(x, 0)) +
                       theme_minimal() +
                       theme(panel.grid.minor.x = element_blank(),
                             panel.grid.major.x = element_blank(),
                             text = element_text(size = 14),
                             axis.ticks.x = element_line(size = .5))))

### Total infringment procedures by policy field
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

# Recognition rate by country over time (2015-2017)
recognition.fig <- asylum.df %>%
  ggplot(aes(x = year, y = recognition_rate)) +
  geom_line(stat = "identity") +
  facet_wrap(~country) +
  ylab("") +
  xlab("") +
  scale_x_continuous(breaks = seq(2015, 2017, 1)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))


# Saving data and figures
### ------------------------------------------------------------------------###
# Data:
saveRDS(asylum.df, file = "./data/AsylumStatistics.rds")
# Plots:
ggsave(filename = "./FRAN-reports/AsylumInfringmentProceduresFig.tiff", 
       plot = inf.count$plot[[1]], device = "tiff", dpi = 600)

ggsave(filename = "./FRAN-reports/TheoreticalConvergence.tiff", 
       plot = convergence.fig, device = "tiff", dpi = 600)
