# Combining existing studies/data on global border fences
# See also:
# - ./analysis/Fence data
# - ./analysis/BordersJoin.R

# Notes:
# -

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, countrycode, tabulizer, qdap, rvest, haven, docxtractr)

# Existing data sets
### ------------------------------------------------------------------------ ###
# - jones2012.df : Jones (2012) Border Walls: Security and the War on Terror, London: Zed Books, p. 10.
# - barriers.wiki : https://en.wikipedia.org/wiki/Border_barrier
# - hassner2015 : Hassner & Wittenberg (2015) https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/U16NEI
# - carter2017 : Carter & Poast (2017) http://www.paulpoast.com/#/original-data/4590503653
# - jellissen2013 : Jellissen & Gottheil (2013) On the utility of security fences along international borders
# - linnell2013 : Linnell et al. (2013) Border Security Fencing and Wildlife
# - avdan2019 : Avdan (2019) Visas and Walls
# - barriers.gsc : GlobalSecurity.org (https://www.globalsecurity.org/military/world/walls.htm)

# Jones (2012): Border Walls
### ------------------------------------------------------------------------ ###
# Data is directed
# Vars:
# state1: initiating country
# state2: affected country
# year: initiated in year 

# Read data and transform
jones2012.df <- import("./data/border data/Jones 2012 - Barriers_p10.xlsx", range = "A3:C28") %>%
  set_names(c("year", "country1", "country2")) %>%
  mutate(state1 = countrycode(country1, "country.name", "iso3c"),
         state2 = countrycode(country2, "country.name", "iso3c"),
         source = "Jones (2012)") %>%
  select(state1, state2, year, -country1, -country2)

# Wikipedia article "border barrier" 
# https://en.wikipedia.org/wiki/Border_barrier
### ------------------------------------------------------------------------ ###
# Data is directed, observation period: 2000+
# Vars:
# state1: initiating country
# state2: affected country
# year: initiated in year 
# length: length of the border fence
# type: reason for fortification

# Get the last edit:
read_html("https://en.wikipedia.org/wiki/Border_barrier") %>%
  html_nodes("#footer-info-lastmod") %>%
  html_text() %>%
  print()

# (1) Scrape the first table (current border barriers)
# (2) Append data on planned/currently constructed barriers (unstructured text)

# (1) Scrape border barriers table
# Full table with all rows
barriers.wiki <- read_html("https://en.wikipedia.org/wiki/Border_barrier") %>%
  html_table(".wikitable", header = TRUE, dec = ",", fill = TRUE) %>%
  .[[1]] %>%
  set_names(c("name", "country", "year", "length", "type"))

# Extract country names using countrycode::codelist for matching
name.dy <- str_extract_all(barriers.wiki$name, 
                           pattern = regex(paste(codelist$country.name.en.regex, collapse = "|"),
                                           ignore_case = TRUE))

country <- str_extract_all(barriers.wiki$country, 
                           pattern = regex(paste(codelist$country.name.en.regex, collapse = "|"), 
                                           ignore_case = TRUE))

# In some cases, the variable "country" entails more information on the countries that build the wall. That is
# the case when "name" < "country"
length.name.dy <- map_int(name.dy, length)
length.country <- map_int(country, length)

# Replace "name" with "country" when above condition is TRUE
for (i in seq_along(name.dy)) {
  name.dy[i] <- ifelse(length.name.dy[i] < length.country[i], country[i], name.dy[i])
}

# Those still < 2 have to be edited manually
which(lengths(name.dy) < 2) 

# Edit those borders that still do not contain valid entries.
name.dy[[3]][2] <- "Malaysia"
name.dy[[5]][2] <- "Morocco"
name.dy[[6]][2] <- "China"
name.dy[[13]][2] <- "Morocco"
name.dy[[14]][2] <- "Serbia"
name.dy[[18]][2] <- "Pakistan"
name.dy[[19]][2] <- "Pakistan"
name.dy[[21]] <- c("North Korea", "South Korea")
name.dy[[26]] <- c("Saudi Arabia", "Yemen")

# United States is coded in the wrong direction
name.dy[[34]] <- c("United States", "Mexico")

# Add dyad identifier to the table and turn into iso3c-code
barriers.wiki <- barriers.wiki %>%
  mutate(country1 = countrycode(unlist(map(name.dy, 1)), "country.name", "country.name"),
         country2 = countrycode(unlist(map(name.dy, 2)), "country.name", "country.name"),
         state1 = countrycode(country1, "country.name", "iso3c"),
         state2 = countrycode(country2, "country.name", "iso3c"),
         source = "Wikipedia") %>%
  select(state1, state2, year, length, type, source, -country1, -country2)

# Hassner & Wittenberg (2015)
# https://en.wikipedia.org/wiki/Border_barrier
### ------------------------------------------------------------------------ ###
# Data is directed, observation period: 1950+
# Vars:
# state1: initiating country
# state2: affected country
# year: fortification project started in
# end: fortification project finished (check!)
# length: length of border wall

# Data available in dta-format

# Custom match for North Korea
custom <- c("Democratic PRK" = "PRK")

# Read data and adjust variables
hassner2015 <- read_dta("./data/border data/barrierstoentry.dta") %>%
  select(state1 = builder, state2 = target, year = begin, 
         end, length = walllength) %>%
  filter(year > 1970) %>%                                                                
  mutate(state1 = countrycode(state1, "country.name", "iso3c", 
                              custom_match = custom),
         state2 = countrycode(state2, "country.name", "iso3c", 
                              custom_match = custom),
         source = "Hassner & Wittenberg (2013)")

# Carter & Poast (2017) 
# Why Do States Build Walls? Political Economy, Security, and Border Stability 
# Journal of Conflict Resolution, 61:2, 239-270
# Data from: 
# https://journals.sagepub.com/doi/abs/10.1177/0022002715596776
# http://www.paulpoast.com/#/original-data/4590503653
### ------------------------------------------------------------------------ ###

# The reproducible data entails many incomprehensible columns and, by now, I can't
# make sense of it. 

# Jellissen & Gottheil (2013)
# Data from: Jellissen & Gottheil (2013) On the utility of security fences along 
#            international borders, p270 + 271
### ------------------------------------------------------------------------ ###

# Data is directed, observation period: 2000+
# Vars:
# state1: initiating country
# state2: affected country
# year: initiated in year 

# Load data into R

# (1) Read tables
# Table 1. Security fences built, under construction, or proposed; placed whole 
#          or in part in disputed space, worldwide
jellissen2013.t1 <- extract_areas("./data/border data/Jellissen & Gottheil 2013 - On the utility of security fences along international borders.pdf",
                                pages = 1) %>%
  .[[1]] %>%
  as_tibble(.) %>%
  setNames(c("state1", "state2", "length", "purpose"))

# Table 2. Security fences built, under construction, or proposed; placed in non- 
#          disputed space, worldwide
jellissen2013.t2 <- extract_areas("./data/border data/Jellissen & Gottheil 2013 - On the utility of security fences along international borders.pdf",
                                pages = 2) %>%
  .[[1]] %>%
  as_tibble(.) %>%
  setNames(c("state1", "state2", "length", "purpose"))

# (2) Combine
jellissen2013.df <- bind_rows(jellissen2013.t1, jellissen2013.t2)

# (3) Data cleaning
jellissen2013.df <- jellissen2013.df %>%
  .[c(-5, -7, -16, -21, -22, -24, - 25, -44),] %>%                               # see below
  mutate(length = ifelse(str_detect(length, "n/a") == TRUE, NA_real_, length),
         state1 = countrycode(state1, "country.name", "iso3c"),
         state2 = countrycode(state2, "country.name", "iso3c"),
         source = "Jellissen & Gottheil (2013)") %>%
  select(state1, state2, length, type = purpose, source)

# State 2 invalid:
# 5: Israel - West Bank
# 7: Morocco - Western Sahara
# 16: UK - Northern Ireland
# 21: Saudi Arabia - Red Sea
# 22: Saudi Arabia - Persian Gulf
# 24 Israel - Gaza
# 25: Egypt - Gaza
# 44: France - Channel to Britain

# Linnell et al (2013) Appendix S1
# Data from: Linnell et al. (2013) Border Security Fencing and Wildlife: 
#            The End of the Transboundary Paradigm in Eurasia? PLoS Biol 14(6): 
#           e1002483. https://doi.org/10.1371/journal.pbio.1002483
### ------------------------------------------------------------------------ ###
# Undirected data
# Vars:
# state1
# state2
# blength: length of the border
# length: length of the border fence
# description: description of the design of the border fence

linnell2013.docx <- read_docx("./data/border data/Linnell et al 2016 - Supporting Material S1.docx")

# Extract the table
linnell2013.df <- linnell2013.docx %>%
  docx_extract_tbl() %>%
  .[-c(1, 23:24, 50:51),] %>%
  setNames(c("countries", "blength", "length", "description")) 

# Data wrangling
# custom match
custom.match <- c("Tadjikistan" = "TKL", "Abkhazia" = "RUS", 
                  "South Ossetia" = "RUS", "North Korea" = "PRK")

linnell2013.df <- linnell2013.df %>%
  mutate(state1 = str_extract(countries, "([:alpha:]+\\s[:alpha:]+)|[:alpha:]+"), # also: tidyr::separate
         state2 = str_extract(countries, "([:alpha:]+\\s[:alpha:]+)|[:alpha:]+$"),
         state1 = countrycode(state1, "country.name", "iso3c", 
                              custom_match = custom.match),
         state2 = countrycode(state2, "country.name", "iso3c", 
                              custom_match = custom.match),
         source = "Linnell et al. (2013)",
         year = "2015/2016") %>%
  select(state1, state2, year, length, blenth, description, source)

# Remove unrecognized or clean unmatched states
# India (Kashmir) = IND; remove Transnistria
linnell2013.df[41, "state2"] <- "IND"
linnell2013.df <- linnell2013.df[-13,]

# Avdan 2019, p. 128-129
# 
# Data from: 
# Avdan, Nazli (2019): Visas and Walls: Border Security in the Age of Terrorism,
# Philadelphia: University of Pennsylvania Press.
### ------------------------------------------------------------------------ ###
# Data is directed
# Vars:
# state1: initiating country
# state2: affected country
# year: initiated in year 

# Page 128
avdan2019.t1 <- import("./data/border data/Avdan 2019 - Visas and Walls_1.xlsx", range = "A3:D44") %>%
  .[-36,] %>%
  set_names(c("state1", "state2", "year", "dismantled")) %>%
  mutate(state1 = countrycode(state1, "country.name.en", "iso3c"),
         state2 = countrycode(state2, "country.name.en", "iso3c"),
         source = "Avdan (2019)") %>%
  filter(is.na(dismantled))

# Page 130
avdan2019.t2 <- import("./data/border data/Avdan 2019 - Visas and Walls_2.xlsx", range = "A2:D41") %>%
  .[-c(27, 35),] %>%
  set_names(c("state1", "state2", "year", "dismantled")) %>%
  mutate(state1 = countrycode(state1, "country.name.en", "iso3c"),
         state2 = countrycode(state2, "country.name.en", "iso3c"),
         source = "Avdan (2019)") %>%
  filter(is.na(dismantled))

# (2) Combine
avdan2019.df <- bind_rows(avdan2019.t1, avdan2019.t2)

# Some countrycodes are not matched properly
barriers.av[1, c("state1", "state2")] <- c("KOR", "PRK")
barriers.av[10, "state2"] <- "PRK"
barriers.av[28, "state2"] <- "PRK"

# GlobalSecurity.org
# 
# Data from: 
# https://www.globalsecurity.org/military/world/walls.htm
### ------------------------------------------------------------------------ ###

# Scrape the content of the table
barriers.gsec <- read_html("https://www.globalsecurity.org/military/world/walls.htm") %>%
  html_nodes("#content > table:nth-child(12)") %>%
  html_table(header = TRUE) %>%
  .[[1]] %>%
  set_names(c("state1", "barrier", "begin", "end"))

# Put the individual data sets into a list.df
### ------------------------------------------------------------------------ ###

fence.df <- tibble(
  source = c("Avdan2019", "Hassner&Wittenberg2015", "Jellissen&Gottheil2013",
             "LinnellEtAl2013", "Jones2012", "Wikipedia2019", "GlobalSecurity2019"),
  data = list(avdan2019.df, hassner2015, jellissen2013.df, linnell2013.df, jones2012.df,
           barriers.wiki, barriers.gsc))

# Save the data
### ------------------------------------------------------------------------ ###
saveRDS(fence.df, file = "./analysis/Fence data/fence.df.RDS")
