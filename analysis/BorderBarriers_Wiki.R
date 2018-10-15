# Wikipedia Data on Border Barriers
# 
# Data from: https://en.wikipedia.org/wiki/Border_barrier

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest, countrycode, qdap)

# Note: Unfortunately, some operations are specific to the current table. Once the table is altered, we have to 
#       check whether the script (name replacement) is still valid. 
#       The latest date was "7 October 2018" and the table had 36 entries.

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
         state2 = countrycode(country1, "country.name", "iso3c"),
         indicator = "fortified") %>%
  select(year, country1, country2, state1, state2, indicator)

# Remove auxiliary objects
rm(i, length.country, length.name.dy, name.dy, country)
