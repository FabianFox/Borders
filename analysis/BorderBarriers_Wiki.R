# Wikipedia Data on Border Barriers
# 
# Data from: https://en.wikipedia.org/wiki/Border_barrier

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest, countrycode, qdap)

# (1) Scrape the first table (current border barriers)
# (2) Append data on planned/currently constructed barriers (unstructured text)

# (1) Scrape border barriers table
# Full table with all rows
barriers.wiki <- read_html("https://en.wikipedia.org/wiki/Border_barrier") %>%
  html_table(".wikitable", header = TRUE, dec = ",", fill = TRUE) %>%
  .[[1]] %>%
  set_names(c("name", "country", "built", "length", "type"))

# Extract country names using countrycode::codelist for matching
name.dy <- str_extract_all(barriers.wiki$name, 
                             pattern = regex(paste(codelist$country.name.en.regex, collapse = "|"),
                                             ignore_case = TRUE))

country <- str_remove_all(barriers.wiki$country, pattern = "and") %>% 
  str_extract_all(pattern = regex(paste(codelist$country.name.en.regex, collapse = "|"), 
                                  ignore_case = TRUE))

#for (i in seq_along(name.dy)) {
#  name.dy[[i]] <- ifelse(length(name.dy[[i]]) < 2, country[[i]], name.dy[[i]])
#}

# Edit some of the border walls 
name.dy[[3]][2] <- "Malaysia"
name.dy[[5]][c(1,2)] <- c("Spain", "Morocco")
name.dy[[6]][c(1,2)] <- c("Hong Kong", "China")
name.dy[[8]][c(1,2)] <- c("China", "North Korea")
name.dy[[13]][c(1,2)] <- c("Spain", "Morocco")
name.dy[[14]][2] <- "Serbia"
name.dy[[18]][2] <- "Pakistan"
name.dy[[19]][2] <- "Pakistan"
name.dy[[21]][c(1,2)] <- c("North Korea", "South Korea")
name.dy[[22]] <- country[[22]]
name.dy[[26]][c(1,2)] <- c("Saudi Arabia", "Yemen")
name.dy[[27]][c(1,2)] <- c("Saudi Arabia", "Iraq")

# Add dyad identifier to the table and turn into iso3c-code
barriers.wiki <- barriers.wiki %>%
  mutate(state1 = countrycode(unlist(map(name.dy, 1)), "country.name", "iso3c"),
         state2 = countrycode(unlist(map(name.dy, 2)), "country.name", "iso3c"))

# Note: Variable "country" identifies direction of the border barrier