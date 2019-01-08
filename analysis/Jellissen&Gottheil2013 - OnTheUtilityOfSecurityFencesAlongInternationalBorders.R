# Data on Border Barriers by Jellissen and Gottheil
# 
# Data from: Jellissen & Gottheil (2013) On the utility of security fences along 
#            international borders, p270 + 271

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, countrycode, rio, tabulizer)

# Load data into R

# (1) Read tables
# Table 1. Security fences built, under construction, or proposed; placed whole 
#          or in part in disputed space, worldwide
barriers.jg.t1 <- extract_areas("./data/border data/Jellissen & Gottheil 2013 - On the utility of security fences along international borders.pdf",
                                  pages = 1) %>%
  .[[1]] %>%
  as_tibble(.) %>%
  setNames(c("state1", "state2", "length", "purpose"))

# Table 2. Security fences built, under construction, or proposed; placed in non- 
#          disputed space, worldwide
barriers.jg.t2 <- extract_areas("./data/border data/Jellissen & Gottheil 2013 - On the utility of security fences along international borders.pdf",
                               pages = 2) %>%
  .[[1]] %>%
  as_tibble(.) %>%
  setNames(c("state1", "state2", "length", "purpose"))

# (2) Combine
barriers.jg <- bind_rows(barriers.jg.t1, barriers.jg.t2)

# (3) Data cleaning
barriers.jg <- barriers.jg %>%
  .[c(-5, -7, -16, -21, -22, -24, - 25, -44),] %>%                               # see below
  mutate(length = ifelse(str_detect(length, "n/a") == TRUE, NA_real_, length),
         state1 = countrycode(state1, "country.name", "iso3c"),
         state2 = countrycode(state2, "country.name", "iso3c"),
         source = "Jellissen&Gottheil (2013)",
         year = "2013",                                                          # Check
         indicator = "fencing")

# State 2 invalid:
# 5: Israel - West Bank
# 7: Morocco - Western Sahara
# 16: UK - Northern Ireland
# 21: Saudi Arabia - Red Sea
# 22: Saudi Arabia - Persian Gulf
# 24 Israel - Gaza
# 25: Egypt - Gaza
# 44: France - Channel to Britain

# (4) Save data
saveRDS(barriers.jg, file = "./data/Jellissen & Gottheil 2013.rds")
