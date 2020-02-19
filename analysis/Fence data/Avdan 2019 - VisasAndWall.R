# Data from Avdan 2019, p. 128-129
# 
# Data from: 
# Avdan, Nazli (2019): Visas and Walls: Border Security in the Age of Terrorism,
# Philadelphia: University of Pennsylvania Press.

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, countrycode, rio, tabulizer)

# Extract data from pdf table
### ------------------------------------------------------------------------ ###
# Page 128
barriers.av.t1 <- import("./data/border data/Avdan 2019 - Visas and Walls_1.xlsx", range = "A3:D44") %>%
  .[-36,] %>%
  set_names(c("state1", "state2", "year", "dismantled")) %>%
  mutate(state1 = countrycode(state1, "country.name.en", "iso3c"),
         state2 = countrycode(state2, "country.name.en", "iso3c"),
         indicator = "fortified",
         source = "Avdan (2019)") %>%
  filter(is.na(dismantled))

# Page 130
barriers.av.t2 <- import("./data/border data/Avdan 2019 - Visas and Walls_2.xlsx", range = "A2:D41") %>%
  .[-c(27, 35),] %>%
  set_names(c("state1", "state2", "year", "dismantled")) %>%
  mutate(state1 = countrycode(state1, "country.name.en", "iso3c"),
         state2 = countrycode(state2, "country.name.en", "iso3c"),
         indicator = "fortified",
         source = "Avdan (2019)") %>%
  filter(is.na(dismantled))

# (2) Combine
barriers.av <- bind_rows(barriers.av.t1, barriers.av.t2)

# Some countrycodes are not matched properly
barriers.av[1, c("state1", "state2")] <- c("KOR", "PRK")
barriers.av[10, "state2"] <- "PRK"
barriers.av[28, "state2"] <- "PRK"

# (4) Save data
saveRDS(barriers.av, file = "./data/border data/Avdan 2019.rds")
