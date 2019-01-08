# Data on Border Barriers by Reece Jones
# 
# Data from: Jones, Reece (2012) Border Walls: Security and the War on Terror, London: Zed Books, p. 10.

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, countrycode, rio)

# Read data and transform
barriers.bw <- import("./data/border data/Jones 2012 - Barriers_p10.xlsx", range = "A3:C28") %>%
  set_names(c("year", "country1", "country2")) %>%
  mutate(state1 = countrycode(country1, "country.name", "iso3c"),
         state2 = countrycode(country2, "country.name", "iso3c"),
         indicator = "fortified",
         source = "Reece (2012)")
