# Data from Benedicto et al. (2020), p. 14-17
# 
# Data from: 
# Benedicto, Ainho Ruiz; Akkerman, Mark & Pere Brunet (2020) A Walled World. Towards 
# A Global Apartheid, Centre Delàs Report 46. URL: https://www.tni.org/files/publication-downloads/informe46_walledwolrd_centredelas_tni_stopwapenhandel_stopthewall_eng_def.pdf

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, countrycode, rio)

# Read table (transformed to .xlsx with Adobe)
walledworld.df <- import("./data/border data/Benedicto et al. 2020 - A Walled World.xlsx", 
                         range = "D2:G73") %>%
  select(state1 = 1, state2 = 2, year = 4, -3) %>%
  .[-c(12, 14, 41, 53, 66),] %>%
  fill(state1, .direction = "down") %>%
  mutate(across(everything(), ~qdap::bracketX(.)))

# Country names to iso3
walledworld.df <- walledworld.df %>%
  mutate(state1 = countrycode(state1, "country.name.en", "iso3c"),
         state2 = countrycode(state2, "country.name.en", "iso3c"))

# Fill missing/incorrect values 
# original table "201" fill with 2015 according to source
walledworld.df[walledworld.df$state1 == "KEN" & walledworld.df$state2 == "SOM", "year"] <- "2015"

# Sahrawi territories coded as ESH (Western Sahara)
walledworld.df[walledworld.df$state1 == "MAR" & is.na(walledworld.df$state2), "state2"] <- "ESH"

# Export data
saveRDS(walledworld.df, file = "./data/border data/Benedicto et al 2020.rds")

