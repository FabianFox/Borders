# Replication data from Hassner/Wittenberg (2015)
# 
# Data from: 
# https://www.mitpressjournals.org/doi/10.1162/ISEC_a_00206
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/U16NEI

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, countrycode, rio, haven)

# Data available in dta-format

# custom match for North Korea
custom <- c("Democratic PRK" = "PRK")

# Read data and adjust variables
barriers.hw <- read_dta("./data/barrierstoentry.dta") %>%
  select(state1 = builder, state2 = target, year = begin, 
         end, walllength) %>%
  filter(year > 1970) %>%                                       # earlier periods retain ceased nations                          
  mutate(state1 = countrycode(state1, "country.name", "iso3c", 
                              custom_match = custom),
         state2 = countrycode(state2, "country.name", "iso3c", 
                              custom_match = custom),
         indicator = "fortified",
         source = "Hassner&Wittenberg (2013)")