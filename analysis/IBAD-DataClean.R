### International Border Agreement Dataset
# Source: https://www.andrewowsiak.org/international-border-agreements-dataset.html
# Download date: 23.05.2019

# Load/install packages
## -------------------------------------------------------------------------- ##
if(!require("pacman")) install.packages("pacman")
p_load(tidyverse, countrycode, haven)

# Load data
## -------------------------------------------------------------------------- ##
ibad.df <- read_dta("C:/Users/guelzauf/Seafile/Meine Bibliothek/Projekte/C01_Grenzen/Data/Data/International Border Agreement Dataset/Replication - IBAD Full Settle Dyad-Year.dta")

# Clean and wrangle
## -------------------------------------------------------------------------- ##
# Some custom matches, i.e. 347 (Kosovo) = XKX, 345 (Serbia) = SRB 
custom.match <- c("345" = "SRB", "347" = "XKX", "816" = "VNM")

# Operations:
# (1) Countrycode: COW -> ISO3
# (2) Direct contiguity & no states without iso3 code
# (3) Latest observation
ibad.df <- ibad.df %>%
  mutate(state1 = countrycode(sourcevar = ccode1, origin = "cown", destination = "iso3c", custom_match = custom.match),
         state2 = countrycode(sourcevar = ccode2, origin = "cown", destination = "iso3c", custom_match = custom.match)) %>%
  filter(conttype == 1,
         !is.na(state1) & !is.na(state2),
         year == 2001) %>%
  select(state1, state2, settle, dyad) %>%
  group_by(dyad) %>%
  slice(n()) %>%
  ungroup() %>%
  arrange(state1, state2) %>%
  select(-dyad)

# Make the data directed (but note that border agreements are always mutually)
## -------------------------------------------------------------------------- ##
# (1) Duplicate dataset, swap country identifiers and rename them
ibad.swap.df <- ibad.df %>%
  select("state2", "state1", everything()) %>%
  rename(state1 = state2,
         state2 = state1)

# (2) Join to original dataset
ibad.df <- ibad.df %>%
  bind_rows(ibad.swap.df) %>%
  arrange(state1, state2)

# Save data
saveRDS(ibad.df, "./data/IBAD_Data.rds")
