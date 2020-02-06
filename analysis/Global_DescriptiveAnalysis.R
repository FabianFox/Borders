# Border Data: Model

# Notes & Issues
# border.df features the case MMR/PAK, which does not share a common border

                            ###################
                            #      SETUP      #
                            ###################

# Load/install packages
## -------------------------------------------------------------------------- ##
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, janitor)

                            ###################
                            #      DATA       #
                            ###################

# Load: Source data and border indicator
### ------------------------------------------------------------------------ ###
# Source data
source("SourceData.R")

# Indicator
indicator.df <- import("Y:\\Grenzen der Welt\\Grenzdossiers\\Typologie\\BorderTypology.xlsx",
                       sheet = 1, na = "NA") %>%
  as_tibble() %>%
  select(1:3, 16) %>%
  filter(!is.na(typology)) %>%
  clean_names()

# Merge
# Duplicates because some countries share multiple borders, i.e. MAR/ESP (Ceuta + Melilla) 
border.df <- indicator.df %>%
  left_join(border.df) %>%
  distinct(state1, state2, .keep_all = TRUE) %>%
  mutate(
    continent1 = countrycode(state1, "iso3c", "continent", custom_match = c("XKX" = "Europe")),
    continent2 = countrycode(state2, "iso3c", "continent", custom_match = c("XKX" = "Europe")),
    region1 = countrycode(state1, "iso3c", "region", custom_match = c("XKX" = "Southern Europe")),
    region2 = countrycode(state2, "iso3c", "region", custom_match = c("XKX" = "Southern Europe")))

                            ###################
                            #       EDA       #
                            ###################

# Exploratory analysis of indicator
### ------------------------------------------------------------------------ ###
# Distribution of indicator (in %)
ind.perc.fig <- border.df %>%
  group_by(typology) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count) * 100) %>%
  ggplot(mapping = aes(x = typology, y = perc)) +
  geom_bar(stat = "identity") 

# Distribution of indicator across continents
ind.perc.region.fig <- border.df %>%
  group_by(typology, continent1) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count) * 100) %>%
  ggplot(mapping = aes(x = typology, y = perc)) +
  geom_bar(stat = "identity") +
  facet_wrap(~continent1)

# 
### ------------------------------------------------------------------------ ###