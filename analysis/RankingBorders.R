# Ranking borders concerning GDP difference
# Data created in SourceData.R

# Note: This script is explorative. For instance, NA's are just removed. 

# Packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse)

# Transform the dyadic data into a data frame of unique borders
# see: https://stackoverflow.com/a/28574764/8484219
border.rank <- border.df %>%
  mutate(key = paste0(pmin(state1, state2), pmax(state1, state2), sep = "")) %>%
  distinct(key, .keep_all = T) %>%
  filter(!is.na(state1.gdp) & !is.na(state2.gdp)) %>%
  mutate(gdp_diff = abs(state1.gdp - state2.gdp)) %>%
  arrange(desc(gdp_diff))

# How many states are left?
with(border.rank, c(state1, state2)) %>%
  n_distinct()

# Have a look at the top25 most unequal borders
border.rank[,c("key", "gdp_diff")][1:25,]
