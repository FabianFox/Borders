# Replication data from Hassner/Wittenberg (2015)
# 
# Data from: Jones, Reece (2012) Border Walls: Security and the War on Terror, London: Zed Books, p. 10.

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, countrycode, rio)

# Unfortunately, the data is saved as RData
barriers.hw <- get(load("./data/barrierstoentry.RData")) %>%
  select(state1 = builder, state2 = target, begin, end, walllength)

# Remove variables created through load
rm(x)

# Note: Should look at the article to get more information on the dataset.