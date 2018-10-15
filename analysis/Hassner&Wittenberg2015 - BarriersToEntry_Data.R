# Replication data from Hassner/Wittenberg (2015)
# 
# Data from: 
# https://www.mitpressjournals.org/doi/10.1162/ISEC_a_00206
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/U16NEI

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, countrycode, rio)

# Unfortunately, the data is saved as RData
barriers.hw <- get(load("./data/barrierstoentry.RData")) %>%
  select(state1 = builder, state2 = target, begin, end, walllength)

# Remove variables created through load
rm(x)

# Note: Should look at the article to get more information on the dataset.