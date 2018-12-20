# Join existing data on border barriers to our source data
### ------------------------------------------------------------------------ ###

# Data from: 
# barriers.bw - Jones, Reece (2012) Border Walls: Security and the War on Terror, London: Zed Books, p. 10.
# barriers.wiki - https://en.wikipedia.org/wiki/Border_barrier
# barriers.hw - Hassner & Wittenberg (2015) https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/U16NEI
# barriers.jg . Jellissen & Gottheil (2013) On the utility of security fences along international borders

# Load/install packages
### ------------------------------------------------------------------------ ###
if(!require("pacman")) install.packages("pacman")
p_load(tidyverse, pryr)

### Source data created in "SourceData.R" ###

# Load existing data sets
### ------------------------------------------------------------------------ ###

# Select variables to keep for the join
vars <- c("state1", "state2", "year", "indicator", "source")

# Reece (2012)
# generated in: Jones2012 - BorderWalls_Data.R
barriers.bw.join <- barriers.bw %>%
  select(vars) %>%
  mutate(year = as.character(year)) %>%
  rename(year = built)

# Wikipedia: Border Barrier
# generated in: BorderBarriers_Wiki.R
barriers.wiki.join <- barriers.wiki %>%
  select(vars) %>%
  rename(year = built)

# Hassner & Wittenberg (2015)
# generated in: Hassner&Wittenberg2015 - BarriersToEntry_Data.R
#  

# Jellissen & Gottheil (2013)
# generated in: Jellissen&Gottheil2013 - OnTheUtilityOfSecurityFencesAlongInternationalBorders.R
barriers.jg.join <- barriers.jg %>%
  select(vars) # Check year variable

# Join
### ------------------------------------------------------------------------ ###
barriers.df <- bind_rows(barriers.bw.join, barriers.jg.join, barriers.wiki.join)
