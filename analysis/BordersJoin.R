# Join existing data on border barriers to our source data
### ------------------------------------------------------------------------ ###

# Data from: 
# - barriers.bw : Jones (2012) Border Walls: Security and the War on Terror, London: Zed Books, p. 10.
# - barriers.wiki : https://en.wikipedia.org/wiki/Border_barrier
# - barriers.hw : Hassner & Wittenberg (2015) https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/U16NEI
# - barriers.jg : Jellissen & Gottheil (2013) On the utility of security fences along international borders
# - barriers.ln : Linnell et al. (2013) Border Security Fencing and Wildlife

# Load/install packages
### ------------------------------------------------------------------------ ###
if(!require("pacman")) install.packages("pacman")
p_load(tidyverse)

### Source data created in "SourceData.R" ###

# Load existing data sets
### ------------------------------------------------------------------------ ###

# Select variables to keep for the join
vars <- c("state1", "state2", "year", "indicator", "source")

# Reece (2012)
# generated in: "Jones2012 - BorderWalls_Data.R"
barriers.bw.join <- barriers.bw %>%
  select(vars) %>%
  mutate(year = as.character(year)) 

# Wikipedia: Border Barrier
# generated in: "BorderBarriers_Wiki.R"
barriers.wiki.join <- readRDS("./data/barriers.wiki.RDS") %>%
  select(vars)

# Hassner & Wittenberg (2015)
# generated in: "Hassner&Wittenberg2015 - BarriersToEntry_Data.R"
#  

# Jellissen & Gottheil (2013)
# generated in: "Jellissen&Gottheil2013 - OnTheUtilityOfSecurityFencesAlongInternationalBorders.R"
barriers.jg.join <- readRDS("./data/Jellissen & Gottheil 2013.rds") %>%
  select(vars) # Check year variable

# Linnell et al. (2013)
# generated in: "Linnell2013 - BorderSecurityFencingAndWildlife.R"
barriers.ln.join <- barriers.ln %>%
  select(vars)
  
# Join
### ------------------------------------------------------------------------ ###
barriers.df <- bind_rows(mget(ls()[which(str_detect(ls(), "join") == TRUE)]))

# Count occurences and remove duplicated borders
# Note: Some datasets do not consider direction of border fortification
barriers.df.join <- barriers.df %>%
  group_by(state1, state2) %>%
  mutate(n = n()) %>%
  select(state1, state2, n) %>%
  filter(n >= 1)

# Housekeeping
### ------------------------------------------------------------------------ ###
rm(list = setdiff(ls(), 
                  c(ls()[which(str_detect(ls(), "join") == TRUE)], "barriers.df")))

