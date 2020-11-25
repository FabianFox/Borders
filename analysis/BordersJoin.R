# Join existing data on border barriers to our source data
### ------------------------------------------------------------------------ ###

# Data from: 
# - barriers.bw : Jones (2012) Border Walls: Security and the War on Terror, London: Zed Books, p. 10.
# - barriers.wiki : https://en.wikipedia.org/wiki/Border_barrier
# - barriers.hw : Hassner & Wittenberg (2015) https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/U16NEI
# - barrier.cp : Carter & Poast (2017) http://www.paulpoast.com/#/original-data/4590503653
# - barriers.jg : Jellissen & Gottheil (2013) On the utility of security fences along international borders
# - barriers.ln : Linnell et al. (2013) Border Security Fencing and Wildlife
# - barriers.av : Avdan (2019) Visas and Walls
# - barriers.bap : Benedicto et al. (2020) A Walled World

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
barriers.bw.join <- import("./data/border data/Jones 2012.rds") %>%
  select(vars) %>%
  mutate(year = as.character(year)) 

# Wikipedia: Border Barrier
# generated in: "BorderBarriers_Wiki.R"
barriers.wiki.join <- readRDS("./data/border data/barriers.wiki.RDS") %>%
  select(vars)

# Hassner & Wittenberg (2015)
# generated in: "Hassner&Wittenberg2015 - BarriersToEntry_Data.R"
barriers.hw.join <- import("./data/border data/Hassner & Wittenberg 2015.rds") %>%
  select(vars) %>%
  mutate(year = as.character(year))

# Jellissen & Gottheil (2013)
# generated in: "Jellissen&Gottheil2013 - OnTheUtilityOfSecurityFencesAlongInternationalBorders.R"
barriers.jg.join <- readRDS("./data/border data/Jellissen & Gottheil 2013.rds") %>%
  select(vars) # Check year variable

# Linnell et al. (2013)
# generated in: "Linnell2013 - BorderSecurityFencingAndWildlife.R"
barriers.ln.join <-  import("./data/border data/Linnell et al. 2016.rds") %>%
  select(vars)
  
# Avdan (2019)
barriers.av.join <- readRDS("./data/border data/Avdan 2019.rds") %>%
  select(vars) %>%
  mutate(year = as.character(year))

# Benedict et al. (2020)
barriers.bap.join <- import("./data/border data/Benedicto et al 2020.rds") %>%
  mutate(indicator = "border walls",
         source = "Benedicto et al. (2020)") %>%
  select(vars)

# Join
### ------------------------------------------------------------------------ ###
barriers.df <- bind_rows(mget(ls()[which(str_detect(ls(), "join") == TRUE)]))

# Export
# export(barriers.df, "./analysis/Fence data/barriers_df.rds")

# Count occurences and remove duplicated borders
# Note: Some datasets do not consider direction of border fortification
barriers.df.join <- barriers.df %>%
  group_by(state1, state2) %>%
  mutate(n = n()) %>%
  select(state1, state2, n) %>%
  filter(n >= 1) %>%
  distinct() %>%
  arrange(state1, state2)

# Housekeeping
### ------------------------------------------------------------------------ ###
rm(list = setdiff(ls(), 
                  c(ls()[which(str_detect(ls(), "join") == TRUE)], "barriers.df")))

# Export
export(barriers.df, "./analysis/Fence data/barriers_df.rds")
