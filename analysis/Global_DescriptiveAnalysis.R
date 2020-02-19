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
  filter(!is.na(typology),
         !(state1 == "ARE" & state2 == "QAT"),        # no shared border (since 1974) 
         !(state1 == "QAT" & state2 == "ARE")) %>%   # https://bit.ly/39EOy4Y
  clean_names() %>%
  distinct(state1, state2, .keep_all = TRUE)

# Add year of border installation
# Data from BordersJoin.R
barriers.df <- import("./analysis/Fence data/barriers_df.rds")

list.fences <- barriers.df %>%
  select(-indicator) %>%
  nest(data = c(-source)) %>%
  mutate(data = set_names(data, source))

list.fences <- map2(.x = list.fences$data, .y =  names(list.fences$data),
                    ~rename(.x, !!.y := year)) %>%
  map_df(., ~as_tibble(.x))

# Join
fortified_borders.df <- indicator.df %>%
  filter(typology %in% c("fortified border", "barrier border")) %>%
  left_join(list.fences, by = c("state1", "state2")) %>%
  as_tibble()

# Merge
# duplicates bc some countries share multiple borders, i.e. RUS/CHN
border.df <- indicator.df %>%
  left_join(border.df) %>%
  mutate(
    continent1 = countrycode(state1, "iso3c", "continent", custom_match = c("XKX" = "Europe")),
    continent2 = countrycode(state2, "iso3c", "continent", custom_match = c("XKX" = "Europe")),
    region1 = countrycode(state1, "iso3c", "region", custom_match = c("XKX" = "Southern Europe")),
    region2 = countrycode(state2, "iso3c", "region", custom_match = c("XKX" = "Southern Europe")))

# Dirty solution to create a directed dyadic typology
# (1) Duplicate dataset, swap country identifiers and rename them
swap.df <- border.df %>%
  select("state2", "state1", "typology") %>%
  rename(
    state1 = state2,
    state2 = state1,
    state2_typology = typology
  )

# (2) Join to original dataset
border.df <- border.df %>%
  left_join(swap.df) %>%
  rename(state1_typology = typology)

                            ###################
                            #       EDA       #
                            ###################

# Exploratory analysis of indicator
### ------------------------------------------------------------------------ ###
# Distribution of indicator (in %)
ind.perc.fig <- border.df %>%
  group_by(state1_typology) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count) * 100) %>%
  ggplot(mapping = aes(x = state1_typology, y = perc)) +
  geom_bar(stat = "identity") 

# Distribution of indicator across continents
ind.perc.region.fig <- border.df %>%
  group_by(state1_typology, continent1) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count) * 100) %>%
  ggplot(mapping = aes(x = typology, y = perc)) +
  geom_bar(stat = "identity") +
  facet_wrap(~continent1)

# Monadic
### ------------------------------------------------------------------------ ###

# Economy
# GDP, Polity IV, military capacity
# & main religion by typology
# --------------------------------- #
# Summary stats
border_monvars <- border.df %>%
  group_by(state1_typology) %>%
  summarise_at(vars(
    # economy
    state1_gdp,
    # politics
    state1_polity,
    # security
    state1_death_toll_3yrs,
    state1_military_expenditure_pc,
    state1_military_pers_pc,
    # culture
    # state1_relig
  ),
               list(~mean(., na.rm = T), 
                    ~sd(., na.rm = T), 
                    ~min(., na.rm = T), 
                    ~max(., na.rm = T), 
                    obs = ~sum(!is.na(.)))
  )

# Prepare
border_monvars <- border_monvars %>% 
  gather(var, value, -state1_typology) %>%
  mutate(measure = str_extract(var, "[:alpha:]+$"),
         variable = str_extract(var, paste0(".+(?=", measure, ")")) %>%
           str_sub(., end = -2)) %>%
  select(-var) %>%
  spread(measure, value)

# List-column
border_monvars.nest <- border_monvars %>%
  group_by(variable) %>%
  nest() %>%
  ungroup() %>%
  mutate(title = c(
    "Victims of terror incidents (2014-2017)",
    "GDP per capita in USD",
    "Military expenditure per one million population",
    "Military personnel per 1.000 population",
    "Political regime (PolityIV)"
  ),
  subtitle = c(
    "\nData: Global Terrorism Database",
    "\nData: WorldBank (2017)",
    "\nData: WorldBank (2017)",
    "\nData: WorldBank (2017)",
    "\nData: PolityIV (2017)"
  ))

# Plot
border_monvars.fig <- border_monvars.nest %>%
  mutate(plots = pmap(list(data, title, subtitle), ~ggplot(data = ..1) +
                        geom_bar(aes(x = state1_typology, y = mean), stat = "identity") +
                        labs(
                          title = ..2,
                          caption = ..3,
                          x = "", y = "") +
                        theme_minimal()
  ))

# Dyadic 
### ------------------------------------------------------------------------ ###

# Function that computes the absolute difference
absdiff_fun <- function(x,y) {
  abs(x - y)
}

# Summary stats
border.df <- border.df %>%
  mutate(
    # economy
    absdiff_GDP = absdiff_fun(state1_gdp, state2_gdp),
    # politics
    absdiff_Polity = absdiff_fun(state1_polity, state2_polity),
    # security
    absdiff_death_toll_3yrs = absdiff_fun(state1_death_toll_3yrs, state2_death_toll_3yrs),
    absdiff_military_expenditure_pc = absdiff_fun(state1_military_expenditure_pc, state2_military_expenditure_pc),
    absdiff_military_pers_pc = absdiff_fun(state1_military_pers_pc, state2_military_expenditure_pc))

border_dyadvars <- border.df %>%
  group_by(state1_typology) %>%
  summarise(
    # economy
    neighbour_absdiffGDP_median = median(absdiff_GDP, na.rm = TRUE),
    # politics
    neighbour_absdiffPolity_median = median(absdiff_Polity, na.rm = TRUE),
    # security
    neigbour_absdiffDeathToll3Y_median = median(absdiff_death_toll_3yrs, na.rm = TRUE),
    neigbour_absdiffMilitaryExpenditure_median = median(absdiff_military_expenditure_pc, na.rm = TRUE),
    neighbour_absdiffMilitaryPersonnel_median = median(absdiff_military_pers_pc, na.rm = TRUE)
    )

# Prepare
border_dyadvars <- border_dyadvars %>% 
  gather(var, value, -state1_typology) %>%
  mutate(measure = str_extract(var, "[:alpha:]+$"),
         variable = str_extract(var, paste0(".+(?=", measure, ")")) %>%
           str_sub(., end = -2)) %>%
  select(-var) %>%
  spread(measure, value)

# List-column
border_dyadvars.nest <- border_dyadvars %>%
  group_by(variable) %>%
  nest() %>%
  ungroup() %>%
  mutate(title = c(
    "Victims of terror incidents (in bordering state) (2014-2017)",
    "GDP per capita in USD (of neighbour)",
    "Military expenditure per one million population (by neighbour)",
    "Military personnel per 1.000 population (in bordering state)",
    "Political regime (PolityIV) (of neigbour)"
  ),
  subtitle = c(
    "\nData: Global Terrorism Database",
    "\nData: WorldBank (2017)",
    "\nData: WorldBank (2017)",
    "\nData: WorldBank (2017)",
    "\nData: PolityIV (2017)"
  ))

# Plot
border_dyadvars.fig <- border_dyadvars.nest %>%
  mutate(plots = pmap(list(data, title, subtitle), ~ggplot(data = ..1) +
                        geom_bar(aes(x = state1_typology, y = median), stat = "identity") +
                        labs(
                          title = ..2,
                          caption = ..3,
                          x = "", y = "") +
                        theme_minimal()
  ))
