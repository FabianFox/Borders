# Exploratory analysis of African borders

# Notes:
# - Countries that need further checking:

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "countrycode", "rio", "janitor", "ggrepel",
            "patchwork")

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
##                            Data preparation                                ##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# Load: Source data and border indicator
### ------------------------------------------------------------------------ ###
# Source data
# source("SourceData.R")
border.df <- import("./output/border.rds")

# Indicator
indicator.df <- import("Y:\\Grenzen der Welt\\Grenzdossiers\\Typologie\\BorderTypology.xlsx",
                       sheet = 1, na = "NA") %>%
  as_tibble() %>%
  select(1:3, 16) %>%
  filter(!is.na(typology),
         !(state1 == "ARE" & state2 == "QAT"),        # no shared border (since 1974) 
         !(state1 == "QAT" & state2 == "ARE")) %>%    # https://bit.ly/39EOy4Y
  clean_names() %>%
  distinct(state1, state2, .keep_all = TRUE)

# INDICATOR
# Limit observations to African countries
### ------------------------------------------------------------------------ ###
custom.match <- c("XKX" = "Europe")

africa.df <- indicator.df %>%
  mutate_at(
    vars(contains("state")),
    funs(cont = countrycode(., "iso3c", "continent",
      custom_match = custom.match
    ))
  ) %>%
  filter(state1_cont == "Africa" | state2_cont == "Africa")

# Conditions for border indicator
### ------------------------------------------------------------------------ ###
# landmark
# frontier border: fortifications == none & BCP == none (& border agreement == none)
# checkpoint border: fortification == none & BCP == basic | extended
# barrier border: fence / wall / additional_fortification == yes (but partially!)
# fortified border: fence / wall / additional_fortification == yes

# SOURCE DATA
# Prepare border.df
## -------------------------------------------------------------------------- ##
border.df <- border.df %>%
  mutate(
    continent1 = countrycode(state1, "iso3c", "continent", custom_match = c("XKX" = "Europe")),
    continent2 = countrycode(state2, "iso3c", "continent", custom_match = c("XKX" = "Europe")),
    region1 = countrycode(state1, "iso3c", "region", custom_match = c("XKX" = "Southern Europe")),
    region2 = countrycode(state2, "iso3c", "region", custom_match = c("XKX" = "Southern Europe"))
  )

# Join the source and indicator
## -------------------------------------------------------------------------- ##
# duplicates bc some countries share multiple borders, i.e. RUS/CHN
africa.df <- border.df %>%
  filter(continent1 == "Africa" | continent2 == "Africa") %>%
  left_join(africa.df) %>%
  distinct(state1, state2, .keep_all = TRUE) %>%
  select(state1, state2, typology, state1_gdp, state2_gdp, state1_polity, state2_polity,
         state1_relig, state2_relig, continent1, continent2)

# Add a measure for partitioned ethnicities
# Source: Michalopoulos, S., & Papaioannou, E. (2016). The Long-Run Effects of the 
#         Scramble for Africa. American Economic Review, 106(7), 1802-1848. 
#         doi:10.1257/aer.20131311
## -------------------------------------------------------------------------- ##
# Load dataset and clean
ethn.df <- import("./data/Appendix_TableA_Partitioned_Ethnicities_Michalopoulos_et_al_2016.xlsx",
                  sheet = 1, range = "A2:E538") %>%
  as_tibble() %>%
  clean_names() %>%
  fill(no) %>%
  filter(x_percent_of_homeland >= 0.1) %>%        # only those that share at least 10% population
  select(-x_number_partitions) %>%                # from here: unfold country column, i.e. make long
  group_by(ethnicity_name) %>% 
  summarise(country  = paste(country_code, collapse =",")) %>%
  separate(country, into = c("m1", "m2", "m3", "m4")) %>%        # ignore warning message ("Expected 4 pieces...")
  nest(-ethnicity_name) %>%
  mutate(data = map(data, ~(.x %>%
                              discard(is.na))),
         colnum = map(data, ncol)) %>%
  filter(colnum > 1) %>%
  mutate(data = map(data, ~(.x %>%
                              combn(., 2, function(x) paste(x, collapse = "_"), simplify = FALSE)))) %>%
  unnest(data) %>%
  separate(data, into = c("state1", "state2"), sep = "_") 
  
# Dirty solution to create a directed typology
# (1) Duplicate dataset, swap country identifiers and rename them
swap.df <- ethn.df %>%
  rename(
    state1 = state2,
    state2 = state1
  )

# (2) Bind back together and compute number of shared ethnicities across border
ethn.df <- ethn.df %>%
  bind_rows(., swap.df) %>%
  group_by(state1, state2) %>%
  mutate(num_share_ethn = n()) %>%
  distinct(state1, state2, .keep_all = TRUE) %>%
  select(-ethnicity_name)

# (3) Join to africa.df
africa.df <- africa.df %>%
  left_join(ethn.df) %>%
  mutate(share_ethn = ifelse(is.na(num_share_ethn), 0, 1),
         num_share_ethn = ifelse(is.na(num_share_ethn), 0, num_share_ethn))

# Add missing GDP from CIA World Factbook
## -------------------------------------------------------------------------- ##
africa.df[africa.df$state1 == "ERI", "state1_gdp"] <- 1600
africa.df[africa.df$state2 == "ERI", "state2_gdp"] <- 1600

# Setup for the analysis and plots:
# (1) Functions that create a factor variable of the indicator
# (2) Theme for the plots
## -------------------------------------------------------------------------- ##

# (1) Factor variable
# English
fac_ind_en <- function(x) {
  factor(x, levels = c("landmark border", "frontier border", "checkpoint border", 
                       "barrier border", "fortified border"),
         labels = c("Landmark Border", "'No-man's-land' Border", "Checkpoint Border",
                    "Barrier Border", "Fortified Border"))
}

# Short version
fac_ind_en_short <- function(x) {
  factor(x, levels = c("landmark border", "frontier border", "checkpoint border", 
                       "barrier border", "fortified border"),
         labels = c("Landmark", "'No-man's-land'", "Checkpoint", 
                       "Barrier", "Fortified Border"))
}

# (2) Theme for the plots
theme.basic <- theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
##                         Monadic data analysis                              ##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# Monadic data analysis
## -------------------------------------------------------------------------- #
# Basic description
# Number of African states (p. 9)
africa.dim <- tibble(
  countries = with(africa.df, unique(c(state1, state2))),
  continent = countrycode(countries, "iso3c", "continent")
) %>%
  group_by(continent) %>%
  mutate(n_states = length(continent)) %>%
  distinct(continent, n_states, .keep_all = TRUE)

# Number of countries
n_countries <- sum(africa.dim$n_states)

# Number of bidrectional dyads
n_borders <- dim(africa.df)[1]

# Breakdown of independent variables
## -------------------------------------------------------------------------- #
# Highest GDP difference overall: GNQ and BDI (excluding Israel and Spain)
max_gdp_diff.df <- africa.df %>%
  filter(continent1 == "Africa" & !is.na(state1_gdp)) %>%
  select(state1, state1_gdp) %>%
  arrange(desc(state1_gdp)) %>%
  filter(row_number() == 1 | row_number() == n()) %>%
  mutate(state1_gdp = round(state1_gdp, 0))

# Ratio: 33
round(max_gdp_diff.df[1, 2] / max_gdp_diff.df[2, 2], 0)

# In contiguous dyads
# Libya has 11.1 times the GDP of Niger
# South Africa has 13.3 times the GDP of Mozambique
max_gdp_diff_cont.df <- africa.df %>%
  filter(continent1 == "Africa" & continent2 == "Africa" & 
           !is.na(state1_gdp) & !is.na(state2_gdp)) %>%
  select(state1, state2, state1_gdp, state2_gdp) %>%
  mutate(gdp_ratio = state1_gdp / state2_gdp) %>%
  arrange(desc(gdp_ratio)) %>%
  slice(1:3)
  
# Overview: Dependent variable
# FIGURE 2
# Indicator
# --------------------------------- #
# Distribution of border types
# Absolute
ind.dist.fig <- ggplot(africa.df, aes(x = fac_ind_de(typology))) +
  geom_bar() +
  theme.basic

# Distribution
# Relative (FIGURE 2)
ind.perc.fig <- africa.df %>%
  group_by(typology) %>%
  summarise(percentage = length(typology) / length(africa.df$typology) * 100,
            count = n()) %>%
  ggplot(aes(x = fac_ind_en(typology), y = percentage)) +
  geom_bar(stat = "identity") +
  geom_text(stat = "identity", aes(label = paste0("N = ", count)), vjust = -1) +
  labs(
    caption = paste0(
      "Number of borders: ", length(africa.df$typology),
      "\nNumber of states: ", length(unique(africa.df$state1))
    ),
    x = "", y = ""
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme.basic +
  theme(axis.text.x = element_text(angle = 0, hjust = .5))

# Overview
africa.df %>%
  group_by(typology) %>%
  summarise(percentage = round(length(typology) / length(africa.df$typology) * 100, 1),
            count = n())

# Bivariate
# --------------------------------- #

# Facetted scatterplot: GDP x PolityIV
## -------------------------------------------------------------------------- ##

# FIGURE 3
# --------------------------------- #
# A (1) Add grouped mean of GDP and PolityIV
gdp_pol.df <- africa.df %>%
  filter(!is.na(state1_gdp) & !is.na(state1_polity)) %>%
  group_by(typology) %>%
  mutate(
    median_gdp = median(log(state1_gdp)),
    median_polity = median(state1_polity)
  )

# A (2) Facetted scatterplot
set.seed(42); gdp_pol.fig <- ggplot(data = gdp_pol.df) +
  geom_jitter(aes(x = log(state1_gdp), y = state1_polity, 
                  color = factor(share_ethn, labels = c("No", "Yes")))) +
  facet_grid(~ fac_ind_en(typology)) +
  geom_hline(aes(yintercept = median_polity, group = typology), colour = "black", 
             alpha = .3, size = 1.5) +
  geom_vline(aes(xintercept = median_gdp, group = typology), colour = "black", 
             alpha = .3, size = 1.5) +
  scale_x_continuous(breaks = log(c(400, 1000, 3000, 8000, 20000)), 
                     labels = c(400, 1000, 3000, 8000, 20000)) +
  scale_colour_manual(values  = c("No" = "grey", "Yes" = "black"), 
                      guide = guide_legend(title = "Shared ethnicity")) +
  labs(x = "GDP p.c. (log.)", y = "Political system",
       caption = paste0(
         "Number of borders: ", length(gdp_pol.df$state1),
         "\nNumber of states: ", length(unique(gdp_pol.df$state1)))) +
  theme.basic

# Mediane
gdp_pol.df %>%
  distinct(typology, median_gdp, median_polity) %>%
  mutate(median_exp_gdp = round(exp(median_gdp), 0)) %>%
  arrange(fac_ind_en_short(typology))

# Number of dyads with shared transnational ethnicities
n_ethn <- africa.df %>%
  group_by(typology) %>%
  summarize(n_ethn = sum(share_ethn), 
            count = n(),
            missing = sum(is.na(share_ethn))) %>%
  mutate(percentage = n_ethn/count * 100)

# Total number of shared ethnicities (irrespective of border type)
africa.df %>% 
  summarise(mean_ethn = mean(share_ethn))

# B (1) Zoomed in scatterplot for barrier & fortified borders
# Figure 6
# --------------------------------- #
gdp_pol_fort.df <- gdp_pol.df %>%
  filter(typology %in% c("fortified border", "barrier border"))

# B (2)                         
# ADD N_borders and N_countries
set.seed(42); gdp_pol_fort.fig <- ggplot(data = gdp_pol_fort.df, 
                           aes(x = log(state1_gdp), y = state1_polity,
                               color = factor(share_ethn, labels = c("No", "Yes")))) +
  geom_jitter() +
  geom_text_repel(label = paste(gdp_pol_fort.df$state1, gdp_pol_fort.df$state2, sep = "-"),
                  segment.color = NA, key_glyph = "point") +
  facet_grid(~ fac_ind_en(typology)) +
  geom_hline(aes(yintercept = median_polity, group = typology), colour = "black",
             alpha = .3, size = 1.5) +
  geom_vline(aes(xintercept = median_gdp, group = typology), colour = "black",
             alpha = .3, size = 1.5) +
  scale_colour_manual(values  = c("No" = "grey", "Yes" = "black"), 
                      guide = guide_legend(title = "Shared ethnicity")) +
  scale_x_continuous(breaks = log(c(400, 1000, 3000, 8000, 20000)), labels = c(400, 1000, 3000, 8000, 20000)) +
  labs(x = "GDP p.c. (log.)", y = "Political system",
       caption = paste0(
         "Number of borders: ", length(gdp_pol_fort.df$state1),
         "\nNumber of states: ", length(unique(gdp_pol_fort.df$state1)))) +
  theme.basic

# World Religion (CoW)
# --------------------------------- #
relig.df <- africa.df %>%
  filter(state1 != "ISR") %>%
  group_by(state1_relig, typology) %>%
  summarise(n = n()) %>%
  group_by(state1_relig) %>%
  mutate(percentage = n / sum(n))

# Figure 4
relig.fig <- relig.df %>%
  ggplot() +
  geom_bar(aes(x = fac_ind_en_short(typology), y = percentage), 
               stat = "identity") +
  geom_text(stat = "identity", aes(x = fac_ind_en_short(typology), y = percentage, 
                                   label = paste0("N = ", n)), vjust = -1) +
  facet_wrap(.~factor(state1_relig, 
                      levels = c("chrst", "islm", "jud"),
                      labels = c("Christian", "Islamic", "Jewish"))) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  labs(x = "", y = "", fill = "Majority religion",
       caption = paste0(
         "Number of borders: ", sum(relig.df$n),
         "\nNumber of states: ", length(unique(africa.df[!is.na(africa.df$state1_relig),]$state1)))) +
  theme.basic


## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
##                           Dyadic data analysis                             ##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# Data preparation
# Note: Several of the following steps are not strictly necessary as we do not 
#       perform a full dyadic analysis. 

# Dirty solution to create a directed typology
# (1) Duplicate dataset, swap country identifiers and rename them
swap.df <- africa.df %>%
  select("state2", "state1", "typology") %>%
  rename(
    state1 = state2,
    state2 = state1,
    state2_typology = typology
  )

# (2) Join to original dataset
africa_dyad.df <- africa.df %>%
  left_join(swap.df) %>%
  rename(state1_typology = typology)

# Compute descriptive statistics
## -------------------------------------------------------------------------- ##

# (Absolute) Difference between state1 and state2
africa_dyad.df <- africa_dyad.df %>%
  mutate(
    
    # GDP
    diffGDP = state1_gdp - state2_gdp,
    absdiffGDP = abs(state1_gdp - state2_gdp),
    
    # Polity
    diffPol = state1_polity - state2_polity,
    absdiffPol = abs(state1_polity - state2_polity))


# Neighbour characteristics
neighbour_char <- africa_dyad.df %>%
  group_by(state1_typology) %>%
  summarise(
    # GDP
    mean_neighbourGDP = mean(state2_gdp, na.rm = TRUE),
    median_neighbourGDP = median(state2_gdp, na.rm = TRUE),
    mean_neighbour_absdiffGDP = mean(absdiffGDP, na.rm = TRUE),
    median_neighbour_absdiffGDP = median(absdiffGDP, na.rm = TRUE),
    # Polity
    mean_neighbourPol = mean(state2_polity, na.rm = TRUE),
    median_neighbourPol = median(state2_polity, na.rm = TRUE),
    mean_neighbour_absdiffPol = mean(absdiffPol, na.rm = TRUE),
    median_neighbour_absdiffPol = median(absdiffPol, na.rm = TRUE),
    
    n = n()) %>%
  arrange(fac_ind_en(state1_typology))

# Number of observations
# GDP
# Observations
n_gdp_obs.df <- africa_dyad.df %>%
  filter(!is.na(state1_gdp) & !is.na(state2_gdp)) %>%
  count(state1_typology = state1_typology, name = "n_gdp_obs")

# States
n_gdp_state.df <- africa_dyad.df %>%
  filter(!is.na(state1_gdp) & !is.na(state2_gdp)) %>%
  summarise(n_gdp_state = length(unique(state1)))

# Polity
# Observations
n_pol_obs.df <- africa_dyad.df %>%
  filter(!is.na(state1_polity) & !is.na(state2_polity)) %>%
  count(state1_typology = state1_typology, name = "n_pol_obs")

# States
n_pol_state.df <- africa_dyad.df %>%
  filter(!is.na(state1_polity) & !is.na(state2_polity)) %>%
  summarise(n_pol_state = length(unique(state1)))

# Pull together
neighbour_char <- list(neighbour_char, n_gdp_obs.df, n_pol_obs.df) %>%
  reduce(left_join, by = "state1_typology")

# Plots
## -------------------------------------------------------------------------- ##

# Plots of neighbour characteristics by state1's typology
## -------------------------------------------------------------------------- ##

# Figure 5
# Neigbours GDP (mean absdiff)
gdp_neighbour_absdiff.fig <- ggplot(neighbour_char) +
  geom_bar(aes(x = fac_ind_en_short(state1_typology), y = median_neighbour_absdiffGDP), 
           stat = "identity") +
  geom_text(aes(x = fac_ind_en_short(state1_typology), y = median_neighbour_absdiffGDP, 
            label = paste0("N=", n_gdp_obs)), vjust = -0.5) +
  labs(x = "", y = "", title = "GDP p.c. (USD)",
       caption = paste0(
         "Number of borders: ", sum(n_gdp_obs.df$n_gdp_obs),
         "\nNumber of states: ", n_gdp_state.df)) +
  theme.basic

# Neigbours Polity (mean absdiff)
pol_neighbour_absdiff.fig <- ggplot(neighbour_char) +
  geom_bar(aes(x = fac_ind_en_short(state1_typology), y = median_neighbour_absdiffPol),
    stat = "identity"
  ) +
  geom_text(aes(
    x = fac_ind_en_short(state1_typology), y = median_neighbour_absdiffPol,
    label = paste0("N=", n_pol_obs)
  ), vjust = -0.5) +
  labs(
    x = "", y = "", title = "Political system",
    caption = paste0(
      "Number of borders: ", sum(n_pol_obs.df$n_pol_obs),
      "\nNumber of states: ", n_pol_state.df)) +
  theme.basic

# Arrange
neighbour.fig <- gdp_neighbour_absdiff.fig | pol_neighbour_absdiff.fig

# Direction of the differences, particularly for large differences, i.e. 
# fortified borders and frontier borders
direction_diff <- africa_dyad.df %>%
  select(state1, state2, state1_typology, state2_typology, state1_polity, state2_polity, diffPol,
         absdiffPol, state1_gdp, state2_gdp, diffGDP, absdiffGDP
         ) %>%
  filter(!is.na(state1_polity) & !is.na(state2_polity) & !is.na(state1_gdp) & !is.na(state2_gdp)) %>%
  arrange(state1_typology) %>%
  mutate(higher_pol = if_else(state1_polity >= state2_polity, 1, 0),
         higher_gdp = if_else(state1_gdp >= state2_gdp, 1, 0),
         symmetric = ifelse(state1_typology == state2_typology, 1, 0)) %>%
#  filter(symmetric == 0) 
  group_by(state1_typology) %>%
  summarise(share_direction_pol = sum(higher_pol) / n() * 100,
            share_direction_gdp = sum(higher_gdp) / n() * 100) 

# Figure 6
# Neighbour religion (COW)
relig_neighbour.df <- africa_dyad.df %>%
  filter(state2 != "ISR") %>%
  group_by(state2_relig, state1_typology) %>%
  summarise(n = n()) %>%
  group_by(state2_relig) %>%
  mutate(percentage = n / sum(n))

relig_neighbour.fig <- relig_neighbour.df %>%
  ggplot() +
  geom_bar(aes(x = fac_ind_en_short(state1_typology), y = percentage), 
           stat = "identity") +
  geom_text(stat = "identity", aes(x = fac_ind_en_short(state1_typology), y = percentage,
                                   label = paste0("N = ", n)), vjust = -1) +
  facet_wrap(.~factor(state2_relig, 
                      levels = c("chrst", "islm", "jud"),
                      labels = c("Christian", "Islamic", "Jewish"))) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  labs(x = "", y = "", fill = "Majority religion",
       caption = paste0(
         "Number of borders: ", sum(relig_neighbour.df$n),
         "\nNumber of states: ", length(unique(africa_dyad.df[!is.na(africa_dyad.df$state2_relig),]$state1)))) +
  theme.basic

# Descriptive summary of independent
# variables (Fig Appendix 1)
# --------------------------------- #
vars <- c("state1_gdp", "state1_polity", "share_ethn")

africa_descriptive <- africa.df %>%
  summarise_at(vars, list(~mean(., na.rm = T), 
                          ~sd(., na.rm = T), 
                          ~min(., na.rm = T), 
                          ~max(., na.rm = T), 
                          obs = ~sum(!is.na(.)))
  ) %>%
  mutate_all(~round(., digits = 3))

# Prepare
# Wide
africa_descriptive <- africa_descriptive %>% 
  gather(var, value) %>%
  mutate(measure = str_extract(var, "[:alpha:]+$"),
         variable = str_extract(var, paste0(".+(?=", measure, ")"))) %>%
  select(-var) %>%
  spread(measure, value) %>%
  mutate(ymin = mean - sd,
         ymax = mean + sd)

# List-column (needs some work...)
africa_descriptive.nest <- africa_descriptive %>%
  group_by(variable) %>%
  nest() %>%
  mutate(plot = map2(data, variable, ~ggplot(data = .x) +
                       geom_errorbar(aes(x = 1, ymin = ymin, ymax = ymax), stat = "identity") +
                       geom_point(aes(x = 1, y = mean) +
                                    theme.basic
                       )))

# Continuous variables
# Divide certain variables to get them on a common scale
# (1) GDP per capita 
africa_descriptive[africa_descriptive$variable == "state1_gdp_", 
                   c("max", "mean", "min", "sd", "ymin", "ymax")] <- 
  africa_descriptive[africa_descriptive$variable == "state1_gdp_", 
                     c("max", "mean", "min", "sd", "ymin", "ymax")] %>% 
  mutate_all(funs(./1000))

# Graphical display of descriptive statistics
africa_descriptive.fig <- africa_descriptive %>%
  ggplot() +
  geom_point(aes(x = mean, y = variable)) +
  geom_errorbarh(aes(y = variable, xmin = ymin, xmax = ymax), height = .1) +
  geom_text(stat = "identity", aes(x = mean, y = variable, 
                                   label = paste0("N = ", obs)), vjust = 2) + 
  scale_x_continuous(breaks = seq(-10, 10, 2), limits = c(-10, 10)) +
  scale_y_discrete(breaks = c("state1_gdp_", "share_ethn_", "state1_polity_"),
                   labels = c("GDP p.c.\n(in $1.000)", 
                              "Shared ethnicity", 
                              "Political system\n(PolityIV)")) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5)
  )

# Add figure of religious affiliation
relig_descriptive <- africa.df %>%
  group_by(state1_relig) %>%
  count() %>%
  ungroup() %>%
  mutate(percentage = n / sum(n)) 

relig_descriptive.fig <- ggplot(data = relig_descriptive, 
                                aes(x = 
                                      factor(
                                        state1_relig, 
                                        label = c("Christian", "Islamic", "Jewish")),
                                    y = percentage)) +
  geom_bar(stat = "identity") +
  geom_text(stat = "identity", aes(label = paste0("N = ", n)), vjust = -0.5) +
  scale_fill_grey() +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(x = "", y = "", fill = "Majority religion", title = "Majority religion",
       caption = paste0(
         "Number of borders: ", sum(relig_descriptive$n),
         "\nNumber of states: ", length(unique(africa.df$state1)))
  ) +
  theme.basic

# Put the descriptive summary statistics together using patchwork
descriptive.fig <- africa_descriptive.fig | relig_descriptive.fig

# Save figures
## -------------------------------------------------------------------------- ##
folder <- "C:/Users/guelzauf/Seafile/Meine Bibliothek/Comparativ/"

# Monadic analysis
# --------------------------------- #

# Figure 2
# Relative distribution of border infrastructure
ggsave(
  plot = ind.perc.fig, str_c(folder, "Figure 2. Relative distribution of the border index.tiff"), width = 8, height = 6, unit = "in",
  dpi = 300
)

# Figure 3
# Scatterplot (A)
ggsave(
  plot = gdp_pol.fig, str_c(folder, "Figure 3. GDP per capita and political system according to the border index.tiff"), width = 9, height = 8, unit = "in",
  dpi = 300
)

# Figure 4
# Religion by typology
ggsave(
  plot = relig.fig, str_c(folder, "Figure 4. Majority religion in relation to the border index.tiff"), width = 8, height = 7, unit = "in",
  dpi = 300
)

# Neighbour characteristics
# Figure 5
# A) Polity & GDP
ggsave(
  plot = neighbour.fig, str_c(folder, "Figure 5. Border index absolute difference in GDP median and political system of a respective neighbouring state.tiff"),
  width = 8, height = 6, unit = "in", dpi = 300
)

# Figure 6
# B) Religion
ggsave(
  plot = relig_neighbour.fig, str_c(folder, "Figure 6. Border index and majority religion in neighbouring states.tiff"), width = 8, height = 7, unit = "in",
  dpi = 300
)

# Figure 7
# Scatterplot (B)
ggsave(
  plot = gdp_pol_fort.fig, str_c(folder, "Figure 7. GDP per capita and democracy index in relation to border type barrier borders
and fortified borders.tiff"), width = 8, height = 8, unit = "in",
  dpi = 300
)

# Appendix
# Figure A1
# Descriptive statistics
ggsave(
  plot = descriptive.fig, str_c(folder, "Figure A1. Descriptive summary of independent variables averages 1 standard variance.tiff"), width = 12.5, height = 8, unit = "in",
  dpi = 300
)

# Word table of observations
# --------------------------------- #
word.tbl <- africa.df %>%
  select(state1, state2, typology) %>%
  mutate(state1 = paste0(countrycode(state1, "iso3c", "country.name.de"), " (", state1, ")"),
         state2 = paste0(countrycode(state2, "iso3c", "country.name.de"), " (", state2, ")"),
         typology = case_when(
           typology == "landmark border" ~ "Grenzmarkierung",
           typology == "frontier border" ~ "Niemandslandgrenze",
           typology == "checkpoint border" ~ "Kontrollpunktgrenze",
           typology == "barrier border" ~ "Barrieregrenze",
           typology == "fortified border" ~ "fortifizierte Grenze"
         ))

export(word.tbl, "O:/Grenzen der Welt/Projekte/Afrikanische Grenzen/Daten/Indikator.csv")