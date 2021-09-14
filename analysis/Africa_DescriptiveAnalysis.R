# Exploratory analysis of African borders

# Notes:
# - Countries that need further checking:

# Issues:
# - Add religion to descriptive summary
# - set seed for reproducible jittering
# - add number of observations to remaining plots

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "countrycode", "igraph", "rio", "janitor", "ggrepel",
            "patchwork", "cowplot")

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

# indicator
indicator.df <- import("Y:\\Grenzen der Welt\\Grenzdossiers\\Typologie\\BorderTypology.xlsx",
  sheet = 1
) %>%
  as_tibble() %>%
  select(1:3, 16) %>%
  clean_names()

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
  distinct(state1, state2, .keep_all = TRUE)

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

# German
fac_ind_de <- function(x) {
  factor(x, levels = c("landmark border", "frontier border", "checkpoint border", 
                       "barrier border", "fortified border"),
         labels = c("Grenzmarkierung", "Niemandslandgrenze", "Kontrollpunktgrenze", 
                       "Barrieregrenze", "fortifizierte Grenze"))
}

# German shortened
fac_ind_de_short <- function(x) {
  factor(x, levels = c("landmark border", "frontier border", "checkpoint border", 
                       "barrier border", "fortified border"),
         labels = c("Grenz-\nmarkierung", "Niemands-\nlandgrenze", "Kontrollpunkt-\ngrenze", 
                    "Barriere-\ngrenze", "fortifizierte-\nGrenze"))
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
  filter(state1_cont == "Africa" & !is.na(state1_gdp)) %>%
  select(state1, state1_gdp) %>%
  arrange(desc(state1_gdp)) %>%
  filter(row_number() == 1 | row_number() == n()) %>%
  mutate(state1_gdp = round(state1_gdp, 0))

# Ratio: 33
round(max_gdp_diff.df[1, 2] / max_gdp_diff.df[2, 2], 0)

# In contiguous dyads
# Libya has 15.3 times the GDP of Niger
# South Africa has 13.3 times the GDP of Mozambique
max_gdp_diff_cont.df <- africa.df %>%
  filter(state1_cont == "Africa" & state2_cont == "Africa" & 
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
  geom_hline(aes(yintercept = median_polity, group = typology), colour = "black", alpha = .3, size = 1.5) +
  geom_vline(aes(xintercept = median_gdp, group = typology), colour = "black", alpha = .3, size = 1.5) +
  scale_x_continuous(breaks = log(c(400, 1000, 3000, 8000, 20000)), labels = c(400, 1000, 3000, 8000, 20000)) +
  scale_colour_manual(values  = c("No" = "grey", "Yes" = "black"), guide = guide_legend(title = "Shared ethnicity")) +
  labs(x = "GDP p.c. (log.)", y = "Political system",
       caption = paste0(
         "Number of borders: ", length(gdp_pol.df$state1),
         "\nNumber of states: ", length(unique(gdp_pol.df$state1)))) +
  theme.basic

# Mediane
gdp_pol.df %>%
  distinct(typology, median_gdp, median_polity) %>%
  mutate(median_exp_gdp = round(exp(median_gdp), 0))

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
  labs(x = "GDP p.c. (log.)", y = "Political system") +
  theme.basic

# World Religion (CoW)
# --------------------------------- #
relig.df <- africa.df %>%
  filter(state1 != "ISR") %>%
  group_by(state1_relig, typology) %>%
  summarise(n = n()) %>%
  group_by(state1_relig) %>%
  mutate(percentage = n / sum(n))

relig.fig <- relig.df %>%
  ggplot() +
  geom_bar(aes(x = fac_ind_en_short(typology), y = percentage), 
               stat = "identity") +
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

# Next steps depend on our conceptualization of border infrastructures. Are they
# always directed (i.e. allow asymmetry) or are they undirected (i.e. symmetric).
# For the time being, I tend to go with directed dyads.

# Create a directed dyadic dataset
# (1) all unique combinations of religion
relig_comb <- expand(border.df, state1_relig, state2_relig) %>%
  mutate(combs = paste0(comb = paste(state1_relig, state2_relig, sep = "_"))) %>%
  pull(combs)

# (2) Make a dyadic dataset
africa_dyad.df <- africa_dyad.df %>%
  mutate(dyad_typ = paste(state1_typology, state2_typology, sep = "_"),
         dyad_relig = paste(state1_relig, state2_relig, sep = "_")) %>%
  
  # distinct(dyadID, .keep_all = TRUE) %>%                 # undirected dyad
  mutate(fdyad_typ = factor(dyad_typ, levels = c(

    # Symmetric
    "frontier border_frontier border",
    "landmark border_landmark border",
    "checkpoint border_checkpoint border",
    "barrier border_barrier border",
    "fortified border_fortified border",

    # Asymmetric
    "frontier border_landmark border",
    "frontier border_checkpoint border",
    "frontier border_barrier border",
    "frontier border_fortified border",

    "landmark border_frontier border",
    "landmark border_checkpoint border",
    "landmark border_barrier border",
    "landmark border_fortified border",

    "checkpoint border_frontier border",
    "checkpoint border_landmark border",
    "checkpoint border_barrier border",
    "checkpoint border_fortified border",

    "barrier border_frontier border",
    "barrier border_landmark border",
    "barrier border_checkpoint border",
    "barrier border_fortified border",

    "fortified border_frontier border",
    "fortified border_landmark border",
    "fortified border_checkpoint border",
    "fortified border_barrier border"
  )),
  fdyad_relig = factor(dyad_relig, levels = c(relig_comb))
  )

# Shorten factor levels
short.level <- c(
  "FF", "LL", "CC", "BB", "WW",
  "FL", "FC", "FB", "FW",
  "LF", "LC", "LB", "LW",
  "CF", "CL", "CB", "CW",
  "BF", "BL", "BC", "BW",
  "WF", "WL", "WC", "WB"
)

# Change factor levels according to "short.level"
levels(africa_dyad.df$fdyad_typ) <- short.level

# Shorten further
# i.e. make undirected
africa_dyad.df <- africa_dyad.df %>%
  mutate(
    fdyad_shtyp =
      fct_collapse(fdyad_typ,
        FL = c("FL", "LF"),
        FC = c("FC", "CF"),
        FB = c("FB", "BF"),
        FW = c("FW", "WF"),

        LC = c("LC", "CL"),
        LB = c("LB", "BL"),
        LW = c("LW", "WL"),

        CB = c("CB", "BC"),
        CW = c("CW", "WC"),

        BW = c("BW", "WB")
      ),
    fdyad_shrelig = 
      case_when(
        state1_relig == state2_relig ~ "symmetric",
        TRUE ~ "asymmetric")
  ) %>%
  mutate(ind_symmetry = ifelse(fdyad_typ %in% c("FF", "LL", "CC", "BB", "WW"), 1, 0))

# Number of (as)symmetric dyads
africa_dyad.df %>%
  count(ind_symmetry) %>%
  mutate(perc = n / sum(n) * 100)

# Compute descriptive statistics
## -------------------------------------------------------------------------- ##

# (Absolute) Difference between state1 and state2
africa_dyad.df <- africa_dyad.df %>%
  mutate(
    
    # GDP
    diffGDP = state1_gdp - state2_gdp,
    absdiffGDP = abs(state1_gdp - state2_gdp),
    ratioGDP = state1_gdp / state2_gdp,
    absratioGDP = ifelse(ratioGDP < 1 & ind_symmetry == 1, 1 / ratioGDP, ratioGDP), # use greater ratio for symmetric dyads
    
    # Polity
    diffPol = state1_polity - state2_polity,
    absdiffPol = abs(state1_polity - state2_polity),
    ratioPol =  state1_polity / state2_polity,
    absratioPol = ifelse(ratioPol < 1 & ind_symmetry == 1, 1 / ratioPol, ratioPol) # use greater ratio for symmetric dyads
  )

# Absolute GDP ratio by dyadic border typology
dyad.vars <- africa_dyad.df %>%
  filter(!is.na(state1_gdp) & !is.na(state2_gdp)) %>%
  group_by(fdyad_typ) %>%
  summarise(
    mGDPratio = mean(absratioGDP), # GDP absratio
    mGDPdiff =  mean(diffGDP),     # GDP diff
    
    n = n()
  ) %>%
  mutate(asymmetry = ifelse(fdyad_typ %in% c("FF", "LL", "CC", "BB", "WW"), 0, 1)) %>%
  arrange(asymmetry, mGDPdiff)

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
  arrange(fac_ind_de(state1_typology))


# Plots
## -------------------------------------------------------------------------- ##

# Plots of neighbour characteristics by state1's typology
## -------------------------------------------------------------------------- ##

# Figure 4
# Neigbours GDP (mean absdiff)
gdp_neighbour_absdiff.fig <- ggplot(neighbour_char) +
  geom_bar(aes(x = fac_ind_en_short(state1_typology), y = median_neighbour_absdiffGDP), 
           stat = "identity") +
  geom_text(aes(x = fac_ind_en_short(state1_typology), y = median_neighbour_absdiffGDP, 
            label = paste0("N=", n)), vjust = -0.5) +
  labs(x = "", y = "", title = "GDP p.c. (USD)") +
  theme.basic

# Neigbours Polity (mean absdiff)
pol_neighbour_absdiff.fig <- ggplot(neighbour_char) +
  geom_bar(aes(x = fac_ind_en_short(state1_typology), y = median_neighbour_absdiffPol),
    stat = "identity"
  ) +
  geom_text(aes(
    x = fac_ind_en_short(state1_typology), y = median_neighbour_absdiffPol,
    label = paste0("N=", n)
  ), vjust = -0.5) +
  labs(
    x = "", y = "", title = "Political system",
    caption = paste0(
      "Number of borders: ", sum(neighbour_char$n),
      "\nNumber of states: ",
      length(
        unique(
          africa_dyad.df[!is.na(africa_dyad.df$state2_polity) &
            !is.na(africa_dyad.df$state2_gdp), ]$state1
        )
      )
    )
  ) +
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

# Figure 5
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
  facet_wrap(.~factor(state2_relig, 
                      levels = c("chrst", "islm", "jud"),
                      labels = c("Christian", "Islamic", "Jewish"))) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  labs(x = "", y = "", fill = "Majority religion",
       caption = paste0(
         "Number of borders: ", sum(relig_neighbour.df$n),
         "\nNumber of states: ", length(unique(africa_dyad.df[!is.na(africa_dyad.df$state2_relig),]$state1)))) +
  theme.basic

# Plots for full dyadic data (bordertypology pairs)
## -------------------------------------------------------------------------- ##

# GDPDiff by typology
# --------------------------------- #
# Plot: GDP difference
# Note: Typology sorted by order in dyad.vars
gdp_diff_dyad.fig <- ggplot(dyad.vars) +
  geom_bar(aes(x = factor(fdyad_typ, fdyad_typ), y = mGDPdiff), stat = "identity") +
  geom_text(aes(x = factor(fdyad_typ, fdyad_typ), y = mGDPdiff, label = paste0("N=", n)), vjust = -1) +
  labs(x = "", y = "") +
  theme.basic

# Plot: GDP ratio
# For symmetric dyads the greater ratio is used
gdp_ratio_dyad.fig <- dyad.vars %>%
  arrange(asymmetry, mGDPratio) %>%
  ggplot() +
  geom_bar(aes(x = factor(fdyad_typ, fdyad_typ), y = mGDPratio), stat = "identity") +
  geom_text(aes(x = factor(fdyad_typ, fdyad_typ), y = mGDPratio, label = paste0("N=", n)), vjust = -1) +
  labs(x = "", y = "") +
  theme.basic

# Polity difference by typology
# --------------------------------- #
# PolityIV difference
pol_diff_dyad.fig <- dyad.df %>%
  group_by(fdyad_typ) %>%
  summarise(
    mPoldiff = mean(diffPol), 
    n = n()
  ) %>%
  mutate(asymmetry = ifelse(fdyad_typ %in%
    c("FF", "LL", "CC", "BB", "WW"), 0, 1)) %>%
  arrange(asymmetry, mPoldiff) %>%
  ggplot() +
  geom_bar(aes(x = factor(fdyad_typ, fdyad_typ), y = mPoldiff), stat = "identity") +
  geom_text(aes(
    x = factor(fdyad_typ, fdyad_typ), y = mPoldiff,
    label = paste0("N=", n)
  ), vjust = -1) +
  labs(x = "", y = "") +
  theme.basic

# PolityIV ratio
# For symmetric dyads the greater ratio is used
pol_ratio_dyad.fig <- dyad.df %>%
  group_by(fdyad_typ) %>%
  summarise(
    mPolratio = mean(absratioPol),
    n = n()
  ) %>%
  mutate(asymmetry = ifelse(fdyad_typ %in%
                              c("FF", "LL", "CC", "BB", "WW"), 0, 1)) %>%
  arrange(asymmetry, mPolratio) %>%
  ggplot() +
  geom_bar(aes(x = factor(fdyad_typ, fdyad_typ), y = mPolratio), stat = "identity") +
  geom_text(aes(
    x = factor(fdyad_typ, fdyad_typ), y = mPolratio,
    label = paste0("N=", n)
  ), vjust = -1) +
  labs(x = "", y = "") +
  theme.basic

# Religion by typology
# --------------------------------- #
border_relig.df <- africa_dyad.df %>%
  group_by(fdyad_typ, fdyad_shrelig) %>%
  count(fdyad_shrelig) %>%
  group_by(fdyad_typ) %>%
  mutate(perc = n / sum(n) * 100)

border_relig.fig <- border_relig.df %>%
  ggplot() +
  geom_bar(aes(x = fdyad_typ, y = perc, fill = fdyad_shrelig), stat = "identity") +
  scale_fill_grey(guide = guide_legend(title = "Religion")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "", y = "") +
  theme.basic

# Typology by ethnicity
# --------------------------------- #
border_ethn.df <- africa_dyad.df %>%
  group_by(fdyad_typ) %>%
  count(share_ethn) %>%
  mutate(perc = n / sum(n) * 100)

border_ethn.fig <- border_ethn.df %>%
  ggplot() +
  geom_bar(aes(x = fdyad_typ, y = perc, fill = factor(share_ethn, labels = c("No", "Yes"))), stat = "identity") +
  scale_fill_grey(guide = guide_legend(title = "Shared ethnicities")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "", y = "") +
  theme.basic

# 
border_num_ethn.df <- africa_dyad.df %>%
  group_by(fdyad_typ) %>%
  summarise(mean = mean(num_share_ethn))

# Facetted scatterplot: GDP x PolityIV
# (dyadic variables)
# --------------------------------- #
# A (1) Add grouped mean of GDP and PolityIV

# A (2) Facetted scatterplot
gdp_pol_dyad.fig <- ggplot(data = africa_dyad.df) +
  geom_jitter(aes(x = ratioGDP, y = diffPol)) +
  facet_grid(~ factor(state1_typology,
                      levels = c("landmark border", "frontier border", "checkpoint border", "barrier border", "fortified border")
  )) +
  labs(x = "GDP p.c. (logged)", y = "PolityIV") +
  theme.basic

# Descriptive summary of independent
# variables (Fig Appendix 1)
# --------------------------------- #
vars <- c("state1_gdp", "state1_polity", "share_ethn", 
          "state1_military_expenditure_perc_gdp_log",
          "state1_military_pers_pc", "state1_nterror_log")

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

# (2) Nterror
africa_descriptive[africa_descriptive$variable == "state1_nterror_log_", 
                   c("max", "mean", "min", "sd", "ymin", "ymax")] <- 
  africa_descriptive[africa_descriptive$variable == "state1_nterror_log_", 
                     c("max", "mean", "min", "sd", "ymin", "ymax")] %>% 
  mutate_all(funs(./100))

# Graphical display of descriptive statistics
africa_descriptive.fig <- africa_descriptive %>%
  filter(!variable %in% c("state1_military_expenditure_perc_gdp_log_",
                          "state1_nterror_log_", "state1_military_pers_pc_")) %>%
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

# Independent variables by typology
# Note:
# States have multiple borders. Each border influences the descriptive statistics. 
# Hence, states with a greater number of borders exert a heavier influence on the
# statistics such as the median and mean. 

# GDP, Polity IV, military capacity
# & main religion by typology
# --------------------------------- #
# Summary stats
border_af_bvars <- africa.df %>%
  group_by(typology) %>%
  summarise_at(vars[c(1:2, 3:6)],
               list(~mean(., na.rm = T), 
                    ~sd(., na.rm = T), 
                    ~min(., na.rm = T), 
                    ~max(., na.rm = T), 
                    obs = ~sum(!is.na(.)))
  )

# Prepare
border_af_bvars <- border_af_bvars %>% 
  gather(var, value, -typology) %>%
  mutate(measure = str_extract(var, "[:alpha:]+$"),
         variable = str_extract(var, paste0(".+(?=", measure, ")")) %>%
           str_sub(., end = -2)) %>%
  select(-var) %>%
  spread(measure, value)

# List-column
border_af_bvars.nest <- border_af_bvars %>%
  group_by(variable) %>%
  nest() %>%
  ungroup() %>%
  mutate(title = c(
    "Shared ethnicity",
    "GDP p.c. (in US$)",
    "Military expenditures per one million population (log.)",
    "Military personnel per 1.000 population",
    "Terror incidents",
    "Political system (PolityIV)"
  ),
  subtitle = c(
    "\nSource: Michalopoulos, S., & Papaioannou, E. (2016)",
    "\nSource: WorldBank (2017)",
    "\nSource: WorldBank (2017)",
    "\nSource: WorldBank (2017)",
    "\nSource: Global Terrorism Database",
    "\nSource: PolityIV (2017)"
  ))

# Plot
border_af_bvars.nest.fig <- border_af_bvars.nest %>%
  mutate(plots = pmap(list(data, title, subtitle, variable), ~ggplot(data = ..1) +
                        geom_bar(aes(x = fac_ind_en_short(typology), y = mean), stat = "identity") +
                        labs(
                          title = ..2,
                          caption = paste0(
                            "N(borders) = ", sum(.x$obs),
                            "\nN(countries) = ",
                            length(unique(africa.df[!is.na(paste0("africa.df$",..4)),]$state1)),
                            ..3
                          ),
                          x = "", y = ""
                        ) + 
                        theme.basic
  ))

# Combine plots
combined_bvars <- plot_grid(plotlist = border_af_bvars.nest.fig$plots[c(-1,-3)])

# Save figures
## -------------------------------------------------------------------------- ##

# Monadic analysis
# --------------------------------- #

# Figure 2
# Relative distribution of border infrastructure
ggsave(
  plot = ind.perc.fig, "Y:/Grenzen der Welt/Projekte/Afrikanische Grenzen/Grafiken/Fig 2 - Africa_RelativeDistribution.tiff", width = 8, height = 6, unit = "in",
  dpi = 300
)

# Figure 3
# Scatterplot (A)
ggsave(
  plot = gdp_pol.fig, "Y:/Grenzen der Welt/Projekte/Afrikanische Grenzen/Grafiken/Fig 3 - Africa_ScatterGDP_Pol.tiff", width = 9, height = 8, unit = "in",
  dpi = 300
)

# Figure 4
# Religion by typology
ggsave(
  plot = relig.fig, "Y:/Grenzen der Welt/Projekte/Afrikanische Grenzen/Grafiken/Fig 4 - Africa_religion_typology.tiff", width = 8, height = 6, unit = "in",
  dpi = 300
)

# Neighbour characteristics
# Figure 5
# A) Polity & GDP
ggsave(
  plot = neighbour.fig, "Y:/Grenzen der Welt/Projekte/Afrikanische Grenzen/Grafiken/Fig 5 - Africa_NeighbourDescriptive.tiff", width = 8, height = 6, unit = "in",
  dpi = 300
)

# Figure 6
# B) Religion
ggsave(
  plot = relig_neighbour.fig, "Y:/Grenzen der Welt/Projekte/Afrikanische Grenzen/Grafiken/Fig 6 - Africa_NeighbourReligion.tiff", width = 8, height = 6, unit = "in",
  dpi = 300
)

# Figure 7
# Scatterplot (B)
ggsave(
  plot = gdp_pol_fort.fig, "Y:/Grenzen der Welt/Projekte/Afrikanische Grenzen/Grafiken/Fig 7 - Africa_ScatterGDP_Pol_Fort.tiff", width = 8, height = 8, unit = "in",
  dpi = 300
)

# Appendix
# Figure A1
# Descriptive statistics
ggsave(
  plot = descriptive.fig, "Y:/Grenzen der Welt/Projekte/Afrikanische Grenzen/Grafiken/Fig A1 - DescriptiveStats.tiff", width = 12.5, height = 8, unit = "in",
  dpi = 300
)

# Plots of indicator by independent variables
ggsave(
  plot = combined_bvars, "./output/figures/DescriptivePlots.tiff", width = 12.5, height = 8, unit = "in",
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

## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

# PLAYGROUND
## -------------------------------------------------------------------------- ##

# Figure 2 but z-standardized
# --------------------------------- #
# A (1) Add grouped mean of GDP and PolityIV
z_gdp_pol.df <- africa.df %>%
  filter(!is.na(state1_gdp) & !is.na(state1_polity)) %>%
  mutate(state1_z_gdp = scale(state1_gdp, center = TRUE, scale = TRUE),
         state1_z_pol = scale(state1_polity, center = TRUE, scale = TRUE)) %>%
  group_by(typology) %>%
  mutate(
    median_z_gdp = median(state1_z_gdp),
    median_z_pol = median(state1_z_pol)
  ) %>%
  select(state1, state1_gdp, state1_polity, state1_z_gdp, state1_z_pol, 
         median_z_gdp, median_z_pol, typology, share_ethn)

# A (2) Facetted scatterplot
z_gdp_pol.fig <- ggplot(data = z_gdp_pol.df) +
  geom_point(data = gdp_pol.df, aes(x = state1_z_gdp, y = state1_z_pol, 
                                    color = factor(share_ethn, labels = c("Nein", "Ja")))) +
  facet_grid(~ fac_ind_de(typology)) +
  geom_hline(aes(yintercept = median_z_pol, group = typology), colour = "black", alpha = .3, size = 1.5) +
  geom_vline(aes(xintercept = median_z_gdp, group = typology), colour = "black", alpha = .3, size = 1.5) +
  scale_colour_manual(values  = c("Nein" = "grey", "Ja" = "black"), guide = guide_legend(title = "Geteilte Ethnien")) +
  labs(x = "BIP pro Kopf (log.)", y = "Politisches System") +
  theme.basic