# Exploratory analysis of African borders

# Notes:
# - Countries that need further checking:
#   - Cases with insufficient information:
#   - africa.df %>%
#        filter(information_density == "low")

# Issues:
# - Add religion to descriptive summary
# - set seed for reproducible jittering
# - add number of observations to remaining plots

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, countrycode, igraph, rio, janitor, cowplot, ggrepel)

# Load: Source data and border indicator
### ------------------------------------------------------------------------ ###
# source data
source("SourceData.R")

# indicator
indicator.df <- import("Y:\\Grenzen der Welt\\Grenzdossiers\\Typologie\\BorderTypology.xlsx",
  sheet = 1
) %>%
  as_tibble() %>%
  select(1:3, 5:8, 12, 16:17) %>%
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
  filter(percent_of_homeland >= 0.1) %>%        # only those that share at least 10% population
  select(-number_partitions) %>%                # from here: unfold country column, i.e. make long
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
                       "barrier border", "fortified border"))
}

# German
fac_ind_de <- function(x) {
  factor(x, levels = c("landmark border", "frontier border", "checkpoint border", 
                       "barrier border", "fortified border"),
         labels = c("Grenzstein", "Niemandslandgrenze", "Kontrollpunktgrenze", 
                       "Barrieregrenze", "fortifizierte Grenze"))
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

# Descriptive analysis
## -------------------------------------------------------------------------- ##
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
  
# Overview: Dependent variable
# Indicator
# --------------------------------- #
# Distribution of border types
# Absolute
ind.dist.fig <- ggplot(africa.df, aes(x = fac_ind_de(typology))) +
  geom_bar() +
  theme.basic

# Distribution
# Relative (Figure 1)
ind.perc.fig <- africa.df %>%
  group_by(typology) %>%
  summarise(percentage = length(typology) / length(africa.df$typology) * 100,
            count = n()) %>%
  ggplot(aes(x = fac_ind_de(typology), y = percentage)) +
  geom_bar(stat = "identity") +
  geom_text(stat = "identity", aes(label = paste0("N = ", count)), vjust = -1) +
  labs(
    caption = paste0(
      "N(Grenzen) = ", length(africa.df$typology),
      "\nN(Staaten) = ", length(unique(africa.df$state1))
    ),
    x = "", y = ""
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme.basic

# Descriptive summary of independent
# variables
# --------------------------------- #
vars <- c("state1_gdp", "state1_polity", "share_ethn", 
          "state1_military_expenditure_log_pc",
          "state1_military_pers_pc", "state1_nterror_3yrs")

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
africa_descriptive[africa_descriptive$variable == "state1_nterror_3yrs_", 
                   c("max", "mean", "min", "sd", "ymin", "ymax")] <- 
  africa_descriptive[africa_descriptive$variable == "state1_nterror_3yrs_", 
                     c("max", "mean", "min", "sd", "ymin", "ymax")] %>% 
  mutate_all(funs(./100))

# Graphical display of descriptive statistics
africa_descriptive.fig <- africa_descriptive %>%
  filter(!variable %in% c("state1_military_expenditure_log_pc_", "share_ethn_")) %>%
  ggplot() +
  geom_point(aes(x = mean, y = variable)) +
  geom_errorbarh(aes(y = variable, xmin = ymin, xmax = ymax), height = .2) +
  scale_x_continuous(breaks = seq(-10, 10, 2), limits = c(-10, 10)) +
  scale_y_discrete(breaks = c("state1_gdp_", "state1_military_pers_pc_", "state1_nterror_3yrs_",
                            "state1_polity_"),
                   labels = c("GDP p.c.\n(in 1.000 US$)", 
                              "Military personnel\n(per 1.000 population)", 
                              "Terror incidents\n(in hundreds)", 
                              "PolityIV")) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5)
  )

# By regions
# --------------------------------- #

# / -------------- /

# Bivariate
# --------------------------------- #

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
    "Mean share of partitioned ethnicities",
    "Mean GDP per capita, Africa",
    "Mean military expenditures per million in population (logged), Africa",
    "Mean military personnel per 1000 in population, Africa",
    "Incidents of terror (2015-2018), Africa",
    "Mean political system, Africa"
  ),
  subtitle = c(
    "\nData: Michalopoulos, S., & Papaioannou, E. (2016)",
    "\nData: WorldBank (2017)",
    "\nData: WorldBank (2017)",
    "\nData: WorldBank (2017)",
    "\nData: Global Terrorism Database",
    "\nData: PolityIV (2017)"
  ))

# Plot
border_af_bvars.nest.fig <- border_af_bvars.nest %>%
  mutate(plots = pmap(list(data, title, subtitle, variable), ~ggplot(data = ..1) +
                           geom_bar(aes(x = fac_ind_de(typology), y = mean), stat = "identity") +
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

# Facetted scatterplot: GDP x PolityIV
## -------------------------------------------------------------------------- ##

# Figure 2
# --------------------------------- #
# A (1) Add grouped mean of GDP and PolityIV
set.seed(200819)
gdp_pol.df <- africa.df %>%
  filter(!is.na(state1_gdp) & !is.na(state1_polity)) %>%
  group_by(typology) %>%
  mutate(
    median_gdp = median(log(state1_gdp)),
    median_polity = median(state1_polity)
  )

# A (2) Facetted scatterplot
gdp_pol.fig <- ggplot(data = gdp_pol.df) +
  geom_jitter(data = gdp_pol.df, aes(x = log(state1_gdp), y = state1_polity, 
                                     color = factor(share_ethn, labels = c("No", "Yes")))) +
  facet_grid(~ factor(typology,
    levels = c("landmark border", "frontier border", "checkpoint border", "barrier border", "fortified border")
  )) +
  geom_hline(aes(yintercept = median_polity, group = typology), colour = "black", alpha = .3, size = 1.5) +
  geom_vline(aes(xintercept = median_gdp, group = typology), colour = "black", alpha = .3, size = 1.5) +
  scale_x_continuous(breaks = log(c(400, 1000, 3000, 8000, 20000)), labels = c(400, 1000, 3000, 8000, 20000)) +
  scale_colour_manual(values  = c("No" = "grey", "Yes" = "black"), guide = guide_legend(title = "Shared ethnicities")) +
  labs(x = "GDP p.c. (logged)", y = "PolityIV") +
  theme.basic

# B (1) Zoomed in scatterplot for barrier & fortified borders
# Figure 3
# --------------------------------- #
gdp_pol_fort.df <- gdp_pol.df %>%
  filter(typology %in% c("fortified border", "barrier border"))

# B (2)                         
# ADD N_borders and N_countries
gdp_pol_fort.fig <- ggplot(data = gdp_pol_fort.df, aes(x = log(state1_gdp), y = state1_polity,
                                                       color = factor(share_ethn, labels = c("No", "Yes")))) +
  geom_jitter() +
  geom_text_repel(label = paste(gdp_pol_fort.df$state1, gdp_pol_fort.df$state2, sep = "-"),
                  segment.color = NA) +
  facet_grid(~ factor(typology,
                      levels = c("landmark border", "frontier border", "checkpoint border", "barrier border", "fortified border")
  )) +
  geom_hline(aes(yintercept = median_polity, group = typology), colour = "black", alpha = .3, size = 1.5) +
  geom_vline(aes(xintercept = median_gdp, group = typology), colour = "black", alpha = .3, size = 1.5) +
  scale_colour_manual(values  = c("No" = "grey", "Yes" = "black"), guide = guide_legend(title = "Shared ethnicities")) +
  scale_x_continuous(breaks = log(c(400, 1000, 3000, 8000, 20000)), labels = c(400, 1000, 3000, 8000, 20000)) +
  labs(x = "GDP p.c. (logged)", y = "PolityIV") +
  theme.basic

# World Religion (CoW)
# --------------------------------- #
relig.fig <- africa.df %>%
  group_by(state1_relig, typology) %>%
  summarise(n = n())

# Global Mobility
# --------------------------------- #
# Recchi & Deutschmann (2019)
# / -------------- /

# Dyadic analysis
## -------------------------------------------------------------------------- ##

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

# Absolute GDP ratio by border typology
dyad.vars <- africa_dyad.df %>%
  filter(!is.na(state1_gdp) & !is.na(state2_gdp)) %>%
  group_by(fdyad_typ) %>%
  summarise(
    mGDPratio = mean(absratioGDP),
    mGDPdiff =  mean(diffGDP),
    n = n()
  ) %>%
  mutate(asymmetry = ifelse(fdyad_typ %in% c("FF", "LL", "CC", "BB", "WW"), 0, 1)) %>%
  arrange(asymmetry, mGDPdiff)

# Plots
## -------------------------------------------------------------------------- ##

# GDPDiff by typology
# --------------------------------- #
# GDP difference
# Note: Typology sorted by order in dyad.vars
gdp_diff_dyad.fig <- ggplot(dyad.vars) +
  geom_bar(aes(x = factor(fdyad_typ, fdyad_typ), y = mGDPdiff), stat = "identity") +
  geom_text(aes(x = factor(fdyad_typ, fdyad_typ), y = mGDPdiff, label = paste0("N=", n)), vjust = -1) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5)
  )

# Plot: GDP ratio
# For symmetric dyads the greater ratio is used
gdp_ratio_dyad.fig <- dyad.vars %>%
  arrange(asymmetry, mGDPratio) %>%
  ggplot() +
  geom_bar(aes(x = factor(fdyad_typ, fdyad_typ), y = mGDPratio), stat = "identity") +
  geom_text(aes(x = factor(fdyad_typ, fdyad_typ), y = mGDPratio, label = paste0("N=", n)), vjust = -1) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5)
  )

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
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5)
  )

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
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5)
  )

# Religion by typology
# --------------------------------- #
border_relig.df <- dyad.df %>%
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
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5)
  )

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
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5)
  )

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
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save figures
## -------------------------------------------------------------------------- ##

# Figure 1
# Relative distribution of border infrastructure
ggsave(
  plot = ind.perc.fig, "./output/figures/Africa_RelativeDistribution.tiff", width = 8, height = 6, unit = "in",
  dpi = 300
)

# Descriptive statistics
ggsave(
  plot = africa_descriptive.fig, "./output/figures/Africa_DescriptiveStats.tiff", width = 6, height = 6, unit = "in",
  dpi = 300
)

# Plots of indicator by independent variables
ggsave(
  plot = combined_bvars, "./output/figures/DescriptivePlots.tiff", width = 12.5, height = 8, unit = "in",
  dpi = 300
)

# Figure 2
# Scatterplot (A)
ggsave(
  plot = gdp_pol.fig, "./output/figures/Africa_ScatterGDP_Pol.tiff", width = 8, height = 8, unit = "in",
  dpi = 300
)

# Figure 3
# Scatterplot (B)
ggsave(
  plot = gdp_pol_fort.fig, "./output/figures/Africa_ScatterGDP_Pol_Fort.tiff", width = 6, height = 6, unit = "in",
  dpi = 300
)

# Polity
ggsave(
  plot = pol_diff_dyad.fig, "./output/figures/Africa_PolDyad_Diff.tiff", width = 6, height = 6, unit = "in",
  dpi = 300
)

# GDP (diff)
ggsave(
  plot = gdp_diff_dyad.fig, "./output/figures/Africa_GDPDyad_Diff.tiff", width = 6, height = 6, unit = "in",
  dpi = 300
)

# GDP (ratio)
ggsave(
  plot = gdp_ratio_dyad.fig, "./output/figures/Africa_GDPDyad_Ratio.tiff", width = 6, height = 6, unit = "in",
  dpi = 300
)

# Relig by border type
ggsave(
  plot = border_relig.fig, "./output/figures/Africa_Border_Relig.tiff", width = 6, height = 6, unit = "in",
  dpi = 300
)

# Arrange plots with cowplot
plot_grid(gdp.fig, polity.fig, nrow = 2)