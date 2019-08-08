# Exploratory analysis of African borders

# Notes:
# - Countries that need further checking:
#   - Cases with insufficient information:
#   - africa.df %>%
#        filter(information_density == "low")
# - Data for administrative and police capacities

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

# Function creates a factor variable of the indicator
## -------------------------------------------------------------------------- ##
fac_ind <- function(x) {
  factor(x, levels = c("landmark border", "frontier border", "checkpoint border", 
                       "barrier border", "fortified border"))
}

# Descriptive analysis
## -------------------------------------------------------------------------- ##
# Basic description
# Number of African states (p. 10)
africa.dim <- tibble(
  countries = with(africa.df, unique(c(state1, state2))),
  continent = countrycode(countries, "iso3c", "continent")
) %>%
  group_by(continent) %>%
  mutate(n_states = length(continent)) %>%
  distinct(continent, n_states, .keep_all = TRUE)

# Number of bidrectional dyads
n_borders <- dim(africa.df)
  
# Univariate
# --------------------------------- #
# Distribution of border types
# Absolute
ind.dist.fig <- ggplot(africa.df, aes(x = fac_ind(typology))) +
  geom_bar() +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5)
  )

# Distribution
# Relative (Figure 1)
ind.perc.fig <- africa.df %>%
  group_by(typology) %>%
  summarise(perc = length(typology) / length(africa.df$typology) * 100) %>%
  ggplot(aes(x = fac_ind(typology), y = perc)) +
  geom_bar(stat = "identity") +
  labs(
    caption = paste0(
      "N(borders) = ", length(africa.df$typology),
      "\nN(countries) = ", length(unique(africa.df$state1))
    ),
    x = "", y = ""
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5)
  )

# Descriptive summary of independent
# variables
# --------------------------------- #
vars <- c("state1_gdp", "state1_polity", "share_ethn")

africa_descriptive <- africa.df %>%
  summarise_at(vars, list(~mean(., na.rm = T), 
                          ~sd(., na.rm = T), 
                          ~min(., na.rm = T), 
                          ~max(., na.rm = T), 
                          ~sum(!is.na(.)))
               )

# By regions
# --------------------------------- #

# Bivariate
# --------------------------------- #

# States have multiple borders. Each border influences the descriptive statistics. 
# Hence, states with a greater number of borders exert a heavier influence on the
# statistics such as the median and mean. 

# GDP, Polity IV & Religion
# --------------------------------- #
# Summary stats
border_af_bvars <- africa.df %>%
  group_by(typology) %>%
  summarise_at(vars[1:2],
               list(~mean(., na.rm = T), 
                    ~sd(., na.rm = T), 
                    ~min(., na.rm = T), 
                    ~max(., na.rm = T), 
                    ~sum(!is.na(.)))
  )

# GDP
# World Bank Indicators: "NY.GDP.PCAP.CD", "SP.POP.TOTL"
# Year: 2018 (accessed: 2018/08/08)
# --------------------------------- #
gdp.fig <- ggplot(border_af_bvars) +
  geom_bar(aes(x = fac_ind(typology), y = state1_gdp_mean), stat = "identity") +
  labs(
    title = "Mean GDP per capita, Africa",
    caption = paste0(
      "N(borders) = ", sum(border_af_bvars$state1_gdp_sum),
      "\nN(countries) = ",
      length(unique(africa.df[!is.na(africa.df$state1_gdp),]$state1)),
      "\nData: WorldBank (2018)"
    ),
    x = "", y = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5)
  )

# PolityIV
# Variable: Polity2
# Year: 2017 
# --------------------------------- #
polity.fig <- ggplot(border_af_bvars) +
  geom_bar(aes(x = fac_ind(typology), y = state1_polity_mean), stat = "identity") +
  labs(
    title = "Mean political system, Africa",
    caption = paste0(
      "N(borders) = ", sum(border_af_bvars$state1_polity_sum),
      "\nN(countries) = ",
      length(unique(africa.df[!is.na(africa.df$state1_polity),]$state1)),
      "\nData: PolityIV (2017)"
    ),
    x = "", y = ""
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5)
  )

# Facetted scatterplot: GDP x PolityIV
# Figure 2
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
gdp_pol.fig <- ggplot(data = gdp_pol.df) +
  geom_jitter(data = gdp_pol.df, aes(x = log(state1_gdp), y = state1_polity)) +
  facet_grid(~ factor(typology,
    levels = c("landmark border", "frontier border", "checkpoint border", "barrier border", "fortified border")
  )) +
  geom_hline(aes(yintercept = median_polity, group = typology), colour = "black", alpha = .3, size = 1.5) +
  geom_vline(aes(xintercept = median_gdp, group = typology), colour = "black", alpha = .3, size = 1.5) +
  scale_x_continuous(breaks = log(c(400, 1000, 3000, 8000, 20000)), labels = c(400, 1000, 3000, 8000, 20000)) +
  labs(x = "GDP p.c. (logged)", y = "PolityIV") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# B (1) Zoomed in scatterplot for barrier & fortified borders
# Figure 3
# --------------------------------- #
gdp_pol_fort.df <- gdp_pol.df %>%
  filter(typology %in% c("fortified border", "barrier border"))

# B (2)                         # ---------------------------------------------------------------------------------------------------
# ADD N_borders and N_countries # ---------------------------------------------------------------------------------------------------
gdp_pol_fort.fig <- ggplot(data = gdp_pol_fort.df, aes(x = log(state1_gdp), y = state1_polity)) +
  geom_jitter() +
  geom_text_repel(label = paste(gdp_pol_fort.df$state1, gdp_pol_fort.df$state2, sep = "-"),
                  segment.color = NA) +
  facet_grid(~ factor(typology,
                      levels = c("landmark border", "frontier border", "checkpoint border", "barrier border", "fortified border")
  )) +
  geom_hline(aes(yintercept = median_polity, group = typology), colour = "black", alpha = .3, size = 1.5) +
  geom_vline(aes(xintercept = median_gdp, group = typology), colour = "black", alpha = .3, size = 1.5) +
  scale_x_continuous(breaks = log(c(400, 1000, 3000, 8000, 20000)), labels = c(400, 1000, 3000, 8000, 20000)) +
  labs(x = "GDP p.c. (logged)", y = "PolityIV") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# World Religion (CoW)
# --------------------------------- #
relig.fig <- africa.df %>%
  group_by(state1_relig, typology) %>%
  summarise(n = n())

# Global Mobility
# --------------------------------- #


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
africa.df <- africa.df %>%
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
dyad.df <- africa.df %>%
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
levels(dyad.df$fdyad_typ) <- short.level

# Shorten further
# i.e. make undirected
dyad.df <- dyad.df %>%
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
dyad.df %>%
  count(ind_symmetry) %>%
  mutate(perc = n / sum(n) * 100)

# Compute descriptive statistics
## -------------------------------------------------------------------------- ##
dyad.df <- dyad.df %>%
  mutate(
    
    # GDP
    diffGDP = state1_gdp - state2_gdp,
    absdiffGDP = abs(state1_gdp - state2_gdp),
    ratioGDP = state1_gdp / state2_gdp,
    absratioGDP = ifelse(ratioGDP < 1 & ind_symmetry == 1, 1 / ratioGDP, ratioGDP), # compute greater ratio for symmetric dyads
    
    # Polity
    diffPol = state1_polity - state2_polity,
    absdiffPol = abs(state1_polity - state2_polity),
    ratioPol =  state1_polity / state2_polity,
    absratioPol = ifelse(ratioPol < 1 & ind_symmetry == 1, 1 / ratioPol, ratioPol)
  )

# Absolute GDP ratio by border typology
dyad.vars <- dyad.df %>%
  filter(!is.na(state1_gdp) & !is.na(state2_gdp)) %>%
  group_by(fdyad_typ) %>%
  summarise(
    mGDPratio = mean(absratioGDP),
    mGDPdiff =  mean(diffGDP),
    n = n()
  ) %>%
  mutate(asymmetry = ifelse(fdyad_typ %in% c("FF", "LL", "CC", "BB", "WW"), 0, 1)) %>%
  arrange(asymmetry, mGDPdiff)

# Plot
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
border_ethn.df <- dyad.df %>%
  group_by(fdyad_typ, share_ethn) %>%
  count(fdyad_typ) %>%
  group_by(share_ethn) %>%
  mutate(perc = n / sum(n) * 100)

border_ethn.fig <- border_ethn.df %>%
  ggplot() +
  geom_bar(aes(x = share_ethn, y = perc, fill = fdyad_typ), stat = "identity") +
  scale_fill_grey(guide = guide_legend(title = "Shared")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5)
  )

# Save figures
## -------------------------------------------------------------------------- ##

# Figure 1
# Relative distribution of border infrastructure
ggsave(
  plot = ind.perc.fig, "./output/figures/Africa_RelativeDistribution.tiff", width = 6, height = 6, unit = "in",
  dpi = 300
)

# Figure 2
# Scatterplot (A)
ggsave(
  plot = gdp_pol.fig, "./output/figures/Africa_ScatterGDP_Pol.tiff", width = 6, height = 6, unit = "in",
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