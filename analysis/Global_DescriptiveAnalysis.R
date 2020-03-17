# Border Data: Model

# Notes & Issues
# - border.df features the case MMR/PAK, which does not share a common border
# - monadic descriptive analysis needs updating

                            ###################
                            #      SETUP      #
                            ###################

# Load/install packages
## -------------------------------------------------------------------------- ##
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, janitor, broom, margins, patchwork)

# (1) Indicator factor levels
fac_ind_en <- function(x) {
  factor(x, levels = c("landmark border", "frontier border", "checkpoint border", 
                       "barrier border", "fortified border"),
         labels = c("Landmark", "Frontier", "Checkpoint", 
                    "Barrier", "Fortified"))
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
         !(state1 == "QAT" & state2 == "ARE")) %>%    # https://bit.ly/39EOy4Y
  clean_names() %>%
  distinct(state1, state2, .keep_all = TRUE)

# Add year of border installation
# Data from BordersJoin.R
barriers.df <- import("./analysis/Fence data/barriers_df.rds")

list.fences <- barriers.df %>%
  select(-indicator) %>%
  nest(data = c(-source)) %>%
  mutate(data = set_names(data, source))

# Rename year-columns to data source and unnest
list.fences <- map2(
  .x = list.fences$data, 
  .y =  names(list.fences$data),
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
ind.perc.df <- border.df %>%
  group_by(state1_typology) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count) * 100,
         continent1 = "World")

# Only the global distribution
ind.perc.fig <- ind.perc.df %>%
  ggplot(mapping = aes(x = state1_typology, y = perc)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "") +
  theme.basic +
  theme(axis.text.x = element_text(angle = 0, hjust = .5)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

# Distribution of indicator across continents + global
ind.perc.region.fig <- border.df %>%
  group_by(continent1, state1_typology) %>%
  summarise(count = n()) %>%
  mutate(group_n = sum(count),
         perc = count / group_n * 100) %>% 
  select(-group_n) %>%
  bind_rows(ind.perc.df) %>%
  ggplot(mapping = aes(x = fac_ind_en(state1_typology), y = perc)) +
  geom_bar(stat = "identity") +
  geom_text(stat = "identity", aes(label = paste0("N = ", count)), vjust = -0.3,
            size = 2.8) +
  facet_wrap(~continent1) +
  labs(x = "", y = "") +
  theme.basic +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

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
    state1_military_expenditure_lcu_pc,
    state1_military_pers_p1000,
    # culture
    # state1_relig
    # migration
    hosted_refugees_agg_pc,
    hosted_refugees_pc
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
    "Total number of hosted refugees",
    "Number of refugees from neigboring country",
    "Victims of terror incidents (2014-2017)",
    "GDP per capita in USD",
    "Military expenditure per one million population",
    "Military personnel per 1.000 population",
    "Political regime (PolityIV)"
  ),
  subtitle = c(
    "\nData: World Refugee Dataset (2015)",
    "\nData: World Refugee Dataset (2015)",
    "\nData: Global Terrorism Database",
    "\nData: WorldBank (2017)",
    "\nData: WorldBank (2017)",
    "\nData: WorldBank (2017)",
    "\nData: PolityIV (2017)"
  ))

# Plot
border_monvars.fig <- border_monvars.nest %>%
  mutate(plots = pmap(list(data, title, subtitle), ~ggplot(data = ..1) +
                        geom_bar(aes(x = fac_ind_en(state1_typology), y = mean), stat = "identity") +
                        labs(
                          title = ..2,
                          caption = ..3,
                          x = "", y = "") +
                        theme_minimal()
  ))

# Dyadic 
### ------------------------------------------------------------------------ ###

# Create dyadic variables
# On transformations, see Fox & Weisberg: CAR, p. 131f. 
border.df <- border.df %>%
  mutate(
    # economy
    diff_gdp = state1_gdp - state2_gdp,
    logdiff_gdp = log(state1_gdp) - log(state2_gdp),
    absdiff_gdp = abs(state1_gdp - state2_gdp),
    ratio_gdp = state1_gdp / state2_gdp,
    # trade
    log_diff_trade = log1p(export) - log1p(import),
    # politics
    diff_pol = state1_polity - state2_polity,
    absdiff_pol = abs(state1_polity - state2_polity),
    # security
    absdiff_death_toll_3yrs = abs(state1_death_toll_3yrs - state2_death_toll_3yrs),
    # for transformation, see Fox & Weisberg: CAR, p. 131f.
    diff_military_expenditure_pc = state1_military_expenditure_perc_gdp - state2_military_expenditure_perc_gdp,
    logdiff_military_expenditure_pc = log1p(state1_military_expenditure_perc_gdp) - log1p(state2_military_expenditure_perc_gdp),
    absdiff_military_expenditure_pc = state1_military_expenditure_perc_gdp - state2_military_expenditure_perc_gdp,
    
    diff_military_pers_pc = state1_military_pers_pc - state2_military_pers_pc,
    logdiff_military_pers_pc = log1p(state1_military_pers_pc) - log1p(state2_military_pers_pc),
    absdiff_military_pers_pc = abs(state1_military_pers_pc - state2_military_pers_pc))

# Summarise by group
border_dyadvars <- border.df %>%
  group_by(state1_typology) %>%
  summarise(
    # economy
    neighbour_absdiff_gdp_median = median(absdiff_GDP, na.rm = TRUE),
    # politics
    neighbour_absdiff_polity_median = median(absdiff_Polity, na.rm = TRUE),
    # security
    neigbour_absdiff_deathtoll3y_median = median(absdiff_death_toll_3yrs, na.rm = TRUE),
    neigbour_absdiff_militaryexp_median = median(absdiff_military_expenditure_pc, na.rm = TRUE),
    neighbour_absdiff_militarypers_median = median(absdiff_military_pers_pc, na.rm = TRUE)
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

                          ##########################
                          #       REGRESSION       #
                          ##########################

# Logistic regression on indicators
### ------------------------------------------------------------------------ ###

# Approach
# Bivariate
# (A) Builder characteristics & neigbour characteristics
# Multivariate
# (B) Full model (of A)
# (C) Flows
# (D) Combined effects (dyadic effects)
# (E) Combination of border typology, i.e. fortified_fortified...

# Note:
# Keep the dataframe after sjmisc::to_dummy and create a formula for each DV and all IV
# then map_df

# Create dummy variables of typology
border.df <- border.df %>%
  sjmisc::to_dummy(state1_typology, state2_typology, suffix = "label") %>%
  bind_cols(border.df) %>%
  rename_at(vars(contains("state1_typology_")), list(~make_clean_names(.)))

dv <- c("state1_typology_landmark_border", "state1_typology_frontier_border", 
        "state1_typology_checkpoint_border", "state1_typology_barrier_border", 
        "state1_typology_fortified_border")

# Models
# Create model formula
iv <- c(
  # (A) Builder characteristics (bivariate)
  "state1_gdp", 
  "state1_polity", 
  "state1_death_toll_3yrs",
  "state1_military_expenditure_perc_gdp",
  "state1_military_pers_p1000",
  state1_
  # (A) Neighbour characteristics (bivariate)
  "state2_gdp", 
  "state2_polity", 
  "state2_death_toll_3yrs",
  "state2_military_expenditure_perc_gdp",
  "state2_military_pers_p1000",
  # (B) Builder characteristics (full)
  "state1_gdp +
  state1_polity + 
  state1_death_toll_3yrs +
  state1_military_expenditure_perc_gdp +
  state1_military_pers_p1000",
  # (B) Neighbour characteristics (full)
  "state2_gdp +
  state2_polity + 
  state2_death_toll_3yrs +
  state2_military_expenditure_perc_gdp +
  state2_military_pers_p1000",
  # (C) Flows
  
  # (D) Dyadic
  )

model <- expand_grid(iv, dv) %>%
  mutate(formula = paste0(dv, " ~ ", iv))

# Apply the glm-formula
result.df <- model %>%
  mutate(model = map(formula, ~glm(as.formula(.), 
                                   family = binomial(link = "logit"), 
                                   data = border.df) %>%
                       margins(.) %>%
                       summary(.)))

# Nest results for (A 'bivariate') by DV
### ------------------------------------------------------------------------ ###
result_bivariate.df <- result.df %>%
  filter(str_detect(iv, "[+]") != TRUE) %>%
  unnest(model) %>%
  group_by(dv) %>%
  nest()

# Create coefplots
result_bivariate.df <- result_bivariate.df %>%
  mutate(plots = map2(.x = data, .y = dv, ~ggplot(data = .x) +
                        geom_point(aes(x = factor, y = AME), stat = "identity") +
                        geom_errorbar(aes(x = factor, 
                                          ymin = AME - (SE * qnorm((1-0.95)/2)),
                                          ymax = AME + (SE * qnorm((1-0.95)/2))
                                          )) +
                        ylim(-0.15, 0.15) +
                        coord_flip() +
                        labs(
                          title = .y,
                          x = "", y = "") +
                        theme_minimal()
  ))

# Display all coefplots
wrap_plots(result_bivariate.df$plots)

# Nest results for (B 'full model') by DV
### ------------------------------------------------------------------------ ###
result_multivariate.df <- result.df %>%
  filter(str_detect(iv, "[+]") == TRUE) %>%
  unnest(model) %>%
  group_by(dv) %>%
  nest()

# Create coefplots
result_multivariate.df <- result_multivariate.df %>%
  mutate(data = map(data, ~ .x %>%
                      filter(str_detect(iv, "2") != TRUE))) %>%
  mutate(plots = map2(.x = data, .y = dv, ~ggplot(data = .x) +
                        geom_point(aes(x = factor, y = AME), stat = "identity") +
                        geom_errorbar(aes(x = factor, 
                                          ymin = AME - (SE * qnorm((1-0.95)/2)),
                                          ymax = AME + (SE * qnorm((1-0.95)/2))
                        )) +
                        ylim(-0.09, 0.09) +
                        coord_flip() +
                        labs(
                          title = .y,
                          x = "", y = "") +
                        theme_minimal()
  ))

# Display all coefplots
wrap_plots(result_multivariate.df$plots)

# Nest results for (C 'flows') by DV
### ------------------------------------------------------------------------ ###


# Nest results for (D 'dyadic effects') by DV
### ------------------------------------------------------------------------ ###

# Remove NA/NaN/Inf
border_clean.df <- border.df %>%
  select(state1, state2,
         contains("state1_typology_"),
         logdiff_gdp, log_diff_trade,  diff_pol,
         logdiff_military_expenditure_pc, 
         logdiff_military_pers_pc) %>%
  filter_all(all_vars(!is.na(.)))

iv <- c("logdiff_gdp + log_diff_trade +  diff_pol + 
logdiff_military_expenditure_pc + logdiff_military_pers_pc")

model_dyad.df <- expand_grid(iv, 
                             dv) %>%
  mutate(formula = paste0(dv, " ~ ", iv))

# Apply the glm-formula
result_dyadic.df <- model_dyad.df %>%
  mutate(model = map(formula, ~glm(as.formula(.), 
                                   family = binomial(link = "logit"), 
                                   data = border.df) %>%
                       margins(.) %>%
                       summary(.)))

result_dyadic.df <- result_dyadic.df %>% 
  unnest(model) %>%
  group_by(dv) %>%
  nest()

# Create coefplots
result_dyadic.df <- result_dyadic.df %>%
  mutate(plots = map2(.x = data, .y = dv, ~ggplot(data = .x) +
                        geom_point(aes(x = factor, y = AME), stat = "identity") +
                        geom_errorbar(aes(x = factor, 
                                          ymin = AME - (SE * qnorm((1-0.95)/2)),
                                          ymax = AME + (SE * qnorm((1-0.95)/2))
                        )) +
                        ylim(-1, 1) +
                        coord_flip() +
                        labs(
                          title = .y,
                          x = "", y = "") +
                        theme_minimal()
  ))

# Display all coefplots
wrap_plots(result_dyadic.df$plots)

                          ##########################
                          #       EXPORT FIGS      #
                          ##########################

# Figure 1
# Relative distribution of border infrastructure
ggsave(
  plot = ind.perc.region.fig, "Y:/Grenzen der Welt/Projekte/Walls, barriers, checkpoints and landmarks/Figures/Fig2 - Typology By Region.tiff", width = 8, height = 6, unit = "in",
  dpi = 300
)
