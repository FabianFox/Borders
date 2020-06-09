# Border Data: Model

# Notes & Issues
# - border.df features the case MMR/PAK, which does not share a common border
# - monadic descriptive analysis needs updating
# - FRA-GUY must be added [X]
# - missing values: mice
# 

                            ###################
                            #      SETUP      #
                            ###################

# Load/install packages
## -------------------------------------------------------------------------- ##
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "janitor", "broom", "margins", "patchwork", "gtools",
            "nnet", "ggeffects", "mice", "rio")


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

# Create a column that indicates whether a valid year exist
# See: https://stackoverflow.com/questions/55671205/fill-missing-values-rowwise-right-left
built <- t(zoo::na.locf(t(fortified_borders.df[, c(5:10)])))[,6]

# Add to dataframe
fortified_borders.df <- fortified_borders.df %>%
  mutate(built_combined = built)

# Join indicator data
# duplicates because some countries share multiple borders, i.e. RUS/CHN
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

# Define factor variables and style scheme
## -------------------------------------------------------------------------- ##
# (1) Indicator factor levels
fac_ind_en <- function(x) {
  factor(x, levels = c("frontier border", "landmark border", "checkpoint border", 
                       "barrier border", "fortified border"),
         labels = c("'No man's land'","Landmark", "Checkpoint", 
                    "Barrier", "Fortified"))
}

# (2) Factor levels for DV
fac_ind_dv <- function(x) {
  factor(x, levels = c("state1_typology_frontier_border", 
                       "state1_typology_landmark_border",
                       "state1_typology_checkpoint_border",
                       "state1_typology_barrier_border",   
                       "state1_typology_fortified_border"),
         labels = c("'No man's land'", "Landmark", "Checkpoint", 
                    "Barrier", "Fortified"))
}

# (3) Theme for the plots
theme.basic <- theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(size = 14),
    axis.ticks.x = element_line(size = .5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

                            ###################
                            #       EDA       #
                            ###################

#                               UNIVARIATE                                     

# Exploratory analysis of indicator
### ------------------------------------------------------------------------ ###
# Distribution of indicator (in %)
ind_perc.df <- border.df %>%
  group_by(state1_typology) %>%
  summarise(count = n()) %>%
  mutate(perc = count / sum(count) * 100,
         continent1 = "World")

# Round global distribution
border.df %>%
  group_by(state1_typology) %>%
  summarise(count = n()) %>%
  mutate(group_n = sum(count),
         perc = count / group_n * 100, continent1 = "World") %>%
  mutate(rounded_perc = round(perc, digit = 1))

# Only the global distribution
ind_perc.fig <- ind_perc.df %>%
  ggplot(mapping = aes(x = state1_typology, y = perc)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "") +
  theme.basic +
  theme(axis.text.x = element_text(angle = 0, hjust = .5)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

# Distribution of indicator across continents + global
ind_perc_region.fig <- border.df %>%
  mutate(continent1 = if_else(state1 == "PNG", "Asia", continent1)) %>%
  group_by(continent1, state1_typology) %>%
  summarise(count = n()) %>%
  mutate(group_n = sum(count),
         perc = count / group_n * 100) %>% 
  select(-group_n) %>%
  bind_rows(ind_perc.df) %>%
  ggplot(mapping = aes(x = fac_ind_en(state1_typology), y = perc)) +
  geom_bar(stat = "identity") +
  geom_text(stat = "identity", aes(label = paste0("N = ", count)), vjust = -0.3,
            size = 2.8) +
  facet_wrap(~factor(continent1, 
                     levels = c("Africa", "Americas", "Asia", "Europe", "World"),
                     labels = c("Africa", "North & South America", "Asia (incl. Oceania)", 
                                "Europe", "World"))) +
  labs(x = "", y = "") +
  theme.basic +
  theme(axis.text = element_text(size = 8)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

# Round global distribution (regions)
global_dist.df <- border.df %>%
  mutate(continent1 = if_else(state1 == "PNG", "Asia", continent1)) %>%
  group_by(continent1, state1_typology) %>%
  summarise(count = n()) %>%
  mutate(group_n = sum(count),
         perc = count / group_n * 100) %>%
  mutate(rounded_perc = round(perc, digit = 1)) %>%
  select(continent1, state1_typology, rounded_perc) %>%
  pivot_wider(names_from = continent1, values_from = rounded_perc) %>%
  arrange(fac_ind_en(state1_typology))

# Barriers and fortified borders most common in Asia 
#global_dist.df %>%
#  select(state1_typology, Asia) %>%
#  filter(state1_typology %in% c("barrier border", "fortified border")) %>%
#  summarise(colsum = colSums(.[,2]))


#                                  MONADIC
### ------------------------------------------------------------------------ ###

# Economy
# GDP, Polity IV, military capacity
# & main religion by typology
# --------------------------------- #
# Summary stats
border_monvars <- border.df %>%
  mutate(state1_pop_per_million = state1_pop / 1000000) %>%
  group_by(state1_typology) %>%
  summarise_at(vars(
    # control
    # state1_pop_per_million,
    # economy
    state1_gdp_log,
    export_log,
    import_log,
    # politics
    state1_polity,
    # security
    state1_military_expenditure_perc_gdp,
    state1_nterror_log,
    # culture
    
    # migration
    refugees_incoming_log
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
  mutate(
    obs = map(data, ~sum(.x$obs)),
    title = c(
    "Dyadic export (in USD), log",
    "Dyadic import (in USD), log",
    "Dyadic refugee inflow, log",
    "GDP per capita (in USD), log",
    "Military expenditure (as % of GDP)",
    "Terror incidents (annual), log",
    "Political regime (PolityIV)"
  ),
  subtitle_1 = c(
    "\nData: COW: Trade (2014)",
    "\nData: COW: Trade (2014)",
    "\nData: World Refugee Dataset (2015)",
    "\nData: WorldBank (2017)",
    "\nData: WorldBank (2017)",
    "\nData: Global Terrorism Database (2017)",
    "\nData: PolityIV (2017)"
  ),
  subtitle = paste0(subtitle_1, "\nObservations: ", obs)) %>%
  select(-subtitle_1)

# Plot
border_monvars.fig <- border_monvars.nest %>%
  mutate(plots = pmap(list(data, title, subtitle), ~ggplot(data = ..1) +
                        geom_bar(aes(x = fac_ind_en(state1_typology), y = mean),
                                 stat = "identity") +
                        scale_y_continuous(breaks = seq(0, 10, 2), limits = c(-0.4,10.5)) +
                        labs(
                          title = ..2,
                          caption = ..3,
                          x = "", y = "") +
                        theme.basic +
                        theme(
                          plot.title = element_text(size = 10, face = "bold"),
                          plot.caption = element_text(size = 8),
                          axis.text = element_text(size = 10))
                      ))

# Chart for different majority religions across the border
diff_relig.fig <- border.df %>%
  group_by(state1_typology) %>%
  summarise(diff_relig = sum(diff_relig) / n() * 100) %>%
  mutate(not_diff_relig = 100 - diff_relig) %>%
  ggplot() +
  geom_bar(aes(x = fac_ind_en(state1_typology), y = diff_relig),
           stat = "identity") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Share of different majority religions",
    caption = paste0("\nData: COW: World Religion Data (2010)", "\nObservations: ",
                     sum(!is.na(border.df$diff_relig))),
    x = "", y = "") +
  theme.basic +
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.caption = element_text(size = 8),
        axis.text = element_text(size = 10))

# Put figures together with patchwork
border_monvars_pwork.fig <- wrap_plots(border_monvars.fig$plots) + diff_relig.fig


#                                 DYADIC 
### ------------------------------------------------------------------------ ###
# Create dyadic variables
# On transformations, see Fox & Weisberg: CAR, p. 131f. 
border.df <- border.df %>%
  mutate(
    # economy
    diff_gdp = state1_gdp - state2_gdp,
    logdiff_gdp = state1_gdp_log - state2_gdp_log,
    absdiff_gdp = abs(state1_gdp - state2_gdp),
    ratio_gdp = state1_gdp / state2_gdp,
    
    # trade
    log_diff_trade = export_log - import_log,
    
    # politics
    diff_pol = state1_polity - state2_polity,
    absdiff_pol = abs(state1_polity - state2_polity),
    
    # refugee flows
    diff_refugees = refugees_outgoing - refugees_incoming,
    logdiff_refugees = refugees_outgoing_log - refugees_incoming_log,
    
    # security
    absdiff_nkill = abs(state1_death_toll - state2_death_toll),
    # for transformation, see Fox & Weisberg: CAR, p. 131f.
    diff_military_expenditure_pc = state1_military_expenditure_perc_gdp - state2_military_expenditure_perc_gdp,
    logdiff_military_expenditure_pc = state1_military_expenditure_perc_gdp_log - state2_military_expenditure_perc_gdp_log,
    absdiff_military_expenditure_pc = state1_military_expenditure_perc_gdp - state2_military_expenditure_perc_gdp,
    
    diff_military_pers_pc = state1_military_pers_pc - state2_military_pers_pc,
    logdiff_military_pers_pc = state1_military_pers_pc_log - state2_military_pers_pc_log,
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
                        theme_basic
  ))

                          ##########################
                          #       REGRESSION       #
                          ##########################

# Approach
# Bivariate
# (A)  Bivariate regression
# Multivariate
# (B1) Full model
# (B2) More dyad variables
# (C)  Combination of border typology, i.e. fortified_fortified...

# Note:

# Logit models:
# Fitted probabilities between 0 and 1 occurred: 
# - Landmark ~ Polity (https://stats.stackexchange.com/questions/336424/issue-with-complete-separation-in-logistic-regression-in-r)

# Multinomial regression
# - The measure for military disputes behaves badly because several categories
#   are not involved in any disputes (disp_from_2000_to_2010)

# Create dummy variables of typology
border.df <- border.df %>%
  sjmisc::to_dummy(state1_typology, state2_typology, suffix = "label") %>%
  bind_cols(border.df) %>%
  rename_at(vars(contains("state1_typology_")), list(~make_clean_names(.)))

dv <- c("state1_typology_frontier_border", "state1_typology_landmark_border", 
        "state1_typology_checkpoint_border", "state1_typology_barrier_border", 
        "state1_typology_fortified_border")

# Models
# Create model formula
iv <- c(
  # (A1) Builder characteristics (bivariate)
  "state1_gdp_log",
  "ratio_gdp",
  "export_log",
  "import_log",
  "state1_polity",
  "refugees_incoming_log",
  "state1_nterror_log",
  "diff_relig",
  "state1_military_expenditure_perc_gdp_log",
  # (A2) Neighbour characteristics (bivariate)

  # (B1) Full model
  "state1_gdp_log +
  ratio_gdp +
  state1_polity +
  refugees_incoming_log +
  state1_nterror_log +
  diff_relig +
  state1_military_expenditure_perc_gdp_log"
  
  # (B2) Dyad variables

  # (C)
  
  # (D)
  )

# Create model dataframe
model <- expand_grid(iv, dv) %>%
  mutate(formula = paste0(dv, " ~ ", iv))


#                             LOGISTIC REGRESSION
### ------------------------------------------------------------------------ ###
# Apply the glm-formula
result_glm.df <- model %>%
  mutate(model = map(formula, ~glm(as.formula(.), 
                                   family = binomial(link = "logit"), 
                                   data = border.df)))

# Compute AME (infinitesimal small change)                    
result_ame.df <- result_glm.df %>%
  mutate(model = map(model, 
                     ~margins(.) %>%
                       summary(.))) %>%
  # Add p*-stars
  mutate(model = map(model, ~mutate(., 
                                    pstars = stars.pval(p))))

# AME of mean -/+ 1 SD                     
result_sd.df <- result_glm.df %>%
  mutate(model = map(model, 
                     ~margins(., change = "sd") %>%
                       summary(.))) %>%
  # Add p*-stars
  mutate(model = map(model, ~mutate(., 
                                    pstars = stars.pval(p))))
         
# Results: Model - bivariate
### ------------------------------------------------------------------------ ###
# Filter to model A
result_bivariate_A.df <- result_ame.df %>%
  filter(!(str_detect(iv, "[+]"))) %>%
  unnest(model) %>%
  group_by(dv) %>%
  nest()

# Create coefplots
result_bivariate_A.df <- result_bivariate_A.df %>%
  mutate(plots = map2(.x = data, .y = fac_ind_dv(dv), ~ggplot(data = .x) +
                        geom_point(aes(x = factor, y = AME), stat = "identity") +
                        geom_errorbar(aes(x = factor, 
                                          ymin = AME - (SE * qnorm((1-0.95)/2)),
                                          ymax = AME + (SE * qnorm((1-0.95)/2))
                                          )) +
                        geom_hline(yintercept = 0, colour = "gray", linetype = 2) +
                        ylim(-2, 2) +
                        coord_flip() +
                        labs(
                          title = .y,
                          x = "", y = "") +
                        theme_minimal()
  ))

# Display all coefplots
bivariate_A.fig <- wrap_plots(result_bivariate_A.df$plots)

# Results: Model A2 'neighbour effects'
### ------------------------------------------------------------------------ ###

# /

# Results B: Full model
### ------------------------------------------------------------------------ ###
# Filter to model A1
result_B.df <- result_ame.df %>%
  filter(str_detect(iv, "[+]")) %>%
  unnest(model) %>%
  group_by(dv) %>%
  nest()

# Create coefplots
result_B.df <- result_B.df %>%
  mutate(plots = map2(.x = data, .y = fac_ind_dv(dv), ~ggplot(data = .x) +
                        geom_point(aes(x = factor, y = AME), stat = "identity") +
                        geom_errorbar(aes(x = factor, 
                                          ymin = AME - (SE * qnorm((1-0.95)/2)),
                                          ymax = AME + (SE * qnorm((1-0.95)/2))
                        )) +
                        geom_hline(yintercept = 0, colour = "gray", linetype = 2) +
                        ylim(-.5, .5) +
                        coord_flip() +
                        labs(
                          title = .y,
                          x = "", y = "") +
                        theme_minimal()
  ))

# Display all coefplots
multivariate_B.fig <- wrap_plots(result_B.df$plots)


#                           MULTINOMIAL REGRESSION
### ------------------------------------------------------------------------ ###
# nnet::multinom 
border.df <- border.df %>%
  mutate(state1_typology_fct = fac_ind_en(state1_typology),
         state1_typology_fct = fct_relevel(state1_typology_fct, "Checkpoint"))

# Apply multinom
model_mnom.df <- multinom(
  as.formula(paste0("state1_typology_fct", " ~ ", iv[10])),
  Hess = TRUE,
  data = border.df)

# Tidy
# Log odds
result_mnom.df <- model_mnom.df %>%
  tidy(., conf.int = TRUE, conf.level = 0.95, exponentiate = FALSE) %>% # exponentiate: TRUE
  filter(term != "(Intercept)") %>%
  mutate(pstars = stars.pval(p.value),
         y.level = factor(y.level, 
                       levels = c("'No man's land'", "Landmark", "Checkpoint", 
                                  "Barrier", "Fortified")),
         term_fc = fct_rev(factor(term, 
                                  levels = c("state1_gdp_log",
                                             "ratio_gdp",
                                             "state1_polity",
                                             "refugees_incoming_log",
                                             "state1_nterror_log",
                                             "diff_relig",
                                             "state1_military_expenditure_perc_gdp_log"),
                                  labels = c("GDP pc (log), builder",
                                             "GDP pc, ratio",
                                             "Polity, builder",
                                             "Refugees, incoming",
                                             "Terror incidents (log), builder",
                                             "Different majority religion",
                                             "Military expenditures pc (log), builder"))))
 

# Odds ratios
result_mnom_rr.df <- model_mnom.df %>%
  tidy(., conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% # exponentiate: TRUE
  filter(term != "(Intercept)") %>%
  mutate(pstars = stars.pval(p.value),
         y.level = factor(y.level, 
                          levels = c("'No man's land'", "Landmark", "Checkpoint", 
                                     "Barrier", "Fortified")),
         term_fc = fct_rev(factor(term, 
                                  levels = c("state1_gdp_log",
                                             "ratio_gdp",
                                             "state1_polity",
                                             "refugees_incoming_log",
                                             "state1_nterror_log",
                                             "diff_relig",
                                             "state1_military_expenditure_perc_gdp_log"),
                                  labels = c("GDP pc (log), builder",
                                             "GDP pc, ratio",
                                             "Polity, builder",
                                             "Refugees, incoming",
                                             "Terror incidents (log), builder",
                                             "Different majority religion",
                                             "Military expenditures pc (log), builder")))) 

# AME
result_mnom_ame.df <- tibble(
  category = model_mnom.df$lev,
  model = list(model_mnom.df)) %>%
  mutate(estimate = map2(.x = model, .y = category, 
                    ~summary(margins(model = .x, category = .y)))) %>%
  select(-model) %>%
  unnest(cols = estimate) %>%
  select(1:3) %>%
  mutate(category = factor(category, 
                          levels = c("'No man's land'", "Landmark", "Checkpoint", 
                                     "Barrier", "Fortified")),
         term_fc = fct_rev(factor(factor, 
                                  levels = c("state1_gdp_log",
                                             "ratio_gdp",
                                             "state1_polity",
                                             "refugees_incoming_log",
                                             "state1_nterror_log",
                                             "diff_relig",
                                             "state1_military_expenditure_perc_gdp_log"),
                                  labels = c("GDP pc (log), builder",
                                             "GDP pc, ratio",
                                             "Polity, builder",
                                             "Refugees, incoming",
                                             "Terror incidents (log), builder",
                                             "Different majority religion",
                                             "Military expenditures pc (log), builder")))) 

# Plots
# ---------------------------------------------------------------------------- #
# Plot (log odds)
ggplot(data = result_mnom.df) +
  # not significant
  geom_point(data = function(x){x[!x$pstars %in% c("**", "***"),]},
             aes(x = term_fc, y = estimate), stat = "identity", alpha = .3) +
  geom_errorbar(data = function(x){x[!x$pstars %in% c("**", "***"),]}, 
    aes(x = term_fc, ymin = conf.low, ymax = conf.high), alpha = .3) +
  # significant
  geom_point(data = function(x){x[x$pstars %in% c("**", "***"),]},
             aes(x = term_fc, y = estimate), stat = "identity") +
  geom_errorbar(data = function(x){x[x$pstars %in% c("**", "***"),]}, 
                aes(x = term_fc, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, colour = "gray", linetype = 2) +
  facet_wrap(~y.level) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-15, 15, 5), 
                     labels = c("-15", "-10", "-5", "0", "5", "10", "15"),
                     limits = c(-16, 16)) +
  labs(
    title = "",
    x = "", y = "") +
  theme_minimal()

# Plot (AME)
mnom_ame.fig <- ggplot(data = result_mnom_ame.df %>%
                         filter(category != "Checkpoint")) +
  # not significant
  geom_point(aes(x = term_fc, y = AME), stat = "identity", alpha = .5) +
  geom_hline(yintercept = 0, colour = "gray", linetype = 2) +
  facet_wrap(~category) +
  ylim(-0.4, 0.4) +
  coord_flip() +
  labs(
    title = "",
    x = "", y = "") +
  theme_minimal()

# ggeffects (see: https://strengejacke.github.io/ggeffects/))
result_mnom.gg <- ggeffect(model_mnom.df, terms = "state1_gdp_log") %>%
  mutate(response.level = if_else(response.level == "X.No.man.s.land.", 
                                  "'No man's land'", response.level),
         response.level = factor(response.level, 
                                levels = c("'No man's land'", "Landmark", "Checkpoint", 
                                           "Barrier", "Fortified")))
# Plot
ggplot(result_mnom.gg, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  facet_wrap(~response.level) +
  scale_x_continuous(breaks = log1p(c(400, 1000, 3000, 8000, 20000, 80000)), 
                     labels = c(400, 1000, 3000, 8000, 20000, 80000)) + 
  labs(
    title = "",
    x = "", y = "") +
  theme_minimal()

# Interpretation: AME at specific values
# ---------------------------------------------------------------------------- #
# AME (+/- 1 SD)
result_mnom_ame_sd.df <- tibble(
  category = model_mnom.df$lev,
  model = list(model_mnom.df)) %>%
  mutate(estimate = map2(
    .x = model, .y = category, 
    ~summary(
      margins(
        model = .x, category = .y, 
        variables = c("state1_gdp_log", 
                      "state1_military_expenditure_perc_gdp_log"), 
        change = "sd")))) %>%
  select(-model) %>%
  unnest(cols = estimate) %>%
  select(1:3) %>% 
  arrange(factor)

                          ##########################
                          #   MULTIPLE IMPUTATION  #
                          ##########################

# Get the variables used in the multinomial model
vars <- str_replace_all(paste0("state1_typology|", iv[[10]]), "[ +\n ]+", "|")

# Select models vars
# Note: state1: clustervar
model.df <- border.df %>%
  select(matches(vars), state1) %>%
  select(-c(1:5)) %>%
  mutate(state1_typology = fac_ind_en(state1_typology),
         state1_typology = fct_relevel(state1_typology, "Checkpoint"),
         diff_relig = factor(diff_relig)) %>%
  rename(state1_military = state1_military_expenditure_perc_gdp_log)

# Distribution of NA
model.df %>%
  summarise_all(~sum(is.na(.)) / length(.) * 100)

# Remove variables with excessive number of missings
# none

# Run an "empty" imputation and adjust elements
mice.mat <- mice(model.df, maxit = 0)

# Elements that need to be adjusted
# Predictor matrix
pred.mat <- mice.mat$predictorMatrix

# Do not use for imputation
pred.mat[, c("state1_typology")] <- 0
pred.mat[, c("state1")] <- 0

# Edit imputation method
imp_method <- mice.mat$method

# logistic regression for binary vars
imp_method[c("diff_relig")] <- "logreg"

# Create imputed datasets
model_imp.df <- mice(model.df, m = 25, predictorMatrix = pred.mat, 
                     method = imp_method, print =  FALSE)

# Export to Stata
# Adopted from: https://stackoverflow.com/questions/49965155/importing-mice-object-to-stata-for-analysis
# Gather complete models incl. base data
imp_out <- mice::complete(model_imp.df, "long", include = TRUE) %>%
  rename("_mj" = ".imp",
         "_mi" = ".id")

# Export
export(imp_out, file = "./output/stata/imputed_data.dta")

# Stata code
# ---------------------------------------------------------------------------- #
# * Load file created in R
# use "C:\Users\guelzauf\Seafile\Meine Bibliothek\Projekte\C01_Grenzen\Data\Analysis\Border Data\output\stata\imputed_data.dta" 

# * Declare as multiple imputed data
# mi import ice

# * Multinomial regression
# mi estimate : mlogit state1_typology state1_gdp_log export_log import_log state1_polity refugees_incoming_log i.diff_relig state1_military state1_nterror_log, vce(cluster state1)

# * https://www.stata.com/statalist/archive/2012-03/msg00927.html
# est sto ml

# forval i = 2/5 {
#   est res ml
#   mimrgns, dydx(*) pr(out(`i')) post
#  est sto ml`i'
# }

# * Export results
# esttab ml2 ml3 ml4 ml5 using "C:\Users\guelzauf\Seafile\Meine Bibliothek\Projekte\C01_Grenzen\Data\Analysis\Border Data\output\stata\mlogit_results.csv", cells(b se t p ci) nostar plain replace

# Re-import results
# ---------------------------------------------------------------------------- #
# Load results
ame_results.df <- import("./output/stata/mlogit_results.csv", skip = 2) %>%
  magrittr::set_colnames(c("variable", 
                           # "checkpoint border", 
                           "frontier border",
                           "landmark border",
                           "barrier border",
                           "fortified border")) %>%
  slice(1:n()-1)

# Create a tidy df
ame_results.df[seq(2, nrow(ame_results.df), 5), "variable"] <- paste0(ame_results.df[seq(1, nrow(ame_results.df), 5), "variable"], "_se")
ame_results.df[seq(3, nrow(ame_results.df), 5), "variable"] <- paste0(ame_results.df[seq(1, nrow(ame_results.df), 5), "variable"], "_t")
ame_results.df[seq(4, nrow(ame_results.df), 5), "variable"] <- paste0(ame_results.df[seq(1, nrow(ame_results.df), 5), "variable"], "_p")
ame_results.df[seq(5, nrow(ame_results.df), 5), "variable"] <- paste0(ame_results.df[seq(1, nrow(ame_results.df), 5), "variable"], "_ci")
ame_results.df[seq(1, nrow(ame_results.df), 5), "variable"] <- paste0(ame_results.df[seq(1, nrow(ame_results.df), 5), "variable"], "_coef")

# Make longer
ame_results.df <- ame_results.df %>%
  filter(str_detect(variable, "^1", negate = TRUE)) %>%
  mutate(variable = str_replace_all(variable, "2.", ""), 
         type = str_extract(variable, "coef$+|se$+|t$+|p$+|ci$+"),
         variable = str_replace(variable, "_coef$+|_se$+|_t$+|_p$+|_ci$+", "")) %>%
  pivot_longer(2:5, names_to = "typology") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  separate(ci, into = c("conf.low", "conf.high"), sep = ",") %>%
  mutate_at(vars(3:8), as.numeric) %>%
  mutate(pstars = stars.pval(p),
         variable = fct_rev(factor(variable, 
                                   levels = c("state1_gdp_log",
                                              "gdp_ratio",
                                              "state1_polity",
                                              "refugees_incoming_log",
                                              "diff_relig",
                                              "state1_military",
                                              "state1_nterror_log"),
                                   labels = c("GDP pc (log), builder",
                                              "GDP pc, ratio",
                                              "Polity, builder",
                                              "Refugees, incoming (log)",
                                              "Different majority religion",
                                              "Military expenditures pc (log), builder",
                                              "Terror incidents (log), builder"))))

# Plot
# Create coefplots
result_mnom_ame.fig <- ame_results.df %>%
  ggplot() +
  geom_point(data = function(x){x[!x$pstars %in% c("*" ,"**", "***"),]},
             aes(x = variable, y = coef), stat = "identity", alpha = .3) +
  geom_errorbar(data = function(x){x[!x$pstars %in% c("*" ,"**", "***"),]}, 
                aes(x = variable, ymin = conf.low, ymax = conf.high), alpha = .3) +
  # significant
  geom_point(data = function(x){x[x$pstars %in% c("*" ,"**", "***"),]},
             aes(x = variable, y = coef), stat = "identity") +
  geom_errorbar(data = function(x){x[x$pstars %in% c("*" ,"**", "***"),]}, 
                aes(x = variable, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, colour = "gray", linetype = 2) +
  facet_wrap(.~fac_ind_en(typology)) +
  ylim(-.2, .2) +
  coord_flip() +
  labs(x = "", y = "") +
  theme_minimal()

                          ##########################
                          #       EXPORT FIGS      #
                          ##########################

# Figure 2
# Relative distribution of border infrastructure
ggsave(
  plot = ind_perc_region.fig, "Y:/Grenzen der Welt/Projekte/Walls, barriers, checkpoints and landmarks/Figures/Fig2 - Typology By Region.tiff", width = 8, height = 6, unit = "in",
  dpi = 300
)

# Figure 3
# Bivariate relationship
ggsave(
  plot = border_monvars_pwork.fig, "Y:/Grenzen der Welt/Projekte/Walls, barriers, checkpoints and landmarks/Figures/Fig3 - Bivariate Relationship.tiff", width = 12, height = 8, unit = "in",
  dpi = 300
)

# Figure 4
# Multinomial regression (AME)
ggsave(
  plot = result_mnom_ame.fig, "Y:/Grenzen der Welt/Projekte/Walls, barriers, checkpoints and landmarks/Figures/Fig4 - Multinomial Regression - AME.tiff", width = 14, height = 8, unit = "in",
  dpi = 300
)

# Appendix
# Figure A1
# Independent variables
ggsave(
  plot = border_monvars_pwork.fig, "Y:/Grenzen der Welt/Projekte/Walls, barriers, checkpoints and landmarks/Figures/FigA1 - Independent Variables.tiff", width = 14, height = 8, unit = "in",
  dpi = 300
)

# Figure A2
# Logistic regression: A1
ggsave(
  plot = bivariate_A1.fig, "Y:/Grenzen der Welt/Projekte/Walls, barriers, checkpoints and landmarks/Figures/A2 - Logistic Regression A1.tiff", width = 14, height = 8, unit = "in",
  dpi = 300
)
