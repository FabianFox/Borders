# Border Data: Model

# Notes & Issues
# - CoW Direct Contiguity is expanded by dyad between NGA-TCD due to aridification
#   of lake Chad, erroneous entries - ARE-QAT, MMR-PAK - are removed

                            ###################
                            #      SETUP      #
                            ###################

# Load/install packages
## -------------------------------------------------------------------------- ##
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "countrycode", "janitor", "broom", "margins", "patchwork", "gtools",
            "nnet", "ggeffects", "mice", "rio", "gt")


                            ###################
                            #      DATA       #
                            ###################

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

# (3) Factor levels for small multiples
fac_ind_sm <- function(x) {
  factor(x, levels = c("frontier border", "landmark border", "checkpoint border", 
                       "barrier border", "fortified border"),
         labels = c("N","L", "C", "B", "F"))
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
                                "Europe", "Global distribution"))) +
  labs(x = "", y = "") +
  theme.basic +
  theme(axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing.y = unit(5, "lines")) +
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
  pivot_wider(names_from = continent1, values_from = rounded_perc, values_fill = 0) %>%
  arrange(fac_ind_en(state1_typology))

# Total of barriers and fortified borders
# most prevalent in Asia
global_dist.df %>%
  filter(state1_typology %in% c("barrier border", "fortified border")) %>%
  bind_rows(summarise(., 
                      across(where(is.numeric), sum),
                      across(where(is.character), ~ "Total")))


#                        CREATE INDEPENDENT VARIABLES
### ------------------------------------------------------------------------ ###
# Limit data to variables used in the analysis
border.df <- border.df %>%
  select(state1, state2, state1_typology, 
         # Economy
         state1_gdp, state2_gdp, state1_gdp_log, state2_gdp_log, ratio_gdp,
         # Polity
         state1_polity, state2_polity, absdiff_pol,
         # Security
         state1_military_expenditure_perc_gdp, state2_military_expenditure_perc_gdp,
         state1_nterror_log, state2_nterror_log,
         # Culture
         state1_relig_shrt, state2_relig_shrt, diff_relig, diff_relig_shrt,
         colony, comlang_off,
         # Controls
         refugees_incoming_log, refugees_incoming_pc)


#                      DESCRIPTIVE STATISTICS & FIGURES
### ------------------------------------------------------------------------ ###
# Create a table with descriptive statistics (1) and small multiple for categories (2)
# (1) Descriptive statistics 
border_vars <- border.df %>%
  summarise_at(vars(
    # economy
    state1_gdp_log,
    ratio_gdp,
    # politics
    state1_polity,
    absdiff_pol,
    # security
    state1_military_expenditure_perc_gdp,
    state1_nterror_log,
    refugees_incoming_log,
    # culture
    diff_relig_shrt,
    # Control
    colony,
    comlang_off
  ),
  list(~mean(., na.rm = T), 
       ~sd(., na.rm = T), 
       ~min(., na.rm = T), 
       ~max(., na.rm = T), 
       obs = ~sum(!is.na(.)))
  )

# Prepare
border_vars <- border_vars %>% 
  gather(var, value) %>%
  mutate(measure = str_extract(var, "[:alpha:]+$"),
         variable = str_extract(var, paste0(".+(?=", measure, ")")) %>%
           str_sub(., end = -2)) %>%
  select(-var) %>%
  spread(measure, value) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2))) 

# Ordering for gt table
### ------------------------------------------------------------------------ ###
border_vars <- border_vars %>%
  mutate(variable = factor(
    variable,
    levels = c("state1_gdp_log", "ratio_gdp", "state1_polity", 
               "absdiff_pol", "state1_military_expenditure_perc_gdp",
               "state1_nterror_log", "refugees_incoming_log", 
               "diff_relig_shrt", 
               "colony", "comlang_off"),
    labels = c("GDP per capita (in USD), log",
               "GDP per capita (in USD), ratio",
               "Political regime",
               "Difference in political regimes",
               "Military expenditure (as % of GDP)",
               "Terrorist incidents (annual), log",
               "Stock of refugees from neighbor, log",
               "Different majority religion",
               "Shared colonial history",
               "Common official language")),
    source = c("PolityIV (2017)",
               "CEPII (2011)",
               "CEPII (2011)",
               "COW: World Religion Data (2010)",
               "World Bank (2017)",
               "World Refugee Dataset (2015)",
               "World Bank (2017)",
               "World Bank (2017)",
               "Global Terrorism Database (2017)",
               "PolityIV (2017)"),
    plot = NA)

# Rename columns
border_vars <- border_vars %>%
  arrange(variable) %>%
  select(Variable = variable, Mean = mean, SD = sd, Max = max, Min = min, 
         Observations = obs, Plot = plot, Source = source)

# (2) Create small multiple
### ------------------------------------------------------------------------ ###
# Summary stats
border.plots <- border.df %>%
  group_by(state1_typology) %>%
  summarise_at(vars(
    # economy
    state1_gdp_log,
    ratio_gdp,
    # politics
    state1_polity,
    absdiff_pol,
    # security
    state1_military_expenditure_perc_gdp,
    state1_nterror_log,
    refugees_incoming_log,
    # culture
    diff_relig_shrt,
    # Control
    colony,
    comlang_off
  ),
  list(~mean(., na.rm = T), 
       ~sd(., na.rm = T), 
       ~min(., na.rm = T), 
       ~max(., na.rm = T), 
       obs = ~sum(!is.na(.)))
  )

# Prepare
border.plots <- border.plots %>% 
  gather(var, value, -state1_typology) %>%
  mutate(measure = str_extract(var, "[:alpha:]+$"),
         variable = str_extract(var, paste0(".+(?=", measure, ")")) %>%
           str_sub(., end = -2)) %>%
  select(-var) %>%
  spread(measure, value)

# List-column
border.plots <- border.plots %>%
  group_by(variable) %>%
  nest() %>%
  ungroup() %>%
  mutate(obs = map(data, ~sum(.x$obs)),
    title = c("Difference in political regimes",
              "Shared colonial history",
              "Common official language",
              "Different majority religion",
              "GDP per capita (in USD), ratio",
              "Stock of refugees from neighbor, log",
              "GDP per capita (in USD), log",
              "Military expenditure (as % of GDP)",
              "Terrorist incidents (annual), log",
              "Political regime"),
   variable = factor(
     variable,
     levels = c("state1_gdp_log", "ratio_gdp", "state1_polity", 
                "absdiff_pol", "state1_military_expenditure_perc_gdp",
                "state1_nterror_log", "refugees_incoming_log", 
                "diff_relig_shrt", 
                "colony", "comlang_off"),
     labels = c("GDP per capita (in USD), log",
                "GDP per capita (in USD), ratio",
                "Political regime",
                "Difference in political regimes",
                "Military expenditure (as % of GDP)",
                "Terrorist incidents (annual), log",
                "Stock of refugees from neighbor, log",
                "Different majority religion",
                "Shared colonial history",
                "Common official language"))) %>%
  arrange(variable)

# Add grand mean
border.plots <- border.plots %>%
  left_join(y = border_vars %>% 
              select(Variable, Mean), by = c("variable" = "Variable")) %>%
  select(-obs) %>% 
  unnest(cols = data) %>% 
  group_by(variable) %>% 
  nest() %>%
  ungroup()

# Create plots
border.plots <- border.plots %>%
  mutate(plots = map(.x = data, ~ggplot(data = .x) +
                       geom_bar(aes(x = fac_ind_sm(state1_typology), y = mean),
                                stat = "identity") +
                       geom_hline(mapping = aes(yintercept = Mean), 
                                  size = 2, linetype = 2) +
                       theme_void() +
                       theme(axis.text.x = element_text(size = 30))
  ))

# Plot for religion (categorical)
# Global distribution
relig.glob <- border.df %>%
  group_by(state1_relig_shrt) %>%
  count() %>%
  ungroup() %>%
  mutate(n_total = sum(n),
         perc = n / n_total * 100,
         state1_typology = "global") 

# Distribution by category  
relig.typ <- border.df %>%
  group_by(state1_typology, state1_relig_shrt) %>%
  count() %>%
  ungroup %>%
  group_by(state1_typology) %>%
  mutate(n_total = sum(n),
         perc = n / n_total * 100)

# Figure
relig.fig <- relig.typ %>%
  bind_rows(relig.glob) %>%
  ggplot() +
  geom_bar(aes(x = factor(state1_typology, 
                          levels = c("frontier border", "landmark border", 
                                     "checkpoint border", "barrier border", 
                                     "fortified border", "global"),
                          labels = c("'No man's land'","Landmark", "Checkpoint", 
                                     "Barrier", "Fortified", "Global")), 
               y = perc, fill = state1_relig_shrt),
           stat = "identity") +
  theme.basic +
  labs(x = "", y = "",
       caption = paste("Data: COW: World Religion Data (2010)\nObservations:", 
             sum(!is.na(border.df$state1_relig_shrt)))) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_grey(start = 0.8, end = 0.2,
                  name = "Majority religion",
                  labels = c("Christian", "Islamic", "Other"))

# Create small multiple table
### ------------------------------------------------------------------------ ###
border_vars.plot <- gt(border_vars) %>%
  fmt_missing(columns = everything(), missing_text = "---") %>%
  text_transform(
    locations = cells_body(vars(Plot)),
    fn = function(x) {
      map(border.plots$plots, ggplot_image, height = px(100))
    }
  ) %>%
  cols_align(align = "left", columns = c("Variable", "Source")) %>%
  cols_align(align = "right", columns = c("Mean", "SD", "Max", "Min", "Observations", "Plot"))


                          ##########################
                          #       REGRESSION       #
                          ##########################

# Approach
# Bivariate
# (A)  Bivariate regression
# (B1) Full model

# Notes
# Logit models:
# Fitted probabilities between 0 and 1 occurred: 
# - Landmark ~ Polity (https://stats.stackexchange.com/questions/336424/issue-with-complete-separation-in-logistic-regression-in-r)

# Multinomial regression:
# - The measure for military disputes behaves badly because several categories
#   are not involved in any disputes (disp_from_2000_to_2010)

# Prepare models
### ------------------------------------------------------------------------ ###
# Create dummy variables of typology
border.df <- border.df %>%
  sjmisc::to_dummy(state1_typology, suffix = "label") %>%
  bind_cols(border.df) %>%
  rename_at(vars(contains("state1_typology_")), list(~make_clean_names(.)))

dv <- c("state1_typology_frontier_border", "state1_typology_landmark_border", 
        "state1_typology_checkpoint_border", "state1_typology_barrier_border", 
        "state1_typology_fortified_border")

# Models
# Create model formula
iv <- c(
  # Bivariate
  "state1_gdp_log",
  "ratio_gdp",
  "state1_polity",
  "absdiff_pol", 
  "state1_military_expenditure_perc_gdp",
  "state1_nterror_log",
  "refugees_incoming_log",
  "state1_relig_shrt",
  "diff_relig_shrt",
  "colony", 
  "comlang_off",

  # Full model
  "state1_gdp_log +
  ratio_gdp +
  state1_polity +
  absdiff_pol + 
  state1_military_expenditure_perc_gdp +
  state1_nterror_log +
  refugees_incoming_log +
  state1_relig_shrt +
  diff_relig_shrt +
  colony +
  comlang_off"
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
# Filter to bivariate relations
result_bivariate.df <- result_ame.df %>%
  filter(!(str_detect(iv, "[+]"))) %>%
  unnest(model) %>%
  group_by(dv) %>%
  nest()

# Create coefplots
result_bivariate.df <- result_bivariate.df %>%
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
bivariate.fig <- wrap_plots(result_bivariate.df$plots)

# Full model
### ------------------------------------------------------------------------ ###
# Filter to multivariate model
result_full.df <- result_ame.df %>%
  filter(str_detect(iv, "[+]")) %>%
  unnest(model) %>%
  group_by(dv) %>%
  nest()

# Create coefplots
result_full.df <- result_full.df %>%
  mutate(plots = map2(.x = data, .y = fac_ind_dv(dv), ~ggplot(data = .x) +
                        geom_point(aes(x = factor, y = AME), stat = "identity") +
                        geom_errorbar(aes(x = factor, 
                                          ymin = AME - (SE * qnorm((1-0.95)/2)),
                                          ymax = AME + (SE * qnorm((1-0.95)/2))
                        )) +
                        geom_hline(yintercept = 0, colour = "gray", linetype = 2) +
                        ylim(-1, 1) +
                        coord_flip() +
                        labs(
                          title = .y,
                          x = "", y = "") +
                        theme_minimal()
  ))

# Display all coefplots
multivariate.fig <- wrap_plots(result_full.df$plots)


#                           MULTINOMIAL REGRESSION
### ------------------------------------------------------------------------ ###
# Notes/issues:
# Large confidence intervals for...
# - 'No man's land': variable 'colony' only two cases 
# - Landmark border: variable 'state1_relig_shrt' no cases in category 'islm'

# nnet::multinom 
border.df <- border.df %>%
  mutate(state1_typology_fct = fac_ind_en(state1_typology),
         state1_typology_fct = fct_relevel(state1_typology_fct, "Checkpoint"))

# Apply multinom
model_mnom.df <- multinom(
  as.formula(paste0("state1_typology_fct", " ~ ", iv[12])),
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
         term = case_when(
           term == "state1_relig_shrtislm" ~ "relig_muslim",
           term == "state1_relig_shrtother" ~ "relig_other",
           TRUE ~ as.character(term)),
         term_fc = fct_rev(factor(term, 
                                  levels = c("state1_gdp_log",
                                             "ratio_gdp",
                                             "state1_polity",  
                                             "absdiff_pol", 
                                             "state1_military_expenditure_perc_gdp",
                                             "state1_nterror_log",
                                             "refugees_incoming_log",
                                             "relig_muslim",
                                             "relig_other",
                                             "diff_relig_shrt",
                                             "colony", 
                                             "comlang_off"),
                                  labels = c("GDP pc (log), builder",
                                             "GDP pc, ratio",
                                             "Polity, builder",
                                             "Polity, abs. difference",
                                             "Military expenditures pc (log), builder",
                                             "Terror incidents (log), builder",
                                             "Refugees, incoming (log)",
                                             "Religion, Muslim\n[Ref.: Christian]",
                                             "Religion, Other\n[Ref.: Christian]",
                                             "Same religion",
                                             "Colonial history",
                                             "Common language"))))
 
# Odds ratios
result_mnom_rr.df <- model_mnom.df %>%
  tidy(., conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% # exponentiate: TRUE
  filter(term != "(Intercept)") %>%
  mutate(pstars = stars.pval(p.value),
         y.level = factor(y.level, 
                          levels = c("'No man's land'", "Landmark", "Checkpoint", 
                                     "Barrier", "Fortified")),
         term = case_when(
           term == "state1_relig_shrtislm" ~ "relig_muslim",
           term == "state1_relig_shrtother" ~ "relig_other",
           TRUE ~ as.character(term)),
         term_fc = fct_rev(factor(term, 
                                  levels = c("state1_gdp_log",
                                             "ratio_gdp",
                                             "state1_polity",  
                                             "absdiff_pol", 
                                             "state1_military_expenditure_perc_gdp",
                                             "state1_nterror_log",
                                             "refugees_incoming_log",
                                             "relig_muslim",
                                             "relig_other",
                                             "diff_relig_shrt",
                                             "colony", 
                                             "comlang_off"),
                                  labels = c("GDP pc (log), builder",
                                             "GDP pc, ratio",
                                             "Polity, builder",
                                             "Polity, abs. difference",
                                             "Military expenditures pc (log), builder",
                                             "Terror incidents (log), builder",
                                             "Refugees, incoming (log)",
                                             "Religion, Muslim\n[Ref.: Christian]",
                                             "Religion, Other\n[Ref.: Christian]",
                                             "Same religion",
                                             "Colonial history",
                                             "Common language"))))

# AME
result_mnom_ame.df <- tibble(
  category = model_mnom.df$lev,
  model = list(model_mnom.df)) %>%
  mutate(estimate = map2(.x = model, .y = category, 
                    ~summary(margins(model = .x, category = .y,)))) %>% # also useful: change = "sd"
  select(-model) %>%
  unnest(cols = estimate) %>%
  select(1:3) %>%
  mutate(category = factor(category, 
                          levels = c("'No man's land'", "Landmark", "Checkpoint", 
                                     "Barrier", "Fortified")),
         factor = case_when(
           factor == "state1_relig_shrtislm" ~ "relig_muslim",
           factor == "state1_relig_shrtother" ~ "relig_other",
           TRUE ~ as.character(factor)),
         term_fc = fct_rev(factor(factor, 
                                  levels = c("state1_gdp_log",
                                             "ratio_gdp",
                                             "state1_polity",  
                                             "absdiff_pol", 
                                             "state1_military_expenditure_perc_gdp",
                                             "state1_nterror_log",
                                             "refugees_incoming_log",
                                             "relig_muslim",
                                             "relig_other",
                                             "diff_relig_shrt",
                                             "colony", 
                                             "comlang_off"),
                                  labels = c("GDP pc (log), builder",
                                             "GDP pc, ratio",
                                             "Polity, builder",
                                             "Polity, abs. difference",
                                             "Military expenditures pc (log), builder",
                                             "Terror incidents (log), builder",
                                             "Refugees, incoming (log)",
                                             "Religion, Muslim\n[Ref.: Christian]",
                                             "Religion, Other\n[Ref.: Christian]",
                                             "Same religion",
                                             "Colonial history",
                                             "Common language"))))

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
  ylim(-0.6, 0.6) +
  coord_flip() +
  labs(
    title = "",
    x = "", y = "") +
  theme_minimal()

# ggeffects (see: https://strengejacke.github.io/ggeffects/)
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
        variables = c("state1_gdp_log"), 
        change = "sd")))) %>%
  select(-model) %>%
  unnest(cols = estimate) %>%
  select(1:3) %>% 
  arrange(factor)

                          ##########################
                          #   MULTIPLE IMPUTATION  #
                          ##########################

# Get the variables used in the multinomial model
vars <- str_replace_all(paste0("state1_typology|", iv[[12]]), "[ +\n ]+", "|")

# Select models vars
# Note: state1: clustervar
model.df <- border.df %>%
  select(matches(vars), state1) %>%
  select(-c(1:5, "state1_typology_fct")) %>%
  mutate(state1_typology = fac_ind_en(state1_typology),
         state1_typology = fct_relevel(state1_typology, "Checkpoint"),
         state1_relig_shrt = factor(state1_relig_shrt)) %>%
  rename(state1_military = state1_military_expenditure_perc_gdp)

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
# excluded: imp_method[c("diff_relig")] <- "logreg"
imp_method[c("state1_relig_shrt")] <- "polyreg"

# Create imputed datasets
model_imp.df <- mice(model.df, m = 50, predictorMatrix = pred.mat, 
                     method = imp_method, print = FALSE)           #set.seed

# Export to Stata
# Adopted from: https://stackoverflow.com/questions/49965155/importing-mice-object-to-stata-for-analysis
# Gather complete models incl. base data
imp_out <- mice::complete(model_imp.df, "long", include = TRUE) %>%
  rename("_mj" = ".imp",
         "_mi" = ".id")

# Export
# Stata
export(imp_out, file = "./output/stata/imputed_data.dta")

# R
export(model_imp.df, file = "./output/imputed_data.rds")

                           ##########################
                           #         Stata          #
                           ##########################

# ---------------------------------------------------------------------------- #
# * Load file created in R
# use "C:\Users\guelzauf\Seafile\Meine Bibliothek\Projekte\C01_Grenzen\Data\Analysis\Border Data\output\stata\imputed_data.dta" 

# * Declare as multiple imputed data
# mi import ice

# * Multinomial regression
# mi estimate : mlogit state1_typology state1_gdp_log ratio_gdp state1_polity absdiff_pol state1_military state1_nterror_log i.state1_relig_shrt i.diff_relig_shrt i.colony i.comlang_off refugees_incoming_log, vce(cluster state1)

# * https://www.stata.com/statalist/archive/2012-03/msg00927.html
# est sto ml

# forval i = 2/5 {
#  est res ml
#  mimrgns, dydx(*) pr(out(`i')) post
# est sto ml`i'
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
  mutate(across(everything(), ~car::recode(.x,  "c('0', '.', '.,.') = NA_character_"))) %>%
  filter(across(everything(), ~!is.na(.x))) %>%
  mutate(type = str_extract(variable, "coef$+|se$+|t$+|p$+|ci$+"),
         variable = str_replace(variable, "_coef$+|_se$+|_t$+|_p$+|_ci$+", "")) %>%
  pivot_longer(2:5, names_to = "typology") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  separate(ci, into = c("conf.low", "conf.high"), sep = ",") %>%
  mutate_at(vars(3:8), as.numeric) %>%
  mutate(variable = case_when(
    variable == "2.state1_relig_shrt" ~ "relig_muslim",
    variable == "3.state1_relig_shrt" ~ "relig_other",
    TRUE ~ as.character(variable)),
    variable = str_replace_all(variable, "1\\.", "")) %>%
  mutate(pstars = stars.pval(p),
         variable = fct_rev(factor(variable, 
                                   levels = c("state1_gdp_log",
                                              "ratio_gdp",
                                              "state1_polity",  
                                              "absdiff_pol", 
                                              "state1_military",
                                              "state1_nterror_log",
                                              "refugees_incoming_log",
                                              "relig_muslim",
                                              "relig_other",
                                              "diff_relig_shrt",
                                              "colony", 
                                              "comlang_off"),
                                   labels = c("GDP pc (log), builder",
                                              "GDP pc, ratio",
                                              "Polity, builder",
                                              "Polity, abs. difference",
                                              "Military expenditures pc (log), builder",
                                              "Terror incidents (log), builder",
                                              "Refugees, incoming (log)",
                                              "Religion, Muslim\n[Ref.: Christian]",
                                              "Religion, Other\n[Ref.: Christian]",
                                              "Same religion",
                                              "Colonial history",
                                              "Common language"))))

# Plot
# Create coefplots
result_mnom_ame.fig <- ame_results.df %>%
  ggplot() +
  # not siginificant
  geom_point(data = function(x){x[!x$pstars %in% c("*" ,"**", "***"),]},
             aes(x = variable, y = coef), stat = "identity", alpha = .3) +
  geom_errorbar(data = function(x){x[!x$pstars %in% c("*" ,"**", "***"),]}, 
                aes(x = variable, ymin = conf.low, ymax = conf.high), alpha = .3) +
  # significant
  geom_point(data = function(x){x[x$pstars %in% c("*" ,"**", "***"),]},
             aes(x = variable, y = coef), stat = "identity") +
  geom_errorbar(data = function(x){x[x$pstars %in% c("*" ,"**", "***"),]}, 
                aes(x = variable, ymin = conf.low, ymax = conf.high)) +
  #
  geom_hline(yintercept = 0, colour = "gray", linetype = 2) +
  facet_wrap(.~fac_ind_en(typology)) +
  scale_x_discrete(drop = FALSE) + 
  ylim(-.2, .2) +
  coord_flip() +
  labs(x = "", y = "") +
  theme.basic +
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.spacing.x = unit(2, "lines"))

                          ##########################
                          #   MICE: MULTINOM IN R  #
                          ##########################

# adopted for mice from: https://www.andrewheiss.com/blog/2018/03/07/amelia-tidy-melding/

# Tidy dataframe with nested imputations
imp_nest.df <- import("./output/imputed_data.rds") %>%
  complete(., include = F, action = "long") %>%
  rename(imp_no = `.imp`) %>%
  group_by(imp_no) %>%
  nest() %>%
  mutate(formula = paste0("state1_typology", " ~ ", 
                          str_replace_all(iv[12],
                                          "state1_military_expenditure_perc_gdp",
                                          "state1_military")))

# Model across dataframes
imp_nest.df <- imp_nest.df %>%
  mutate(model = map2(.x = formula, .y = data, 
                      ~multinom(as.formula(.x), data = .y, 
                                family = binomial(link = "logit"))))

# Combine dataframes with categories in order to map margins-command
imp_nest.df <- imp_nest.df %>%
  mutate(category = c("'No man's land';Checkpoint;Fortified;Barrier;Landmark")) %>%
  separate_rows(category, sep = ";")

# Apply margins-command
imp_nest.df <- imp_nest.df %>%
  mutate(ame = pmap(list(model, category, data),
                    ~summary(margins(model = ..1, category = ..2, data = ..3,  change = "sd")) %>%
                      select(factor, AME)))

# Retain only AMEs and prepare for combining using Amelia::mi.meld
imp_ame.df <- imp_nest.df %>%
  unnest(cols = ame) %>%
  select(imp_no, category, factor, ame = AME) %>%
  spread(factor, ame) %>%
  group_by(category) %>%
  nest()

# Combine with adjusted mi.meld
# ---------------------------------------------------------------------------- #
# Code adopted from Amelia::mi.meld

# Wrap Amelia::mi.meld into a function
# Note: We only have coefficients not SE (not yet supported by margins)
meld_coef_fun <- function(x){
  # data
  df <- x %>% 
    select(-1)
  # nrow
  am.m <- nrow(df)
  # ones 
  ones <- matrix(1, nrow = 1, ncol = am.m)
  # combine quantities
  imp.q <- as_tibble((ones %*% as.matrix(df)) / am.m)
  # return
  return(imp.q)
}

# Apply function
imp_ame.df <- imp_ame.df %>%
  mutate(imp_ame = map(.x = data, 
                       ~meld_coef_fun(.x)))


# Computations for the results section
# ---------------------------------------------------------------------------- #
# change from mean(df$var) +/- sd(df$var)
ame_sd.df <- imp_ame.df %>%
  unnest(imp_ame) 

ame_sd.df %>%
  select(category)

# Mean, SD, Mean +/- 1 SD
# Continuous predictors
pred <- colnames(ame_sd.df)[c(3:12)]
pred[which("state1_military" == pred)] <- "state1_military_expenditure_perc_gdp"

# Get distribution on continuous predictors
mean_sd.df <- border.df %>%
  select(all_of(pred)) %>%
  psych::describe() %>%
  as.data.frame() %>%
  select(mean, sd) %>%
  rownames_to_column(., var = "variable") %>%
  mutate(from = mean - sd,
         to = mean + sd)

                          ##########################
                          #       EXPORT FIGS      #
                          ##########################

# Figure 2
# Relative distribution of border infrastructure
ggsave(
  plot = ind_perc_region.fig, "Y:/Grenzen der Welt/Projekte/Walls, barriers, checkpoints and landmarks/Figures/Fig2 - Typology By Region.tiff", 
  width = 8, height = 8, unit = "in",
  dpi = 300
)

# Table 1
# Bivariate relationship
gtsave(
  border_vars.plot, "Y:/Grenzen der Welt/Projekte/Walls, barriers, checkpoints and landmarks/Figures/Tab1 - Descriptive Statistics.png"
)

# Figure 3
# Religion, categorical
ggsave(
  plot = relig.fig, "Y:/Grenzen der Welt/Projekte/Walls, barriers, checkpoints and landmarks/Figures/Fig3 - Religion.tiff", 
  width =  8, height = 4, unit = "in",
  dpi = 300
)

# Figure 4
# Multinomial regression (AME)
ggsave(
  plot = result_mnom_ame.fig, "Y:/Grenzen der Welt/Projekte/Walls, barriers, checkpoints and landmarks/Figures/Fig4 - Multinomial Regression - AME.tiff", 
  width = 14, height = 8.5, unit = "in",
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
