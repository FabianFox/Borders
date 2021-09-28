# Graphical representations of the AMEs across range of predictors

# Load/install packages
## -------------------------------------------------------------------------- ##
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "countrycode", "janitor", "broom", "margins",
            "patchwork", "gtools", "nnet", "ggeffects", "mice", "rio", "gt", "lemon")

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
  "disp_from_2000_to_2014",
  "state1_relig_shrt",
  "relig_prox",
  "colony", 
  "comlang_off",
  
  # Full model
  "state1_gdp_log +
  ratio_gdp +
  state1_polity +
  absdiff_pol + 
  state1_military_expenditure_perc_gdp +
  state1_nterror_log +
  disp_from_2000_to_2014 + 
  state1_relig_shrt +
  relig_prox +
  colony +
  comlang_off"
)

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
imp_nest_long.df <- imp_nest.df %>%
  mutate(category = c("'No man's land';Checkpoint;Fortified;Barrier;Landmark")) %>%
  separate_rows(category, sep = ";") # Apply margins-command (here: sd-change; to replicate stata use "dydx")

# Plot 1: GDP per capita
## -------------------------------------------------------------------------- ##
# Change for different graphs
var <- border.df$state1_nterror_log

# Get AMEs for mean +/- SD
imp_nest_long.df <- imp_nest_long.df %>%
  mutate(ame = pmap(list(model, category, data),
                    ~summary(margins(model = ..1, category = ..2, data = ..3,  
                                     at = list(state1_nterror_log = c(mean(var, na.rm = TRUE) - sd(var, na.rm = TRUE),
                                                   mean(var, na.rm = TRUE),
                                                   mean(var, na.rm = TRUE) + sd(var, na.rm = TRUE))))) %>%
                      filter(factor == "state1_nterror_log") %>%
                      select(value = state1_nterror_log, AME) %>%
                      mutate(variable = c("Mean - 1SD", "Mean", "Mean + 1SD"))))
                      
# Retain only AMEs and prepare for combining using Amelia::mi.meld
imp_ame.df <- imp_nest_long.df %>%
  unnest(cols = ame) %>%
  select(imp_no, category, variable, value, ame = AME) %>%
  group_by(category, variable) %>%
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

# Prepare data for plotting
imp_ame.df <- imp_ame.df %>%
  select(-data) %>%
  unnest(imp_ame) 

# Plotting
# ---------------------------------------------------------------------------- #
# Indexing
index.df <- imp_ame.df %>%
  ungroup() %>%
  filter(variable == "Mean + 1SD") %>%
  select(category, ame)

imp_ame.df <- imp_ame.df %>%
  mutate(category_fct = factor(category, 
                               levels = c("'No man's land'",
                                          "Landmark", 
                                          "Checkpoint",
                                          "Barrier",
                                          "Fortified"),
                               labels = c("'No-man's-land'",
                                          "Landmark", 
                                          "Checkpoint", 
                                          "Barrier", 
                                          "Fortified")))

# Plot
ame_across_pred5.fig <- ggplot(imp_ame.df, 
                              aes(x = value, y = ame, 
                                  shape = category_fct)) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_shape_manual(values = c(15, 16, 17, 18, 8)) +
  labs(x = "", y = "", 
       title = "Terrorist incidents (log), builder (mean -/+ 1 SD)", shape = "Legend") +
  theme.basic +
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),
        strip.text = element_text(size = 14),
        panel.spacing.x = unit(3, "lines"),
        panel.spacing.y = unit(3, "lines"),
        legend.position = "none") # remove to get a legend for g_legend below

# Combine plots
## -------------------------------------------------------------------------- ##
# extract the legend
legend <- lemon::g_legend(ame_across_pred5.fig)

# Combine
ame_across_pred.fig <- wrap_plots(ame_across_pred1.fig, ame_across_pred2.fig,
                                     ame_across_pred3.fig, ame_across_pred4.fig,
                                     ame_across_pred5.fig, legend, nrow = 3, ncol = 2)

# Save plot
ggsave(
  plot = ame_across_pred.fig, "Y:/Grenzen der Welt/Projekte/Walls, barriers, checkpoints and landmarks/Figures/Figure A - AME across Predictor Range.tiff", 
  width = 16, height = 9, unit = "in",
  dpi = 300
)
