# Exploratory analysis of African borders

# Notes:
# - Countries that need further checking:
#   - Israel borders Egypt but is not yet coded
#   - Cases with insufficient information:
#   - 

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, countrycode, igraph, rio, janitor)

# Load: Source data and border indicator
### ------------------------------------------------------------------------ ###
# source data
source("SourceData.R")

# indicator
indicator.df <- import("O:\\Grenzen der Welt\\Grenzdossiers\\Typologie\\BorderTypology.xlsx",
                        sheet = 1) %>%
  as_tibble() %>%
  select(1:3, 5:6, 12, 16:17) %>%
  clean_names()

# INDICATOR
# Limit observations to African countries
### ------------------------------------------------------------------------ ###
custom.match <- c("XKX" = "Europe")

africa.df <- indicator.df %>%
  mutate_at(vars(contains("state")), 
            funs(cont = countrycode(., "iso3c", "continent", 
                                    custom_match = custom.match))) %>%
  filter(state1_cont == "Africa" | state2_cont == "Africa")

# Conditions for border indicator

# landmark
# frontier border: fortifications == none & BCP == none (& border agreement == none)
# checkpoint border: fortification == none & BCP == basic | extended
# barrier border: fence / wall / additional_fortificatin == yes (but partially)
# fortified border: fence / wall / additional_fortification == yes 

# SOURCE DATA
# Prepare border.df
## -------------------------------------------------------------------------- ##
border.df <- border.df %>%
  mutate(
    continent1 = countrycode(state1, "iso3c", "continent", custom_match = c("XKX" = "Europe")),
    continent2 = countrycode(state2, "iso3c", "continent", custom_match = c("XKX" = "Europe"))
  ) 

# Join the source and indicator
## -------------------------------------------------------------------------- ##
africa.df <- border.df %>%
  filter(continent1 == "Africa" | continent2 == "Africa") %>%
  left_join(africa.df) %>%
  distinct(state1, state2, .keep_all = TRUE)

# Descriptive analysis
## -------------------------------------------------------------------------- ##

# Function creates a factor variable of the indicator
fac_ind <- function(x){
  factor(x, levels = c("landmark border", "frontier border", "checkpoint border", "barrier border", "fortified border"))
}

# Distribution of border types
# Absolute
ind.dist.fig <- ggplot(africa.df, aes(x = fac_ind(typology))) +
  geom_bar() +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))

# Distribution
# Relative
ind.perc.fig <- africa.df %>%
  group_by(typology) %>%
  summarise(perc = length(typology) / length(africa.df$typology) * 100) %>%
  ggplot(aes(x = fac_ind(typology), y = perc)) +
  geom_bar(stat = "identity") +
  labs(title = "Border typology (relative distribution), Africa",
       caption = paste0("N(borders) = ", length(africa.df$typology),
                        "\nN(countries) = ", length(unique(africa.df$state1))),
       x = "", y = "") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))

# Bivariate 
# GDP, PolityIV
border.af.bvars <- africa.df %>%
  group_by(typology) %>%
  summarise(gdp.mean = mean(state1.gdp, na.rm = TRUE),
            gdp.sd = sd(state1.gdp, na.rm = TRUE),
            polity.mean = median(state1.polity, na.rm = TRUE),
            polity.sd = sd(state1.polity, na.rm = TRUE),
            n = n())

# Plots
gdp.fig <- ggplot(border.af.bvars) +
  geom_bar(aes(x = fac_ind(typology), y = gdp.mean), stat = "identity") +
  labs(title = "Mean GDP per capita, Africa",
       caption = paste0("N(borders) = ", sum(border.af.bvars$n),
                        "\nN(countries) = ", 
                        length(unique(africa.df[!is.na(africa.df$state1.gdp),]$state1)),
                        "\nData: WorldBank (2017)"),
       x = "", y = "") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))

polity.fig <- ggplot(border.af.bvars) +
  geom_bar(aes(x = fac_ind(typology), y = polity.mean), stat = "identity") +
  labs(title = "Mean political system, Africa",
       caption = paste0("N(borders) = ", sum(border.af.bvars$n),
                        "\nN(countries) = ", 
                        length(unique(africa.df[!is.na(africa.df$state1.polity),]$state1)),
                        "\nData: PolityIV (2016)"),
       x = "", y = "") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))   

