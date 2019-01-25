# Data on temporarily reinstated border controls in the Schengen Area

# Source: https://ec.europa.eu/home-affairs/what-we-do/policies/borders-and-visas/schengen/reintroduction-border-control_en
# pdf: https://ec.europa.eu/home-affairs/sites/homeaffairs/files/what-we-do/policies/borders-and-visas/schengen/reintroduction-border-control/docs/ms_notifications_-_reintroduction_of_border_control_en.pdf

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest, qdap, rio, tidyr, stringr, countrycode, cowplot)

# Temporary border checks
### ------------------------------------------------------------------------###
# Location of file
loc <- "./FRAN-reports/Member States' Notifications of Reintroduction of border control.xlsx"

# Load the data
bcontrol.df <- import(loc)[2:96,] %>%
  setNames(.[1,]) %>%
  .[-1,]

# Seperate dates 
year_duration <- str_extract_all(bcontrol.df$Duration, "[:digit:]{4}")

# Add Begin/End of border control to data frame
bcontrol.df <- bcontrol.df %>%
  mutate(Begin = map(year_duration, 1),
         End = map(year_duration, 2),
         End = if_else(lengths(year_duration) == 1, Begin, End),
         Begin = unlist(Begin),
         End = unlist(End))

# Correct a parsing problem 
bcontrol.df[which(bcontrol.df$Begin == "4008"), c("Begin", "End")] <- c("2009", "2009")

# By now excluding some events concerning security issues
bcontrol.df <- bcontrol.df %>%
  mutate(
    migration = case_when(
      str_detect(tolower(`Reasons/Scope`), "migration|migrant|migratory|influx|recommendation|movements") == TRUE ~ "Migration",
      str_detect(tolower(`Reasons/Scope`), "migration|migrant|migratory|influx|recommendation|movements") == FALSE ~ "Other"),
    country = countrycode(`Member State`, "country.name.en", "iso3c"),
    Begin = as.numeric(Begin),
    End = as.numeric(End)) %>%
  as_tibble() 

# Join to EU28-base
year <- 2015:2018
migration <- c("Migration", "Other")

bcontrol.expand.df <- tibble(
  country = countrycode::codelist$iso3c
) %>%
  mutate(eu28 = countrycode(country, "iso3c", "eu28")) %>%
  filter(eu28 == "EU") %>%
  expand(country, year, migration) 

# By individual Member States
bcontrol.expand.join <- bcontrol.df %>%
  filter(Begin >= 2015) %>%
  group_by(`Member State`, Begin, migration, country) %>%
  distinct(`Member State`, Begin, migration, .keep_all = TRUE) %>%
  summarize(checks = n()) %>%
  arrange(desc(checks))

# Join data to the expanded df
bcontrol.expand.df <- bcontrol.expand.df %>%
  left_join(bcontrol.expand.join, by = c("country", "migration", "year" = "Begin")) %>%
  mutate(member_state = countrycode(country, "iso3c", "country.name.en"),
         checks = ifelse(is.na(checks), 0, checks)) %>%
  select(country, year, member_state, migration, checks) 

# Housekeeping
rm(year_duration, bcontrol.expand.join, loc, migration, year)


# Visualization
### ------------------------------------------------------------------------###

# Note: States can reinstate border checks several times per year. Here, each type is counted only once
#       per year. 

# Over time (only countries in grouping variable, see AsylumData.R))
bcontrol.plot <- bcontrol.df %>%
  group_by(Begin, migration) %>%
  distinct(`Member State`, Begin, migration, .keep_all = TRUE) %>%
  mutate(group = case_when(
    country %in% c("CZE", "HUN", "POL", "SVK") ~ "Visegrád Group",
    country %in% c("AUT", "BEL", "DNK",  "DEU", "FIN", "LUX", "SWE") ~ "Host states",
    country %in% c("CYP", "GRC", "ITA", "MLT") ~ "Frontline states"
  )) %>%
  filter(!is.na(group)) %>%
  summarize(checks = n()) %>%
  ggplot() +
  geom_bar(aes(x = Begin, y = checks, fill = migration), stat = "identity") +
  labs(title = "Total number of temporary border controls, 2006 - 2018",
       caption = "Source: European Commission: Migration and Home Affairs.\nNote: Each type of border control is only counted once per year.",
       x = "", y = "") +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,10)) +
  scale_x_continuous(breaks = seq(2006, 2018, 4)) +
  scale_fill_manual("Reason: ", values = c("Migration" = "#CCCCCC", "Other" = "#4D4D4D")) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))

# Countries that reinstated border controls at least once from 2015-2018
bcontrol.year.plot <- bcontrol.expand.df %>%
  filter(migration == "Migration") %>%
  group_by(country) %>%
  mutate(sum_checks = sum(checks)) %>%
  filter(sum_checks >= 1,
         checks != 0) %>%
  ggplot() +
  geom_waffle(aes(x = fct_infreq(country), y = year), fill = "#000000") +
  labs(title = "Number of temporary border controls, 2015-2018",
       caption = "Source: European Commission: Migration and Home Affairs.\nNote: Each type of border control is only counted once per year.",
       x = "", y = "") +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))
  

# By grouping variable (see AsylumData.R)
bcontrol.group.df <- bcontrol.expand.df %>%
  group_by(country, year, migration) %>%
  distinct(country, year, migration, .keep_all = TRUE) %>%
  mutate(group = case_when(
    country %in% c("CZE", "HUN", "POL", "SVK") ~ "Visegrád Group",
    country %in% c("AUT", "BEL", "DNK",  "DEU", "FIN", "LUX", "SWE") ~ "Host states",
    country %in% c("CYP", "GRC", "ITA", "MLT") ~ "Frontline states"
  )) %>%
  filter(!is.na(group)) %>%
  group_by(group, year, migration) %>%
  mutate(n_country = n(),
         checks = sum(checks)) %>%
  select(group, year, checks, migration, n_country) %>%
  distinct()

bcontrol.group.plot <- ggplot(bcontrol.group.df) +
  geom_bar(aes(x = year, y = checks, fill = migration), stat = "identity") +
  facet_wrap(~group) +
  labs(title = "Number of temporary border controls by group, 2006 - 2018",
       caption = "Source: European Commission: Migration and Home Affairs.\nNote: Each type of border control is only counted once per year.",
       x = "", y = "") +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,10)) +
  scale_x_continuous(breaks = seq(2015, 2018, 1)) +
  scale_fill_manual("Reason", values = c("Migration" = "#CCCCCC", "Other" = "#4D4D4D")) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))

# Combined plots
legend <- get_legend(bcontrol.plot + theme(legend.position = "bottom"))

combined.control.plot <- plot_grid(
  bcontrol.plot + theme(legend.position = "none"),
  bcontrol.group.plot + theme(legend.position = "none"),
  nrow = 3,
  rel_heights = c(0.4, 0.4, 0.1),
  legend
  )

# Saving data and figures
### ------------------------------------------------------------------------###
# Data:
saveRDS(bcontrol.member.df, file = "./data/TempControlSchengen.rds")

# Plots:
ggsave(filename = "./FRAN-reports/TempControlsSchengenFig.tiff", 
       plot = combined.control.plot, device = "tiff", dpi = 600)

ggsave(filename = "./FRAN-reports/TempControlsSchengenFig_ByCountry.tiff", 
       plot = bcontrol.year.plot, device = "tiff", dpi = 600)
