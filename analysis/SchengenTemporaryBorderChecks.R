# Data on temporarily reinstated border controls in the Schengen Area

# Source: https://ec.europa.eu/home-affairs/what-we-do/policies/borders-and-visas/schengen/reintroduction-border-control_en
# pdf: https://ec.europa.eu/home-affairs/sites/homeaffairs/files/what-we-do/policies/borders-and-visas/schengen/reintroduction-border-control/docs/ms_notifications_-_reintroduction_of_border_control_en.pdf

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest, qdap, rio, tidyr, stringr, countrycode)

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
    End = as.numeric(End))

# Prepare the data frame for plotting (general number of temporary border controls)
bcontrol.plot.df <- bcontrol.df %>%
  group_by(Begin, migration) %>%
  distinct(`Member State`, Begin, migration, .keep_all = TRUE) %>%
  summarize(checks = n())

# By individual Member States
bcontrol.member.df <-
  bcontrol.df %>%
  filter(Begin >= 2015, migration == "Migration") %>%
  group_by(`Member State`) %>%
  distinct(`Member State`, Begin, .keep_all = TRUE) %>%
  summarize(checks = n()) %>%
  arrange(desc(checks))

# Plot of border checks by year and type
# Note: States can reinstate border checks several times per year. Here, each type is counted only once
#       per year. 
bcontrol.plot <- ggplot(bcontrol.plot.df) +
  geom_bar(aes(x = Begin, y = checks, fill = migration), stat = "identity") +
  labs(title = "Number of temporary border controls, 2006 - 2017",
       caption = "Source: European Commission: Migration and Home Affairs.\nNote: Each type of border control is only counted once per year.",
       x = "", y = "") +
  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0,10)) +
  scale_x_continuous(breaks = seq(2006, 2018, 4)) +
  scale_fill_manual("Reason", values = c("Migration" = "#CCCCCC", "Other" = "#4D4D4D")) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))

# ggsave(filename = "./FRAN-reports/BorderChecksFigure.tiff", device = "tiff", dpi = 600, plot = bcontrol.plot)
saveRDS(bcontrol.member.df, file = "./data/TempControlSchenge.rds")
