# Border Data: Mapping border fortification
### ------------------------------------------------------------------------ ###

# Notes:
# - add Ceuta/Melilla

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, countrycode, sf, rnaturalearth, cowplot, gganimate)

# Get base maps and data
### ------------------------------------------------------------------------ ###
# Get a map from rnaturalearth
world.sf <- ne_countries(returnclass = "sf") %>%
  filter(geounit != "Antarctica" & geounit != "French Southern and Antarctic Lands")

# Boundaries also from rnaturalearth
# boundaries <- ne_download(scale = 50, type = 'boundary_lines_land', category = 'cultural', returnclass = "sf")
boundaries <- st_read("C:\\Users\\guelzauf\\Seafile\\Meine Bibliothek\\Code\\NaturalEarth\\ne_10m_admin_0_boundary_lines_land")

# Read data on border fences
# Avdan (2019) (created in: BordersJoin.R)
barriers.av <- readRDS("./data/border data/Avdan 2019.rds")

# Create a dataframe where the dyads are mirrored
### ------------------------------------------------------------------------ ###
barriers.av %>%
  mutate(stateA = state2, 
         stateB = state1) %>%
  select(-c(state1, state2)) %>%
  rename(state1 = stateA,
         state2 = stateB) -> barriers.trnd

# Join both dataframes (fence data is undirected now)
barriers.av %>%
  bind_rows(barriers.trnd) %>%
  arrange(state1, state2) -> barriers.join

# Join to boundary data
boundaries %>%
  left_join(barriers.join, by = c("adm0_a3_l" = "state1", "adm0_a3_r" = "state2")) %>%
  filter(!is.na(indicator) & !is.na(year)) -> boundaries.fncd 

# Visualization
### ------------------------------------------------------------------------ ###
fence.map <- ggplot(data = world.sf) +
  geom_sf() +
  geom_sf(data = boundaries.fncd, colour = "black", size = 1.2) +
  labs(title = "Global border fences, 1950-2015",
       caption = "Data: Avdan (2019)",
       x = "", 
       y = "") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = "transparent"),
        text = element_text(size = 14))

### ------------------------------------------------------------------------ ###
# Bar graph
barriers.av %>%
  mutate(ryear = round(year / 5) * 5) %>%
  group_by(ryear) %>%
  summarise(count = n()) -> fence.bar
 
# Cumulative line graph
barriers.av %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  mutate(csum = cumsum(count)) -> fence.line

# (Missings are not plotted)
fence.addin <- ggplot() +
  geom_bar(data = fence.bar, 
           aes(x = ryear, y = count), stat = "identity") +
  geom_line(data = fence.line, aes(x = year, y = csum), size = 1) +
  labs(title = "Border fences over time",
       subtitle = "Line indicates cumulative sum",
       x = "", y = "") +
  scale_x_continuous(breaks = seq(1950, 2020, 5), 
                     labels = c("1950", "", "1960", "", "1970", "", "1980", "",
                                "1990", "", "2000", "", "2010", "", "")) +
  theme_minimal() + 
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))

# Combine plots
fence.fig <- ggdraw() +
  draw_plot(fence.map, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(fence.addin, .3, -.05, .5, .4, scale = .7)

# left side (bar chart)
# -0.1, 0.12, 0.5, 0.4, scale = .65

# Distribution of fences across continents and subregions 
# Source: https://en.wikipedia.org/wiki/UN_M.49
### ------------------------------------------------------------------------ ###
barriers.av %>%
  mutate(region1 = countrycode(state1, "iso3c", "region"),
         region2 = countrycode(state2, "iso3c", "region"),
         continent1 = countrycode(state1, "iso3c", "continent"),
         continent2 = countrycode(state2, "iso3c", "continent")) -> barriers.av
  
barriers.av %>%
  group_by(continent1) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         percentage = n / total * 100)

#
beepr::beep(4)