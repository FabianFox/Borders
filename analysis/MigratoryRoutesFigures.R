# FRONTEX Risk Analysis Network (FRAN) Reports: 
# FIGURES
# Data from: https://frontex.europa.eu/along-eu-borders/migratory-map/

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, sf, rnaturalearth, ggmap, ggforce, ggrepel, cowplot)

# Load data
# Routes.df
routes.df <- import(file = "https://frontex.europa.eu/assets/Migratory_routes/Detections_of_IBC_2018_07_06.xlsx",
                    sheet = 1) %>%
  gather(year, crossings, -c(Route, BorderLocation, NationalityLong)) %>%
  mutate(date = as.Date(as.numeric(year), origin = "1899-12-30"))

# Load lines.df
lines.df <- readRDS("./FRAN-reports/lines.RDS")

# label.df 
label.df <- tibble(
  route = c("Black Sea\nroute",
            "Central Medi-\nterranean route",
            "Circular\nRoute", 
            "Eastern\n borders route",
            "Eastern\nMediterranean route",
            "Other\nroutes",
            "Western African\nroute",
            "Western Balkan\nroute",
            "Western Medit-\nerranean route"),
  lon = c(32.5, 18.5, 19, 26, 29, -10, -13, 23, -0.5),
  lat = c(43.5, 36, 40, 53, 41, 49, 31, 45, 36)
)

# Plot
map.fig <- ggplot(data = world.sf) +
  geom_sf() +
  coord_sf(xlim = c(-20, 50), ylim = c(20, 65)) +
  geom_bspline(data = lines.df, mapping =  aes(x = lon, y = lat, group = route),
               arrow = arrow(length = unit(0.4, unit = "cm"))) +
  geom_text(data = label.df, mapping = aes(x = lon, y = lat, label = route, hjust = "center")) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = "transparent"))

# Also possible to map by size of unauthorized border crossings
# For the latest month
routes.df <- routes.df %>%
  mutate(year = as.numeric(str_extract_all(date, "[:digit:]{4}"))) %>%
  group_by(Route, year) %>%
  summarise(crossings = sum(crossings)) 

# Join to the lines.df
routes.df <- routes.df %>%
  left_join(lines.df, by = c("Route" = "route"))

# Plot with linesize (only 2018)
route.map <- ggplot(data = world.sf) +
  geom_sf() +
  coord_sf(xlim = c(-20, 50), ylim = c(20, 65)) +
  geom_bspline(data = routes.df[routes.df$year == "2018",], mapping =  aes(x = lon, y = lat, group = Route, size = crossings),
               arrow = arrow(length = unit(0.2, unit = "cm"), type = "open")) +
  theme_void() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())

# Facetted line plot  
routes.fig <- ggplot(data = routes.df) +
  geom_line(aes(x = year, y = crossings, group = Route)) +
  facet_wrap(~Route) +
  theme_minimal()

# Combine figures
plot_grid(map.fig, routes.fig, ncol = 1)

# Extension
# Read up on gganimate
# i.e. https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate