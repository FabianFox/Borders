# FRONTEX Risk Analysis Network (FRAN) Reports
# Data from: https://frontex.europa.eu/along-eu-borders/migratory-map/

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, sf, rnaturalearth)

# from Frontex (xlsx)
## ------------------------------------------------------------------------------------------------------------ ##
routes.df <- import(file = "https://frontex.europa.eu/assets/Migratory_routes/Detections_of_IBC_2018_07_06.xlsx",
                    sheet = 1) %>%
  gather(year, crossings, -c(Route, BorderLocation, NationalityLong)) %>%
  mutate(year = as.Date(as.numeric(year), origin = "1899-12-30"))

# Base map from rnaturalearth
world.sf <- ne_countries(returnclass = "sf") %>%
  filter(region_wb %in% c("Europe & Central Asia", "Middle East & North Africa")) 

# Plot
ggplot(data = world.sf) +
  geom_sf() +
  coord_sf(xlim = c(-20, 50), ylim = c(30, 65)) +
  geom_bspline(data = lines.df, mapping =  aes(x = lon, y = lat, group = route),
                                               arrow = arrow(length = unit(0.2, unit = "cm")))
