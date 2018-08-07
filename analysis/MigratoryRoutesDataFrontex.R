# FRONTEX Risk Analysis Network (FRAN) Reports
# Data from: https://frontex.europa.eu/along-eu-borders/migratory-map/

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, sf, rnaturalearth, ggmap)

# from Frontex (xlsx)
## ------------------------------------------------------------------------------------------------------------ ##
routes.df <- import(file = "https://frontex.europa.eu/assets/Migratory_routes/Detections_of_IBC_2018_07_06.xlsx",
                    sheet = 1) %>%
  gather(year, crossings, -c(Route, BorderLocation, NationalityLong)) %>%
  mutate(year = as.Date(as.numeric(year), origin = "1899-12-30"))

# Base map from rnaturalearth
world.sf <- ne_countries(returnclass = "sf") %>%
  filter(region_wb %in% c("Europe & Central Asia", "Middle East & North Africa")) 

# Create a data set for the lines
# (1) Data set with empty coordinates which roughly comply with the routes provided by Frontex
lines.df <- tibble(
  route = c(rep("Black Sea", 2),
            rep("Central Mediterranean", 3)),
  location = c("Amasra, Turkey", "Vama Veche, Romania", 
               "Sirte, Libya", "Birbuba, Malta", "Caltanissetta, Italy"),
  lon = NA,
  lat = NA
)

# (2) Fill the missing coordinates using ggmap::geocode
# for loop with if-condition for warnings
# run several times until no more NA's are returned
for(i in 1:nrow(lines.df)) {
  if (is.na(lines.df$lon[i])) {
    result <- tryCatch(geocode(lines.df$location[i], output = "latlon", source = "google"),
                       warning = function(x) data.frame(lon = NA, lat = NA))
    lines.df$lon[i] <- as.numeric(result[1])
    lines.df$lat[i] <- as.numeric(result[2])
  }
  i <- i + 1
  Sys.sleep(sample(seq(.5, 2, 0.5), 1))  
}

# Plot
ggplot(data = world.sf) +
  geom_sf() +
  coord_sf(xlim = c(-20, 50), ylim = c(30, 65)) +
  geom_bspline(data = lines.df, mapping =  aes(x = lon, y = lat, group = route),
                                               arrow = arrow(length = unit(0.2, unit = "cm")))
