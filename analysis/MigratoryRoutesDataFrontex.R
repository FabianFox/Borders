# FRONTEX Risk Analysis Network (FRAN) Reports
# Data from: https://frontex.europa.eu/along-eu-borders/migratory-map/

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, sf, rnaturalearth, ggmap, ggforce)

# from Frontex (xlsx)
## ------------------------------------------------------------------------------------------------------------ ##
routes.df <- import(file = "https://frontex.europa.eu/assets/Migratory_routes/Detections_of_IBC_2018_07_06.xlsx",
                    sheet = 1) %>%
  gather(year, crossings, -c(Route, BorderLocation, NationalityLong)) %>%
  mutate(date = as.Date(as.numeric(year), origin = "1899-12-30"))

# Base map from rnaturalearth
world.sf <- ne_countries(returnclass = "sf") %>%
  filter(region_wb %in% c("Europe & Central Asia", "Middle East & North Africa", "Sub-Saharan Africa")) 

# Create a data set for the lines
# (1) Data set with empty coordinates which roughly comply with the routes provided by Frontex
lines.df <- tibble(
  route = c(rep("Black Sea", 2),
            rep("Central Mediterranean", 3),
            rep("Circular Route from Albania to Greece", 2), 
            rep("Eastern Land Borders", 3),
            rep("Eastern Mediterranean", 3),
            rep("Other", 2),
            rep("Western African", 2),
            rep("Western Balkans", 4),
            rep("Western Mediterranean", 4)),
  location = c("Amasra, Turkey", "Vama Veche, Romania", 
               "Sirte, Libya", "Birbuba, Malta", "Caltanissetta, Italy",
               "Tirana, Albania", "Tricca, Greece",
               "Schytomyr, Ukraine", "Pinks, Belarus", "Warsaw, Poland",
               "Kayseri, Turkey", "Istanbul, Turkey", "Lamia, Greece",
               "Ponta Delgada, Portugal", "Ousseant, France",
               "Dakhla, Western Sahara", "Santa Cruz de Tenerife, Canarias",
               "Thessaloniki, Greece", "Skopje, Macedonia", "Pristina, Kosovo", "Belgrade, Serbia",
               "Abadla, Algeria", "Taza, Marocco", "El Jebha, Marocco", "Madrid, Spain"),
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

# Ponte Delgada seems a little far
# Choose random closer point in the Atlantic Sea
lines.df[14,3] <- -12.92
lines.df[14,4] <- 46.36

# Plot
ggplot(data = world.sf) +
  geom_sf() +
  coord_sf(xlim = c(-20, 50), ylim = c(30, 65)) +
  geom_bspline(data = lines.df, mapping =  aes(x = lon, y = lat, group = route),
                                               arrow = arrow(length = unit(0.4, unit = "cm")))

# Also possible to map by size of unauthorized border crossings
# For the latest month
routes.df <- routes.df %>%
  mutate(year = as.numeric(str_extract_all(date, "[:digit:]{4}"))) %>%
  filter(year == 2018) %>%
  group_by(Route) %>%
  summarise(crossings = sum(crossings)) 

# Join to the lines.df
lines.df <- lines.df %>%
  left_join(routes.df, by = c("route" = "Route"))

# Plot with linesize (only 2018)
ggplot(data = world.sf) +
  geom_sf() +
  coord_sf(xlim = c(-20, 50), ylim = c(20, 65)) +
  geom_bspline(data = lines.df, mapping =  aes(x = lon, y = lat, group = route, size = crossings),
               arrow = arrow(length = unit(0.2, unit = "cm"), type = "open"))