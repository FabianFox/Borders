# FRONTEX Risk Analysis Network (FRAN) Reports
# Data from: https://frontex.europa.eu/along-eu-borders/migratory-map/

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, sf, rnaturalearth, ggmap, ggforce, ggrepel)

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
# Run only once. The saved file is available below.
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
  location = c("Kayseri, Turkey", "Vama Veche, Romania", 
               "Sirte, Libya", "Birbuba, Malta", "Caltanissetta, Italy",
               "Tirana, Albania", "Tricca, Greece",
               "Schytomyr, Ukraine", "Pinks, Belarus", "Warsaw, Poland",
               "Kayseri, Turkey", "Istanbul, Turkey", "Lamia, Greece",
               "Ponta Delgada, Portugal", "Ousseant, France",
               "Dakhla, Western Sahara", "Funchal, Portugal",
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
lines.df[14,3] <- -12.92 # lon
lines.df[14,4] <- 46.36 # lat