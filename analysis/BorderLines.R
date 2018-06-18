# Border Data: Mapping border fortification

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, countrycode, sp, rnaturalearth, rgeos)

# By now, this is just a test case. The ultimate goal is to identify border lines between
# neighbouring countries. 

# Get a map from rnaturalearth
world.sp <- ne_countries(returnclass = "sp")

# Filter to two exemplary countries
europe.sp <- world.sp[world.sp@data$region_un == "Europe" & world.sp@data$sovereignt != "Iceland",]

# Create a list that tells us whether two countries touch
# (Code adopted from StackOverflow https://stackoverflow.com/questions/35794772/r-gis-find-borders-between-polygons)
borders <- gDifference(
  as(europe.sp,"SpatialLines"),
  as(gUnaryUnion(europe.sp),"SpatialLines"),
  byid = TRUE)

## Test case for color plotting of borders ## 

# Create an identifying variable without missings
iso_a3 <- europe.sp@data$iso_a3 
iso_a3[[21]] <- "XKX"

# Add the variable to the SpatialLines object
for (id in 1:38) {
  borders@lines[[id]]@ID <- iso_a3[[id]]
}

# Create a SpatialLinesDataFrame
borders.spdf <- SpatialLinesDataFrame(borders, data = data.frame(iso_a3 = iso_a3,
                                                                variable = round(seq(1, 4, length.out = 38), 0),
                                                                row.names = iso_a3),
                                     match.ID = TRUE)

# Plotting
# Coloring could be adjusted following: https://gis.stackexchange.com/questions/91775/coloring-spatiallines-by-attribute-in-r
# Base map
plot(europe.sp)

# Overplot with emphasized (shared) borders
plot(borders.spdf, add = TRUE, lwd = 2, col = borders.spdf@data$variable) 
