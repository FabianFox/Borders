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
  byid=TRUE)

# Plotting
# Coloring could be adjusted following: https://gis.stackexchange.com/questions/91775/coloring-spatiallines-by-attribute-in-r
# Base map
plot(europe.sp)

# Overplot with emphasized (shared) borders
plot(borders, add = TRUE, lwd = 2, col = "navy") 

