# Border Data: Mapping border fortification

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, countrycode, sf, rnaturalearth)

# By now, this is just a test case. The ultimate goal is to identify border lines between
# neighbouring countries. 

# Get a map from rnaturalearth
world.sf <- ne_countries(returnclass = "sf")

# Filter to two exemplary countries
europe.sf <- world.sf %>%
  filter(sovereignt %in% c("Germany", "Netherlands"))

# Create a list that tells us whether two countries touch
# (Code adopted from StackOverflow https://tinyurl.com/yacnowxu)
Touching_List <- st_touches(europe.sf)

# Get the line between those touching countries
from <- 1
to <- 1
line <- st_intersection(europe.sf[from,], europe.sf[Touching_List[[from]][to],])

# plot
plot(st_geometry(europe.sf[c(from, Touching_List[[from]][to]),]))
plot(line, add = TRUE, col = "red", lwd = 2)
