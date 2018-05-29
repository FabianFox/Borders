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
# (Code adopted from StackOverflow https://tinyurl.com/yacnowxu)
Touching_List <- gTouches(europe.sp, byid = TRUE, returnDense = FALSE)

# Perimeters
perimeters <- sp::SpatialLinesLengths(as(europe.sp, "SpatialLines"))

# Loop over the Touching_List and return lines
lines <- vector(mode = "list", length = length(Touching_List))
result <- vector(mode = "list", length = length(Touching_List))
for (from in seq_along(Touching_List)) {
  lines[[from]] <- rgeos::gIntersection(europe.sp[from,], europe.sp[Touching_List[[from]],], byid = TRUE)
  l_lines <- sp::SpatialLinesLengths(lines[[from]])
  result[[from]] <- data.frame(origin = from,
                                perimeter = perimeters[from],
                                touching = Touching_List[[from]],
                                t.length = l_lines,
                                t.pc = 100*l_lines/perimeters[from])
}

# Create data frame with information on shared borders
# Basic data frame 
result.df <- do.call("rbind", result)

# Lookup table for matching
lookup.cntry <- tibble(
  number = seq_along(unique(europe.sp$sovereignt)),
  country = unique(europe.sp$sovereignt)
)

# Lookup-match
result.df$origin.name <- lookup.cntry[match(result.df$origin, lookup.cntry$number),]$country
result.df$touching.name <- lookup.cntry[match(result.df$touching, lookup.cntry$number),]$country

# Plotting
# Coloring could be adjusted following: https://gis.stackexchange.com/questions/91775/coloring-spatiallines-by-attribute-in-r
# Base map
plot(europe.sp)

# Overplot with emphasized (shared) borders
map(lines, ~plot(., add = TRUE, lwd = 2, col = 1 + 1:length(Touching_List)))

