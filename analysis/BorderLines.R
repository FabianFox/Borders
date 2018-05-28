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
all.length.list <- lapply(1:length(Touching_List), function(from) {
  lines <- rgeos::gIntersection(europe.sp[from,], europe.sp[Touching_List[[from]],], byid = TRUE)
  l_lines <- sp::SpatialLinesLengths(lines)
  res <- data.frame(origin = from,
                    perimeter = perimeters[from],
                    touching = Touching_List[[from]],
                    t.length = l_lines,
                    t.pc = 100*l_lines/perimeters[from])
  res
})
all.length.df <- do.call("rbind", all.length.list)

