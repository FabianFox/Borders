# Data on EU's 'hotspot' approach

# Load/install packages
### ------------------------------------------------------------------------###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, tabulizer, ggmap, rnaturalearth, sf, ggrepel)

# Data from:
# - http://www.europarl.europa.eu/RegData/etudes/BRIE/2018/623563/EPRS_BRI(2018)623563_EN.pdf
# also available: https://ec.europa.eu/home-affairs/sites/homeaffairs/files/what-we-do/policies/european-agenda-migration/press-material/docs/state_of_play_-_hotspots_en.pdf

# Data from EP (2018) pdf-doc
### ------------------------------------------------------------------------###
# Get the pdf-document
url <- "http://www.europarl.europa.eu/RegData/etudes/BRIE/2018/623563/EPRS_BRI(2018)623563_EN.pdf"
# Download
download.file(url = url, destfile = "./data/EP2018_Hotspots at EU external borders.pdf", mode = "wb")

# Read tables from pdf
loc <- "./data/EP2018_Hotspots at EU external borders.pdf"
 
# Hotspots in Greece 
hotspot.greece <- extract_tables(loc, 3, guess = TRUE)

hotspot.greece <- hotspot.greece[[1]][c(8,10,13,16,20),c(4,5)] %>%
  as.tibble() %>%
  rename(hotspot = V1, capacity = V2) %>%
  mutate(capacity = as.numeric(str_replace_all(capacity, pattern = " ", replacement = "")),
         hotspot = str_extract(hotspot, pattern = "[:alpha:]+(?=[:blank:])"))

# Hotspots in Italy (tabulizer does not recognize the table)
hotspot.italy <- tibble(
  hotspot = c("Taranto", "Trapani", "Messina", "Pozzallo", "Lampedusa"),
  capacity = c(400, 400, 250, 300, 500)
)

# Combine
hotspot.df <- full_join(hotspot.greece, hotspot.italy) %>%
  mutate(lon = NA,
         lat = NA,
         hotspot = paste0(hotspot, c(rep(", Greece", 5), rep(", Italy", 5))))

# Add spatial lon/lat
for(i in 1:nrow(hotspot.df)) {
  if (is.na(hotspot.df$lon[i])) {
    result <- tryCatch(geocode(hotspot.df$hotspot[i], output = "latlon", source = "dsk"),
                       warning = function(x) data.frame(lon = NA, lat = NA))
    hotspot.df$lon[i] <- as.numeric(result[1])
    hotspot.df$lat[i] <- as.numeric(result[2])
  }
  i <- i + 1
  Sys.sleep(sample(seq(.5, 2, 0.5), 1))  
}

# Visualization
### ------------------------------------------------------------------------###
# Plot a map with capacities of hotspots
# (1) Read hotspot data
hotspot.df <- readRDS(file = "./data/hotspot.RDS")

# (2) Get map
world.sf <- ne_countries(returnclass = "sf") %>%
  filter(region_wb %in% c("Europe & Central Asia", "Middle East & North Africa", "Sub-Saharan Africa")) 

# (3) Plot using ggplot2 and geom_sf
hotspot.fig <- ggplot(data = world.sf) +
  geom_sf() +
  geom_point(data = hotspot.df, aes(x = lon, y = lat, size = capacity)) +
  geom_text_repel(data = hotspot.df, aes(x = lon, y = lat,
                                         label = str_extract(hotspot.df$hotspot, "[:alpha:]*(?=,)")),
                  point.padding = 0.3) +
  coord_sf(xlim = c(0, 40), ylim = c(30, 50)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = "transparent"),
        text = element_text(size = 14)) +
  labs(title = "Location and capacity of 'hotspots' in Greece and Italy",
       caption = "Data from EP (2018): goo.gl/W9hyuj",
       size = "Capacity")

# Save
### ------------------------------------------------------------------------###
# Plots
ggsave(filename = "./FRAN-reports/HotspotFig.tiff", 
       plot = hotspot.fig, device = "tiff", dpi = 600)

# Save to disk
saveRDS(object = hotspot.df, file = "./data/hotspot.RDS")
