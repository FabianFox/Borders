# FRONTEX Risk Analysis Network (FRAN) Reports: 
# FIGURES
# Data from: https://frontex.europa.eu/along-eu-borders/migratory-map/

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, sf, rnaturalearth, rgeos, ggmap, ggforce, ggrepel, cowplot)

# Load data
# (1) Routes.df
routes.df <- import(file = "https://frontex.europa.eu/assets/Migratory_routes/Detections_of_IBC_2018_07_06.xlsx",
                    sheet = 1) %>%
  gather(year, crossings, -c(Route, BorderLocation, NationalityLong)) %>%
  mutate(date = as.Date(as.numeric(year), origin = "1899-12-30"))

# (2) lines.df
lines.df <- readRDS("./FRAN-reports/lines.RDS")

# (3) Map: sf
world.sf <- ne_countries(returnclass = "sf") %>%
  filter(region_wb %in% c("Europe & Central Asia", "Middle East & North Africa", "Sub-Saharan Africa")) 

# (4) label.df 
label.df <- tibble(
  route = c("Black Sea\nroute",
            "Central\nMediterranean route",
            "Circular\nRoute", 
            "Eastern\n borders route",
            "Eastern\nMediterranean route",
            "Other\nroutes",
            "Western African\nroute",
            "Western Balkan\nroute",
            "Western\nMediterranean route"),
  lon = c(32.5, 20, 18.5, 26, 29, -10, -12, 16, 1),
  lat = c(44, 35, 39, 53.5, 38, 49, 29, 45, 36)
)

# Identifying shared borders in order to plot erected fences
# Base data frame
# based on: https://p.dw.com/p/2eoYy
fence.df <- tibble(
  from = c("BGR", "HUN", "HUN", "SVK", "AUT", "MKD"),
  to = c("TUR", "SRB", "HRV", "HRV", "SVK", "GRC"),
  year = c(2015, 2015, 2015, 2015, 2015, 2015)
)

# Spatial map of Europe
europe.sf <- world.sf %>%
  filter(region_wb %in% c("Europe & Central Asia"),
         !is.na(iso_a3)) 

# List to store the intersections
fence <- vector(mode = "list", length = nrow(fence.df))

# Create intersections
for (i in 1:nrow(fence.df)) {
  fence[[i]] <- st_intersection(
    europe.sf[europe.sf$iso_a3 == fence.df$from[i],], europe.sf[europe.sf$iso_a3 == fence.df$to[i],]
  )
  names(fence)[i] <- paste0(fence.df$from[i], "-", fence.df$to[i])
}

fences <- map(fence, "geometry") 
fences <- fences[lengths(fences) > 0]
fences <- map(fences, st_sf) %>%
  bind_rows()

# Plot
map.fig <- ggplot(data = world.sf) +
  geom_sf() +
  geom_bspline(data = lines.df, mapping =  aes(x = lon, y = lat, group = route), size = 1.5,
               arrow = arrow(length = unit(0.4, unit = "cm"))) +
  geom_label(data = label.df, mapping = aes(x = lon, y = lat, label = route, hjust = "center")) +
  geom_sf(data = fences, fill = NA, show.legend = F, color = "red", lwd = 1) +
  coord_sf(xlim = c(-20, 50), ylim = c(20, 65)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = "transparent"),
        text = element_text(size = 14)) +
  labs(title = "Main irregular migration routes and flows, 2009-2017",
       subtitle = "Detections of unauthorized border crossings at the EU's external border, yearly",
       caption = "Fabian Gülzau (https://fguelzau.rbind.io/)\nData from Frontex: https://frontex.europa.eu/along-eu-borders/migratory-map/")

# Also possible to map by size of unauthorized border crossings
# For the latest month
routes.df <- routes.df %>%
  mutate(year = as.numeric(str_extract_all(date, "[:digit:]{4}"))) %>%
  group_by(Route, year) %>%
  summarise(crossings = sum(crossings)) %>%
  filter(year != 2018)

# Join to the lines.df
routes.df <- routes.df %>%
  left_join(lines.df, by = c("Route" = "route"))

# Summarize minor routes and plot multiple line graps
routes.figs <- routes.df %>%
  mutate(routeAGG = case_when(
      Route %in% c("Black Sea","Circular Route from Albania to Greece",
                   "Eastern Land Borders", "Other", "Western African", 
                   "Western Mediterranean") ~ "Remaining routes (aggregated)",
      Route == "Central Mediterranean" ~ "Central Mediterranean route",
      Route == "Eastern Mediterranean" ~ "Eastern Mediterranean route",
      Route == "Western Balkans" ~ "Western Balkan route"
                   )) %>%
  distinct(routeAGG, year, .keep_all = TRUE) %>%
  group_by(routeAGG, year) %>%
  summarise(crossings = sum(crossings)) %>%
  group_by(routeAGG) %>%
  nest() %>%
  mutate(plot = map2(data, routeAGG, ~ggplot(data = .x) +
                      geom_line(aes(x = year, y = crossings)) +
                      geom_point(aes(x = year, y = crossings)) +
                      ggtitle(.y) +
                      ylab("") +
                      xlab("") +
                      coord_cartesian(ylim = c(0, 900000)) +
                      scale_x_continuous(breaks = c(2010, 2012, 2014, 2016)) +
                                        # labels = c("2010", "2012", "2014", "2016")) +
                      theme_minimal() +
                      theme(panel.grid.minor.x = element_blank(),
                            panel.grid.major.x = element_blank(),
                            text = element_text(size = 14),
                            axis.ticks.x = element_line(size = .5))))

# Combine figures
routemap.fig <- ggdraw() +
  draw_plot(map.fig, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(routes.figs$plot[[3]], -0.05, 0.3, 0.5, 0.4, scale = 0.4) +
  draw_plot(routes.figs$plot[[1]], 0.63, 0.1, 0.5, 0.4, scale = 0.4) +
  draw_plot(routes.figs$plot[[2]], 0.63, 0.3, 0.5, 0.4, scale = 0.4) +
  draw_plot(routes.figs$plot[[4]], 0.63, 0.5, 0.5, 0.4, scale = 0.4)
  
# Extension
# Read up on gganimate
# i.e. https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate