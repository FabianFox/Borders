# European border fences
# Length and construction date

# Data from: 
# Deutsche Welle (23.06.2017 )
# Link: https://p.dw.com/p/2eoYy

# Reuters (04.04.2016)
# Link: https://www.reuters.com/article/us-europe-migrants-fences-insight-idUSKCN0X10U7

# Washington Post (28.10.2015)
# Link: https://www.washingtonpost.com/news/worldviews/wp/2015/08/28/map-the-walls-europe-is-building-to-keep-people-out/?noredirect=on&utm_term=.53035eb2761b

# Financial Times (26.03.2017)
# Link: https://www.ft.com/content/9d4d10cc-0e28-11e7-b030-768954394623

# New York Times (16.10.2015)
# Link: https://www.nytimes.com/interactive/2015/09/15/world/europe/migrant-borders-europe.html

# Radio Free Europe
# Link: https://www.rferl.org/a/fencing-off-europe/27562610.html

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rio, cowplot, patchwork)

## ------------------------------------------------------------------------------------------------------------ ##

# Load data
EUwalls <- import("./data/EU-Walls.xlsx") %>%
  mutate(length = as.numeric(length)) %>%
  arrange(begin)

# Basic description
walls.df <- EUwalls %>%
  filter(state1 != "MKD", !is.na(length),
         begin >= 2010) %>%
  group_by(begin) %>%
  summarise(length = sum(length), n = n()) %>%
  mutate(cumsum_length = cumsum(length),
         cumsum_n = cumsum(n))

# Plot number of fences and length
nfence <- ggplot(walls.df, aes(x = begin, y = n)) +
  geom_bar(stat = "identity") +
  geom_point(aes(x = begin, y = cumsum_n), size = 2) +
  geom_line(aes(x = begin, y = cumsum_n), size = 1) +
  labs(x = "Year", y = "Number",
       title = "Number of border barriers in Europe, 2010-2020") + 
  scale_x_continuous(breaks = seq(2010, 2020, 1)) + 
  theme_minimal_grid()

lfence <- ggplot(walls.df, aes(x = begin, y = cumsum_length)) +
  geom_point(size = 2) +
  geom_line(size = 1) + 
  labs(x = "Year", y = "Length in km",
       title = "Length of border barriers in Europe, 2010-2020") +
  scale_x_continuous(breaks = seq(2010, 2020, 1)) + 
  theme_minimal_grid()

# Arrange plots
EU_walls.fig <- nfence / lfence

# Export
ggsave(plot = EU_walls.fig, file = "O:/Grenzen der Welt/Projekte/Die Zeit/figures/Num_EU-Fences.png")
