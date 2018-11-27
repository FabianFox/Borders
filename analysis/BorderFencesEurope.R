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
p_load(tidyverse, rio, cowplot)

## ------------------------------------------------------------------------------------------------------------ ##

# Load data
EUwalls <- import("./data/EU-Walls.xlsx") %>%
  mutate(length = as.numeric(length)) %>%
  arrange(begin)

# Basic description
walls.df <- EUwalls %>%
  filter(begin >= 2010 & reason == "migration",
         state1 != "MKD") %>%
  group_by(begin) %>%
  summarise(length = sum(length), n = n()) %>%
  mutate(cumsum = cumsum(length))

walls.df <- walls.df %>%
  filter(begin >= 2014)

# Plot number of fences and length
nfence <- ggplot(walls.df, aes(x = begin, y = n)) +
  geom_bar(stat = "identity") +
  ylab("") +
  xlab("") +
  scale_x_continuous(breaks = seq(2010, 2016, 1)) + 
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))

lfence <- ggplot(walls.df, aes(x = begin, y = cumsum)) +
  geom_point() +
  ylab("") +
  xlab("") +
  scale_x_continuous(breaks = seq(2010, 2016, 1)) + 
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 14),
        axis.ticks.x = element_line(size = .5))

# Arrange plots
plot_grid(lfence, nfence, nrow = 2, align = "v")
