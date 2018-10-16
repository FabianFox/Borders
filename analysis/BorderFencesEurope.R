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

# Reuters (27.04.2016) Austria only
# Link: https://www.reuters.com/article/us-europe-migrants-brenner-idUSKCN0XO1HS

# DW (18.09.2015) HUN HRV
# Link: https://www.dw.com/en/hungary-starts-building-fence-on-croatian-border/a-18721670

# Baltic Times (05.02.2017) LVA RUS
# Link: https://www.baltictimes.com/fence_stretching_23-km_already_up_on_latvia-russia_border/

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse)

## ------------------------------------------------------------------------------------------------------------ ##

# Load data
EUwalls <- read.csv("./data/EU-Walls.csv", sep = ";", stringsAsFactors = FALSE, na = "")

# Basic description
EUwalls %>%
  filter(begin >= 2015 & reason == "migration") %>%
  group_by(state1) %>%
  summarise(sum(length, na.rm = TRUE))