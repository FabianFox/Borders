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

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse)

## ------------------------------------------------------------------------------------------------------------ ##

EUwalls <- read.csv("./data/Zaeune_Mauern_Europa.csv", sep = ";")
