# Data on border walls by Global Security
# 
# Data from: GlobalSecurity.org
# URL: https://www.globalsecurity.org/military/world/walls.htm

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest, countrycode)

# Scrape the content of the table
barriers.gsec <- read_html("https://www.globalsecurity.org/military/world/walls.htm") %>%
  html_nodes("#content > table:nth-child(12)") %>%
  html_table(header = TRUE) %>%
  .[[1]] %>%
  set_names(c("state1", "barrier", "begin", "end"))