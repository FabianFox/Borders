# FRONTEX Risk Analysis Network (FRAN) Reports
# Extracting tables from pdfs

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest, tabulizer, pdftools, qdap)

# Risk Analysis
# (1) Risk Analysis 2015 (contains data from 2009 to 2014)
loc <- "./FRAN-reports/ARA 2015.pdf"

# tabulizer
tbl2015 <- extract_tables(loc, pages = 18, guess = TRUE, encoding = "UTF-8")

# Remove unnecessary columns and transfrom to tibble
tbl2015 <- tbl2015[[1]][,1:7] %>%
  as_tibble(.)

# first row as colnames
colnames(tbl2015) <- tbl2015[1, 1:7]

# delete the respective row afterwards
tbl2015 <- tbl2015[-1, 1:7] %>%
  filter(str_detect(Routes, "route") == TRUE) %>%
  map_at(., .at = c(2:7), ~str_replace_all(., pattern = " ", replacement = "")) %>%
  map_at(., .at = c(2:7), ~as.numeric(.)) %>%
  as_tibble() %>%
  mutate(Routes = bracketX(Routes)) %>%
  arrange(Routes)

# (2) Risk Analysis 2015 (contains data from 2012 to 2016)
loc <- "./FRAN-reports/ARA 2017.pdf"

# tabulizer (interactive because tbl is embedded in text)
tbl2017 <- extract_areas(loc, pages = 21, guess = TRUE, encoding = "UTF-8")

# Remove unnecessary columns and transfrom to tibble
tbl2017 <- tbl2017[[1]][,c(1,5,6)] %>%
  as_tibble(.) %>%
  rename("Routes" = V1, "2015" = V2, "2016" = V3)

# delete the respective row afterwards
tbl2017 <- tbl2017[-1, 1:3] %>%
  filter(str_detect(Routes, "route") == TRUE) %>%
  map_at(., .at = c(2:3), ~str_replace_all(., pattern = " ", replacement = "")) %>%
  map_at(., .at = c(2:3), ~as.numeric(.)) %>%
  as_tibble() %>%
  arrange(Routes)

# (3) Join data from tbl2015 and tbl2017
# then information spans 2009-2016
routes.df <- left_join(tbl2015, tbl2017)

# (4) Data for 2017 is only available in a figure
tbl2018 <- tibble(
  Routes = routes.df$Routes,
  "2017" = c(537, 118962, 6396, 776, 42305, 421, 12178, 23143)
)

# (5) Join once again
routes.df <- routes.df %>%
  left_join(tbl2018)

# (6) Long format
routes.df <- routes.df %>%
  gather(year, crossings, -Routes) %>%
  mutate(year = as.numeric(year))

# Housekeeping
rm(list = setdiff(ls(), "routes.df"))
