# Construction year of barrier/fortified border

# Load/install packages
## -------------------------------------------------------------------------- ##
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "janitor", "rio")

# Load indicator
## -------------------------------------------------------------------------- ##
indicator.df <- import("Y:\\Grenzen der Welt\\Grenzdossiers\\Typologie\\BorderTypology.xlsx",
                       sheet = 1, na = "NA") %>%
  as_tibble() %>%
  select(1:3, 16) %>%
  filter(!is.na(typology),
         !(state1 == "ARE" & state2 == "QAT"),        # no shared border (since 1974) 
         !(state1 == "QAT" & state2 == "ARE")) %>%    # https://bit.ly/39EOy4Y
  clean_names() %>%
  distinct(state1, state2, .keep_all = TRUE) %>%
  filter(typology %in% c("barrier border", "fortified border")) %>%
  arrange(state1, typology)

# Add year of border installation
## -------------------------------------------------------------------------- ##
# Data from BordersJoin.R
barriers.df <- import("./analysis/Fence data/barriers_df.rds")

list.fences <- barriers.df %>%
  select(-indicator) %>%
  nest(data = c(-source)) %>%
  mutate(data = set_names(data, source))

# Rename year-columns to data source and unnest
list.fences <- map2(
  .x = list.fences$data, 
  .y =  names(list.fences$data),
  ~rename(.x, !!.y := year)) %>%
  map_df(., ~as_tibble(.x))

# Join
fortified_borders.df <- indicator.df %>%
  filter(typology %in% c("fortified border", "barrier border")) %>%
  left_join(list.fences, by = c("state1", "state2")) %>%
  as_tibble() 

# Determine preference regarding source of construction year 
# from left - Wikipedia - to right - Avdan, (2019)
fortified_borders.df <- fortified_borders.df %>%
  select(state1, state2, typology, Wikipedia, `Benedicto et al. (2020)`, `Jellissen&Gottheil (2013)`,
         `Hassner&Wittenberg (2013)`, `Reece (2012)`, `Avdan (2019)`)

# Fill missing values by preference
# See: https://stackoverflow.com/questions/55671205/fill-missing-values-rowwise-right-left
built <- t(zoo::na.locf(t(fortified_borders.df[, c(4:9)])))[,6]

# Add to dataframe
fortified_borders.df <- fortified_borders.df %>%
  mutate(built = built) 

# Use preferred source when multiple entries exist
fortified_borders.df <- fortified_borders.df %>%
  group_by(state1, state2) %>%
  filter(row_number() == 1) %>%
  select(state1, state2, typology, built)

# Remove
rm(list = setdiff(ls(), "fortified_borders.df"))

# Fill missing values
## -------------------------------------------------------------------------- ##
# Source: https://www.buzzfeednews.com/article/karlazabludovsky/argentina-separation-anxiety
fortified_borders.df[fortified_borders.df$state1 == "ARG" & fortified_borders.df$state2 == "PRY", "built"] <- "2014"
# Source: Smolnik (2016) "Secessionist Rule Protracted Conflict and Configurations of Non-state Authority", Campus: Frankfurt aM.
fortified_borders.df[fortified_borders.df$state1 == "ARM" & fortified_borders.df$state2 == "AZE", "built"] <- "1994" # estimate
# Source: https://www.dw.com/en/ghosts-of-the-past-haunt-turkish-armenian-border/a-18395546
fortified_borders.df[fortified_borders.df$state1 == "ARM" & fortified_borders.df$state2 == "TUR", "built"] <- "1993" # estimate
# Source: https://jam-news.net/landmines-on-the-georgian-azerbaijani-border/
fortified_borders.df[fortified_borders.df$state1 == "AZE" & fortified_borders.df$state2 == "GEO", "built"] <- "1991" # estimate (concerning minefields)
# Source:
# fortified_borders.df[fortified_borders.df$state1 == "AZE" & fortified_borders.df$state2 == "RUS", "built"] <- "1991" # estimate (concerning minefields)
# Source: https://sofiaglobe.com/2018/08/30/bulgaria-completes-fence-at-border-with-romania-against-african-swine-fever/
fortified_borders.df[fortified_borders.df$state1 == "BGR" & fortified_borders.df$state2 == "ROU", "built"] <- "2018" # Swine fever
# Source: https://belarusdigest.com/story/fencing-off-the-war-in-ukraine-belarus-strengthens-its-borders/
fortified_borders.df[fortified_borders.df$state1 == "BLR" & fortified_borders.df$state2 == "UKR", "built"] <- "2014" # estimate
# Source: Kowalcyzk et al (2012): Do Fences or Humans Inhibit the Movements of Large Mammals in Białowieża Primeval Forest?, in: Somers, Michael J. & Hayward, Matthew (Eds.): Fencing for Conservation, New York: Springer, 235-243.
fortified_borders.df[fortified_borders.df$state1 == "BLR" & fortified_borders.df$state2 == "POL", "built"] <- "1981"
# Source: https://www.law.ox.ac.uk/research-subject-groups/centre-criminology/centreborder-criminologies/blog/2017/03/time-new-border
fortified_borders.df[fortified_borders.df$state1 == "CHL" & fortified_borders.df$state2 == "BOL", "built"] <- "1973" # estimate
# Source: https://www.reuters.com/article/us-chile-migrants-widerimage/chile-cracks-down-on-migrants-but-many-still-try-their-luck-idUSKBN1OG1P6
fortified_borders.df[fortified_borders.df$state1 == "CHL" & fortified_borders.df$state2 == "PER", "built"] <- "1973" # estimate
# Source: http://www.ecns.cn/cns-wire/2015/04-30/163731.shtml
fortified_borders.df[fortified_borders.df$state1 == "CHN" & fortified_borders.df$state2 == "VNM", "built"] <- "2015"
# Source: http://usa.chinadaily.com.cn/china/2014-09/25/content_18657413.htm
# fortified_borders.df[fortified_borders.df$state1 == "CHN" & fortified_borders.df$state2 == "AFG", "built"] <- "2010" # between 2000-2010
# Source: https://www.forbes.com/sites/wadeshepard/2016/07/26/an-inside-look-at-icbc-khorgos-china-and-kazakhstans-cross-border-free-trade-zone/#48e046d35c8f
# fortified_borders.df[fortified_borders.df$state1 == "CHN" & fortified_borders.df$state2 == "KAZ", "built"] <- ""
# Source: Likely after border agreement was finalized in 2009
# fortified_borders.df[fortified_borders.df$state1 == "CHN" & fortified_borders.df$state2 == "KGZ", "built"] <- ""
# Source: Trouwborst, A., Fleurke, F. and Dubrulle, J. (2016), Border Fences and their Impacts on Large Carnivores, Large Herbivores and Biodiversity: An International Wildlife Law Perspective. RECIEL, 25: 291-306.
fortified_borders.df[fortified_borders.df$state1 == "CHN" & fortified_borders.df$state2 == "MNG", "built"] <- "2008"
# Source: 
# fortified_borders.df[fortified_borders.df$state1 == "CHN" & fortified_borders.df$state2 == "RUS", "built"] <- "2008"
# Source: http://english.chinamil.com.cn/view/2017-09/19/content_7762174.htm
# fortified_borders.df[fortified_borders.df$state1 == "CHN" & fortified_borders.df$state2 == "TJK", "built"] <- ""
# Source: https://www.journalducameroun.com/en/cameroon-extends-trench-prevent-armed-militants-far-north/
fortified_borders.df[fortified_borders.df$state1 == "CMR" & fortified_borders.df$state2 == "NGA", "built"] <- "2016"
# Source: https://en.wikipedia.org/wiki/United_Nations_Buffer_Zone_in_Cyprus
fortified_borders.df[fortified_borders.df$state1 == "CYP" & fortified_borders.df$state2 == "TUR", "built"] <- "1974"
# Source: https://www.theguardian.com/weather/2019/jan/28/denmark-begins-work-on-wall-to-keep-out-wild-boar
fortified_borders.df[fortified_borders.df$state1 == "DNK" & fortified_borders.df$state2 == "DEU", "built"] <- "2019" # Swine fever
# Source: http://www.ieee.es/en/Galerias/fichero/docs_opinion/2016/DIEEEO60-2016_Muros_Fronterizos_America_FFurlan.pdf
fortified_borders.df[fortified_borders.df$state1 == "DOM" & fortified_borders.df$state2 == "HTI", "built"] <- "2014" 
# Source: https://www.medias24.com/MAROC/INTERNATIONAL/166749-La-fin-de-l-UMA-Murs-a-la-frontiere-Maroc-Algerie-tranchees-sur-les-frontieres-avec-la-Tunisie-et-la-Libye.html
fortified_borders.df[fortified_borders.df$state1 == "DZA" & fortified_borders.df$state2 == "LBY", "built"] <- "2015" 
# Source: https://fanack.com/algeria/history-past-to-present/is-algeria-north-africa-new-migration-hub-sub-saharan-migrants/
fortified_borders.df[fortified_borders.df$state1 == "DZA" & fortified_borders.df$state2 == "MLI", "built"] <- "2012" 




