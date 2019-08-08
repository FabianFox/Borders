# Border Data: Source

### Direct Contiguity Data Version 3.20
### Download data: 03.04.2018

# Notes & Issues
# - Add data on territorial disputes: https://www.paulhensel.org/icowterr.html

# Load/install packages
## -------------------------------------------------------------------------- ##
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, countrycode, readxl, jsonlite, igraph, wbstats, haven)

# Load data:
# - Direct Contiguity
## -------------------------------------------------------------------------- ##
# Directed dyads retrieved from http://www.correlatesofwar.org/data-sets/direct-contiguity
contdird <- read.csv(file = "./data/contdird.csv", 
                     header = TRUE, stringsAsFactors = FALSE)

# Select only the latest observation (2016), land borders (conttype)
# and remove unnecessary variables
contdird <- contdird %>%
  select(state1no, state2no, year, conttype) %>%
  filter(year == 2016 & conttype == 1) 

# Turn Correlates of War IDs into ISO3 codes
# (1) Some custom matches, i.e. 347 (Kosovo) = XKX, 345 (Serbia) = SRB 
custom.match <- c("345" = "SRB", "347" = "XKX", "816" = "VNM")

# (2) Transform
contdird <- contdird %>%
  mutate(state1 = countrycode(sourcevar = state1no, origin = "cown", destination = "iso3c", custom_match = custom.match),
         state2 = countrycode(sourcevar = state2no, origin = "cown", destination = "iso3c", custom_match = custom.match)) %>%
  select(state1, state2, conttype, year) %>%
  arrange(state1, state2)

# Join International Border Agreement Dataset (Owsiak et al. 2018)
# Note: Data up until 2001
# Created in: IBAD-DataClean.R
## -------------------------------------------------------------------------- ##
# Load
ibad.df <- readRDS("./data/IBAD_Data.rds")

# Join
contdird <- contdird %>%
  left_join(ibad.df)

# Join macro level indicators
## -------------------------------------------------------------------------- ##
# Data:
# - World Bank (GDP p.c. and Population)
# - Polity IV 
# - Visa Network Data
# - CoW World Religion

# World Bank
# World Bank Indicators: "NY.GDP.PCAP.CD", "SP.POP.TOTL"
# Year: 2018 (accessed: 2018/08/08)
## -------------------------------------------------------------------------- ##
# (1)
# Download data (mrv = newest available)
wb.info <- wb(country = unique(contdird$state1),
              indicator = c("NY.GDP.PCAP.CD", "SP.POP.TOTL"), 
              mrv = 1, return_wide = TRUE)

# (2) Match to base data
border.df <- contdird %>%
  mutate(
    state1_pop = wb.info[match(contdird$state1, wb.info$iso3c),]$SP.POP.TOTL,
    state2_pop = wb.info[match(contdird$state2, wb.info$iso3c),]$SP.POP.TOTL,
    state1_gdp = wb.info[match(contdird$state1, wb.info$iso3c),]$NY.GDP.PCAP.CD,
    state2_gdp = wb.info[match(contdird$state2, wb.info$iso3c),]$NY.GDP.PCAP.CD
  )

# Polity IV
# Variable: Polity2
# Year: 2017 
## -------------------------------------------------------------------------- ##
# (1) Load the data retrieved from www.systemicpeace.org/inscrdata.html
polityIV <- foreign::read.spss("http://www.systemicpeace.org/inscr/p4v2017.sav", 
                               to.data.frame = TRUE,
                               use.value.labels = FALSE)  

# (a) Drop some variables, filter year == 2017 and special treatment of Kosovo
polityIV <- polityIV %>%
  select(ccode, scode, country, year, polity2) %>%
  filter(year == 2017) %>%
  mutate(iso3 = countrycode(scode, "p4c", "iso3c", custom_match = c("KOS" = "XKX", "GMY" = "DEU")))

# (3) Join 
border.df <- border.df %>%
  mutate(state1_polity = polityIV[match(border.df$state1, polityIV$iso3),]$polity2,
         state2_polity = polityIV[match(border.df$state2, polityIV$iso3),]$polity2)

# Visa Network Data
## -------------------------------------------------------------------------- ##
# retrieved from https://www.fiw.uni-bonn.de/demokratieforschung/personen/laube/visanetworkdata

# (1) Load data
visa <- read_xls(path = "./data/Visa Network Data_1969_2010.xls",
                 sheet = 2, range = "C5:FN172", 
                 col_types = c("text", rep("numeric", 167)), 
                 na = "/")

# (2) Prepare data
# Delete unnecessary rows and columns
visa <- visa[-1,]
visa <- visa[,-2]

# Self-ties are included as "NA", however, they should be coded as "0".
visa[is.na(visa)] <- 0

# Rename the first column (country IDs) and unambiguous country names
visa <- visa %>%
  rename(Name = "Home country:", 
         "Central African Republic" = "Central African Rep.", 
         "Comoro Islands" = "Comores Islands",
         "North Korea" = "Korea (Peoples Rep.)",
         "Swaziland" = "Swasiland",
         "Kyrgyzstan" = "Kyrgystan")

# Transform to common ISO3 codes
iso3 <- countrycode(colnames(visa)[2:167], "country.name.en", "iso3c")

# As a matrix object
visa.mat <- as.matrix(visa[,2:167])

# Use ISO3 codes as row and column names
rownames(visa.mat) <- iso3
colnames(visa.mat) <- iso3

# Igraph object
# Step 1: Create an igraph object
visa.graph <- igraph::graph.adjacency(visa.mat, mode = "directed",
                                      diag = FALSE, add.colnames = TRUE)

# Step 2: Transform into an edgelist 
visa.edge <- igraph::get.edgelist(visa.graph, names = TRUE)

# Edgelist (dyadic format)
# Step 1: Transform the edgelist into a dataframe
visa.edge <- tibble(
  from.no = visa.edge[,1],
  to.no = visa.edge[,2])

# Step 2: Create a lookup table for matching
lookup <- tibble(
  country = colnames(visa.mat),
  no = 1:166)

# Step 3: Replace numbers with ISO3 codes
visa.edge$state1 <- lookup[match(visa.edge$from.no, lookup$no),]$country
visa.edge$state2 <- lookup[match(visa.edge$to.no, lookup$no),]$country

# Step 4: Remove unnecessary variables and add a column that tells us whether a visa is waived = 1
visa.edge <- visa.edge %>%
  mutate(visa = 1) %>%
  select(-from.no, -to.no)

# (3) Match data sets
# (A) Identify visa waivers
border.df <- border.df %>%
  left_join(y = visa.edge) %>%
  mutate(visa.available = match(border.df$state1, colnames(visa.mat)))

# (B) Identify cases with visa restrictions and cases that are NA
border.df$visa[is.na(border.df$visa) & !is.na(border.df$visa.available)] <- 0

# Remove auxiliary variables
border.df <- border.df %>%
  select(-visa.available)

# CoW World Religion Data
## -------------------------------------------------------------------------- ##
# retrieved from http://www.correlatesofwar.org/data-sets/world-religion-data

relig.df <- import("http://www.correlatesofwar.org/data-sets/world-religion-data/wrp-national-data-1/at_download/file", format = ",") %>%
  select(1:40, -sumrelig) %>%
  filter(year == 2010) %>%
  gather(religion, rel_pop, -year, -name, -state, -pop) %>%
  mutate(
    relfam = case_when(
      religion %in% c(
        "chrstprot", "chrstcat", "chrstorth", "chrstang", "chrstothr",
        "chrstgen"
      ) ~ "chrst",
      religion %in% c(
        "judorth", "jdcons", "judref", "judothr", "judgen"
      ) ~ "jud",
      religion %in% c(
        "islmsun", "islmshi", "islmibd", "islmnat", "islmalw", "islmahm",
        "islmothr", "islmgen"
      ) ~ "islm",
      religion %in% c("budmah", "budthr", "budothr", "budgen") ~ "bud",
      TRUE ~ as.character(religion)
    ),
    state = countrycode(state, "cown", "iso3c", custom_match = custom.match)
  ) %>%
  group_by(state, relfam) %>%
  summarise(pop_relfam = sum(rel_pop)) %>%
  slice(which.max(pop_relfam))

# Join main religious group to border.df
border.df <- border.df %>%
  mutate(state1_relig = relig.df[match(border.df$state1, relig.df$state),]$relfam,
         state2_relig = relig.df[match(border.df$state2, relig.df$state),]$relfam)

# Missing values filled by Pew Research Center: Religious Composition by Country, 2010-2050
# retrieved from: https://www.pewforum.org/2015/04/02/religious-projection-table/2010/number/all/
# accessed: 2019/05/31

border.df[border.df$state1 =="SSD",]$state1_relig <- "chrst"
border.df[border.df$state2 =="SSD",]$state2_relig <- "chrst"

# The CIA World Factbook 
## -------------------------------------------------------------------------- ##
# - Length of shared borders
# - Terrain description  

# Issues: 
# - States are stored in their respective regional folder 
# - Information is a character string

# (1) Get all files (location on hard disk)
# List the region folders who store country json-files
folders <- list.dirs(path = "./data/factbook.json-master")

# Information stored in certain folders not needed
folders <- folders[-c(1, 9, 15)]

# List the files in the region folders
files <- vector("list", length(folders))

for (i in seq_along(folders)) {
  files[[i]] <- paste0(folders[i], "/", list.files(folders[i]))
}

# All files in the respective region folders
files <- flatten_chr(files)

# (2) Extract the respective information from json-files
border.cia <- vector("list", length(files))
state <- vector("character", length(files))
  
for (i in seq_along(files)) {
  state[i] <- str_extract_all(files[i], pattern = "(?<=/)[:alpha:]{2}(?=.json)")
  
  file <- fromJSON(files[i], simplifyVector = FALSE)
  
  blength <- file %>%
    .$Geography %>%
    .$`Land boundaries` %>%
    .$`border countries` %>%
    .$text
  
  terrain <- file %>%
    .$Geography %>% 
    .$Terrain %>% 
    .$text
  
  border.cia[[i]]$blength <- ifelse(length(blength) == 0, NA, blength)
  border.cia[[i]]$terrain <- terrain
}

# Check here: https://www.cia.gov/library/publications/the-world-factbook/appendix/appendix-d.html

# Name the list elements (country identifier, FIPS105 -> ISO3)
names(border.cia) <- countrycode(flatten_chr(state), origin = "fips", destination = "iso3c")

# Remove unidentified countries
border.cia <- border.cia[-c(which(is.na(names(border.cia))))] 

# Keep those countries from the CIA Factbook that are part of the base data
border.cia <- border.cia[intersect(unique(border.df$state1), names(border.cia))]

# Clean the character string with border length of neighbouring states
# (A) Extract
border.cia <- tibble(
  country = names(border.cia),
  blength = map_chr(border.cia, 1) %>%
    map_chr(~paste0(", ", ., collapse = "")),
  terrain = map(border.cia, 2) %>%
    map_chr(~ifelse(is_empty(.), NA, .))
)

# Clean string "blength"
bstate <- border.cia$blength %>%
  str_extract_all(pattern = "(?<=,\\s)(.)*?(?=\\s[:digit:])")

border.cia <- border.cia %>%
  mutate(blength = str_replace_all(blength, pattern = "(?<=[:digit:]),(?=[:digit:])", "."),
         blength = str_replace_all(blength, pattern = "\\([^()]*\\)", replacement = ""),
         blength = str_extract_all(blength, pattern = "(?<=\\s)[:digit:]+.?[:digit:]*(?=\\skm)"))

# Repeat state to conform to dyad format
state1 <- vector("list", length(border.cia$country))

# 
for(i in seq_along(border.cia$country)){
  state1[[i]] <- if (lengths(bstate[i]) == 0) {
    rep(border.cia$country[i], times = 1)
  } else {
    rep(border.cia$country[i], times = lengths(bstate[i]))
  }
}

# Countries with no land borders should get an NA instead of character(0).
# Thus, they conform to our base data (contiguity)
bstate[lengths(bstate) == 0] <- NA_character_
border.cia[lengths(border.cia$blength) == 0,] <- NA_character_

# Create a borderlength.df
borderlength.df <- tibble(
  state1 = flatten_chr(state1),
  bstate = flatten_chr(bstate),
  blength = flatten_chr(border.cia$blength)
)

# Transform English country names to ISO3 codes
# Use case_when to replace specifically.
borderlength.df <- borderlength.df %>%
  mutate(bstate = countrycode(bstate, origin = "country.name", destination = "iso3c",
                              custom_match = c("Kosovo" = "XKX")),
         blength = as.numeric(
           ifelse(
             str_detect(blength, "[:digit:]?\\.[:digit:]{3}") == TRUE,
             str_replace(blength, "\\.", ""), 
             blength)
           )
         )
         
# Some countries (BHR, CYP, SGP) are marked as not having a neigbour in the CIA World
# Factbook 
borderlength.df[which(is.na(borderlength.df$bstate)),] <- c("SAU", "TUR", "MYS")

# Rename borderlength.df and join to base data
borderlength.df <- borderlength.df %>%
  rename(state2 = bstate)

# Join the individual border lengths to the main data
border.df <- border.df %>%
  left_join(y = borderlength.df) %>%
  left_join(y = border.cia %>%
              select(country, terrain), by = c("state1" = "country"))

# Create a dyad identifier variable
## -------------------------------------------------------------------------- ##
# Function to create a dyad identifier 
# from: https://stackoverflow.com/questions/52316998/create-unique-id-for-dyads-non-directional

dyadId_fun <- function(x,y) paste(sort(c(x, y)), collapse="_")
dyadId_fun <- Vectorize(dyadId_fun)

# Apply the function
border.df <- border.df %>% 
  mutate(dyadName = dyadId_fun(state1, state2),
         dyadID = as.numeric(as.factor(dyadName))) %>%
  as_tibble()

# Keep only the final data
## -------------------------------------------------------------------------- ##
rm(list = setdiff(ls(), "border.df"))
