# Border Data: Source

### Direct Contiguity Data Version 3.20
### Download data: 03.04.2018

# Notes & Issues
# - CoW Direct Contiguity is expanded by dyad between NGA-TCD due to aridification
#   of lake Chad, erroneous entries - ARE-QAT, MMR-PAK - are removed

# Load/install packages
## -------------------------------------------------------------------------- ##
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "countrycode", "readxl", "jsonlite", "igraph", "wbstats", "rio")

# Load data:
# - Direct Contiguity
## -------------------------------------------------------------------------- ##
# Directed dyads retrieved from http://www.correlatesofwar.org/data-sets/direct-contiguity
# Latest observation: 2016
contdird <- import(file = "./data/contdird.csv", 
                   header = TRUE, stringsAsFactors = FALSE)

# Select only the latest observation (2016), land borders (conttype)
# and remove unnecessary variables
contdird <- contdird %>%
  select(state1no, state2no, year, conttype) %>%
  filter(year == 2016 & conttype == 1)

# Turn Correlates of War IDs into ISO3 codes
# (1) Some custom matches, i.e. 347 (Kosovo) = XKX, 345 (Serbia) = SRB 
custom.match <- c("345" = "SRB", "347" = "XKX")

# (2) Transform
contdird <- contdird %>%
  mutate(state1 = countrycode(sourcevar = state1no, origin = "cown", destination = "iso3c", custom_match = custom.match),
         state2 = countrycode(sourcevar = state2no, origin = "cown", destination = "iso3c", custom_match = custom.match)) %>%
  select(state1, state2, conttype, year) %>%
  add_row(state1 = c("NGA", "TCD"), state2 = c("TCD", "NGA"), # Lake Chad fell dry
          conttype = c(1, 1), year = c(2016, 2016)) %>%
  arrange(state1, state2) %>%
  filter(!(state1 == "ARE" & state2 == "QAT"), # no shared border (since 1974): https://bit.ly/39EOy4Y
         !(state1 == "QAT" & state2 == "ARE"),
         !(state1 == "MMR" & state2 == "PAK"), # never shared a border 
         !(state1 == "PAK" & state2 == "MMR"))

# Join macro level indicators
## -------------------------------------------------------------------------- ##
# Data:
# - IBAD (Border Agreement Data) [2001]
# - World Bank (GDP p.c. in current US$ | Total Population |
#               Armed forces personnel, total | Military expenditure, current LCU) 
#   [2017]
# - COW: Trade [2014]
# - Polity IV [2017]
# - Visa Network Data [2010]
# - COW: World Religion [2010]
# - COW: Militarized Interstate Disputes (v4.3) [2010]
# - START: Global Terrorism Database [2017]
# - World Refugee Dataset [2015]
# - CEPII: GeoDist 
# - CIA World Factbook Data

# Join International Border Agreement Dataset (Owsiak et al. 2018)
# Latest observation: 2001
# Created in: IBAD-DataClean.R
## -------------------------------------------------------------------------- ##
# Load
ibad.df <- import("./data/IBAD_Data.rds")

# Join
contdird <- contdird %>%
  left_join(y = ibad.df)

# World Bank
# World Bank Indicators: 
# GDP p.c. in current US$ - "NY.GDP.PCAP.CD"
# Total Population - "SP.POP.TOTL"
# Armed forces personnel, total - MS.MIL.TOTL.P1
# Military expenditure (% of GDP) - MS.MIL.XPND.GD.ZS

# not available via API (yet)
# Military expenditure (current USD) - MS.MIL.XPND.CD

# Year: 2017 (accessed: 2019/11/05)
## -------------------------------------------------------------------------- ##
# (1)
# Download data (mrv = newest available; here: 2017)
wb.info <- wb_data(country = unique(contdird$state1),
              indicator = c("NY.GDP.PCAP.CD", "SP.POP.TOTL", "MS.MIL.TOTL.P1", 
                            "MS.MIL.XPND.GD.ZS"), 
              start_date = 2017, end_date = 2017, return_wide = TRUE)

wb_2014.info <- wb_data(country = unique(contdird$state1),
                   indicator = c("NY.GDP.MKTP.PP.CD"), 
                   start_date = 2014, end_date = 2014, return_wide = TRUE)

# (2) Match to base data
border.df <- contdird %>%
  mutate(
    state1_pop = wb.info[match(contdird$state1, wb.info$iso3c),]$SP.POP.TOTL,
    state2_pop = wb.info[match(contdird$state2, wb.info$iso3c),]$SP.POP.TOTL,
    state1_gdp = wb.info[match(contdird$state1, wb.info$iso3c),]$NY.GDP.PCAP.CD,
    state2_gdp = wb.info[match(contdird$state2, wb.info$iso3c),]$NY.GDP.PCAP.CD,
    state1_military_pers = wb.info[match(contdird$state1, wb.info$iso3c),]$MS.MIL.TOTL.P1,
    state2_military_pers = wb.info[match(contdird$state2, wb.info$iso3c),]$MS.MIL.TOTL.P1,
#   state1_military_expenditure_usd = wb.info[match(contdird$state1, wb.info$iso3c),]$MS.MIL.XPND.CD, (not available)
#   state2_military_expenditure_usd = wb.info[match(contdird$state2, wb.info$iso3c),]$MS.MIL.XPND.CD, (not available)
    state1_military_expenditure_perc_gdp = wb.info[match(contdird$state1, wb.info$iso3c),]$MS.MIL.XPND.GD.ZS,
    state2_military_expenditure_perc_gdp = wb.info[match(contdird$state2, wb.info$iso3c),]$MS.MIL.XPND.GD.ZS,
    state1_military_pers_pc = state1_military_pers / state1_pop,
    state2_military_pers_pc = state2_military_pers / state2_pop,
    state1_military_pers_p1000 = (state1_military_pers / state1_pop) * 1000,
    state2_military_pers_p1000 = (state2_military_pers / state2_pop) * 1000,
#   GDP from 2014 for trade dependence
    state1_gdp_2014 = wb_2014.info[match(contdird$state1, wb_2014.info$iso3c),]$NY.GDP.MKTP.PP.CD,
    state2_gdp_2014 = wb_2014.info[match(contdird$state2, wb_2014.info$iso3c),]$NY.GDP.MKTP.PP.CD)

# Create logged versions of the above variables
border.df <- border.df %>%
  mutate_at(vars(6:17), list("log" = log1p))

# Create additional variables (GDP ratio, population in millions)
border.df <- border.df %>%
  mutate(ratio_gdp = state1_gdp / state2_gdp,
         state1_pop_in_million = state1_pop / 1000000,
         state2_pop_in_million = state2_pop / 1000000)

# COW: Trade v4.0
# Variable: flow1, flow2
# Year: 2014
# retrieved from https://correlatesofwar.org/data-sets/bilateral-trade
## -------------------------------------------------------------------------- ##
trade.df <- import("C:/Users/guelzauf/Seafile/Meine Bibliothek/Projekte/C01_Grenzen/Data/Data/COW_Trade_4.0/Dyadic_COW_4.0.csv") %>%
  select(state1 = ccode1, state2 = ccode2, year, import = flow1, export = flow2) %>%
  filter(year == 2014) %>%
  mutate(state1 = countrycode(sourcevar = state1, origin = "cown", destination = "iso3c", custom_match = custom.match),
         state2 = countrycode(sourcevar = state2, origin = "cown", destination = "iso3c", custom_match = custom.match),
         import = na_if(import, -9),
         export = na_if(export, -9)) %>%
  group_by(state1) %>%
  # total import/export are not valid due to missing values
  mutate(total_import = sum(import, na.rm = TRUE),
         total_export = sum(export, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(share_import = import / total_import * 100,
         share_export = export / total_export * 100)
  
# Make the dataset (long) dyadic
# (1) Duplicate dataset, swap country identifiers and rename them
swap.df <- trade.df %>%
  rename(
    state1 = state2,
    state2 = state1,
  ) %>%
  rename(export = import,
         import = export,
         share_export = share_import,
         share_import = share_export
         )

# (2) Merge trade.df and swap.df
trade.df <- trade.df %>%
  bind_rows(., swap.df) %>%
  mutate(dyadName = paste(state1, state2, sep = "_")) %>%
  select(dyadName, state1, state2, contains(c("export", "import"))) %>%
  arrange(dyadName) %>%
  mutate(export_log = log1p(export),
         import_log = log1p(import))

# (3) Join to border.df
border.df <- border.df %>%
  left_join(y = trade.df) %>%
  # Economic dependence (Boehmer & Pena 2012, p. 278)
  mutate(economic_dependence = (import + export) / state1_gdp_2014,
         economic_dependence_log = log1p(economic_dependence))

# Polity IV
# Variable: Polity2
# Year: 2017 
# 2018 available as http://www.systemicpeace.org/inscr/p4v2018.xls
## -------------------------------------------------------------------------- ##
# (1) Load the data retrieved from www.systemicpeace.org/inscrdata.html
polityIV <- import("http://www.systemicpeace.org/inscr/p4v2017.sav", 
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

# (4) Difference in political regime
border.df <- border.df %>%
  mutate(diff_pol = state1_polity - state2_polity,
         absdiff_pol = abs(state1_polity - state2_polity))

# COW: World Religion Data
# Year: 2010
# retrieved from http://www.correlatesofwar.org/data-sets/world-religion-data
## -------------------------------------------------------------------------- ##
relig.df <- import("https://correlatesofwar.org/data-sets/world-religion-data/wrp-national-data-1/@@download/file/WRP_national.csv", format = ",") %>%
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
border.df[border.df$state1 == "SSD",]$state1_relig <- "chrst"
border.df[border.df$state2 == "SSD",]$state2_relig <- "chrst"

# Create variables: 
# - muslim majority (binary) 
# - collapsed majority religion (chrst, islm, other)
# - different majority religions in dyad
border.df <- border.df %>%
  mutate(state1_relig_shrt = if_else(state1_relig %in% c("bud", "hindgen", "jud", 
                                                         "nonrelig", "syncgen"), "other",
                                     state1_relig),
         state2_relig_shrt = if_else(state2_relig %in% c("bud", "hindgen", "jud", 
                                                         "nonrelig", "syncgen"), "other",
                                     state2_relig),
         state1_muslim = if_else(state1_relig == "islm", 1, 0),
         state2_muslim = if_else(state2_relig == "islm", 1, 0),
         diff_relig = if_else(state1_relig != state2_relig, 1, 0),
         diff_relig_shrt = if_else(state1_relig_shrt != state2_relig_shrt, 1, 0))

# COW: Dyadic MIDs and Dyadic Wars V4.1
# Variable: statea, stateb, strtyr, endyear, year, outcome
# Year: 2014
# Note: variable highact entails categories for 'fortify border' & 'border violation'
## -------------------------------------------------------------------------- ##
# retrieved from http://www.correlatesofwar.org/data-sets/MIDs
# landing page: https://correlatesofwar.org/data-sets/MIDs/dyadic_mid_4-01.zip/view

# Preprocess MID
dispute.df <- import("./data/dyadic_mid_4.01.dta") %>%
  group_by(statea, stateb) %>%
  filter(row_number(desc(year)) == 1,
         strtyr >= 2000) %>%
  ungroup() %>%
  mutate(state1 = countrycode(sourcevar = statea, origin = "cown", 
                              destination = "iso3c", custom_match = custom.match),
         state2 = countrycode(sourcevar = stateb, origin = "cown", 
                              destination = "iso3c", custom_match = custom.match),
         disp_from_2000_to_2014 = 1) %>%
  select(state1, state2, disp_year = year, 
         disp_outcome = outcome, 
         disp_from_2000_to_2014)

# Join to source data 
border.df <- border.df %>%
  left_join(dispute.df) %>%
  mutate(disp_from_2000_to_2014 = if_else(is.na(disp_from_2000_to_2014), 0, 1))

# START: Global Terrorism Database 
# Variable:
# Year: 2014-2017
# Goal: Number of (fatal) terror incidents on homeland territory & number of terror
#       incidents in neighbouring country
## -------------------------------------------------------------------------- ##
# retrieved from https://gtd.terrorismdata.com/files/gtd-1970-2018/

# Preprocess GTD
#gtd.df <- import("./data/globalterrorismdb_0919dist.xlsx") %>%
#   filter(iyear == 2017) %>%
#   select(eventid, iyear, imonth, iday, approxdate,   # date of incident
#          country, country_txt, longitude, latitude,  # location of incident
#          starts_with("natlty"),                      # nationality of victims
#          nkill, nwound,                              # no. killed and injured
#          starts_with("INT", ignore.case = FALSE))    # INT_LOG - group crossed border

# Save/load prepared dataset
# export(gtd.df, "./data/gtd_prepared.rds")
gtd.df <- import("./data/gtd_prepared.rds")

# Aggregate by number of victim deaths on country's territory
# Custom match for countrycode
gtd.custom.match <- c("Kosovo" = "XKX")

# Aggregate and complete missing NA's
gtd.agg.df <- gtd.df %>%
  group_by(country_txt, iyear) %>%
  summarise(nterror = n(), 
            nkill_agg = sum(nkill, na.rm = TRUE),
            nwound_agg = sum(nwound, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cntry_iso3 = countrycode(country_txt, "country.name.en", "iso3c", 
                                  custom_match = gtd.custom.match)) %>%
  complete(nesting(country_txt, cntry_iso3), iyear,
           fill = list(nterror = 0, nkill_agg = 0, nwound_agg = 0)) # %>%            
#  group_by(cntry_iso3) %>%  
#  summarise(nterror_3yrs = sum(nterror),
#            death_toll_3yrs = sum(nkill_agg),
#            nwound_3yrs = sum(nwound_agg))

# Join to border.df
# Join main religious group to border.df
border.df <- border.df %>%
  mutate(state1_nterror = gtd.agg.df[match(border.df$state1, gtd.agg.df$cntry_iso3),]$nterror,
         state2_nterror = gtd.agg.df[match(border.df$state2, gtd.agg.df$cntry_iso3),]$nterror,
         state1_death_toll = gtd.agg.df[match(border.df$state1, gtd.agg.df$cntry_iso3),]$nkill_agg,
         state2_death_toll = gtd.agg.df[match(border.df$state2, gtd.agg.df$cntry_iso3),]$nkill_agg,
         state1_nterror_log = log1p(state1_nterror),
         state2_nterror_log = log1p(state2_nterror),
         state1_death_toll_log = log1p(state1_death_toll),
         state2_death_toll_log = log1p(state2_death_toll)) %>%
  mutate_at(vars(matches("terror|toll")), replace_na, 0)

# World Refugee Dataset
# Variable:
# Year: 2015 (latest)
## -------------------------------------------------------------------------- ##
# retrieved from: Marbach (2018), Link: https://github.com/sumtxt/wrd

# Load data and prepare variables: 
# refugees from neighboring country, total N of refugees hosted
wrd.df <- import("https://raw.githubusercontent.com/sumtxt/wrd/master/usedata/wrd_1.1.0.csv") %>%
  filter(year == 2015,
         asylum_ccode != 667) %>% # drop Palestine as host country
  mutate(state1 = countrycode(sourcevar = asylum_ccode, origin = "cown", destination = "iso3c", custom_match = custom.match),
         state2 = countrycode(sourcevar = origin_ccode, origin = "cown", destination = "iso3c", custom_match = custom.match)) %>%
  group_by(state1) %>%
  mutate(refugees_incoming_agg = sum(ylinpol, na.rm = TRUE)) %>%
  ungroup() %>%
  select(state1, state2, refugees_incoming = ylinpol, refugees_incoming_agg) 

# Join to border.df and compute refugees hosted per capita
border.df <- border.df %>%
  left_join(wrd.df) %>%
  mutate(refugees_incoming_pc = refugees_incoming / state1_pop,
         refugees_incoming_agg_pc = refugees_incoming_agg / state1_pop,
         # Log
         refugees_incoming_log = log1p(refugees_incoming),
         refugees_incoming_agg_log = log1p(refugees_incoming_agg),
         refugees_incoming_pc_log = log1p(refugees_incoming_pc),
         refugees_incoming_agg_pc_log = log1p(refugees_incoming_agg_pc))
         
# Create column for refugees sent and total number of refugees in neigbouring country
wrd.df <- wrd.df %>%
  select(refugees_outgoing = refugees_incoming,
         refugees_outgoing_agg = refugees_incoming_agg, 
         state1 = state2,
         state2 = state1)

# Join
border.df <- border.df %>%
  left_join(wrd.df) %>%
  mutate(refugees_outgoing_pc = refugees_outgoing / state2_pop,
         refugees_outgoing_agg_pc = refugees_outgoing_agg / state2_pop,
         # Log
         refugees_outgoing_log = log1p(refugees_outgoing),
         refugees_outgoing_agg_log = log1p(refugees_outgoing_agg),
         refugees_outgoing_pc_log = log1p(refugees_outgoing_pc),
         refugees_outgoing_agg_pc_log = log1p(refugees_outgoing_agg_pc))

# CEPII Gravity (Version: 2021/02/06)
# Variable: Colonies & religious similarity
## -------------------------------------------------------------------------- ##
# retrieved from: http://www.cepii.fr/CEPII/en/bdd_modele/presentation.asp?id=8
# missing in gravity.df: XKX (Kosovo)
gravity.df <- import("./data/Gravity_V202102.rds") %>%
  filter(iso3_o != iso3_d,
         year == 2019) %>% # no self-ties
  select(state1 = iso3_o, state2 = iso3_d, state1_colonizer = heg_o, 
         state2_colonizer = heg_d, colony = col_dep_ever, 
         comlang_off, relig_prox = comrelig)

# Join to border.df
border.df <- border.df %>%
  left_join(gravity.df, by = c("state1", "state2"))

# Add missing countries via CIA World Factbook
border.df[border.df$state1 == "ALB" & border.df$state2 == "XKX", c("state1_colonizer", "state2_colonizer", "colony", "comlang_off")] <- c(0, 0, 0, 1)
border.df[border.df$state1 == "MKD" & border.df$state2 == "XKX", c("state1_colonizer", "state2_colonizer", "colony", "comlang_off")] <- c(0, 0, 0, 0)
border.df[border.df$state1 == "MNE" & border.df$state2 == "XKX", c("state1_colonizer", "state2_colonizer", "colony", "comlang_off")] <- c(0, 0, 0, 0)
border.df[border.df$state1 == "SRB" & border.df$state2 == "XKX", c("state1_colonizer", "state2_colonizer", "colony", "comlang_off")] <- c(1, 0, 1, 1)
border.df[border.df$state1 == "XKX" & border.df$state2 == "ALB", c("state1_colonizer", "state2_colonizer", "colony", "comlang_off")] <- c(0, 0, 0, 1)
border.df[border.df$state1 == "XKX" & border.df$state2 == "MKD", c("state1_colonizer", "state2_colonizer", "colony", "comlang_off")] <- c(0, 0, 0, 0)
border.df[border.df$state1 == "XKX" & border.df$state2 == "MNE", c("state1_colonizer", "state2_colonizer", "colony", "comlang_off")] <- c(0, 0, 0, 0)
border.df[border.df$state1 == "XKX" & border.df$state2 == "SRB", c("state1_colonizer", "state2_colonizer", "colony", "comlang_off")] <- c(0, 1, 1, 1)

# Create a variable: (0) no colonial history, (1) colonizer, (2) colonized
border.df <- border.df %>%
  mutate(colonial_status = case_when(
    state1_colonizer == 0 & state2_colonizer == 0 ~ "no colony",
    state1_colonizer == 1 & state2_colonizer == 0 ~ "colonizer",
    state1_colonizer == 0 & state2_colonizer == 1 ~ "colonized",
    TRUE ~ NA_character_
  ))

# The CIA World Factbook 
## -------------------------------------------------------------------------- ##
# - Length of shared borders
# - Terrain description  

# (1) Get all files (location on hard disk)
# List the region folders who store country json-files
folders <- list.dirs(path = "./data/factbook.json-master")

# Information stored in certain folders (master, meta, world) not needed
folders <- folders[-c(1, 9, 15)]

# List the files in the region folders (n = 260)
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
# Which countrynames are missing (add as custom_match)
missing <- flatten_chr(state)[which(is.na(countrycode(flatten_chr(state), origin = "fips", destination = "iso3c")))]

# Custom match for several cases
custom.match.cia <- c("OD" = "SSD", "KV" = "XKX")

# Drop: at: Ashmore and Cartier Islands, cr: Coral Sea Islands, um: US Pacific Island Wildlife Refuges, 
#       wq: Wake Island, bq: Navassa Island, cc: Curacao, rn: Sant Marten, 
#       sk: Sint Maarten, pf: Paracel Islands, pg: Spratly Islands,
#       ax: Akrotiri, dx: Dhekelia, ee: European Union, jn: Jan Mayen, gz: Gaza Strip,
#       ip: Clipperton Island, oo: Southern Ocean, xo: Indian Ocean, xq: Arctic Ocean,
#       zh: Atlantic Ocean, zn: Pacific Ocean

# Name the list elements (country identifier, FIPS105 -> ISO3)
names(border.cia) <- countrycode(flatten_chr(state), origin = "fips", 
                                 destination = "iso3c", 
                                 custom_match = custom.match.cia)

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

# Create a borderlength.df
borderlength.df <- border.cia %>%
  mutate(bstate = bstate) %>%
  unnest(cols = c(blength, bstate))

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
  rename(state1 = country, state2 = bstate) %>%
  group_by(state1, state2) %>%
  summarise(blength = sum(strtoi(blength), na.rm = FALSE))

# Join the individual border lengths to the main data
border.df <- border.df %>%
  left_join(borderlength.df, by = c("state1", "state2")) %>%
  left_join(border.cia %>%
              select(state1 = country, terrain), by = c("state1"))

# Create a dyad identifier variable
## -------------------------------------------------------------------------- ##
# Function to create a dyad identifier 
# from: https://stackoverflow.com/questions/52316998/create-unique-id-for-dyads-non-directional

dyadId_fun <- function(x,y) paste(sort(c(x, y)), collapse = "_")
dyadId_fun <- Vectorize(dyadId_fun)

# Apply the function
border.df <- border.df %>% 
  mutate(dyadName = dyadId_fun(state1, state2),
         dyadID = as.numeric(as.factor(dyadName))) %>%
  as_tibble()

# Add a variable that describes the geography of the border region between countries
# 
# Variable: see below
# Year: 2001
# Available at https://www.prio.org/Publications/Publication/?x=5142
# (code by AJO)
## -------------------------------------------------------------------------- ##
# Geography from Brochmann, Rod & Gleditsch (2012)
# Data years 1945-2001

# Custom match for Serbia (SRB, 345)
custom.match <- custom.match[1]

# Read in the data and prepare
geo.df <- import("./data/Replication_BrochmannRodGleditsch.dta") %>%
  filter(year == 2001) %>%
  mutate_at(vars(statea, stateb), 
            ~countrycode(., origin =  "cown", destination = "iso3c", 
                         custom_match = custom.match)) %>%
  as_tibble() %>%
  select(
    statea, stateb,
    lndistan, # distance between capitals (ln)
    rti, lnrti, # rugged terrain
    border_length_geo = GeoDist, # geodesic distance, i.e. border length
    distance,
    perc_forest = PercForest, perc_swamp = PercSwamp, perc_river = PercRiv, 
    perc_lake = PercLak, perc_river_lake = PercRivLak, perc_mountain = PercMnt,
    PercCF1, PercCF2, PopCnt, lnpopcnt ## ask authors for details about these vars
  )

# Some are duplicated in the geography dataset
geo.df[duplicated(geo.df$statea, geo.df$stateb), c("statea", "stateb")] 

# Dirty solution to create a directed version
# (1) Duplicate dataset, swap country identifiers and rename them
swap.df <- geo.df %>%
  rename(
    statea = stateb,
    stateb = statea
  )

# (2) Bind back together
geo.df <- geo.df %>%
  bind_rows(., swap.df) %>%
  mutate(dyadName = paste(statea, stateb, sep = "_")) %>%
  select(-c(statea, stateb)) %>%
  select(dyadName, everything()) %>%
  arrange(dyadName)

# See duplicates here:
geo.df[duplicated(geo.df$dyadName), "dyadName"]

# Remove duplicated entries
geo.df <- geo.df %>%
  distinct(dyadName, .keep_all = TRUE)

# (3) Match with the source data
border.df <- border.df %>%
  left_join(y = geo.df, by = c("dyadName"))

# Keep only the final data
## -------------------------------------------------------------------------- ##
rm(list = setdiff(ls(), "border.df"))

# Export
export(border.df, "./output/border.rds")
