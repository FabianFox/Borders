# Asylum data
### ------------------------------------------------------------------------###

# - number of asylum applications (eurostat)
# ID: tps00191: Asylum and first time asylum applicants - annual aggregated data 
#     (rounded)
# Metadata: http://ec.europa.eu/eurostat/cache/metadata/en/migr_asyapp_esms.htm
# Further information: https://bit.ly/2LXrsKB and 
#                      http://dd.eionet.europa.eu/vocabulary/eurostat/asyl_app

# Clean eurostat-cache from time to time
# clean_eurostat_cache(cache_dir = NULL)

# !!! MONTHLY !!! applications from 2015 - 2017
# Yearly applications from 2015 - 2017
asylum.df <- get_eurostat("migr_asyappctzm", time_format = "date", 
                          stringsAsFactors = FALSE, filters = 
                            list(asyl_app = "ASY_APP", citizen = "TOTAL",
                                 age = "TOTAL")) %>%
  select(country = geo, applicants = values, year = time) %>%
  filter(!country %in% c("EU28", "TOTAL") & between(year, as.Date("2015-01-01"), as.Date("2017-01-01"))) %>%
  mutate(country = countrycode(country, "eurostat", "iso3c"),
         year = year(year)) %>%
  group_by(country, year) %>%
  summarise(applicants = sum(applicants))
