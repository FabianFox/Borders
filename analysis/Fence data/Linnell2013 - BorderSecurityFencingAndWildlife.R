# Data on Border Barriers by Linnell et al (2013) Appendix S1
### ------------------------------------------------------------------------ ###

# Data from: Linnell et al. (2013) Border Security Fencing and Wildlife: 
#            The End of the Transboundary Paradigm in Eurasia? PLoS Biol 14(6): 
#           e1002483. https://doi.org/10.1371/journal.pbio.1002483

# Data is undirected!

# Load/install packages
### ------------------------------------------------------------------------ ###
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, countrycode, rio, docxtractr)

# Load data into R
### ------------------------------------------------------------------------ ###
barriers.docx <- read_docx("./data/border data/Linnell et al 2016 - Supporting Material S1.docx")

# Extract the table
barriers.ln <- barriers.docx %>%
  docx_extract_tbl() %>%
  .[-c(1, 23:24, 50:51),] %>%
  setNames(c("countries", "border.length", "fence.length", "description")) 

# Data wrangling
# Abkhazia and 
custom.match <- c("Tadjikistan" = "TKL", "Abkhazia" = "RUS", 
                  "South Ossetia" = "RUS", "North Korea" = "PRK")

barriers.ln <- barriers.ln %>%
  mutate(state1 = str_extract(countries, "([:alpha:]+\\s[:alpha:]+)|[:alpha:]+"), # also: tidyr::separate
         state2 = str_extract(countries, "([:alpha:]+\\s[:alpha:]+)|[:alpha:]+$"),
         state1 = countrycode(state1, "country.name", "iso3c", 
                              custom_match = custom.match),
         state2 = countrycode(state2, "country.name", "iso3c", 
                              custom_match = custom.match),
         source = "Linnell et al. (2013)",
         year = "2015/2016",  
         indicator = "fencing")

# Remove unrecognized or clean unmatched states
# India (Kashmir) = IND; remove Transnistria
barriers.ln[41, "state2"] <- "IND"
barriers.ln <- barriers.ln[-13,]

# Export
export(barriers.ln, "./data/border data/Linnell et al. 2016.rds")
