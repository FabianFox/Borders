# FRONTEX Risk Analysis Network (FRAN) Reports

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest, rio)

# The reports are available through:
# - EU Open Data Portal (Query: Frontex Risk Analysis Network)
# Link: https://data.europa.eu/euodp/en/data/dataset?q=Frontext+Risk+Analysis+Network&ext_boolean=all&sort=
# - Frontex website 
# Link 1 (pdf-docs): https://frontex.europa.eu/publications/?category=riskanalysis
# Link 2 (xlsx): https://frontex.europa.eu/assets/Migratory_routes/Detections_of_IBC_2018_07_06.xlsx

# Of course, the excel-sheet is the best source. However, I did only find about it after extracting the data from 
# the pdfs. Because it is a useful example of extracting tables from pdfs, I'll keep the code. 

# from Frontex (xlsx)
## ------------------------------------------------------------------------------------------------------------ ##
routes.df <- import(file = "https://frontex.europa.eu/assets/Migratory_routes/Detections_of_IBC_2018_07_06.xlsx",
                    sheet = 1) %>%
  gather()



# from EU Open Data
## ------------------------------------------------------------------------------------------------------------ ##
# Find the available reports 
# There are three pages with information

# (1) Get individual page links
pages <- vector(mode = "character", length = 3)

for (i in seq_along(pages)) {
  pages[i] <- paste0("https://data.europa.eu/euodp/en/data/dataset?q=Frontext+Risk+Analysis+Network&ext_boolean=all&sort=&page=", i) 
}

# (2) Get titles and URL to the pdfs
frontex <- tibble(
  title = vector("list", length = 3),
  link = vector("list", length = 3)
)

for(i in seq_along(pages)) {
  link <- read_html(pages[i])
  
  frontex$title[[i]] <- link %>%
    html_nodes(css = ".item_link strong") %>%
    html_text(link)
  
  frontex$link[[i]] <- link %>%
    html_nodes(css = ".item_link:nth-child(3)") %>%
    html_attr(name = "href") %>%
    paste0("https://data.europa.eu/euodp", .)
  
  Sys.sleep(sample(seq(0, 2, 0.5), 1))
}

# (3) Flatten
frontex.df <- frontex %>%
  map(unlist) %>%
  as_tibble(.)

# Housekeeping
rm(list = setdiff(ls(), "frontex.df"))

# Download the reports to disk
# (1) Get the links to the pdf-docs
pdf <- vector("character", length = length(frontex.df$link))

for(i in seq_along(pdf)) {
  pdf[[i]] <- read_html(frontex.df$link[i]) %>%
    html_nodes(css = ".button-box") %>%
    html_attr("href")
  
  Sys.sleep(sample(seq(0, 2, 0.5), 1))
}

# (2) Merge to frontex.df
frontex.df <- frontex.df %>%
  mutate(pdf = pdf,
         dest = paste0("./FRAN-reports/", title, ".pdf")) 

# (3) Download the reports
map2(.x = frontex.df$pdf, .y = frontex.df$dest, .f = ~{
  Sys.sleep(3)
  download.file(url = .x, destfile = .y, mode = "wb")
  })

# from Frontex (pdf-docs)
## ------------------------------------------------------------------------------------------------------------ ##
# (1) Basic df
info <- read_html("https://frontex.europa.eu/publications/?category=riskanalysis") %>%
  html_nodes(".hidden-xs a")

frontex.df <- tibble(
  title = html_text(info),
  link = paste0("https://frontex.europa.eu/",
                html_attr(info, "href")),
  dest = paste0("./FRAN-reports/", title, ".pdf")
)

# (2) Get URLs to pdf-docs
pdf <- vector("character", length = length(frontex.df$link))

for(i in seq_along(pdf)) {
  pdf[[i]] <- read_html(frontex.df$link[i]) %>%
    html_nodes(css = "#content a") %>%
    html_attr("href") %>%
    paste0("https://frontex.europa.eu/", .)
  
  Sys.sleep(sample(seq(0, 2, 0.5), 1))
}

# (3) Merge to frontex.df
frontex.df <- frontex.df %>%
  mutate(pdf = pdf)

# (4) Download the reports
map2(.x = frontex.df$pdf, .y = frontex.df$dest, .f = ~{
  Sys.sleep(3)
  download.file(url = .x, destfile = .y, mode = "wb")
})

# All docs
sum(length(dir("./FRAN-reports/"))) # yep