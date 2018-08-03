# FRONTEX Risk Analysis Network (FRAN) Reports

# Load/install packages
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, rvest)

# The reports are available through:
# - EU Open Data Portal (Query: Frontex Risk Analysis Network)
# Link: https://data.europa.eu/euodp/en/data/dataset?q=Frontext+Risk+Analysis+Network&ext_boolean=all&sort=
# - Frontex website 
# Link: https://frontex.europa.eu/publications/?category=riskanalysis

# Find the available reports (from EU Open Data)
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

# (3) Download the protocols
map2(.x = frontex.df$pdf, .y = frontex.df$dest, .f = ~{
  Sys.sleep(2.5)
  download.file(url = .x, destfile = .y, mode = "wb")
  })
