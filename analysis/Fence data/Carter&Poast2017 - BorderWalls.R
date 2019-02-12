# Replication data from Carter & Poast (2017) 
# Why Do States Build Walls? Political Economy, Security, and Border Stability 
# Journal of Conflict Resolution, 61:2, 239-270
# Data from: 
# https://journals.sagepub.com/doi/abs/10.1177/0022002715596776
# http://www.paulpoast.com/#/original-data/4590503653

# The reproducible data entails many incomprehensible columns and, by now, I can't
# make sense of it. 

# Load/install 
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, countrycode, rio)

# Data available as excel-file (though likely SPSS-format)
barriers.cp <- import("./data/border data/Security Barriers 1800 to Present.xlsx", sheet = 1)
