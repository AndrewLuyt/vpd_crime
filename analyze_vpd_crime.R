library(tidyverse)

# download the data yourself as described in the README
#

# It's just faster to load Rdata than parse a CSV
if (Sys.getenv('USERNAME') == "agl") {
  load("data/crime.Rdata")
} else {
  crime <- read.csv("data/crimedata_csv_all_years.csv")
}

# There are a small number of nulls in X/Y columns
# Though they are all clustered in the "Vehicle Collision (with Injury)"
# category, they represent less than 0.3% of that group and we will
# simply discard them.
crime <- crime %>% filter(!is.na(X))


