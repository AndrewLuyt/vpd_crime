library(tidyverse)

# download the data yourself as described in the README
# and run preprocess_data.R, which will transform the data
# slightly and remove some nulls

# Load the .Rdata if it's there, for speed
if (file.exists("data/crime.Rdata")) {
  load("data/crime.Rdata")
} else {
  stop("Please run preprocess_data.R to create the required data")
}





