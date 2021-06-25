# a utility to remove nulls from the data and save it as
# an Rdata file for speedier loading

library(tidyverse)
library(terra)     # to convert UTM coordinates to Latitude/Longitude

crime <- read.csv("data/crimedata_csv_all_years.csv")

# There are a small number of nulls in X/Y columns
# Though they are all clustered in the "Vehicle Collision (with Injury)"
# category, they represent less than 0.3% of that group and we will
# simply discard them.
crime <- crime %>% filter(!is.na(X))

# replace UTM coordinates with Lat/Long
coords <- cbind(crime$X, crime$Y)
utm_coords <- vect(coords, crs="+proj=utm +zone=10 +datum=WGS84  +units=m")
lat_lon <- project(utm_coords, "+proj=longlat +datum=WGS84")
extracted_lat_lon <- geom(lat_lon)[, c("x", "y")]
crime <- crime %>%
  mutate(latitude = extracted_lat_lon[,'y'], longitude = extracted_lat_lon[,'x']) %>%
  select(TYPE, YEAR, MONTH, DAY, HOUR, MINUTE, HUNDRED_BLOCK, NEIGHBOURHOOD, latitude, longitude)

# this is speedier to load
save(crime, file = 'data/crime.Rdata')

# save a CSV for use in Tableau
write.csv(crime, file = "data/processed_crime.csv")
