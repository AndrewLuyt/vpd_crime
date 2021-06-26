# a utility to remove nulls from the data and save it as
# an Rdata file for speedier loading

library(tidyverse)
library(terra)     # to convert UTM coordinates to Latitude/Longitude
library(sf)        # Simple Features (geographical objects): for neighbourhood maps
CRIMEDATA <- "data/crimedata_csv_all_years.csv"
SHAPEDATA <- "data/vancouver_neighbourhood_boundaries_geodata/local-area-boundary.geojson"

# most common result in a vector
getmode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

crime <- read.csv(CRIMEDATA)

# There are a small number of nulls in X/Y columns
# Though they are all clustered in the "Vehicle Collision (with Injury)"
# category, they represent less than 0.3% of that group and we will
# simply discard them.
# TODO: there are a small number of blank neighbourhood values too. Some
# correspond with the blank coordinates but some do have block locations.
# For now we just discard them, but later we might be able to extract
# the actual neighbourhood
crime <- crime %>% filter(!is.na(X) & NEIGHBOURHOOD != "")

# replace UTM coordinates with Lat/Long
coords <- cbind(crime$X, crime$Y)
utm_coords <- vect(coords, crs="+proj=utm +zone=10 +datum=WGS84  +units=m")
lat_lon <- project(utm_coords, "+proj=longlat +datum=WGS84")
extracted_lat_lon <- geom(lat_lon)[, c("x", "y")]
crime <- crime %>%
  mutate(latitude = extracted_lat_lon[,'y'], longitude = extracted_lat_lon[,'x']) %>%
  select(TYPE, YEAR, MONTH, DAY, HOUR, MINUTE, HUNDRED_BLOCK, NEIGHBOURHOOD, latitude, longitude) %>%
  rename_with(tolower) %>%
  rename(crime_type = type) %>%
  # TODO: see below, search for Musqueam
  filter(neighbourhood != 'Musqueam' & neighbourhood != 'Stanley Park')

# this is speedier to load
save(crime, file = 'data/crime.Rdata')

# save a CSV for use in Tableau
write.csv(crime, file = "data/processed_crime.csv")

# read geoJSON data into a dataframe-like object
# the 22 features are the 22 neighbourhoods
<<<<<<< HEAD
# TODO: the crime dataset includes 2 extra neighbourhoods (Musqueam and Stanley Park)
# that are not included in the official vancouver neighbourhood boundaries geodata.
# Can you find shapefiles or whatever for these? For now, drop them from the
# crime dataset
neighbourhoods <- st_read(SHAPEDATA)
neighbourhoods$name[neighbourhoods$name == 'Arbutus-Ridge'] <- 'Arbutus Ridge'
neighbourhoods$name[neighbourhoods$name == 'Downtown'] <- 'Central Business District'


# calculate top crimes
# TODO: when we join it ceases to be an sf object? geometry col seems to vanish
top_crimes <- crime %>%
  group_by(neighbourhood) %>%
  summarise(top_crime = getmode(crime_type)) %>%
  arrange(neighbourhood)

# add a new attribute column to the geodata: top_crime
# Sort to match top_crimes
top_crimes <- arrange(top_crimes, "neighbourhood")
neighbourhoods <- arrange(neighbourhoods, "name")

#top_crimes_sf <- inner_join(top_crimes, neighbourhoods, by = c("neighbourhood" = "name"))
# this is unenlightening: top crime everywhere is theft, mostly from vehicles.

ggplot(data = top_crimes_sf) +
  geom_sf(aes(geometry = geometry, fill=top_crime), col='white')
