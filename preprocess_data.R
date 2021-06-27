# a utility to remove nulls from the data and save it as
# an Rdata file for speedier loading

library(tidyverse)
library(terra)     # to convert UTM coordinates to Latitude/Longitude
library(sf)        # Simple Features (geographical objects): for neighbourhood maps
library(ggrepel)   # automatically move map labels so they don't overlap

CRIMEDATA <- "data/crimedata_csv_all_years.csv"
SHAPEDATA <- "data/vancouver_neighbourhood_boundaries_geodata/local-area-boundary.geojson"

# statistical mode: works for strings too
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

# Musqueam is geographically in Dunbar-Southlands and has a policing
# arrangement with VPD. For mapping purposes, convert Musqueam into D-S incidents
crime <- crime %>%
  mutate(x = extracted_lat_lon[,'x'],
         y = extracted_lat_lon[,'y'],
         NEIGHBOURHOOD = replace(NEIGHBOURHOOD, which(NEIGHBOURHOOD == 'Musqueam'), "Dunbar-Southlands")) %>%
  select(TYPE, YEAR, MONTH, DAY, HOUR, MINUTE, HUNDRED_BLOCK, NEIGHBOURHOOD, x, y) %>%
  rename_with(tolower) %>%
  rename(crime_type = type) %>%
  # We have no map for Stanley Park
  filter(neighbourhood != 'Stanley Park')

# this is speedier to load
save(crime, file = 'data/crime.Rdata')

# save a CSV for use in Tableau
write.csv(crime, file = "data/processed_crime.csv")

# read geoJSON data into a dataframe-like object
# the 22 features are the 22 neighbourhoods

# Ensure neighbourhood names match the geoJSON data
neighbourhoods <- st_read(SHAPEDATA)
neighbourhoods$name[neighbourhoods$name == 'Arbutus-Ridge'] <- 'Arbutus Ridge'
neighbourhoods$name[neighbourhoods$name == 'Downtown'] <- 'Central Business District'

# calculate top crimes
# TODO: when we join it ceases to be an sf object? geometry col seems to vanish
n_crimes = length(crime$crime_type)
top_crimes <- crime %>%
  group_by(neighbourhood) %>%
  summarise(top_crime = getmode(crime_type), crime_proportion = n() / n_crimes) %>%
  arrange(neighbourhood)

# add a new attribute column to the geodata: top_crime
# Sort to match top_crimes
#top_crimes <- arrange(top_crimes, "neighbourhood")
#neighbourhoods <- arrange(neighbourhoods, "name")

# IMPORTANT: when joining an sf object to a dataframe, ensure the first object
# in the pipe is the sf object. Encountered a bug here.
neighbourhoods <- neighbourhoods %>% inner_join(top_crimes, by=c("name" = "neighbourhood"))
# this is unenlightening: top crime everywhere is theft, mostly from vehicles.

ggplot(data = neighbourhoods) +
  # geom_sf(aes(geometry = geometry, fill=crime_proportion), col='white') +
  geom_sf(aes(geometry = geometry, fill=top_crime), col='white') +
  #geom_sf_label(aes(label = name), cex = 3) +
# there is not a geom_sf_label_repel function, so we need to use
# the basic one and add a few elements
# https://www.johan-rosa.com/2019/11/13/creating-maps-with-sf-and-ggplot2/
ggrepel::geom_label_repel(
  aes(label = name, geometry = geometry),
  stat = "sf_coordinates",
  min.segment.length = 0,   # line segment connecting label with center of polygon
  label.size = NA,          # remove border
  # force = 3
)

