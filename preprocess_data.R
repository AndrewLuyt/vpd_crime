# a utility to remove nulls from the data and save it as
# an Rdata file for speedier loading

library(tidyverse)
library(terra)     # to convert UTM coordinates to Latitude/Longitude
library(sf)        # Simple Features (geographical objects): for neighbourhood maps
library(ggrepel)   # automatically move map labels so they don't overlap

CRIMEDATA <- "data/crimedata_csv_all_years.csv"
SHAPEDATA <- "data/vancouver_neighbourhood_boundaries_geodata/local-area-boundary.geojson"

# statistical mode: works for strings too
getmode <- function(vec) {
  uv <- unique(vec)
  uv[which.max(tabulate(match(vec, uv)))]
}
# get a count of the mode in the vector
getmodecount <- function(vec) {
  max(table(vec))
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

# replace UTM coordinates with Lat/Long: this helps with Tableau mapping
coords <- cbind(crime$X, crime$Y)
utm_coords <- vect(coords, crs="+proj=utm +zone=10 +datum=WGS84  +units=m")
lat_lon <- project(utm_coords, "+proj=longlat +datum=WGS84")
extracted_lat_lon <- geom(lat_lon)[, c("x", "y")]

# By the geoJSON data, Musqueam territory is geographically inside
# Dunbar-Southlands. The band has a policing arrangement with VPD,
# and incidents are recorded as "Musqueam".
# For mapping purposes, fold Musqueam into D-S incidents.
crime <- crime %>%
  mutate(x = extracted_lat_lon[,'x'],
         y = extracted_lat_lon[,'y'],
         NEIGHBOURHOOD = replace(NEIGHBOURHOOD,
                                 which(NEIGHBOURHOOD == 'Musqueam'),
                                 "Dunbar-Southlands")) %>%
  select(TYPE, YEAR, MONTH, DAY, HOUR, MINUTE, HUNDRED_BLOCK, NEIGHBOURHOOD, x, y) %>%
  rename_with(tolower) %>%
  rename(crime_type = type) %>%
  # We have no map for Stanley Park
  filter(neighbourhood != 'Stanley Park')

# remove temp variables
rm(coords, utm_coords, lat_lon, extracted_lat_lon)

# group the 11 crimes into 6 more general categories to make vis's simpler
old <- sort(unique(crime$crime_type))
new <- c('Break and Enter', 'Break and Enter', 'Homicide', 'Mischief',
         'Offence Against a Person', 'Theft', 'Theft', 'Theft', 'Theft',
         'Vehicle or Pedestrian Struck',
         'Vehicle or Pedestrian Struck')
crime$general_crime_type <- factor(crime$crime_type, old, new)
rm(new, old)

# a preprocessed binary file is speedier to load than a CSV
save(crime, file = 'data/crime.Rdata')
# a CSV for use in Tableau
write.csv(crime, file = "data/processed_crime.csv")

# read geoJSON data into a dataframe-like object
# the 22 features are the 22 neighbourhoods
# Ensure neighbourhood names match the geoJSON data
neighbourhoods <- st_read(SHAPEDATA)
neighbourhoods$name[neighbourhoods$name == 'Arbutus-Ridge'] <- 'Arbutus Ridge'
neighbourhoods$name[neighbourhoods$name == 'Downtown'] <- 'Central Business District'

# calculate top crimes and add to neighbourhoods
# This seems unenlightening: everywhere, it's theft.
n_crimes = length(crime$crime_type)
top_crimes <- crime %>%
  group_by(neighbourhood) %>%
  summarise(top_crime = getmode(crime_type),
            n_top_crime = getmodecount(crime_type),
            p_top_crime = n_top_crime / n(),
            total_incidents = n()) %>%
  arrange(neighbourhood)
# IMPORTANT: when joining an sf object to a dataframe, ensure the first object
# in the pipe is the sf object. Encountered a bug here.
neighbourhoods <- inner_join(neighbourhoods, top_crimes, by=c("name" = "neighbourhood"))

# calculate top NON-THEFT crimes. We also want the proportion of neighbourhood
# crime it takes up. This takes a few steps.
top_nontheft_crimes <- crime %>%
  group_by(neighbourhood) %>%
  filter(general_crime_type != 'Theft') %>%
  summarise(top_nontheft_crime = getmode(crime_type),
            n_top_nontheft_crime = getmodecount(crime_type)) %>%
  arrange(neighbourhood)
# top_crimes contains the neighbourhood-specific number of incidents.
# If we calculated that after filtering out 'Theft' we would have the wrong n.
top_nontheft_crimes <-
  inner_join(top_nontheft_crimes,
             top_crimes[,c('neighbourhood', 'total_incidents')],
             by = "neighbourhood")
# Now the calculation is simple
top_nontheft_crimes$p_top_nontheft_crime <-
  top_nontheft_crimes$n_top_nontheft_crime / top_nontheft_crimes$total_incidents

# IMPORTANT: when joining an sf object to a dataframe, ensure the first object
# in the pipe is the sf object. Encountered a bug here.
neighbourhoods <-
  inner_join(neighbourhoods,
             top_nontheft_crimes,
             by=c("name" = "neighbourhood"))

ggplot(data = neighbourhoods) +
  # geom_sf(aes(geometry = geometry, fill=crime_proportion), col='white') +
  geom_sf(aes(geometry = geometry, fill=top_crime), col='white') +
  #geom_sf_label(aes(label = name), cex = 3) +
# there is not a geom_sf_label_repel function, so we need to use
# the basic one and add a few elements
# https://www.johan-rosa.com/2019/11/13/creating-maps-with-sf-and-ggplot2/
ggrepel::geom_label_repel(
  # % of ALL city crime, NOT THE NEIGHBOURHOOD CRIME
  aes(label = round(p_top_crime * 100, 1), geometry = geometry),
  stat = "sf_coordinates",
  min.segment.length = 0,   # line segment connecting label with center of polygon
  label.size = NA,          # remove border
  # force = 3
)

ggplot(data = neighbourhoods) +
  # geom_sf(aes(geometry = geometry, fill=crime_proportion), col='white') +
  geom_sf(aes(geometry = geometry, fill=top_nontheft_crime), col='white') +
  #geom_sf_label(aes(label = name), cex = 3) +
  # there is not a geom_sf_label_repel function, so we need to use
  # the basic one and add a few elements
  # https://www.johan-rosa.com/2019/11/13/creating-maps-with-sf-and-ggplot2/
  ggrepel::geom_label_repel(
    aes(label = round(p_top_nontheft_crime * 100, 1), geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,   # line segment connecting label with center of polygon
    label.size = NA,          # remove border
    # force = 3
  )

