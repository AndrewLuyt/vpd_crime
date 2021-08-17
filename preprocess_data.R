# Andrew Luyt, 2021
#
# A utility to preprocess data from the VPD crimes dataset, the Vancouver
# neighbourhoods dataset, and (TODO) the Vancouver Census dataset,
# and then save it in various formats for use in other contexts, like
# mapping in R or Tableau.

library(tidyverse)
library(sf)        # Simple Features (geographical objects): for neighbourhood maps
library(lubridate)

CRIMEDATA <- "data/crimedata_csv_all_years.csv"
SHAPEDATA <- "data/vancouver_neighbourhood_boundaries_geodata/local-area-boundary.geojson"

# the mode (from statistics): works for strings too
getmode <- function(vec) {
  uv <- unique(vec)
  uv[which.max(tabulate(match(vec, uv)))]
}
# get a count of the mode in the vector
getmodecount <- function(vec) {
  max(table(vec))
}

# LOAD AND PROCESS VPD CRIME DATASET ###################################
crime <- read_csv(CRIMEDATA)

# By the geoJSON data, Musqueam territory is geographically inside
# Dunbar-Southlands. The band has a policing arrangement with VPD,
# and incidents are recorded as "Musqueam".
# For mapping purposes, fold Musqueam into D-S incidents.
# Also give downtown its intuitive name.
crime <- crime %>%
  mutate(NEIGHBOURHOOD = replace(NEIGHBOURHOOD,
                                 which(NEIGHBOURHOOD == 'Musqueam'),
                                 "Dunbar-Southlands"),
         NEIGHBOURHOOD = replace(NEIGHBOURHOOD,
                                 which(NEIGHBOURHOOD == "Central Business District"),
                                 "Downtown")) %>%
  select(TYPE, YEAR, MONTH, DAY, HOUR, MINUTE, HUNDRED_BLOCK, NEIGHBOURHOOD, X, Y) %>%
  rename_with(tolower) %>%
  rename(crime_type = type) %>%
  # We have no map for Stanley Park
  filter(neighbourhood != 'Stanley Park')

# There are a large number (over 68000 at time of writing) of incidents
# that have a null or 0 X/Y location.
# These are the Homicide and Offence Against A Person crimes, which
# have been doubly anonymized.
no_loc_crime <- crime %>%
  filter(crime_type == "Homicide" | crime_type == "Offence Against a Person")
  # filter(x == 0 | y == 0 | is.na(x) | is.na(y) | neighbourhood == "")

# crime dataset with complete observations only
crime <- crime %>% filter(x != 0, y != 0, !is.na(x), !is.na(y),
                          neighbourhood != "",
                          crime_type != "Homicide",
                          crime_type != "Offence Against a Person")

# group the 9 remaining crimes into 4 general categories to simplify
old <- sort(unique(crime$crime_type))
new <- c('Break and Enter', 'Break and Enter', 'Mischief',
         'Theft', 'Theft', 'Theft', 'Theft',
         'Vehicular', 'Vehicular')

crime <- crime %>%
  mutate(
    general_crime_type = factor(crime_type, old, new),
    # create weekday feature, week starts Monday, as a factor
    weekday = wday(as.POSIXct(paste(paste(year, month, day, sep = '-'), " ",
                                    paste(hour, minute, sep = ":"))),
                   label = TRUE,
                   week_start = 1),
    dt = as.POSIXct(paste(paste(year, month, day, sep = '-'), " ",
                                paste(hour, minute, sep = ":"))))

# create unique row ID.
crime <- tibble::rowid_to_column(crime, "id")

rm(new, old)

# LOAD Vancouver neighbourhood maps ###################################
# read geoJSON data into a dataframe-like object
# the 22 features are the 22 neighbourhoods
# Ensure neighbourhood names match the geoJSON data
neighbourhoods <- st_read(SHAPEDATA)
neighbourhoods$name[neighbourhoods$name == 'Arbutus-Ridge'] <- 'Arbutus Ridge'

# TOP CRIMES ###########################################
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

# Attach this information to the neighbourhoods
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

# Attach top_nontheft_crimes to the neighbourhoods dataset
# IMPORTANT: when joining an sf object to a dataframe, ensure the first object
# in the pipe is the sf object. Encountered a bug here.
neighbourhoods <-
  inner_join(neighbourhoods,
             top_nontheft_crimes,
             by=c("name" = "neighbourhood")) %>%
  select(-total_incidents.y) %>%
  rename(total_incidents = total_incidents.x)

# Convert UTM 10 coordinates into WGS84 longitude & latitude #################
# First create sf Points for the crime locations, NB: capital-M Map
points <- lapply(Map(c, crime$x, crime$y), st_point)

# Make the sf object with a geometry column of sfc (points collection)
UTM.10 = st_crs(32610)
coords <- st_sf(st_sfc(points, crs = UTM.10))

# rename geometry column, and make sure the sf object knows it was renamed
names(coords) <- c("geometry")
st_geometry(coords) <- "geometry"

# Tableau prefers lon/lat coordinates, not UTM. Convert to NAD83.
coords <- st_transform(coords, 4269)

# Create unique IDs that match to the rows in the "crime" tibble
coords <- tibble::rowid_to_column(coords, "id")

rm(points, UTM.10)

# SAVE PROCESSED DATA ################################################
# a preprocessed binary file is speedier to load than a CSV
save(no_loc_crime, file = 'data/no_loc_crime.Rdata')
# save(crime, file = 'data/crimegeom.Rdata')
save(neighbourhoods, file = 'data/neighbourhoods.Rdata')
# keep a geoJSON file too. Delete the old version first, else st_write
# apparently reads the entire file first (slow)
joined = inner_join(crime, coords, by = 'id')
st_write(obj = joined, dsn = 'data/crimegeom.geojson', delete_dsn = TRUE)

# TODO: this is failing with an error now?
# Tableau likes geojson - save the neighbourhoods in that format
# st_write(obj = neighbourhoods,
#          dsn= "data/processed-neighbourhood/neighbourhoods.geoJSON",
#          delete_dsn = TRUE)

# cleaned crime data with longitude-latitude coordinates
crime <- crime %>%
  mutate(lon = unlist(map(coords$geometry, 1)),
         lat = unlist(map(coords$geometry,2)))

save(crime, file = 'data/crime.Rdata')
write.csv(crime, file = "data/tableau_crime.csv")
