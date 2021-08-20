WIP: Clean and process the VPD Crime dataset, with supplementary data
================
Andrew Luyt
2021-08-20

A utility to preprocess data from the VPD crimes dataset, the Vancouver
neighbourhoods dataset, and the Vancouver Census datasets, then save it
in various formats for use in other contexts, like mapping in R or
Tableau.

## Summary of files exported

-   `crime.*`: Cleaned crime data. Does not include *homicides* or
    *crimes against persons*
-   `no_loc_crime.*`: Homicides and Crimes Against A Person. Lacks
    location and time features - this is a feature of the raw data for
    anonymity.
-   `census.*`: Vancouver neighbourhood- and city-level populations from
    Canada’s official census in 2006, 2011, and 2016.
-   `population.*`: Population figures for all years in the VPD dataset,
    including official census figures and estimates we imputed
    ourselves.
-   `neighbourhoods.*`: Shape data (map outlines) for Vancouver
    neighbourhoods. The `.Rdata` is saved as an `sf` object.

``` r
library(tidyverse)
library(sf)        # Simple Features: for neighbourhood maps
library(lubridate)

CRIMEDATA <- "data/crimedata_csv_all_years.csv"
SHAPEDATA <- "data/vancouver_neighbourhoods/local-area-boundary.geojson"
```

## Helper functions

``` r
# the most commonly occurring value
Mode <- function(vec) {
  uv <- unique(vec)
  uv[which.max(tabulate(match(vec, uv)))]
}

# get a count of the mode in the vector
Modecount <- function(vec) {
  max(table(vec))
}
```

## Load and process the crime dataset

``` r
crime <- read_csv(CRIMEDATA)
```

In the geoJSON map data, Musqueam territory is geographically inside the
Dunbar-Southlands neighbourhood. The band has a policing arrangement
with the VPD and incidents are recorded as “Musqueam”. We don’t have map
data for Musqueam territory, so for mapping purposes we fold Musqueam
into D-S incidents. Also, give Downtown its intuitive name.

``` r
crime <- crime %>%
  mutate(NEIGHBOURHOOD =
           replace(NEIGHBOURHOOD,
                   which(NEIGHBOURHOOD == 'Musqueam'),
                   "Dunbar-Southlands"),
         NEIGHBOURHOOD =
           replace(NEIGHBOURHOOD,
                   which(NEIGHBOURHOOD == "Central Business District"),
                   "Downtown")) %>%
  select(TYPE, YEAR, MONTH, DAY, HOUR, MINUTE, HUNDRED_BLOCK, NEIGHBOURHOOD, X, Y) %>%
  rename_with(tolower) %>%
  rename(crime_type = type) %>%
  # We have no map data for Stanley Park
  filter(neighbourhood != 'Stanley Park')
```

### Split the dataset

There are a large number of incidents (over 68000 at time of writing)
that have a null or 0 X/Y location and time set to midnight. These are
the *Homicide* and *Offence Against A Person* crimes, and the raw VPD
dataset has them recorded in this manner for anonymity. We split them
off to be analyzed separately.

``` r
no_loc_crime <- crime %>%
  filter(crime_type == "Homicide" | crime_type == "Offence Against a Person") %>%
  select(-c(hour, minute, x, y))  # all zero, all useless features

# Create crime dataset for the other crimes
crime <- crime %>% filter(x != 0, y != 0, !is.na(x), !is.na(y),
                          neighbourhood != "",
                          crime_type != "Homicide",
                          crime_type != "Offence Against a Person")
```

Group the 9 remaining crimes into 4 general categories to simplify

``` r
old <- sort(unique(crime$crime_type))
new <- c('Break and Enter', 'Break and Enter', 'Mischief',
         'Theft', 'Theft', 'Theft', 'Theft',
         'Vehicular', 'Vehicular')
crime <- crime %>%
  mutate(
    general_crime_type = factor(crime_type, old, new),
    dt = as.POSIXct(paste(paste(year, month, day, sep = '-'), " ",
                          paste(hour, minute, sep = ":"))),
    weekday = wday(dt, label = TRUE, week_start = 1))

# create unique IDs.
crime <- tibble::rowid_to_column(crime, "id")
rm(new, old)
```

## Convert crime locations from UTM 10 to WGS84

The VPD dataset uses UTM coordinates but some tools work better with
longitude and latitude.

``` r
# Create Points for the crime locations.    NB: capital-M Map
points <- lapply(Map(c, crime$x, crime$y), st_point)

# Make an sf object with a geometry column holding sfc (points collection)
UTM.10 = st_crs(32610)  # VPD dataset uses UTM
coords <- st_sf(st_sfc(points, crs = UTM.10))

# rename geometry column, and make sure the sf object knows it was renamed
names(coords) <- c("geometry")
st_geometry(coords) <- "geometry"

# Convert UTM 10 coordinates into WGS84 longitude & latitude.
# Tableau prefers lon/lat coordinates
coords <- st_transform(coords, 4326)  # 4326 is the EPSG code for WGS84

# Create unique IDs that match to the rows in the "crime" tibble
coords <- tibble::rowid_to_column(coords, "id")

# attach WGS84 to crimes, remove superfluous UTM coords
crime <- inner_join(crime, coords, by = 'id') %>%
  select(-c(x,y))

rm(points, UTM.10)
```

## Load Vancouver neighbourhood maps.

The 22 features are the 22 neighbourhoods. Ensure neighbourhood names
match the crime data

``` r
neighbourhoods <- st_read(SHAPEDATA)
```

    ## Reading layer `local-area-boundary' from data source 
    ##   `/home/agl/projects/vpd_crime/data/vancouver_neighbourhoods/local-area-boundary.geojson' 
    ##   using driver `GeoJSON'
    ## Simple feature collection with 22 features and 3 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -123.2248 ymin: 49.19894 xmax: -123.0232 ymax: 49.29581
    ## Geodetic CRS:  WGS 84

``` r
neighbourhoods$name[neighbourhoods$name == 'Arbutus-Ridge'] <- 'Arbutus Ridge'
neighbourhoods <- neighbourhoods %>%
  select(-geo_point_2d)
```

## Load Census data

The raw census data is a bit untidy, and we only need to extract a
certain section, the so-called “100% data” which represents the entire
population for a region, not broken down by any demographic categories.

*“The data may be reproduced provided they are credited to Statistics
Canada, Census 2016, 2011, 2006, custom order for City of Vancouver
Local Areas”*

``` r
census2016 <- read_csv("data/CensusLocalAreaProfiles2016.csv", skip = 4,
                       col_types = "ncnnnnnnnnnnnnnnnnnnnnnnnn") %>%
  pivot_longer(3:26, names_to = "neighbourhood", values_to = "population") %>%
  filter(Variable == "Total - Age groups and average age of the population - 100% data") %>%
  mutate(year = 2016,
         neighbourhood = replace(neighbourhood, neighbourhood == "Vancouver CSD", "City of Vancouver"),
         neighbourhood = replace(neighbourhood, neighbourhood == "Vancouver CMA", "Metro Vancouver")) %>%
  select(neighbourhood, population, year)

census2011 <- read_csv("data/CensusLocalAreaProfiles2011.csv", skip = 4,
                       col_types = "cnnnnnnnnnnnnnnnnnnnnnnnn") %>%
  pivot_longer(2:25, names_to = "neighbourhood", values_to = "population") %>%
  rename(Variable = 1) %>%
  filter(Variable == "Total population by age groups") %>%
  mutate(year = 2011,
         neighbourhood = replace(neighbourhood, neighbourhood == "Vancouver CSD (City)", "City of Vancouver"),
         neighbourhood = replace(neighbourhood, neighbourhood == "CMA of Vancouver", "Metro Vancouver")) %>%
  select(neighbourhood, population, year)

census2006 <- read_csv("data/CensusLocalAreaProfiles2006.csv", skip = 4,
                       col_types = "cnnnnnnnnnnnnnnnnnnnnnnnn") %>%
  pivot_longer(2:25, names_to = "neighbourhood", values_to = "population") %>%
  rename(Variable = 1) %>%
  filter(Variable == "Male & Female, Total") %>%
  mutate(year = 2006,
         neighbourhood =
           replace(neighbourhood,
                   neighbourhood == "Vancouver CSD (City of Vancouver)", "City of Vancouver"),
         neighbourhood =
           replace(neighbourhood,
                   neighbourhood == "Vancouver CMA  (Metro Vancouver)", "Metro Vancouver")) %>%
  select(neighbourhood, population, year)

census <- rbind(census2016, census2011, census2006) %>%
  mutate(population = as.numeric(population),
         prediction = FALSE)  # Later we'll mark imputations as predictions

rm(census2016, census2011, census2006)
```

We have population data for 2006, 2011, and 2016, but we have crimes for
2003-2021. Per-capita crime figures are more easily comparable, so we’ll
create population estimates for all intervening years by imputing values
using a simple linear model. Note that we’ll be *extrapolating* for
2003-2005 and 2017-2021 so these figures should be seen with a more
doubtful eye. This will have to do until the 2021 census results are
released. We use a linear model with interactions: since population
growth changes over years and neighbourhood growth is different,
population will depend on the combination of year and neighbourhood.

``` r
model <- lm(population ~ year * neighbourhood, data = census)
predict_years <- c(2003:2005, 2007:2010, 2012:2015, 2017:2021)
predict_neighbourhoods <- unique(census$neighbourhood)
# Each year and neighbourhood gets a prediction: get all combinations
x <- expand.grid(predict_neighbourhoods, predict_years) %>%
  rename(neighbourhood = Var1, year = Var2)
predicted_populations <- x %>%
  mutate(population = predict(model, x),
         prediction = TRUE)
```

### Visually check predictions

The points are the real census data, the lines are predictions.

``` r
census %>%
  ggplot(aes(year, population, group = neighbourhood, color = neighbourhood)) +
  geom_point() +
  geom_line(data = predicted_populations,
             mapping = aes(year, population, color = neighbourhood)) +
  theme(legend.position = "none")
```

![](preprocess_data_files/figure-gfm/sanity%20check%20plots-1.png)<!-- -->

``` r
# get a closer look at the smaller neighbourhoods
tmp <- predicted_populations %>%
  filter(neighbourhood != "Metro Vancouver", neighbourhood != "City of Vancouver")
census %>%
  filter(neighbourhood != "Metro Vancouver", neighbourhood != "City of Vancouver") %>%
  ggplot(aes(year, population, group = neighbourhood, color = neighbourhood)) +
  geom_point() +
  geom_line(data = tmp,
             mapping = aes(year, population, color = neighbourhood))+
  theme(legend.position = "none")
```

![](preprocess_data_files/figure-gfm/sanity%20check%20plots-2.png)<!-- -->

Looks fine. Bind the population predictions to the census populations.

``` r
census_with_predictions <-
  rbind(census, predicted_populations) %>%
  arrange(year, neighbourhood)
```

## Save processed data

We’ll save in various formats for convenient use in R, Tableau, etc. A
binary file with R objects is speedier to load in R than a CSV.

``` r
save(census, file = "data/census.Rdata")
write_csv(census, file = "data/census.csv")
save(census_with_predictions, file = "data/population.Rdata")
write_csv(census_with_predictions, file = "data/population.csv")
save(no_loc_crime, file = 'data/no_loc_crime.Rdata')
write_csv(no_loc_crime, file = 'data/no_loc_crime.csv')
save(neighbourhoods, file = 'data/neighbourhoods.Rdata')
st_write(obj = neighbourhoods,
         dsn= "data/neighbourhoods.geoJSON",
         delete_dsn = TRUE)
```

At this point `crime` contains a `geometry` column: a 2-part coordinate
as one Point, which is useful for sf objects. In other contexts we may
like direct access to longitude/latitude, so break Points into two
numeric features.

``` r
crime <- crime %>%
  mutate(lon = unlist(map(coords$geometry, 1)),
         lat = unlist(map(coords$geometry, 2)))

save(crime, file = 'data/crime.Rdata')
st_write(obj = crime, dsn = 'data/crime.geojson', delete_dsn = TRUE)
write_csv(crime, file = "data/crime.csv")
```