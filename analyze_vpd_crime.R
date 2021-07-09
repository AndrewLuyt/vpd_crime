# Andrew Luyt, 2021
# WIP: Analyze the combined VPD Crime and neighbourhood maps.
#
# Download the data yourself as described in the README
# and run preprocess_data.R, which will remove some nulls,
# transform, and combine the data into more convenient form,
# then save various .Rdata, .geoJSON, and .csv files for use
# elsewhere.

library(tidyverse)

# incidents without location information
if (file.exists("data/no_loc_crime.Rdata")) {
  load("data/no_loc_crime.Rdata")
} else {
  stop("Please run preprocess_data.R to create the required data")
}

# Load the crime data
# if (file.exists("data/crime_plain.Rdata")) {
#   load("data/crime_plain.Rdata")
# } else {
#   stop("Please run preprocess_data.R to create the required data")
# }

# Load the crime data + geometry
if (file.exists("data/crimegeom.Rdata")) {
  load("data/crimegeom.Rdata")
} else {
  stop("Please run preprocess_data.R to create the required data")
}

# Load the neighbourhood shapes
if (file.exists("data/neighbourhoods.Rdata")) {
  load("data/neighbourhoods.Rdata")
} else {
  stop("Please run preprocess_data.R to create the required data")
}

# Draw the top crime by neighbourhood, and annotate with the percentage of
# all crime in the neighbourhood that it makes up
ggplot(data = neighbourhoods) +
  # geom_sf(aes(geometry = geometry, fill=crime_proportion), col='white') +
  geom_sf(aes(geometry = geometry, fill=top_crime), col='white') +
  # geom_sf_label(aes(label = round(p_top_crime * 100, 1)), cex = 3)  +
  # geom_sf_text(aes(label = name), cex = 3)
  # there is not a geom_sf_label_repel function, so we need to use
  # the basic one and add a few elements
  # https://www.johan-rosa.com/2019/11/13/creating-maps-with-sf-and-ggplot2/
  ggrepel::geom_label_repel(
    aes(label = paste(name, ": ", round(p_top_crime * 100, 1), " %", sep = ''),
        geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = .1,   # line segment connecting label with center of polygon
    label.size = NA,          # remove border
    force = 3,
    alpha=.9
  ) +
  labs(title = "Top crime by neighbourhood",
       subtitle = "And the percentage it is of that neighbourhood's crime",
       x = "longitude", y = 'latitude')

# Draw the top NON-THEFT crime by neighbourhood, and annotate with the
# percentage of all crime in the neighbourhood that it makes up
neighbourhoods %>% ggplot() +
  # geom_sf(aes(geometry = geometry, fill=crime_proportion), col='white') +
  geom_sf(aes(geometry = geometry, fill=top_nontheft_crime), col='white') +
  # geom_sf_text(aes(label=name), cex=3, alpha = .8) +
  geom_sf_label(aes(label = paste(round(p_top_nontheft_crime, 2) * 100, "%", sep=""))) +
  # there is not a geom_sf_label_repel function, so we need to use
  # the basic one and add a few elements
  # https://www.johan-rosa.com/2019/11/13/creating-maps-with-sf-and-ggplot2/
  # ggrepel::geom_label_repel(
  #   # aes(label = round(p_top_nontheft_crime * 100, 1), geometry = geometry),
  #   aes(label = round(p_top_nontheft_crime * 100, 1),
  #       geometry = geometry),
  #   stat = "sf_coordinates",
  #   min.segment.length = 0,   # line segment connecting label with center of polygon
  #   label.size = NA,          # remove border
  #   force = 3,
  #   alpha = 0.9
  # ) +
  labs(title = "If Not Theft, Then What? Top non-theft crime by neighbourhood",
       subtitle = "And the percentage it is of that neighbourhood's crime",
       x = "longitude", y = 'latitude')

