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

