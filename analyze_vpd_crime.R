#' ---
#' author: "Andrew Luyt"
#' title: "WIP: Explore & analyze the VPD Crime dataset"
#' ---
#'
#' Download the data yourself as described in the README
#' and run `preprocess_data.R`, which will remove some nulls,
#' transform & combine the data into more convenient form,
#' then save various `.Rdata`, `.geoJSON`, and `.csv` files for use
#' elsewhere.

library(tidyverse)
library(sf)

#' **Incidents without location information**
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

#' **Load the crime data with geometry**
if (file.exists("data/crimegeom.Rdata")) {
  load("data/crimegeom.Rdata")
} else {
  stop("Please run preprocess_data.R to create the required data")
}

#' **Load the neighbourhood shapes**
if (file.exists("data/neighbourhoods.Rdata")) {
  load("data/neighbourhoods.Rdata")
} else {
  stop("Please run preprocess_data.R to create the required data")
}


#' ## Total crime by year
crime %>%
  ggplot(aes(x=year)) +
  geom_bar()

#' ## Crime by weekday, 2020.
#' Pretty much identical, in totals...
crime %>%
  filter(year == 2020) %>%
  ggplot(aes(x=weekday)) +
  geom_bar()

#' ## Proportional crime by weekday, showing crime type, 2020.
#' Pretty much identical!
crime %>%
  filter(year == 2021) %>%
  ggplot(aes(x=weekday, fill=crime_type)) +
  geom_bar(position = 'fill')

#' ## Proportional crime by weekday and hour of crime, 2020.
#' Pretty much identical!
crime %>%
  filter(year == 2021) %>%
  ggplot(aes(x=weekday, fill=factor(hour))) +
  geom_bar(position = 'fill')

#' ## Proportional crime by month, 2016-2020.
#' Note the proportional shinkage of crime in the spring and summer months
#' of 2020: is this the Covid-19 effect with people staying home?
crime %>%
  filter(year >= 2016 & year < 2021) %>%
  ggplot(aes(x=year, fill=factor(month))) +
  geom_bar(position='fill')

#' ## What time is most popular for crime, 2020?
#' A huge spike at midnight - is this a data entry issue?
crime %>%
  filter(year == 2020) %>%
  ggplot(aes(hour)) +
  geom_bar()

#' ## What times are most popular for *classes* of crime, 2020?
#' Interestingly all classes show the "midnight spike" **except** vehicular
#' collisions.  Break and enter crimes are surprisingly consistent all day.
crime %>%
  filter(year == 2020) %>%
  ggplot(aes(hour)) +
  geom_bar() +
  facet_wrap(~general_crime_type, scales = 'free_y')

#' ## What times are most popular for *detailed* classes of crime, 2020?
#' *Other theft* peaks earlier (4pm) and *theft from Vehicle* peaks
#' later (7-9pm)
crime %>%
  filter(year == 2020) %>%
  ggplot(aes(hour)) +
  geom_bar() +
  facet_wrap(~crime_type, scales = 'free_y', )

#' ## Have the *proportions* of *types* of crime changed over the years?
#' It appears theft was on the rise until 2020 when it dropped sharply.
crime %>%
  filter(year < 2021) %>%
  ggplot(aes(desc(year), fill = general_crime_type)) +
  geom_bar(position = 'fill', color='black', size=.5, width = .8) +
  coord_flip()

#' ## Same as above but with *raw counts* instead of proportions.
#' Here we can confirm that the drop in crime in 2020 seems to have been
#' driven by a decline in *theft*.
crime %>%
  filter(year < 2021) %>%
  ggplot(aes(desc(year), fill = general_crime_type)) +
  geom_bar(position = 'stack', color='black', size=.5, width = .8) +
  coord_flip()

#' ## Even more detail..
#' Looking closely we can see that the drop in *theft* in 2020 seems mostly to have been
#' driven by a decline in *thefts from vehicles*.
crime %>%
  filter(year < 2021) %>%
  ggplot(aes(desc(year), fill = crime_type)) +
  geom_bar(position = 'stack', color='black', size=.5, width = .8) +
  coord_flip()

#' ## Comparing 2019, 2020, and 2021 so far
#' We saw a major decline in two types of crime in 2020. has the trend
#' continued in 2021?  We'll examine proportions of crime types, since
#' we are only halfway through 2021.
#'
#' It appears that thefts from vehicles have continued to drop in 2021.

# It's hard to get the % annotations using ggplot, so we do it by making
# a table, plotting `geom_col`, then grabbing the value of pct
# for the labels
crime %>%
  as_tibble() %>%  # sf objects really mess with the tidyverse.  Why?
  filter(year >= 2019 &
           crime_type != "Vehicle Collision or Pedestrian Struck (with Fatality)") %>%
  group_by(year, crime_type) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(factor(year, levels = c(2021, 2020, 2019)),
             pct,
             fill = crime_type,
             label = paste(round(pct, 0), "%", sep=""))) +
  geom_col(color='black', size=.5, width = .8) +
  geom_text(position = position_stack(vjust = 0.5)) +
  coord_flip() +
  labs(y = "Percentage of that year's crime", x = "year") +
  theme(legend.position = 'bottom')



#' ## Have the TYPES of crime changed over the years?
#' With more detail on crimes, and with absolute counts by year.
#' We can see the effect of Covid-19
crime %>% ggplot(aes(year, fill = crime_type)) +
  geom_bar(color='black') +
  annotate("text", label = "Covid-19?", x = 2020, y = 50000) +
  annotate("curve", x = 2020, y = 48000, xend = 2020, yend = 34000,
           curvature = -.3, arrow = arrow())

#' ## Some maps (WIP)
#' Draw the top crime by neighbourhood, and annotate with the percentage of
#' all crime in the neighbourhood that it makes up
#+ include=FALSE
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

#' Draw the top NON-THEFT crime by neighbourhood, and annotate with the
#' percentage of all crime in the neighbourhood that it makes up
#+ include=FALSE
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

