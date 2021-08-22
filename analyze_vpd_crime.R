#' ---
#' author: "Andrew Luyt"
#' title: "WIP: Explore & analyze the VPD Crime dataset"
#' output: github_document
#' ---
#'
#' Download the data yourself as described in the README
#' and run `preprocess_data.R`, which will remove some nulls,
#' transform & combine the data into more convenient form,
#' then save various `.Rdata`, `.geoJSON`, and `.csv` files for use
#' elsewhere.

library(tidyverse)
library(sf)
library(gganimate)
library(lubridate)
library(viridis)
library(extrafont)

#' ## Load data
#' Homicide and Offence Against a Person:
#' > Locations for reported incidents involving Offences Against a Person have
#' > been deliberately randomized to several blocks and offset to an
#' > intersection. No time or street location name will be provided for
#' > these offences.
if (file.exists("data/no_loc_crime.Rdata")) {
  load("data/no_loc_crime.Rdata")
} else {
  stop("Please run preprocess_data.R to create the required data")
}

#' All other crimes
if (file.exists("data/crime.Rdata")) {
  load("data/crime.Rdata")
} else {
  stop("Please run preprocess_data.R to create the required data")
}

#' Load the neighbourhood shapes
if (file.exists("data/neighbourhoods.Rdata")) {
  load("data/neighbourhoods.Rdata")
} else {
  stop("Please run preprocess_data.R to create the required data")
}

if (file.exists("data/population.Rdata")) {
  load("data/population.Rdata")
} else {
  stop("Please run preprocess_data.R to create the required data")
}

#' ## Themeing
default_theme <- theme_set(theme_bw())
theme_update(
  text = element_text(family="ubuntu"),
  title = element_text(family = "lato light", size = 16),
  plot.subtitle = element_text(family = "lato light"),
  legend.justification = "left",
  legend.position = "bottom")
# lato, lato light, mathjax_main, MathJax_Typewriter, Tex Gyre Bonum, Tlwg typist, Ubuntu, urw gothic


#' Plot city-wide crimes per 1000 people
city_population <- populations %>%
  group_by(year) %>%
  summarise(citypop = sum(population))
citywide_crime <- crime %>%
  group_by(year, type) %>%
  summarise(incidents = n()) %>%
  inner_join(city_population, by = "year") %>%
  mutate(per_1000 = incidents / citypop * 1000)
citywide_crime %>%
  filter(year < 2021) %>%
  ggplot(aes(year, per_1000, col = type, group = type)) +
  geom_line(size = 2) +
  labs(title = "Vancouver: Criminal Incidents",
       x = NULL,
       y = "Incidents per 1000 people",
       col = NULL)

#' Create yearly per-1000 crime figures
per_1000 <- crime %>%
  filter(year < 2021) %>%
  inner_join(populations, by = c("neighbourhood", "year")) %>%
  group_by(neighbourhood, year, type) %>%
  summarise(population = first(population),
            general_type = first(general_type),
            incidents = n()) %>%
  mutate(per_1000 = incidents / population * 1000)

per_1000 %>%
  filter(general_type == "Theft") %>%
  # filter(year > 2015) %>%
  ggplot(aes(year, per_1000, col = type, group = type)) +
  geom_line(size = 0.8) +
  facet_wrap(~neighbourhood, nrow = 4) +
  guides(col = guide_legend(override.aes = list(size = 3))) +
  labs(title = "Theft in Vancouver Neighbourhoods / 2003-2020",
       subtitle = "Per thousand population",
       x = NULL,
       y = "Incidents per 1000 people",
       col = NULL,
       caption = "Source: VPD Open Data Portal & Census Canada")

crime %>%
  # filter(dt < "2020-08-07") %>%
  group_by(day = date(dt)) %>%
  summarize(crimes = n()) %>%
  ggplot(aes(day, crimes, color = crimes)) +
  geom_line(size = 0.8) +
  geom_smooth(color = "blue3") +
  scale_color_viridis(option = "turbo", begin = 0.05, end = 0.8) +
  ylim(0, 800) +
  labs(x = NULL,
       title = "Daily crime in Vancouver") +
  annotate("text", x = date("2011-06-15"), y = 750, label = "Stanley Cup riot\nJune 15, 2011", vjust = 1,
           fontface = "bold") +
  annotate("text", x = date("2005-08-01"), y = 250, hjust = 0, vjust = 1,
           label = "2008 car-keying crime wave..?", fontface = 'bold') +
  theme(legend.position = "none")

#' The 2nd notable spike: a large jump in *Mischief* incidents?
crime %>%
  filter(date(dt) == "2008-03-12") %>%
  group_by(type) %>%
  summarize(crimes = n())

#' Let's map all the crimes over time. The big spikes in 2011 are the hockey
#' riot. The *mischief* spike in 2008 is mysterious.. but might be related
#' to a wave of cars
crime %>%
  filter(year >= 2018) %>%
  group_by(day = date(dt), type) %>%
  summarise(incidents = n()) %>%
  ggplot(aes(day, incidents, group = type, color = type)) +
  geom_line() +
  geom_smooth(se = FALSE, col = 'black', span =0.5) +
  facet_wrap(~type) +
  theme(legend.position = "none") +
  labs(title = "Vancouver daily crime since 2019")

crime %>%
  filter(year >= 2018,
         type %in% c("Other Theft", "Theft from Vehicle", "Theft of Bicycle", "Mischief"),
         neighbourhood %in% c("Downtown", "Strathcona")) %>%
  group_by(day = date(dt), type, neighbourhood) %>%
  summarise(incidents = n()) %>%
  ggplot(aes(day, incidents, group = type, color = type)) +
  geom_line() +
  geom_smooth(se = FALSE, span =0.5) +
  facet_wrap(~neighbourhood, nrow = 2) +
  # theme(legend.position = "none") +
  labs(title = "Vancouver daily crime since 2019")

crime %>%
  filter(year >= 2019, general_type == "Theft") %>%
  group_by(day = date(dt)) %>%
  summarise(incidents = n()) %>%
  ggplot(aes(day, incidents)) +
  geom_line() +
  geom_smooth(se = FALSE, col = 'black', span =0.5) +
  theme(legend.position = "none") +
  labs(title = "Vancouver daily crime since 2019")

#' #' Animation
#' p <- crime %>%
#'   filter(year == 2021) %>%
#'   ggplot(aes(lon, lat, color = general_type, group = interaction(hour, general_type, lex.order = FALSE))) +
#'   geom_sf(data = neighbourhoods, mapping = aes(), inherit.aes = FALSE) +
#'   geom_point() +
#'   transition_states(interaction(hour, general_type, lex.order = FALSE),
#'                     transition_length = 2, state_length = 2, wrap = TRUE) +
#'   # enter_fade() +
#'   # exit_fade() +
#'   ggtitle("{previous_state}")
#' FPS = 20;W = 640;H = 447;DTL = 3;S = paste0(W,"x",H);NFRAMES = 4*24*1*4 # minutebins*hours*days*framesperbin
#' animate(plot = p, fps = FPS, nframes = NFRAMES, width = W, height = H, detail = DTL)


#' ## Crimes without location information (removed from later analysis)
#' Over 68000 observations in the dataset
no_loc_crime %>%
  filter(type == "Homicide") %>%
  ggplot(aes(year)) +
  geom_bar() +
  labs(title = "Homicides in Vancouver")

p <- no_loc_crime %>%
  filter(type == "Offence Against a Person") %>%
  mutate(day = wday(dt, label = TRUE, abbr = TRUE, week_start = 1)) %>%
  group_by(year, month) %>%
  summarize(crimes = n()) %>%
  ggplot(aes(month, crimes, fill = month, group = month)) +
  geom_col() +
  ggtitle(label = "Offence Against a Person: What day of the week?   {closest_state}") +
  transition_states(states = year, transition_length = 3, state_length = 2) +
  ease_aes('cubic-in-out')
FPS = 18;W = 640;H = 447;DTL = 3;S = paste0(W,"x",H);NFRAMES = 19*15 # 19 years * frames/year
animate(plot = p, fps = FPS, nframes = NFRAMES, width = W, height = H, detail = DTL)


#' ## Total crime by year
crime %>%
  ggplot(aes(x=year)) +
  geom_bar()

#' ## Crime by weekday, 2020.
#' Pretty much identical, in totals...
crime %>%
  filter(year == 2020) %>%
  ggplot(aes(x=wday)) +
  geom_bar()

#' ## Proportional crime by weekday, showing crime type, 2020.
#' Pretty much identical!
crime %>%
  filter(year == 2020) %>%
  ggplot(aes(x=wday, fill=type)) +
  geom_bar()

#' ## Proportional crime by weekday and hour of crime, 2020.
#' Pretty much identical!
crime %>%
  filter(year == 2020) %>%
  ggplot(aes(x=wday, fill=factor(hour))) +
  geom_bar()

#' ##  crime by month, 2016-2020.
#' Note the shinkage of crime in the spring and summer months
#' of 2020: is this the Covid-19 effect with people staying home?
crime %>%
  filter(year >= 2016 & year < 2021) %>%
  ggplot(aes(x=year, fill=factor(month))) +
  geom_bar()

crime %>%
  filter(year >= 2016, year <= 2021) %>%
  filter(!(year == 2021 & month == 8)) %>%
  group_by(year, month) %>%
  summarise(crimes = n()) %>%
  ggplot(aes(month, crimes, color = as_factor(year), group = year)) +
  geom_line()

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
  facet_wrap(~general_type)

#' ## What times are most popular for *detailed* classes of crime, 2020?
#' *Other theft* peaks earlier (4pm) and *theft from Vehicle* peaks
#' later (7-9pm)
crime %>%
  filter(year == 2020) %>%
  ggplot(aes(hour)) +
  geom_bar() +
  facet_wrap(~type)

#' ## Have the *proportions* of *types* of crime changed over the years?
#' It appears theft was on the rise until 2020 when it dropped sharply.
crime %>%
  # filter(year < 2021) %>%
  ggplot(aes(desc(year), fill = general_type)) +
  geom_bar(position = 'fill', color='black', size=.5, width = .8) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(n.breaks = 15) +
  labs(title = "Proportions of crimes over time", y = NULL, x = NULL)

#' ## Same as above but with *raw counts* instead of proportions.
#' Here we can confirm that the drop in crime in 2020 seems to have been
#' driven by a decline in *theft*.
crime %>%
  # filter(year < 2021) %>%
  ggplot(aes(desc(year), fill = general_type)) +
  geom_bar(position = 'stack', color='black', size=.5, width = .8) +
  coord_flip() +
  scale_x_continuous(n.breaks = 15) +
  labs(title = "Number of crimes since 2003", y = NULL, x = NULL, fill = NULL)

#' ## Even more detail..
#' Looking closely we can see that the drop in *theft* in 2020 seems mostly to have been
#' driven by a decline in *thefts from vehicles* (with some help from *other theft*.)
crime %>%
  # filter(year < 2021) %>%
  ggplot(aes(desc(year), fill = type)) +
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
           type != "Vehicle Collision or Pedestrian Struck (with Fatality)") %>%
  group_by(year, type) %>%
  summarise(n = n()) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(factor(year, levels = c(2021, 2020, 2019)),
             pct,
             fill = type,
             label = paste(round(pct, 0), "%", sep=""))) +
  geom_col(color='black', size=.5, width = .8) +
  geom_text(position = position_stack(vjust = 0.5)) +
  coord_flip() +
  labs(y = "Percentage of that year's crime", x = "year") +
  theme(legend.position = 'bottom')

#' ## Have the TYPES of crime changed over the years?
#' With more detail on crimes, and with absolute counts by year.
#' We can see the effect of Covid-19
crime %>% ggplot(aes(year, fill = type)) +
  geom_bar(color='black', width = 0.75) +
  annotate("text", label = "Covid-19?", x = 2020, y = 50000) +
  annotate("curve", x = 2020, y = 48000, xend = 2020, yend = 34000,
           curvature = -.1, arrow = arrow(length = unit(.2, "cm"))) +
  theme_grey()

#' ## Some maps (WIP)
#' Draw the top crime by neighbourhood, and annotate with the percentage of
#' all crime in the neighbourhood that it makes up
#+ echo=FALSE
ggplot(data = neighbourhoods) +
  # geom_sf(aes(geometry = geometry, fill=crime_proportion), col='white') +
  geom_sf(aes(geometry = geometry), col='white') +
  # geom_sf_label(aes(label = round(p_top_crime * 100, 1)), cex = 3)  +
  # geom_sf_text(aes(label = name), cex = 3)
  # there is not a geom_sf_label_repel function, so we need to use
  # the basic one and add a few elements
  # https://www.johan-rosa.com/2019/11/13/creating-maps-with-sf-and-ggplot2/
  # ggrepel::geom_label_repel(
  #   aes(label = paste(name, ": ", round(p_top_crime * 100, 1), " %", sep = ''),
  #       geometry = geometry),
  #   stat = "sf_coordinates",
  #   min.segment.length = .1,   # line segment connecting label with center of polygon
  #   label.size = NA,          # remove border
  #   force = 3,
  #   alpha=.9
  # ) +
  labs(title = "Top crime by neighbourhood",
       subtitle = "And the percentage it is of that neighbourhood's crime",
       x = "longitude", y = 'latitude')

#' Draw the top NON-THEFT crime by neighbourhood, and annotate with the
#' percentage of all crime in the neighbourhood that it makes up
#+ echo=FALSE
neighbourhoods %>% ggplot() +
  # geom_sf(aes(geometry = geometry, fill=crime_proportion), col='white') +
  geom_sf(aes(geometry = geometry), col='white') +
  # geom_sf_text(aes(label=name), cex=3, alpha = .8) +
  # geom_sf_label(aes(label = paste(round(p_top_nontheft_crime, 2) * 100, "%", sep=""))) +
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

p <- crime %>%
  filter(year ==  2021) %>%
  ggplot(aes(lon, lat, col = general_type, group = type)) +
  geom_sf(data = neighbourhoods, mapping = aes(), inherit.aes = FALSE,
          fill = 'grey30', color = "grey50") +
  geom_point(size = 0.7) +
  transition_states(states = general_type, transition_length = 4, state_length = 6) +
  ggtitle("{closest_state}") +
  enter_grow() + exit_shrink()
FPS = 10;W = 800;H = 600;DTL = 1;S = paste0(W,"x",H);NFRAMES = 4*10
animate(plot = p, fps = FPS, nframes = NFRAMES, width = W, height = H, detail = DTL)


hourlabels = tibble(hour = 0:23, txt = paste(as.character(0:23), "00", sep = ":"))

p <- crime %>%
  filter(year >=  2019, type == "Other Theft") %>%
  ggplot(aes(lon, lat, group = hour)) +
  geom_sf(data = neighbourhoods, mapping = aes(), inherit.aes = FALSE) +
  geom_point(size = 0.7) +
  geom_text(data = hourlabels, mapping = aes(label = txt), inherit.aes = FALSE,
            x = -123.197, y = 49.305, size = 16, color = "grey70",
            hjust = 0) +
  # labs(title = "{closest_state}") +
  transition_states(states = hour, transition_length = 4, state_length = 6) +
  enter_grow() + exit_shrink()
FPS = 10;W = 800;H = 600;DTL = 1;S = paste0(W,"x",H);NFRAMES = 24*10
animate(plot = p, fps = FPS, nframes = NFRAMES, width = W, height = H, detail = DTL)

