
## TODO

-   do the neighbourhood names in the geojson file match the names in
    the crimes dataset? There are spelling errors, but are there
    different neighbourhoods listed too??
-   ubc\_report.pdf:
    -   Stanley Park isn’t a residential neighbourhood. UBC dropped
        those incidents. If I can find a shapefile to append I could map
        them too.
-   use the census data: crimes/population, crimes/livable area(?), etc
-   get some weather data and check correlations again with crime
    -   This could be really interesting after the crime data is updated
        after the heat wave…
-   There is one HOMICIDE appearing at Lat:0.00, Lon:-127.489 (in the
    ocean)
-   use NEIGHBOURHOOD in Tableau and join with the shape file to colour
    neighbourhoods by … most common type of crime? crime volume? crime
    per capita (residents), crime per capita( businesses)
-   explain why “Central Business District” (downtown) has so many more
    reported crimes than ANY other neighbourhood.
-   find a shape file or something that defines Vancouver neighborhoods
    which I can add to a Tableau map
-   TODO: figure out what kind of visual to plot higher-crime areas
    -   a DENSITY plot on the map? (“Density” of property crime? Theft?
        Vehicular?)
    -   create higher-level groups like “Theft” or “Vehicular” from the
        11 categories in the dset
    -   Aggregates, plotted by color(?) in neighbourhoods?
        -   in smaller sub-regions (zoning areas? some other city
            classification with GeoJSON?)
-   statistical analysis
    -   CHI2? Are crime types significantly different by neighbourhood?
        -   can this be done via lat/long, or must it be categorical…?
-   ML: A predictor of CALLS/future crimes, based on neighbourhood and
    time of day/month
-   data is supposed to update every Sunday
    -   you can filter by date and train only on old data, then predict
        the next week, next two weeks,…
