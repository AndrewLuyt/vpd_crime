Crime in Vancouver: (work in progress)
================
Andrew Luyt
24/06/2021

## Introduction

An exploration of the Vancouver Police Department’s public crime
database. This project uses the R language.

## Data Sources

The VPD data set is freely available for download at [The VPD Open Data
Website](https://geodash.vpd.ca/opendata/). Save the zip file and
extract it into a directory called `data`. Ensure the data is named
`crimedata_csv_all_years.csv`

Vancouver neighbourhood boundaries (map data) is available on the
[Vancouver Open Data
Portal](https://opendata.vancouver.ca/explore/dataset/local-area-boundary/export/).
This project uses the GeoJSON format.

Census data is also available on the Vancouver Open Data Portal. This
project uses the CSV format.

-   [2016
    .CSV](https://opendata.vancouver.ca/explore/dataset/census-local-area-profiles-2016/information/)
-   [2011
    .CSV](https://opendata.vancouver.ca/explore/dataset/census-local-area-profiles-2011/information/)
-   [2006
    .CSV](https://opendata.vancouver.ca/explore/dataset/census-local-area-profiles-2006/information/)

## Processing the Raw Data

[In this section](./preprocess_data.md) we process the VPD crime
dataset, Vancouver map data, and census information and export a number
of files we can use in other contexts, like Tableau or natively in R.
