GIS 335 Final Project
================
James Jahraus
2021-12-13

-   [Background](#background)
-   [Objective](#objective)
    -   [Scenario to Guide the
        Exploration](#scenario-to-guide-the-exploration)
    -   [Steps to Create the Temperature
        Surface](#steps-to-create-the-temperature-surface)
    -   [Data](#data)
-   [Methods](#methods)
    -   [Generate Shapefile for All
        Sensors](#generate-shapefile-for-all-sensors)
    -   [Explore Sensor Data in GeoDa](#explore-sensor-data-in-geoda)
        -   [Table Investigation](#table-investigation)
-   [Results](#results)
-   [Discussion](#discussion)
-   [Conclusion](#conclusion)
-   [References](#references)

<!-- Environment Setup -->

# Background

Initially the idea was to compare GeoDa to R or ArcGIS. After
investigating I found that the purpose of GeoDa is exploratory spatial
data analysis. This is supported in Johnson (2015) thesis:

> *“In this study, the spatial autocorrelation exploration tool in GeoDa
> was used* *to first determine the presence or absence of any spatial
> relationship in the* *dataset. This provided the logical background
> for further analysis in ArcGIS.”*

Also L4: Cluster Analysis: Spatial Autocorrelation from PennState GEOG
586 states:

> *“This week’s project uses not a GIS program, but a package for
> exploratory* *spatial data analysis called GeoDa. GeoDa is a good
> example of research software.”*

It is not appropriate to compare GeoDa vs. R because they are
complementary tools.

# Objective

Explore how GeoDa can be used in collaboration with R.

The exploration is guided by Kolak (2018) Array of Things (aot)
Workshop, and L4 from PennState GEOG 586.

## Scenario to Guide the Exploration

A local Chicago company is developing a temperature web service. The
goal is to provide a temperature map of Chicago every hour with
temperature data at every location across the city. The website will use
this data to send users the temperature given their lat lon coordinates.

To create the temperature surface the company has a network of sensors.
The aot data set represents a network of sensors throughout Chicago that
collect temperature, humidity, and pressure.

-   Sensor **tsys01** collected only temp data in C [TSYS01 - High
    Accuracy Temperature
    Sensor](https://www.te.com/commerce/DocumentDelivery/DDEController?Action=showdoc&DocId=Data+Sheet%7FTSYS01%7FA%7Fpdf%7FEnglish%7FENG_DS_TSYS01_A.pdf%7FG-NICO-018)

-   Sensor **htu21d** collected temp in C and humidity in RH data
    [HTU21D-F Temperature + Humidity
    Sensor](https://www.adafruit.com/product/1899)

-   Sensor **bmp180** collected temp in C and pressure in hPa [BMP180
    Barometric Pressure/Temperature/Altitude
    Sensor](https://www.adafruit.com/product/1603)

This data is point data, so making the temperature surface requires
interpolation of the point data to the temperature surface. The
deliverable for the web service is an accurate temperature surface
delivered every hour.

## Steps to Create the Temperature Surface

> -   **R** generate a shapefile containing all the sensor data for a
>     one hour range.

> -   **GeoDa** explore the data to determine suitable temperature data
>     for final surface.

> -   **R** generate the final shapefile containing final average
>     temperature for a one hour range.

> -   **GeoDa** explore the temperature data and determine spatial
>     autocorrelation of temperature data.

> -   **R** validate GeoDa spatial autocorrelation with a variogram
>     in R.

> -   **R** use Kriging to interpolate temperature surface raster of
>     Chicago.

After this proof of concept the team will investigate automating the
creation of the interpolated temperature surface every hour.

-   Integration of sensor data delivery to R service.

-   Create R service workers: shapefile generation, data scrubber,
    variogram metadata, and temperature surface generator.

-   Create web service to deliver temperature surface to clients.

## Data

The data used is from the aot workshop and can be found:
<https://geodacenter.github.io/aot-workshop/>

The actual data used is from GIS 335 Lab 5 R interp, included with week
11 materials.

# Methods

## Generate Shapefile for All Sensors

``` r
# Load Packages
library(tidyverse)
library(lubridate)
library(sp)
library(rgdal)

# Clear Workspace
rm(list = ls())

# Read Data
sensor_data <- read.csv("Data/data.csv.gz")
nodes <- read.csv("Data/nodes.csv")

# Fix timestamp so GeoDa doesn't crash on import
sensor_data$timestamp <- ymd_hms(sensor_data$timestamp)
nodes$start_timestamp <- ymd_hms(nodes$start_timestamp)
nodes$end_timestamp <- ymd_hms(nodes$end_timestamp)

# Filter Sensors by Sensor for hour
fhour <- 12  # filter hour

# tsys01 temperature C
temp_tsys01 <- sensor_data %>%
    filter(sensor == "tsys01") %>%
    filter(parameter == "temperature") %>%
    filter(hour(timestamp) >= fhour) %>%
    filter(fhour >= hour(timestamp)) %>%
    group_by(node_id) %>%
    summarize(t_tsys01 = mean(value_hrf))

# htu21d temperature C
temp_htu21d <- sensor_data %>%
    filter(sensor == "htu21d") %>%
    filter(parameter == "temperature") %>%
    filter(hour(timestamp) >= fhour) %>%
    filter(fhour >= hour(timestamp)) %>%
    group_by(node_id) %>%
    summarize(t_htu21d = mean(value_hrf))

# htu21d humidity RH
humid_htu21d <- sensor_data %>%
    filter(sensor == "htu21d") %>%
    filter(parameter == "humidity") %>%
    filter(hour(timestamp) >= fhour) %>%
    filter(fhour >= hour(timestamp)) %>%
    group_by(node_id) %>%
    summarize(h_htu21d = mean(value_hrf))

# bmp180 temperature C
temp_bmp180 <- sensor_data %>%
    filter(sensor == "bmp180") %>%
    filter(parameter == "temperature") %>%
    filter(hour(timestamp) >= fhour) %>%
    filter(fhour >= hour(timestamp)) %>%
    group_by(node_id) %>%
    summarize(t_bmp180 = mean(value_hrf))

# bmp180 pressure hPa
press_bmp180 <- sensor_data %>%
    filter(sensor == "bmp180") %>%
    filter(parameter == "pressure") %>%
    filter(hour(timestamp) >= fhour) %>%
    filter(fhour >= hour(timestamp)) %>%
    group_by(node_id) %>%
    summarize(p_bmp180 = mean(value_hrf))

# Join Sensor Data by Nodes
j1 <- merge(temp_tsys01, temp_htu21d, by = c("node_id"), all = TRUE)
j2 <- merge(j1, temp_bmp180, by = c("node_id"), all = TRUE)
j3 <- merge(j2, press_bmp180, by = c("node_id"), all = TRUE)
j4 <- merge(j3, humid_htu21d, by = c("node_id"), all = TRUE)
node_data <- merge(j4, nodes, by = c("node_id"))

# Convert node data to spatial object
coordinates(node_data) <- node_data[, c("lon", "lat")]

# set data to the same projection proj4string(node.temps)
# <- CRS('+init=epsg:4326') Error in CRS('+init=epsg:4326')
# : NA
# https://gis.stackexchange.com/questions/387072/r-spcrs-returns-na
proj4string(node_data) <- CRS(sf::st_crs(4326)[[2]])

# writeOGR(node_data, '.', 'aot_all_sensors', driver =
# 'ESRI Shapefile')
```

## Explore Sensor Data in GeoDa

Each node has a tsys01 (temperature only), htu21d (temperature and
humidity), and bmp180 (temperature and pressure) sensor. We are only
interested in temperature, yet investigating the data for each sensor
could provide some insight into what data to use for the final
temperature data set.

The tsys01 sensor is a **High Accuracy Temperature Sensor**, so its data
considered as first priority for the node’s final reading. Sometimes the
tsys01 provides readings that do not make sense, and for other nodes
there is no data for the tsys01 sensor. We need to use GeoDa to explore
the variables so the best decision can be made about the final
temperature data.

### Table Investigation

GeoDa has table view to investigate the shapefile attributes.

![GeoDa Table View](Data/table_view.PNG)

| node_id      | t_sys01 | t_ht21d | t_bm180 |
|:-------------|:-------:|:-------:|:-------:|
| 001e06109416 |  21.01  |  21.55  |  40.92  |
| 001e0610fb4c |  20.91  |   NA    |  20.93  |
| 001e0611536c |   NA    |  22.33  |  43.51  |
| 001e0610e809 |  -4.21  |  20.44  |  21.09  |

Sensor Temperature Variation

For this sample bm180 sensor can have higher vl.

# Results

# Discussion

# Conclusion

# References

-   Johnson, Crystal, “Using Kriging, Cokriging, and GIS to Visualize Fe
    and Mn in Groundwater” (2015). Electronic Theses and Dissertations.
    Paper 2498. <https://dc.etsu.edu/etd/2498> ,
    <https://dc.etsu.edu/cgi/viewcontent.cgi?article=3881&context=etd>

-   PennState GEOG 586 - Geographic Information Analysis L4: Cluster
    Analysis: Spatial Autocorrelation
    <https://www.e-education.psu.edu/geog586/node/524>

-   MAPPING AOT DATA WITH SPATIAL STATISTICS, ARRAY OF THINGS WORKSHOP
    2018 FACILITATED BY M. KOLAK
    <https://geodacenter.github.io/aot-workshop/>
