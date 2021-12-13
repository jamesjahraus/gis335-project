GIS 335 Final Project - GeoDa in Collaboration with R
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
    -   [Generate Final Shapefile](#generate-final-shapefile)
    -   [Explore Final Temperature Data
        GeoDa](#explore-final-temperature-data-geoda)
-   [Results](#results)
    -   [Interpolate a Temperature
        Surface](#interpolate-a-temperature-surface)
-   [Conclusion](#conclusion)
-   [References](#references)

<!-- Environment Setup -->

## Background

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

## Objective

Explore how GeoDa can be used in collaboration with R.

The exploration is guided by Kolak (2018) Array of Things (aot)
Workshop, and L4 from PennState GEOG 586.

### Scenario to Guide the Exploration

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

### Steps to Create the Temperature Surface

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

### Data

The data used is from the aot workshop and can be found:
<https://geodacenter.github.io/aot-workshop/>

The actual data used is from GIS 335 Lab 5 R interp, included with week
11 materials.

## Methods

### Generate Shapefile for All Sensors

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

### Explore Sensor Data in GeoDa

**Note:** the shapefile data is temperature, humidity, and pressure data
from Chicago 2018-08-25 between 12:00 and 13:00. This means that because
it is August in Chicago the temperature should be \~ 20 C and probably
not below 0 C or above 40 C.

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

#### Table Investigation

GeoDa has a table view to investigate the shapefile attributes.

![GeoDa Table View](Data/table_view.PNG)

| node_id      | t_tsys01 | t_htu21d | t_bmp180 |
|:-------------|:--------:|:--------:|:--------:|
| 001e06109416 |  21.01   |  21.55   |  40.92   |
| 001e0610fb4c |  20.91   |    NA    |  20.93   |
| 001e0611536c |    NA    |  22.33   |  43.51   |
| 001e0610e809 |  -4.21   |  20.44   |  21.09   |

Sensor Temperature Variation

**Notice** that for this sample some nodes have large differences in
temperature values between sensors, and some nodes have no data for a
particular sensor. To fix this we could use the temperature data from
tsys01 first then if there is a problem, i.e. an outlier or NA then we
could use data from the other sensors from the same node. Also some
values are below 0 C and above 40 C.

#### GeoDa Box Plots

![GeoDa Table View](Data/box_plots_all.PNG)

**Notice** that each sensor has outliers, and some sensors have large
variation between temperature readings.

#### GeoDa Scatter Plots

To answer the question **Why does tsys01 sometimes produce incorrect
data?** we could look for correlation between various sensor data using
scatter plots.

**Compare temp_tsys01 to Each Sensor**

| t_htu21d                                                      |
|:--------------------------------------------------------------|
| <img src="Data/sp1.PNG" id="id" class="class" height="400" /> |
| temp_tsys01 vs. temp_htu21d                                   |

| h_htu21d                                                      |
|:--------------------------------------------------------------|
| <img src="Data/sp2.PNG" id="id" class="class" height="400" /> |
| temp_tsys01 vs. humid_htu21d                                  |

| t_bmp180                                                      |
|:--------------------------------------------------------------|
| <img src="Data/sp3.PNG" id="id" class="class" height="400" /> |
| temp_tsys01 vs. temp_bmp180                                   |

| p_bmp180                                                      |
|:--------------------------------------------------------------|
| <img src="Data/sp4.PNG" id="id" class="class" height="400" /> |
| temp_tsys01 vs. press_bmp180                                  |

### Generate Final Shapefile

There appears to be no correlation between the other temperature sensors
on the same node for extreme values. Also there appears to be no
correlation between pressure or humidity.

**Check Nodes at Various Hours**

| node_id        | Hour7 | Hour12 | Hour22 |
|:---------------|:-----:|:------:|:------:|
| 001e0610e809   | -7.6  |  -4.2  |  18.6  |
| Mean All Nodes | 19.3  |  20.3  |  30.8  |
| 001e0611536c   |  NA   |   NA   |   NA   |

Outlier Nodes Various Hours

It appears that node **001e0610e809** is not working correctly, and node
**001e0611536c** has no tsys01 sensor.

In the future we could replace faulty nodes, add tsys01 sensors, or
create an algorithm to use temperature data from the other sensors.

For the final shapefile we will remove the outlier nodes and use only
the tsys01 sensor data for the temperature surface.

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

# Remove outlier nodes 001e0610e809, and 001e0611536c
temp_tsys01 <- subset(temp_tsys01, node_id != "001e0610e809")
temp_tsys01 <- subset(temp_tsys01, node_id != "001e0611536c")

# Join Sensor Data by Nodes
node_data <- merge(temp_tsys01, nodes, by = c("node_id"))

# Convert node data to spatial object
coordinates(node_data) <- node_data[, c("lon", "lat")]

# set data to the same projection proj4string(node.temps)
# <- CRS('+init=epsg:4326') Error in CRS('+init=epsg:4326')
# : NA
# https://gis.stackexchange.com/questions/387072/r-spcrs-returns-na
proj4string(node_data) <- CRS(sf::st_crs(4326)[[2]])

# writeOGR(node_data, '.', 'aot_final', driver = 'ESRI
# Shapefile')
```

### Explore Final Temperature Data GeoDa

#### Investigate Outliers and Patterns

[GeoDa Cheat Sheet](http://geodacenter.github.io/cheatsheet.html)

| Histogram                                                    |
|:-------------------------------------------------------------|
| <img src="Data/o1.PNG" id="id" class="class" height="400" /> |
| Histogram showing distribution.                              |

| Box_Map                                                      |
|:-------------------------------------------------------------|
| <img src="Data/o2.PNG" id="id" class="class" height="400" /> |
| Spatial histogram.                                           |

| Bubble_Chart                                                 |
|:-------------------------------------------------------------|
| <img src="Data/p1.PNG" id="id" class="class" height="400" /> |
| Visualize temperature groups.                                |

| Scatter_3D                                                   |
|:-------------------------------------------------------------|
| <img src="Data/p2.PNG" id="id" class="class" height="400" /> |
| Visualize temperature groups in 3D, x: lon, y: lat, z: temp  |

#### Investigate Spatial Autocorrelation

[L4: Spatial
Autocorrelation](https://www.e-education.psu.edu/geog586/node/524)

[Weights File
Creation](https://www.e-education.psu.edu/geog586/node/671)

**Global Moran’s I**

![Global Moran’s I](Data/sa1.PNG)

**From L4 Spatial Autocorrelation:**

-   Cases in the upper-right quadrant indicate nodes temps, and local
    average node temps are higher than overall average node temps.

-   Cases in the lower-left quadrand indicate node temps, and local
    average node temps are lower than overall average node temps.

*Indicates positive spatial autocorrelation for the tsys01 data.*

**Local Indicators of Spatial Association - LISA**

![Univariate Local Moran’s I](Data/sa2.PNG)

**Spatial Correlogram**

[Spatial Correlogram - GeoDa
Workbook](https://geodacenter.github.io/workbook/5a_global_auto/lab5a.html#spatial-correlogram)

> *A measure of global spatial autocorrelation… a local regression is
> fit to the* *covariances or correlations computed for all pairs of
> observations as a function* *of the distance between them.*

![Spatial Correlogram](Data/sa3.PNG)

*Depicts how the spatial autocorrelation changes with distance.*

#### Thiessen Polygons

*Early Estimate of Temperature Surface*

![Thiessen Polygons - Natural Breaks](Data/thiessen_polygons.PNG)

## Results

### Interpolate a Temperature Surface

![](gis335_final_JamesJahraus_files/figure-gfm/krig-1.png)<!-- -->![](gis335_final_JamesJahraus_files/figure-gfm/krig-2.png)<!-- -->

## Conclusion

## References

-   Johnson, Crystal, “Using Kriging, Cokriging, and GIS to Visualize Fe
    and Mn in Groundwater” (2015). Electronic Theses and Dissertations.
    Paper 2498. <https://dc.etsu.edu/etd/2498> ,
    <https://dc.etsu.edu/cgi/viewcontent.cgi?article=3881&context=etd>

-   MAPPING AOT DATA WITH SPATIAL STATISTICS, ARRAY OF THINGS WORKSHOP
    2018 FACILITATED BY M. KOLAK
    <https://geodacenter.github.io/aot-workshop/>

-   PennState GEOG 586 - Geographic Information Analysis L4: Cluster
    Analysis: Spatial Autocorrelation
    <https://www.e-education.psu.edu/geog586/node/524>
