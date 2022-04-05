#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Create monthly raster of ET, taking into account the entire time series (ran for tropics manuscript)
# Requires:
#     * GEE account with access to...
# Author:
#     * Adapted from Divino dvsilverio@gmail.com: code.earthengine.google.com/ecb80a929df35e29edd9390e239826bb
#     * esturdivant@woodwellclimate.org, 2022-04-04
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(colorspace)
library(rgee)
ee_Initialize()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get yearly mean of ET (adapted from Greg's yearly mean of surface temp) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Brazil
br <- ee$Geometry$Polygon(
  c(c(c(-23.37890625, -9.44906182688142),
      c(-74.00390625, 17.97873309555617),
      c(-92.4609375, -24.5271348225978),
      c(-29.00390625, -37.02009820136811)))
)

# Union the two polygons
rect <- br

# Load MODIS - Terra Net Evapotranspiration 8-Day Global 500m ----
et <- ee$ImageCollection("MODIS/006/MOD16A2")$
  filterBounds(br)$
  # filter(ee$Filter$calendarRange(2000,2017,'year'))$
  select('ET') # kg/m^2/8day 	

ee_print(et)

# Get yearly averages, first averaging by month ----
# Group by year and month, and then reduce within groups by mean().
# The result is an ImageCollection with one image for each
# year.

map_y <- function(j) {
  
  map_m <- function(i, j) {
    et$
      filter(ee$Filter$calendarRange(j, j, 'year'))$
      filter(ee$Filter$calendarRange(i, i, 'month'))$
      select(0)$ # Select band 0
      mean()$
      multiply(0.1)$
      set("month", i)$
      set("year", j)
  }
  
  # Run for each month - result is list of single-band images
  imgs <- seq(1, 12) %>% purrr::map(map_m, j = j)
  
  # Get annual average. Convert from 8-day total to daily to yearly total
  ee$ImageCollection(imgs)$
    mean()$
    divide(8)$multiply(365)$ # Convert from 8-day to daily to yearly total
    set('year', j)$
    toDouble()$
    clip(br)
}

# Run function for each month of the year
imgs <- seq(2001, 2017) %>% purrr::map(map_y)
ETyear <- ee$ImageCollection(imgs)

# Check
ee_print(ETyear)
ETyear$first()$get('year')$getInfo()

Map$setCenter(-50.80, -10.37, 4)
viz <- list(min= 300, max= 2000, palette = c('#F0FFF0','#87CEFA', '#0000CD'))
Map$addLayer(ETyear$filterMetadata('year', 'equals', 2004)$first(), viz, 'ET') +
  Map$addLayer(ETyear$filterMetadata('year', 'equals', 2005)$first(), viz, 'ET') +
  Map$addLayer(ETyear$filterMetadata('year', 'equals', 2006)$first(), viz, 'ET') +
  Map$addLegend(viz)

# *** E N D ? ***-----



# Load MODIS collection, select ET and date range ----
# Filter ET by year
et <- ee$ImageCollection('MODIS/NTSG/MOD16A2/105')$
  filter(ee$Filter$calendarRange(2000,2020,'year'))$
  select('ET')

# ee_print(et$first(), 'first image') # ee$List of band names
et$size()$getInfo()

# List months
mes <- ee$List$sequence(1, 12)
mes$getInfo()

# Get average ET for each month ----
# Function to get average ET for a month
filt_et_to_yr_mn <- function(m){
  et$filter(ee$Filter$calendarRange(m, m,'month'))$
    mean()$
    set('month', m)$
    toDouble()$
    multiply(0.1)
}

# Run function for each month of the year
# etmes <- ee$ImageCollection$fromImages(mes$map(filt_et_to_yr_mn()))
imgs <- seq(1, 12) %>% purrr::map(filt_et_to_yr_mn)
etmes <- ee$ImageCollection$fromImages(imgs)

# # Check
# ee_print(etmes)
# library(colorspace)
# pal_idx <- sequential_hcl(n = 10, 'inferno')
# vizParams <- list(min = 0, max = 30, palette = pal_idx)
# Map$addLayer(etmes$first(), vizParams, 'test') +
#   Map$addLegend(vizParams)

# Mosaic ----
mosaic <- etmes$mosaic()
mosaic$projection()$nominalScale()$getInfo()
Map$addLayer(mosaic, vizParams, 'test') +
  Map$addLegend(vizParams)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get mean for polygons ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load polygons
tropic <- ee$FeatureCollection("users/esturdivant/HIH_sites/hih_sites_polys")
# Map$setCenter(-50, -15, 4) #long, lat, zoom
# Map$addLayer(tropic,list(),'tropic')

fxn <- function(image) {
  image$reduceRegions(collection = tropic, 
                      reducer = ee$Reducer$mean(), 
                      scale = 500)$
    filter(ee$Filter$neq('mean', NULL))$
    map(function(f) f$set('imageId', image$id()))
}

etm <- etmes$map(fxn)$flatten()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert to GeoJSON ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
task_name <- 'tbl_ET'

task_vector <- etm %>% 
  ee_table_to_drive(description = task_name,
                    folder = 'Earth Engine Exports',
                    fileFormat = 'GeoJSON', 
                    timePrefix = FALSE)
task_vector$start()

# Download (manually) and load table ----
# tbl <- sf::st_read(here::here('~/Downloads/tbl_ET.geojson'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get yearly mean of ET (adapted from Greg's yearly mean of surface temp) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Brazil
br <- ee$Geometry$Polygon(
  c(c(c(-23.37890625, -9.44906182688142),
      c(-74.00390625, 17.97873309555617),
      c(-92.4609375, -24.5271348225978),
      c(-29.00390625, -37.02009820136811)))
)

# Union the two polygons
rect <- br

# Load MODIS - Terra Net Evapotranspiration 8-Day Global 500m ----
et <- ee$ImageCollection("MODIS/006/MOD16A2")$
  filterBounds(br)$
  # filter(ee$Filter$calendarRange(2000,2017,'year'))$
  select('ET') # kg/m^2/8day 	

ee_print(et)

# Get yearly averages, first averaging by month ----
# Group by year and month, and then reduce within groups by mean().
# The result is an ImageCollection with one image for each
# year.

map_y <- function(j) {
  
  map_m <- function(i, j) {
    et$
      filter(ee$Filter$calendarRange(j, j, 'year'))$
      filter(ee$Filter$calendarRange(i, i, 'month'))$
      select(0)$ # Select band 0
      mean()$
      multiply(0.1)$
      set("month", i)$
      set("year", j)
  }
  
  # Run for each month - result is list of single-band images
  imgs <- seq(1, 12) %>% purrr::map(map_m, j = j)
  
  # Get annual average. Convert from 8-day total to daily to yearly total
  ee$ImageCollection(imgs)$
    mean()$
    divide(8)$multiply(365)$ # Convert from 8-day to daily to yearly total
    set('year', j)$
    toDouble()$
    clip(br)
}

# Run function for each month of the year
imgs <- seq(2001, 2017) %>% purrr::map(map_y)
ETyear <- ee$ImageCollection(imgs)

# Check
ee_print(ETyear)
ETyear$first()$get('year')$getInfo()

Map$setCenter(-50.80, -10.37, 4)
viz <- list(min= 300, max= 2000, palette = c('#F0FFF0','#87CEFA', '#0000CD'))
Map$addLayer(ETyear$filterMetadata('year', 'equals', 2004)$first(), viz, 'ET') +
  Map$addLayer(ETyear$filterMetadata('year', 'equals', 2005)$first(), viz, 'ET') +
  Map$addLayer(ETyear$filterMetadata('year', 'equals', 2006)$first(), viz, 'ET') +
  Map$addLegend(viz)

# *** E N D ? ***-----
# Get monthly averages by year ----
# Group by year and month, and then reduce within groups by mean().
# The result is an ImageCollection with one image for each
# year and one band for each month.
map_m <- function(i, j) {
  et$
    filter(ee$Filter$calendarRange(j, j, 'year'))$
    filter(ee$Filter$calendarRange(i, i, 'month'))$
    select(0)$ # Select band 0
    mean()$
    multiply(0.1)$
    set("month", i)$
    set("year", j)
}

img <- map_m(1, 2001)
ee_print(img)

map_y <- function(j) {
  # Get monthly averages for given year

  # Run for every month
  imgs <- seq(1, 12) %>% purrr::map(map_m, j = j)
  
  ee$ImageCollection(imgs)$
    mean()$set('year', j)$
    divide(8)$multiply(365)$
    set('year', j)$
    toDouble()$
    clip(br)
}

ic <- map_y(2001)
ee_print(ic)

# Run function for each month of the year
imgs <- seq(2001, 2017) %>% purrr::map(map_y)
et_years <- ee$ImageCollection(imgs)
# Check
ee_print(et_years)

# Get monthly averages by year ----
# Group by year and month, and then reduce within groups by mean().
# The result is an ImageCollection with one image for each
# month and one band for each year.
map_m <- function(i) {
  # Get monthly average by year for given month

  map_y <- function(j, i) {
    et$
      filter(ee$Filter$calendarRange(j, j, 'year'))$
      filter(ee$Filter$calendarRange(i, i, 'month'))$
      select(0)$
      mean()$
      multiply(0.1)$
      set("month", i)$
      set("year", j)
  }

  # Run for every year
  seq(2001, 2017) %>% purrr::map(map_y, i = i)
}

# Run function for each month of the year
imgs <- seq(1, 12) %>% purrr::map(map_m)
et_months <- ee$ImageCollection(imgs)
# Check
ee_print(et_months)

# Check
pal_idx <- sequential_hcl(n = 10, 'inferno')
vizParams <- list(min = 0, max = 30, palette = pal_idx)
Map$addLayer(et_months$first()$select('ET'), vizParams, 'ET') +
  Map$addLayer(et_months$first()$select('ET_1'), vizParams, 'ET_1') +
  Map$addLayer(et_months$first()$select('ET_2'), vizParams, 'ET_2') +
  Map$addLegend(vizParams)


# GEE map ----
# Group by year, and then reduce within groups by mean();
# the result is an ImageCollection with one image for each
# year.
# map_m <- function(i) {
#   
#   i <- ee$Number(i)
#   
#   years$map(function(j) {
#     et$
#       filter(ee$Filter$calendarRange(j, j, 'year'))$
#       filter(ee$Filter$calendarRange(i, i, 'month'))$
#       select(0)$
#       mean()$
#       multiply(0.1)$
#       set("month",i)$
#       set("year",j)
#   })
# }
# 
# # Run function for each month of the year
# et_months <- ee$ImageCollection(months$map(map_m)$flatten())
# ee_print(et_months, "ET mensal")

# et2001 <- et_months$filterMetadata('year','equals',2001)
# ee_print(et2001)

# Get annual means ----
fxn <- function(y) {
  et_months$
    filterMetadata('year','equals', y)$
    mean()$
    divide(8)$multiply(365)$
    set('year', y)$
    toDouble()$
    clip(br)
}

ee_print(et_years$filterMetadata('year','equals', 2001))

yrly <- seq(2001, 2017) %>% purrr::map(fxn)
ETyear <- ee$ImageCollection$fromImages(yrly)
ee_print(ETyear)

fxn <- function(y) {
  et_years$
    filterMetadata('year','equals', y)$
    mean()$
    divide(8)$multiply(365)$
    set('year', y)$
    toDouble()$
    clip(br)
}

ee_print(et_years$filterMetadata('year','equals', 2005))
yrly <- seq(2001, 2017) %>% purrr::map(fxn)
ETyear <- ee$ImageCollection$fromImages(yrly)
ee_print(ETyear)

Map$setCenter(-50.80, -10.37, 4)
Map$addLayer(ETyear$first(), 
             list(min= 300, max= 2000, 
                  palette = c('F0FFF0','87CEFA', '0000CD')), 'ET')

# Download the image mosaic
# Export$image(ETyear,
#              "ET_MOD16A2v006_brasil_2001_2017",
#              {'scale':500,'maxPixels':1e13,'crs':"EPSG:4326",'region': br, 'driveFolder': 'GEE Exports'}
#              )
