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
# br <- ee$Geometry$Polygon(
#   c(c(c(-23.37890625, -9.44906182688142),
#       c(-74.00390625, 17.97873309555617),
#       c(-92.4609375, -24.5271348225978),
#       c(-29.00390625, -37.02009820136811)))
# )
# 
# # Union the two polygons
# rect <- br

# Load MODIS - Terra Net Evapotranspiration 8-Day Global 500m ----
et <- ee$ImageCollection("MODIS/006/MOD16A2")$
  select('ET') # kg/m^2/8day 	

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
    toDouble()
}

# Run function for each month of the year
imgs <- seq(2005, 2020) %>% purrr::map(map_y)
ETyear <- ee$ImageCollection(imgs)

# Check
# ee_print(ETyear)
# ETyear$first()$get('year')$getInfo()
# Map$setCenter(-50.80, -10.37, 4)
# viz <- list(min= 300, max= 2000, palette = c('#F0FFF0','#87CEFA', '#0000CD'))
# Map$addLayer(ETyear$filterMetadata('year', 'equals', 2021)$first(), viz, 'ET 2021') +
#   Map$addLayer(ETyear$filterMetadata('year', 'equals', 2020)$first(), viz, 'ET 2020') +
#   Map$addLegend(viz)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Model the relationship between ET and FC by location ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
