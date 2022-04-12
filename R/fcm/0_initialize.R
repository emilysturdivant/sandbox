#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Initialize environment for FCM project in RGEE
# Requires:
#     * GEE account
# Author:
#     * esturdivant@woodwellclimate.org, 2021-11-20
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load libraries 
library(rgee)
ee_Initialize()
library(sf)
library(colorspace)
library(countrycode)
library(terra)
library(tmap)
library(tidyverse)

# Initialize variables ----
# Local paths
data_dir <- '~/data'
export_path <- '/Volumes/GoogleDrive/My Drive/Earth Engine Exports'
export_path <- '~/Downloads'

# Visualization parameters
Map$setCenter(30, 0, zoom = 3)
pal_idx <- sequential_hcl(n = 10, 'inferno')
viz_idx_norm <- list(min = 0, max = 1, palette = pal_idx, 
                     values = str_c(seq(0, 90, 10), seq(10, 100, 10), sep = '-'))

# Create tropics extent rectangle ----
tropics_bb <- ee$Geometry$Rectangle(
  coords = c(-180, -35, 180, 35),
  proj = "EPSG:4326",
  geodesic = FALSE
)

# Create world extent rectangle ----
world_bb <- ee$Geometry$Rectangle(
  coords = c(-180, -90, 180, 90),
  proj = "EPSG:4326",
  geodesic = FALSE
)

# Get AGB mask ----
c_mask <- ee$Image("users/sgorelik/WHRC_Global_Biomass_500m_V6/Current_AGB_Mgha")$gt(0)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Functions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Prep to get paths to assets
user <- ee_get_assethome()
addm <- function(x) sprintf("%s/%s", user, x)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Functions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get skewness for a single-band image (1km res, global)
get_skewness <- function(img) {
  img$
    rename('b1')$
    reduceRegion(
      reducer = ee$Reducer$skew(),
      geometry = world_bb,
      scale = 1000,
      maxPixels = 803003482,
      bestEffort = FALSE)$
    getInfo()
}

# Get quantiles
get_quantiles <- function(img, percentiles = c(0, 25, 50, 75, 100)) {
  
  quants <- img$
    rename('b1')$
    reduceRegion(
      reducer = ee$Reducer$percentile(percentiles),
      geometry = tropics_bb,
      bestEffort = TRUE)$
    getInfo()
  
  quants %>% setNames( str_remove_all(names(quants), 'b1_') )
}

# Get certain percentile
get_pctl <- function(img, pctl = 99) {
  
  pctl_list <- c(0, 99)
  if (!pctl %in% pctl_list) pctl_list <- sort(c(pctl_list, pctl))
  
  p <- img$
    rename('b1')$
    reduceRegion(
      reducer = ee$Reducer$percentile(pctl_list),
      geometry = tropics_bb,
      bestEffort = TRUE)$
    get(str_c('b1_p', pctl))$
    getInfo()
  
  return( p )
}

# Rescale to certain percentile
rescale_to_pctl <- function(img, pctls = c(0, 99)) {
  
  # get parameters
  minp <- pctls[[1]]
  maxp <- pctls[[2]]
  
  # get percentiles with reduceRegion
  p <- img$
    rename('b1')$
    reduceRegion(
      reducer = ee$Reducer$percentile(pctls),
      geometry = tropics_bb,
      scale = 1000,
      bestEffort = TRUE)
  
  # request the values in R numeric
  min <- p$get(str_c('b1_p', minp))$getInfo()
  max <- p$get(str_c('b1_p', maxp))$getInfo()
  
  # Rescale so that the min is 0 and the max is 1
  img1 <- img$unitScale(min, max)
  
  # Reclass values outside of 0-1 range
  img <- img1$
    where(img1$gt(1.0), 1.0)$
    where(img1$lt(0.0), 0.0)
  
  return( img )
}
# 
# # Classify image to 10 equal-area ranked classes
# classify_dentiles <- function(img) {
#   qs <- get_quantiles(img, c(10, 20, 30, 40, 50, 60, 70, 80, 90))
#   
#   ee$Image(0)$
#     where(img$lt(qs$p10), 0)$
#     where(img$gte(qs$p10), .10)$
#     where(img$gte(qs$p20), .20)$
#     where(img$gte(qs$p30), .30)$
#     where(img$gte(qs$p40), .40)$
#     where(img$gte(qs$p50), .50)$
#     where(img$gte(qs$p60), .60)$
#     where(img$gte(qs$p70), .70)$
#     where(img$gte(qs$p80), .80)$
#     where(img$gte(qs$p90), .90)$
#     updateMask(img$mask())
# }

# Classify image to 20 equal-area ranked classes
classify_ventiles <- function(img) {
  qs <- get_quantiles(img, seq(5, 95, 5))
  
  ee$Image(0)$
    where(img$lt(qs$p5), .05)$
    where(img$gte(qs$p5), .1)$
    where(img$gte(qs$p10), .15)$
    where(img$gte(qs$p15), .2)$
    where(img$gte(qs$p20), .25)$
    where(img$gte(qs$p25), .3)$
    where(img$gte(qs$p30), .35)$
    where(img$gte(qs$p35), .4)$
    where(img$gte(qs$p40), .45)$
    where(img$gte(qs$p45), .5)$
    where(img$gte(qs$p50), .55)$
    where(img$gte(qs$p55), .6)$
    where(img$gte(qs$p60), .65)$
    where(img$gte(qs$p65), .7)$
    where(img$gte(qs$p70), .75)$
    where(img$gte(qs$p75), .8)$
    where(img$gte(qs$p80), .85)$
    where(img$gte(qs$p85), .9)$
    where(img$gte(qs$p90), .95)$
    where(img$gte(qs$p95), 1)$
    updateMask(img$mask())
}

classify_percentiles <- function(img) {
  qs <- get_quantiles(img, seq(1, 100, 1))
  
  ee$Image(0)$
    where(img$gte(qs$p1), .01)$
    where(img$gte(qs$p2), .02)$
    where(img$gte(qs$p3), .03)$
    where(img$gte(qs$p4), .04)$
    where(img$gte(qs$p5), .05)$
    where(img$gte(qs$p6), .06)$
    where(img$gte(qs$p7), .07)$
    where(img$gte(qs$p8), .08)$
    where(img$gte(qs$p9), .09)$
    where(img$gte(qs$p10), .1)$
    where(img$gte(qs$p11), .11)$
    where(img$gte(qs$p12), .12)$
    where(img$gte(qs$p13), .13)$
    where(img$gte(qs$p14), .14)$
    where(img$gte(qs$p15), .15)$
    where(img$gte(qs$p16), .16)$
    where(img$gte(qs$p17), .17)$
    where(img$gte(qs$p18), .18)$
    where(img$gte(qs$p19), .19)$
    where(img$gte(qs$p20), .2)$
    where(img$gte(qs$p21), .21)$
    where(img$gte(qs$p22), .22)$
    where(img$gte(qs$p23), .23)$
    where(img$gte(qs$p24), .24)$
    where(img$gte(qs$p25), .25)$
    where(img$gte(qs$p26), .26)$
    where(img$gte(qs$p27), .27)$
    where(img$gte(qs$p28), .28)$
    where(img$gte(qs$p29), .29)$
    where(img$gte(qs$p30), .3)$
    where(img$gte(qs$p31), .31)$
    where(img$gte(qs$p32), .32)$
    where(img$gte(qs$p33), .33)$
    where(img$gte(qs$p34), .34)$
    where(img$gte(qs$p35), .35)$
    where(img$gte(qs$p36), .36)$
    where(img$gte(qs$p37), .37)$
    where(img$gte(qs$p38), .38)$
    where(img$gte(qs$p39), .39)$
    where(img$gte(qs$p40), .4)$
    where(img$gte(qs$p41), .41)$
    where(img$gte(qs$p42), .42)$
    where(img$gte(qs$p43), .43)$
    where(img$gte(qs$p44), .44)$
    where(img$gte(qs$p45), .45)$
    where(img$gte(qs$p46), .46)$
    where(img$gte(qs$p47), .47)$
    where(img$gte(qs$p48), .48)$
    where(img$gte(qs$p49), .49)$
    where(img$gte(qs$p50), .5)$
    where(img$gte(qs$p51), .51)$
    where(img$gte(qs$p52), .52)$
    where(img$gte(qs$p53), .53)$
    where(img$gte(qs$p54), .54)$
    where(img$gte(qs$p55), .55)$
    where(img$gte(qs$p56), .56)$
    where(img$gte(qs$p57), .57)$
    where(img$gte(qs$p58), .58)$
    where(img$gte(qs$p59), .59)$
    where(img$gte(qs$p60), .6)$
    where(img$gte(qs$p61), .61)$
    where(img$gte(qs$p62), .62)$
    where(img$gte(qs$p63), .63)$
    where(img$gte(qs$p64), .64)$
    where(img$gte(qs$p65), .65)$
    where(img$gte(qs$p66), .66)$
    where(img$gte(qs$p67), .67)$
    where(img$gte(qs$p68), .68)$
    where(img$gte(qs$p69), .69)$
    where(img$gte(qs$p70), .7)$
    where(img$gte(qs$p71), .71)$
    where(img$gte(qs$p72), .72)$
    where(img$gte(qs$p73), .73)$
    where(img$gte(qs$p74), .74)$
    where(img$gte(qs$p75), .75)$
    where(img$gte(qs$p76), .76)$
    where(img$gte(qs$p77), .77)$
    where(img$gte(qs$p78), .78)$
    where(img$gte(qs$p79), .79)$
    where(img$gte(qs$p80), .8)$
    where(img$gte(qs$p81), .81)$
    where(img$gte(qs$p82), .82)$
    where(img$gte(qs$p83), .83)$
    where(img$gte(qs$p84), .84)$
    where(img$gte(qs$p85), .85)$
    where(img$gte(qs$p86), .86)$
    where(img$gte(qs$p87), .87)$
    where(img$gte(qs$p88), .88)$
    where(img$gte(qs$p89), .89)$
    where(img$gte(qs$p90), .9)$
    where(img$gte(qs$p91), .91)$
    where(img$gte(qs$p92), .92)$
    where(img$gte(qs$p93), .93)$
    where(img$gte(qs$p94), .94)$
    where(img$gte(qs$p95), .95)$
    where(img$gte(qs$p96), .96)$
    where(img$gte(qs$p97), .97)$
    where(img$gte(qs$p98), .98)$
    where(img$gte(qs$p99), .99)$
    where(img$gte(qs$p100), 1)$
    updateMask(img$mask())
}

# Functions to map layers ----
map_norm_idx <- function(img, name, shown = FALSE, palette = NULL,
                         legend = TRUE) {
  
  # Set palette to default
  if (is.null(palette)) {
    palette = pal_idx
  }
  
  # Set visualization parameters
  viz <- list(min = 0, max = 1, palette = palette, 
              values = str_c(seq(0, 90, 10), seq(10, 100, 10), sep = '-'))
  
  # Create layer
  lyr <- Map$addLayer(eeObject = img, 
                      visParams = viz, 
                      name = name, 
                      shown = shown)
  
  # Conditionally add legend
  if( legend ) {
    lyr + Map$addLegend(visParams = viz, name = NA, position = "bottomright", 
                        color_mapping = "character")
  } else {
    lyr
  }
}

# Map 0-1 image in 5 equal intervals (0-100)
map_eq_int <- function(img, name, shown = FALSE, palette = NULL) {
  
  if (is.null(palette)) {
    palette = priorities_5
  }
  
  # Classify 0-1 to equal intervals
  classify_eq_int <- function(img) {
    ee$Image(0)$
      where(img$lt(0.2), 0)$
      where(img$gte(0.2), .20)$
      where(img$gte(0.4), .40)$
      where(img$gte(0.6), .60)$
      where(img$gte(0.8), .80)$
      where(img$gte(0.85), .85)$
      where(img$gte(0.9), .90)$
      where(img$gte(0.95), .95)$
      updateMask(img$mask())
  }
  
  # Classify
  img <- classify_eq_int(img)
  
  # Set visualization parameters
  viz <- list(min = 0, max = .8, palette = palette, 
              values = c('0-20', '20-40', '40-60', '60-80', '80-100'))
  
  # Create layer
  Map$addLayer(eeObject = img, 
               visParams = viz, 
               name = name, 
               shown = shown)
}

# Map 0-1 image in 10 equal intervals (0-100)
map_eq_int_10 <- function(img, name, shown = FALSE, palette = NULL,
                          legend = TRUE) {
  
  # Classify 0-1 to equal intervals
  classify_eq_int_10 <- function(img) {
    ee$Image(0)$
      where(img$lt(0.1), .1)$
      where(img$gte(0.1), .2)$
      where(img$gte(0.2), .3)$
      where(img$gte(0.3), .4)$
      where(img$gte(0.4), .5)$
      where(img$gte(0.5), .6)$
      where(img$gte(0.6), .7)$
      where(img$gte(0.7), .8)$
      where(img$gte(0.8), .9)$
      where(img$gte(0.9), 1)$
      updateMask(img$mask())
  }
  
  # Classify
  img <- classify_eq_int_10(img)
  
  # Set palette
  if (is.null(palette)) {
    palette = pal_idx
  }
  
  # Set visualization parameters
  viz <- list(min = 0, max = 1, palette = palette, 
              values = str_c(seq(0, 90, 10), seq(10, 100, 10), sep = '-'))
  
  # Create layer
  lyr <- Map$addLayer(eeObject = img, 
                      visParams = viz, 
                      name = name, 
                      shown = shown)
  
  if( legend ) {
    lyr + Map$addLegend(visParams = viz, name = NA, position = "bottomright", 
                        color_mapping = "character")
  } else {
    lyr
  }
}

# Map top 20th percentile in 3 intervals (80-90, 90-95, 95-100)
map_top_pctls <- function(pctl_img, name = NULL, shown = FALSE, palette = NULL, 
                          legend = TRUE, eemask = NULL) {
  
  # Set palette
  if (is.null(palette)) {
    palette = priorities_4
  }
  
  # Classify
  img <- ee$Image(0.65)$
    where(pctl_img$gte(.7), .7)$
    where(pctl_img$gte(.8), .8)$
    where(pctl_img$gte(.9), .9)$
    where(pctl_img$gte(.95), .95)
  
  if (!is.null(eemask)) {
    img <- img$updateMask(eemask)
  }
  
  # Visualization parameters
  viz <- list(min = 0.75, max = .95, 
              palette = palette, 
              values = c('<80%', '>80%', '>90%', '>95%'))
  
  # Create layer
  lyr <- Map$addLayer(eeObject = img, 
                      visParams = viz, 
                      name = name, 
                      shown = shown)
  
  if( legend ) {
    lyr + Map$addLegend(visParams = viz, name = NA, position = "bottomright", 
                        color_mapping = "character")
  } else {
    lyr
  }
}

lgnd_norm_idx <- function(palette = NULL) {
  
  if (is.null(palette)) {
    palette = pal_idx
  }
  
  viz <- list(min = 0, max = 1, palette = palette, 
              values = str_c(seq(0, 90, 10), seq(10, 100, 10), sep = '-'))
  
  Map$addLegend(visParams = viz, name = NA, 
                color_mapping = "character")
  
}

lgnd_top_pctls_3class <- function(lower = 80) {
  
  min <- lower / 100
  viz <- list(min = min, max = 1, palette = priorities_3, 
              values = c('<80%', '>80%', '>90%'))
  
  Map$addLegend(visParams = viz,
                name = NA, 
                position = "bottomright", 
                color_mapping = "character")
}

lgnd_top_pctls <- function(lower = 70) {
  
  viz <- list(min = 0.75, max = .95, 
              palette = priorities_4, 
              values = c('<80%', '>80%', '>90%', '>95%'))
  
  Map$addLegend(visParams = viz,
                name = NA, 
                position = "bottomright", 
                color_mapping = "character")
}

