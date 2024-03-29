#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Export 1 km total carbon stock from 500m AGB (2016), BGB (2016), and SOC (2010)
# Requires:
#     * export extent polygon
#     * GEE account
# Author:
#     * esturdivant@woodwellclimate.org, 2021-10-10
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load libraries 
library(rgee)
ee_Initialize()
library(sf)
library(colorspace)
library(tidyverse)

# Initialize variables 
# Woodwell Carbon and Tropics palettes
pal_arctic <- c('#d4e5e6', '#afd2d4', '#8ac1c4', '#62b2b5', '#24a4a6', '#0d7072', '#15c9ca')
pal_risk <- c('#f6e9cd', '#f0daa4', '#ebcb7d', '#e5be52', '#dfb125', '#937613', '#f6cf1c')
pal_carbon <- c('#d8d4cd', '#bab4a7', '#9d9786', '#847f6b', '#6b6a52')
pal_tropics <- c('#d5ead3', '#b1d9ad', '#8bca8b', '#62bd67', '#1ab14b', '#037a31')
pal_tropics_accents <- c(pal_tropics[6], '#0ecd5d')

pal_raisg <- c('#fdf2a9', '#fdf2a9', '#56b35f', '#2b827b', '#2d477c', '#2e1d4b')

# FQ index
pal_forestbio <- c(pal_carbon[1], pal_arctic)
pal_humz <- c(pal_carbon[c(2,1)], pal_risk[1:6])
# demoplot(rev(pal_forestbio), type = 'heatmap')
# demoplot(rev(pal_humz), type = 'heatmap')
pal_forestbio <- c(pal_carbon[c(1,2,3)], pal_arctic[c(4, 6)], '#00f8ec')
pal_humz <- c(pal_carbon[c(1,2,3)], pal_risk[c(4, 6)])

# Combine
priorities_idx_pal <- c(pal_carbon, pal_tropics_accents)
pal_carbontropics <- c(rev(pal_carbon), pal_tropics)
priorities_11 <- c(pal_carbon, pal_tropics[6], '#0ecd5d')
priorities_5 <- c(pal_tropics[1:5], pal_tropics[6], '#0ecd5d')
priorities_4 <- c(pal_tropics[c(1, 4)], pal_tropics[6], '#0ecd5d')
priorities_3 <- c(pal_tropics[1], pal_tropics[6], '#0ecd5d')

# Dark green highest
priorities_11 <- c(pal_carbon[c(1,2,3)], pal_tropics[c(4, 6)])
priorities_3 <- c(pal_tropics[c(1, 4, 6)])
priorities_4 <- c(pal_tropics[c(1, 2, 4, 6)])
priorities_4 <- c(pal_tropics[c(1, 4, 6)], '#0ecd5d')

# Set a region of interest and center map display
Map$setCenter(30, 0, zoom = 3)

# Local paths
data_dir <- '/Volumes/ejs_storage/data'
final_polys_dir <- '/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/data/hih_sites'

viz_idx_norm <- list(min = 0, max = 1, palette = priorities_11, 
            values = str_c(seq(0, 90, 10), seq(10, 100, 10), sep = '-'))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Functions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prep to get paths to assets
user <- ee_get_assethome()
addm <- function(x) sprintf("%s/%s", user, x)

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
  
  p <- img$
    rename('b1')$
    reduceRegion(
      reducer = ee$Reducer$percentile(pctls),
      geometry = tropics_bb,
      bestEffort = TRUE)
  
  minp <- pctls[[1]]
  maxp <- pctls[[2]]
  min <- p$get(str_c('b1_p', minp))$getInfo()
  max <- p$get(str_c('b1_p', maxp))$getInfo()
  
  # Divide by 99th percentile and reclass values outside of 0-1 range
  img1 <- img$unitScale(min, max)
  
  # Reclass values outside of 0-1 range
  img <- img1$
    where(img1$gt(1.0), 1.0)$
    where(img1$lt(0.0), 0.0)
  
  return( img )
}

# Classify image to 10 equal-area ranked classes
classify_dentiles <- function(img) {
  qs <- get_quantiles(img, c(10, 20, 30, 40, 50, 60, 70, 80, 90))
  
  ee$Image(0)$
    where(img$lt(qs$p10), 0)$
    where(img$gte(qs$p10), .10)$
    where(img$gte(qs$p20), .20)$
    where(img$gte(qs$p30), .30)$
    where(img$gte(qs$p40), .40)$
    where(img$gte(qs$p50), .50)$
    where(img$gte(qs$p60), .60)$
    where(img$gte(qs$p70), .70)$
    where(img$gte(qs$p80), .80)$
    where(img$gte(qs$p90), .90)$
    updateMask(img$mask())
}

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

# Classify image to equal-area ranked classes
classify_finer_percentiles <- function(img) {
  qs <- get_quantiles(img, c(seq(5, 80, 5), seq(81, 100, 1)))
  
  ee$Image(0)$
    where(img$lt(qs$p5), 0)$
    where(img$gte(qs$p5), .05)$
    where(img$gte(qs$p10), .1)$
    where(img$gte(qs$p15), .15)$
    where(img$gte(qs$p20), .2)$
    where(img$gte(qs$p25), .25)$
    where(img$gte(qs$p30), .3)$
    where(img$gte(qs$p35), .35)$
    where(img$gte(qs$p40), .4)$
    where(img$gte(qs$p45), .45)$
    where(img$gte(qs$p50), .5)$
    where(img$gte(qs$p55), .55)$
    where(img$gte(qs$p60), .6)$
    where(img$gte(qs$p65), .65)$
    where(img$gte(qs$p70), .7)$
    where(img$gte(qs$p75), .75)$
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

# Classify upper 10th percentile to 5 equal-area (percentile) classes
classify_top10pctl <- function(img) {
  qs <- get_quantiles(img, seq(90, 98, 2))
  
  ee$Image(0)$
    where(img$lt(qs$p90), 0)$
    where(img$gte(qs$p90), .9)$
    where(img$gte(qs$p92), .92)$
    where(img$gte(qs$p94), .94)$
    where(img$gte(qs$p96), .96)$
    where(img$gte(qs$p98), .98)$
    updateMask(img$mask())
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

# Functions to map layers ----
map_norm_idx <- function(img, name, shown = FALSE, palette = NULL,
                         legend = TRUE) {
  
  # Set palette to default
  if (is.null(palette)) {
    palette = priorities_11
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

map_eq_int <- function(img, name, shown = FALSE) {
  img <- classify_eq_int(img)
  
  viz <- list(min = 0, max = .8, palette = priorities_5, 
                         values = c('0-20', '20-40', '40-60', '60-80', '80-100'))
  
  Map$addLayer(eeObject = img, 
               visParams = viz, 
               name = name, 
               shown = shown)
}

map_eq_int_10 <- function(img, name, shown = FALSE, palette = NULL,
                          legend = TRUE) {
  img <- classify_eq_int_10(img)
  
  if (is.null(palette)) {
    palette = priorities_11
  }
  
  viz <- list(min = 0, max = 1, palette = palette, 
              values = str_c(seq(0, 90, 10), seq(10, 100, 10), sep = '-'))
  
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

map_top_pctls_3class <- function(pctl_img, name = NULL, shown = FALSE, palette = NULL,
                                 legend = TRUE) {
  
  lower <- 80
  
  if (is.null(palette)) {
    palette = priorities_3
  }
  
  img <- ee$Image(0)$
    where(pctl_img$lt(.80), 0.8)$
    where(pctl_img$gte(.80), .9)$
    where(pctl_img$gte(.90), 1)$
    updateMask(dhf_mask)
  
  min <- lower / 100
  viz <- list(min = min, max = 1, 
              palette = palette, 
              values = c('<80%', '>80%', '>90%'))
  
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

map_top_pctls <- function(pctl_img, name = NULL, shown = FALSE, palette = NULL, 
                          legend = TRUE) {
  
  if (is.null(palette)) {
    palette = priorities_4
  }
  
  img <- ee$Image(0.65)$
    where(pctl_img$gte(.7), .7)$
    where(pctl_img$gte(.8), .8)$
    where(pctl_img$gte(.9), .9)$
    where(pctl_img$gte(.95), .95)$
    updateMask(dhf_mask)
  
  viz <- list(min = 0.75, max = .95, 
              palette = palette, 
              values = c('<80%', '>80%', '>90%', '>95%'))
  
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
    palette = priorities_11
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

map_top_ventiles <- function(ventile_img, lower = 80, 
                             name = NULL, shown = FALSE) {
  
  min <- lower / 100
  viz <- list(min = min-0.1, max = 1, palette = priorities_5, 
              values = c(str_c('<', lower), 
                         seq(lower, 95, 5) %>% str_c('>', .)))
  
  Map$addLayer(eeObject = ventile_img, 
               visParams = viz, 
               name = name, 
               shown = shown)
}

lgnd_top_ventiles <- function(lower = 80) {
  
  min <- lower / 100
  viz <- list(min = min-0.05, max = 0.96, palette = priorities_5, 
              values = c(str_c('<', lower), 
                         seq(lower, 95, 5) %>% str_c('>', .)))
  
  Map$addLegend(visParams = viz,
                  name = NA, 
                  position = "bottomright", 
                  color_mapping = "character")
}

map_top_10pctl <- function(img, name, shown = FALSE) {
  
  viz <- list(min = 0.9, max = 0.98, palette = priorities_5, 
              values = seq(92, 100, 2))
  
  Map$addLayer(eeObject = img, 
               visParams = viz, 
               name = name, 
               shown = shown) +
    Map$addLegend(visParams = viz, name = NA, 
                  position = "bottomleft", color_mapping = "character")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tropical boundaries ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create tropics extent rectangle ----
tropics_bb <- ee$Geometry$Rectangle(
  coords = c(-180, -35, 180, 35),
  proj = "EPSG:4326",
  geodesic = FALSE
)

# Convert to raster for masking ----
# Convert to raster
tropics_r <- ee$FeatureCollection(tropics_bb)$
  map(function(f) f$set("tropics", 1))$
  reduceToImage(
    properties = list('tropics'),
    reducer = ee$Reducer$first()
  )

# Dense humid forests biome ----
biome_dhf_id <- addm('BIOME_Dense_Humid_Forests')

# Get dense humid forests biome
alist <- ee_manage_assetlist(path_asset = addm(""))
if(!biome_dhf_id %in% alist$ID) {
  
  # Load ecoregions and filter to dense humid forests biome
  biome_dhf <- ee$FeatureCollection("RESOLVE/ECOREGIONS/2017")$
    filter(
      ee$Filter$eq('BIOME_NAME', 'Tropical & Subtropical Moist Broadleaf Forests')
    )
  
  # Convert to raster
  dense_humid_forests <- biome_dhf$
    reduceToImage(
      properties = list('BIOME_NUM'), 
      reducer = ee$Reducer$first()
    )$
    setDefaultProjection(crs = 'EPSG:4326', scale = 1000)
  
  # Save image as EE asset
  task_img <- ee_image_to_asset(dense_humid_forests,
                                'BIOME_Dense_Humid_Forests',
                                assetId = biome_dhf_id,
                                # region = tropics_bb,
                                crs = 'EPSG:4326',
                                scale = 1000,
                                maxPixels = 803042888)
  task_img$start()
}

dense_humid_forests <- ee$Image(biome_dhf_id)
dhf_mask <- dense_humid_forests$mask()

# Create Biomes layer ----
colorUpdates = list(
  list(ECO_ID = 204, COLOR = '#B3493B'),
  list(ECO_ID = 245, COLOR = '#267400'),
  list(ECO_ID = 259, COLOR = '#004600'),
  list(ECO_ID = 286, COLOR = '#82F178'),
  list(ECO_ID = 316, COLOR = '#E600AA'),
  list(ECO_ID = 453, COLOR = '#5AA500'),
  list(ECO_ID = 317, COLOR = '#FDA87F'),
  list(ECO_ID = 763, COLOR = '#A93800')
)

ecoRegions = ee$FeatureCollection("RESOLVE/ECOREGIONS/2017")$
  map(function(f) {
    color = f$get('COLOR_BIO')
    f$set(list(style = list(color = color, width = 0)))
  })

ecoRegions = ecoRegions$
  filter(ee$Filter$inList('BIOME_NUM', c(1,2,3,7,14)))$
  merge(colorUpdates[[i]]$layer)

imageRGB = ecoRegions$style(styleProperty = 'style')
biomes_lyr <- Map$addLayer(imageRGB, name = 'RESOLVE/ECOREGIONS/2017', show = FALSE)

# Map ecoregions
# ecoRegions <- ee$FeatureCollection("RESOLVE/ECOREGIONS/2017")
# ecoRegions = ecoRegions$map(function(f) {
#   color = f$get('COLOR')
#   f$set(list(style = list(color = color, width = 0)))
# })
# 
# for (i in 1:length(colorUpdates)) {
#   colorUpdates[[i]]$layer = ecoRegions$
#     filterMetadata('ECO_ID', 'equals', colorUpdates[[i]]$ECO_ID)$
#     map(function(f) {
#       f$set(list(style = list(color = colorUpdates[[i]]$COLOR, width = 0)))
#   })
#   
#   ecoRegions = ecoRegions$
#     filterMetadata('ECO_ID','not_equals', colorUpdates[[i]]$ECO_ID)$
#     merge(colorUpdates[[i]]$layer)
# }
# 
# imageRGB = ecoRegions$style(styleProperty = 'style')
# Map$addLayer(imageRGB, name = 'RESOLVE/ECOREGIONS/2017');

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HIH sites ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# polys_fp <- here::here(final_polys_dir, 'hih_sites_polys.shp')
# pts_fp <- here::here(final_polys_dir, 'hih_sites_pts.shp')
# 
# # Upload shapefile
# hih_sites <- st_read(polys_fp) %>% sf_as_ee()
# hih_pts <- st_read(pts_fp) %>% sf_as_ee()

# Load assets
hih_sites <- ee$FeatureCollection(addm('HIH_sites/hih_sites_polys'))
hih_pts <- ee$FeatureCollection(addm('HIH_sites/hih_sites_pts'))

# Paint all the polygon edges with the same number and width, display.
outline <- ee$Image()$byte()$paint(featureCollection = hih_sites, color = 1, width = 2)
hih_sites_lyr <- Map$addLayer(outline, name = 'HIH sites', shown = FALSE)
hih_pts_lyr <- Map$addLayer(hih_pts, name = 'HIH points', shown = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RAISG ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load assets
raisg_lim <- ee$FeatureCollection(addm('Lim_Raisg'))

# Paint all the polygon edges with the same number and width, display.
raisg_outline <- ee$Image()$byte()$paint(featureCollection = raisg_lim, color = 'red', width = 2)
raisg_lyr <- Map$addLayer(raisg_outline, name = 'RAISG', shown = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Carbon density ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wbd_mgha <- ee$Image("users/sgorelik/WHRC_Global_Biomass_500m_V6/Current_AGB_BGB_Mgha")
soc_mgha <- ee$Image("users/sgorelik/WHRC_Global_Biomass_500m_V6/Current_SOC_Mgha")

# Get mask
c_mask <- wbd_mgha$updateMask(dhf_mask)$mask()

# Convert to carbon density 
wcd_mgcha <- wbd_mgha$divide(2)$updateMask(dhf_mask)
soc_mgcha <- soc_mgha$divide(2)$updateMask(c_mask)

# Rescale / adjust outliers
# get_pctl(wcd_mgcha, 99) # max: 306.5 MgC/ha; 98th: 209; 99th: 217
# get_pctl(soc_mgcha, 99) # max: 2,399 MgC/ha; 98th: 535; 99th: 631
wcd_idx <- rescale_to_pctl(wcd_mgcha)
soc_idx <- rescale_to_pctl(soc_mgcha)

wcd_vent <- classify_percentiles(wcd_mgcha)
soc_vent <- classify_percentiles(soc_mgcha)

# Combine 1:1 / simple average
carbon_idx <- wcd_idx$
  setDefaultProjection(crs = 'SR-ORG:6974', scale = 463.3127165275)$
  add(soc_idx$setDefaultProjection(crs = 'SR-ORG:6974', scale = 463.3127165275))$
  divide(2)

# get_pctl(carbon_idx, 98) # max: 1; 98th: .73; 99th: 0.79
carbon_norm <- rescale_to_pctl(carbon_idx)

# Reclass to ventiles
carbon_vent <- classify_percentiles(carbon_idx)

# # View
# map_norm_idx(soc_idx, 'SOC', shown = TRUE) +
#   map_norm_idx(wcd_idx, 'Woody biomass carbon') +
#   map_norm_idx(carbon_idx, 'Carbon index (.5*Woody Carbon + .5*SOC)') +
#   map_norm_idx(carbon_norm, 'Carbon normalized') +
#   map_norm_idx(carbon_vent, "Carbon normalized ventiles")

# Look at histogram
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Forest Landscape Integrity Index ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
flii <- ee$ImageCollection(c(
  "users/esturdivant/flii/flii_Africa",
  "users/esturdivant/flii/flii_Asia",
  "users/esturdivant/flii/flii_NorthAmerica",
  "users/esturdivant/flii/flii_Oceania",
  "users/esturdivant/flii/flii_SouthAmerica"
))

# Mask each image
flii <- flii$map(function(img) {
  img$updateMask(img$neq(-9999))
  })

# Mosaic
flii <- flii$mosaic()$
  setDefaultProjection(crs = 'EPSG:4326', scale = 300)

# Scale values to 0-1 scale
# get_pctl(flii, 99) # 99th: 9996 # 78th: 9666
flii_norm <- rescale_to_pctl(flii, c(0, 78))$updateMask(dhf_mask)

# Map and reclass to ventiles
flii_vent <- classify_percentiles(flii_norm)

# # View
# map_norm_idx(flii, 'FLII') +
#   # map_norm_idx(flii_vent1, 'FLII ventiles') +
#   map_norm_idx(flii_norm, 'FLII normalized') +
#   map_norm_idx(flii_vent, "FLII normalized ventiles")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Key Biodiversity Areas ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kba_id <- addm('KBAs_2021_Sep02_maskDHF')

alist <- ee_manage_assetlist(path_asset = addm(""))
if(!kba_id %in% alist$ID) {
  
  # Load and convert to raster where KBA = 0.5 and AZE = 1
  kbas <- ee$FeatureCollection(addm("KBAsGlobal_2021_September_02_POL"))
  
  # Set sig_level to 1 or 0.9 to indicate significance level of site
  aze_true <- kbas$
    filter(ee$Filter$eq('AzeStatus', 'confirmed'))$
    map(function(f) {f$set("sig_level", 1)})
  aze_false <- kbas$
    filter(ee$Filter$neq('AzeStatus', 'confirmed'))$
    map(function(f) {f$set("sig_level", 0.95)})
  
  # Merge the subsets
  kbas <- aze_true$merge(aze_false)
  
  # Convert to raster
  kba_r <- kbas$
    reduceToImage(
      properties = list('sig_level'), 
      reducer = ee$Reducer$first()
    )$
    unmask()$
    setDefaultProjection(crs = 'EPSG:4326', scale = 1000)$
    updateMask(dhf_mask)

  # Save image as EE asset
  task_img <- ee_image_to_asset(kba_r,
                                'KBAs_2021_Sep02_masked', 
                                assetId = kba_id,
                                region = tropics_bb,
                                crs = 'EPSG:4326',
                                scale = 1000,
                                maxPixels = 312352344,
                                overwrite = TRUE)
  task_img$start()

}

kba_r <- ee$Image(kba_id)

# Reclass values outside of 0-1 range
kba_r <- kba_r$
  where(kba_r$lt(0.94), 0.8)$
  where(kba_r$lt(0.96), 0.9)

# View
# map_norm_idx(kba_r, "Key Biodiversity Areas", TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Biodiversity Intactness Index, 2005 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lbii <- ee$Image(addm('lbii_2005'))

# Rescale
# get_pctl(infant_mort) # 99.7
lbii_norm <- rescale_to_pctl(lbii)$updateMask(dhf_mask)

# Ventiles
lbii_vent <- classify_percentiles(lbii_norm)

# # View
# map_eq_int(lbii, 'Average') +
#   map_eq_int(lbii_norm, 'Average and normalize') +
#   map_eq_int(lbii_vent, 'Percentiles')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Species richness 5km ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rich <- ee$Image(addm('IUCN_Richness_all_5km'))

# Rescale
# get_pctl(infant_mort) # 99.7
rich_norm <- rescale_to_pctl(rich)$updateMask(dhf_mask)

# Ventiles
rich_vent <- classify_percentiles(rich_norm)

# # View
# map_eq_int(lbii, 'Average') +
#   map_eq_int(lbii_norm, 'Average and normalize') +
#   map_eq_int(lbii_vent, 'Percentiles')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Development Potential Indices ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Create ImageCollection
# ee_manage_create(
#   path_asset = addm("DPI"),
#   asset_type = "ImageCollection"
# )
# 
# dpi_eelist <- ee_manage_assetlist(path_asset = addm("DPI_v1"))
# 
# # Move images from DPI_v1 folder into ImageCollection
# ee_manage_move(
#   path_asset = addm("DPI_v1"),
#   final_path = addm("DPI")
# )

dti_id <- addm('DTI/DTI_2016_pctls_maskDHF')

alist <- ee_manage_assetlist(path_asset = addm("DTI"))
if(!dti_id %in% alist$ID) {
  
  # Load ImageCollection 
  dpi_eelist <- ee_manage_assetlist(path_asset = addm("DPI"))
  
  # Reclass each index to dense humid forest biome
  dpi <- dpi_eelist$ID %>% 
    purrr::map(function(x) {
      img <- ee$Image(x)$unmask()
      # rescale_to_pctl(img, c(0, 100))
      classify_percentiles(img)
    }) %>% 
    ee$ImageCollection()
  
  # Additive / equal weights / simple average 
  dti <- dpi$
    sum()$
    setDefaultProjection(crs = 'EPSG:4326', scale = 1000)$
    updateMask(dhf_mask)
  
  # Normalize
  dti_norm <- rescale_to_pctl(dti, c(0, 100))
  
  # Save image to EE asset
  task_img2 <- ee_image_to_asset(dti_norm,
                                 'DTI_2016', 
                                 assetId = dti_id,
                                 region = tropics_bb,
                                 crs = 'EPSG:4326',
                                 scale = 1000,
                                 maxPixels = 312352344, 
                                 overwrite = TRUE)
  task_img2$start()
  
}

dti_norm <- ee$Image(dti_id)$updateMask(dhf_mask)

dti_vent <- classify_percentiles(dti_norm)

# # View
# map_norm_idx(dti_norm, 'Average and normalize') +
#   map_norm_idx(dti_vent, 'Ventiles') +
#     map_norm_idx(carbon_vent, 'Carbon')

# map_eq_int(dti, 'Average') +
#   map_eq_int(dti_norm, 'Average and normalize') +
#   map_eq_int(dti_vent, 'Percentiles') 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subnational Infant Mortality Rate ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
infant_mort <- ee$Image(addm("subnational_infant_mortality_rates_v2_01"))

# Mask 
infant_mort <- infant_mort$updateMask(infant_mort$gte(0))

# Rescale
# get_pctl(infant_mort) # 99.7
imr_norm <- rescale_to_pctl(infant_mort)$updateMask(dhf_mask)

# Ventiles
imr_vent <- classify_percentiles(imr_norm)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Zoonotic spillover risk (from Allen et al. 2017) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
zoonotic_risk_all <- ee$Image(addm("zoonotic_eid_risk"))$
  setDefaultProjection(crs = 'EPSG:4326', scale = 100000)$
  resample() # Downsample zoonotic risk to smooth for display

# Reweighted by population
zs_weight_pop <- zoonotic_risk_all$select('b3')$updateMask(dhf_mask)
zs_wpop_norm <- rescale_to_pctl(zs_weight_pop)
zs_wpop_vent <- classify_percentiles(zs_weight_pop)

# # View
# map_eq_int(zs_wpubs_norm, 'ZS') +
#   map_eq_int(zs_wpubs_ea, 'ZS')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Human modification ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load
gHM <- ee$ImageCollection("CSP/HM/GlobalHumanModification")$first() # only image is '2016'

# Rescale
# get_pctl(gHM, 100) # 99th: 0.76; 98th: 0.72; 95th: 0.66; max: 0.99
hm_norm <- rescale_to_pctl(gHM)$updateMask(dhf_mask)

# Map and reclass to ventiles
hm_vent <- classify_percentiles(hm_norm)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Human footprint (from Wild Areas v3) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
hf <- ee$Image(addm("wildareas-v3-2009-human-footprint"))$updateMask(dhf_mask)

# Rescale
# get_pctl(hf, 98) # max: 50; 99th: 24.6; 98th: 20.7
hf_norm <- rescale_to_pctl(hf, c(0, 98))
# Map$addLayer(eeObject = hf_norm, visParams = viz_idx_norm)

# Map and reclass to ventiles
hf_vent <- classify_percentiles(hf)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Population density ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
popd <- ee$ImageCollection("CIESIN/GPWv411/GPW_UNWPP-Adjusted_Population_Density")$
  first()$
  updateMask(dhf_mask)

# Rescale
# get_pctl(popd, 100) # 98th: 36 p/km2; max: 162974.2
popd_norm <- rescale_to_pctl(popd)$updateMask(dhf_mask)
# Map$addLayer(eeObject = popd_norm, visParams = viz_idx_norm)

# Map and reclass to ventiles
popd_vent <- classify_percentiles(popd)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GDL Health indicators ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hdi_fc <- ee$FeatureCollection(addm('GDL_subnational_hdi_le_hi'))

# Life expectancy ----
le <- hdi_fc$
  reduceToImage(
    properties = list('LE'), 
    reducer = ee$Reducer$first()
  )$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)

# Rescale
# get_pctl(le, 1) # 51.9 years is the 1st percentile and 47.75 is the min
le <- rescale_to_pctl(le, c(0, 100))$updateMask(dhf_mask)

# Invert values
le_norm <- le$multiply(-1)$add(1)

# Rescale to Percentiles 
le_vent <- classify_percentiles(le_norm)$updateMask(dhf_mask)

# View
# Map$addLayer(eeObject = le, visParams = viz_idx_norm, name = "Life expectancy")

# Health index ----
hi <- hdi_fc$
  reduceToImage(properties = list('HI'), reducer = ee$Reducer$first())$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)
hi_norm <- rescale_to_pctl(hi, c(0, 100))$updateMask(dhf_mask)

# Human development index ----
hdi <- hdi_fc$
  reduceToImage(properties = list('shdi'), reducer = ee$Reducer$first())$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)
hdi_norm <- rescale_to_pctl(hdi, c(0, 100))$updateMask(dhf_mask)

# Map$addLayer(eeObject = hdi,
#              visParams = viz_idx_norm, 
#              name = "SHDI")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Accessibility to Healthcare ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
hc_access <- ee$Image("Oxford/MAP/accessibility_to_healthcare_2019")$
  select('accessibility_walking_only')$
  rename('b1')

# Rescale
# get_pctl(hc_access,  95) # 6380 min
hcw_norm <- rescale_to_pctl(hc_access, c(0, 95))$updateMask(dhf_mask)
# Map$addLayer(eeObject = hc_access, visParams = viz_idx_norm)

# Rescale to Percentiles 
hcw_vent <- classify_percentiles(hcw_norm)$updateMask(dhf_mask)

# Load 
hc_motor <- ee$Image("Oxford/MAP/accessibility_to_healthcare_2019")$
  select('accessibility')$
  rename('b1')

# Rescale
# get_pctl(hc_motor,  95)
hcm_norm <- rescale_to_pctl(hc_motor, c(0, 95))$updateMask(dhf_mask)
# Map$addLayer(eeObject = hc_motor, visParams = viz_idx_norm)

# Rescale to Percentiles 
hcm_vent <- classify_percentiles(hcm_norm)$updateMask(dhf_mask)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Protected Areas ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load
pas <- ee$FeatureCollection("WCMC/WDPA/current/polygons")$
  filter(ee$Filter(c(
    ee$Filter$neq('MARINE', '2'),
    ee$Filter$gt('REP_AREA', 0))))$
  filterBounds(tropics_bb)

# pas$size()$getInfo()

pa_lyr <- Map$addLayer(eeObject = pas, name = "Protected areas", opacity = 0.5)

# Create fill
pas_fill <- pas$draw(color = 'green', strokeWidth = 0)
pas_lyr <- Map$addLayer(pas_fill, name = 'Protected areas', 
                        opacity = 0.5, shown = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MSF interventions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# countries_msf_shp <- file.path(data_dir, 'gadm', 'gadm0_tropics_simp01big9.shp')
# countries <- st_read(countries_msf_shp) %>% 
#   mutate(MSF = ifelse(is.na(MSF), 0, 1))
# 
# # Upload countries
# countries_ee <- countries %>% sf_as_ee()
# 
# # Filter to MSF countries
# msf_ee <- countries_ee$filter(ee$Filter$eq('MSF', 1))
# 
# # Simplify
# msf_simp <- msf_ee$map(
#   function(f) f$simplify(maxError = ee$ErrorMargin(50000, 'meters'))
# )
# 
# # Create outline
# msf_outline <- ee$Image()$byte()$paint(featureCollection = msf_simp, width = 2)
# msf_lyr <- Map$addLayer(msf_outline, list(palette = c('#bdbdbd')),
#                         name = 'MSF operations', shown = FALSE)
# msf_lyr <- Map$addLayer(msf_outline, list(palette = c('black')),
#                         name = 'MSF operations', shown = FALSE)
# 
# # Get non-MSF countries
# no_msf_id <- addm('non_MSF_countries_masked')
# alist <- ee_manage_assetlist(path_asset = addm(""))
# if(!no_msf_id %in% alist$ID) {
#   
#   no_msf_ee <- countries_ee$filter(ee$Filter$neq('MSF', 1))
#   
#   # Convert to raster
#   no_msf_r <- no_msf_ee$
#     reduceToImage(
#       properties = list('MSF'), 
#       reducer = ee$Reducer$first()
#     )$
#     setDefaultProjection(crs = 'EPSG:4326', scale = 1000)$
#     updateMask(dhf_mask)
#   
#   # Save image as EE asset
#   task_img <- ee_image_to_asset(no_msf_r,
#                                 'non_MSF_countries_masked',
#                                 assetId = ,
#                                 region = tropics_bb,
#                                 crs = 'EPSG:4326',
#                                 scale = 1000,
#                                 maxPixels = 191434770)
#   task_img$start()
# }
# 
# no_msf_r <- ee$Image(no_msf_id)
# 
# # Create layer
# no_msf_lyr <- Map$addLayer(no_msf_r, list(palette = c('#bdbdbd')),
#                            name = 'non-MSF', 
#                            opacity = 0.8, shown = FALSE)
# 
# no_msf_lyr <- Map$addLayer(no_msf_r, list(palette = c('black')),
#                            name = 'non-MSF', 
#                            opacity = 0.8, shown = FALSE)
