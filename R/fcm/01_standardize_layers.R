#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Standardize layers in GEE for use in the forest carbon composite indicator
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
library(tidyverse)

# Initialize variables ----
# Local paths
data_dir <- '/Users/emilysturdivant/data'
export_path <- '/Volumes/GoogleDrive/My Drive/Earth Engine Exports'

# Visualization parameters
Map$setCenter(30, 0, zoom = 3)

pal_idx <- sequential_hcl(n = 10, 'inferno')
# hcl_palettes(plot = TRUE, n = 10)
# demoplot(rev(pal_idx), type = 'heatmap')

viz_idx_norm <- list(min = 0, max = 1, palette = pal_idx, 
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tropical boundaries ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create tropics extent rectangle ----
tropics_bb <- ee$Geometry$Rectangle(
  coords = c(-180, -35, 180, 35),
  proj = "EPSG:4326",
  geodesic = FALSE
)

# # Convert to raster for masking ----
# # Convert to raster
# tropics_r <- ee$FeatureCollection(tropics_bb)$
#   map(function(f) f$set("tropics", 1))$
#   reduceToImage(
#     properties = list('tropics'),
#     reducer = ee$Reducer$first()
#   )
# 
# # Dense humid forests biome ----
# biome_dhf_id <- addm('BIOME_Dense_Humid_Forests')
# 
# # Get dense humid forests biome
# alist <- ee_manage_assetlist(path_asset = addm(""))
# if(!biome_dhf_id %in% alist$ID) {
#   
#   # Load ecoregions and filter to dense humid forests biome
#   biome_dhf <- ee$FeatureCollection("RESOLVE/ECOREGIONS/2017")$
#     filter(
#       ee$Filter$eq('BIOME_NAME', 'Tropical & Subtropical Moist Broadleaf Forests')
#     )
#   
#   # Convert to raster
#   dense_humid_forests <- biome_dhf$
#     reduceToImage(
#       properties = list('BIOME_NUM'), 
#       reducer = ee$Reducer$first()
#     )$
#     setDefaultProjection(crs = 'EPSG:4326', scale = 1000)
#   
#   # Save image as EE asset
#   task_img <- ee_image_to_asset(dense_humid_forests,
#                                 'BIOME_Dense_Humid_Forests',
#                                 assetId = biome_dhf_id,
#                                 # region = tropics_bb,
#                                 crs = 'EPSG:4326',
#                                 scale = 1000,
#                                 maxPixels = 803042888)
#   task_img$start()
# }
# 
# dense_humid_forests <- ee$Image(biome_dhf_id)
# dhf_mask <- dense_humid_forests$mask()
# 
# # Create Biomes layer ----
# colorUpdates = list(
#   list(ECO_ID = 204, COLOR = '#B3493B'),
#   list(ECO_ID = 245, COLOR = '#267400'),
#   list(ECO_ID = 259, COLOR = '#004600'),
#   list(ECO_ID = 286, COLOR = '#82F178'),
#   list(ECO_ID = 316, COLOR = '#E600AA'),
#   list(ECO_ID = 453, COLOR = '#5AA500'),
#   list(ECO_ID = 317, COLOR = '#FDA87F'),
#   list(ECO_ID = 763, COLOR = '#A93800')
# )
# 
# ecoRegions = ee$FeatureCollection("RESOLVE/ECOREGIONS/2017")$
#   map(function(f) {
#     color = f$get('COLOR_BIO')
#     f$set(list(style = list(color = color, width = 0)))
#   })
# 
# ecoRegions = ecoRegions$
#   filter(ee$Filter$inList('BIOME_NUM', c(1,2,3,7,14)))$
#   merge(colorUpdates[[i]]$layer)
# 
# imageRGB = ecoRegions$style(styleProperty = 'style')
# biomes_lyr <- Map$addLayer(imageRGB, name = 'RESOLVE/ECOREGIONS/2017', show = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AGB+BGB carbon density ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wbd_mgha <- ee$Image("users/sgorelik/WHRC_Global_Biomass_500m_V6/Current_AGB_BGB_Mgha")

# Get mask
c_mask <- wbd_mgha$gt(0)

# Convert to carbon density 
wcd_mgcha <- wbd_mgha$divide(2)$updateMask(c_mask)

# Normalize WCD to 0-1 from 0-200 MgC/ha
wcd_mgcha <- wcd_mgcha$unitScale(0, 200)
wcd_norm <- wcd_mgcha$
  where(wcd_mgcha$gt(1.0), 1.0)$
  where(wcd_mgcha$lt(0.0), 0.0)

# # View
# map_norm_idx(wcd_norm, 'Woody biomass carbon', palette = pal_idx) +
#   Map$addLayer(c_mask)

# # Look at histogram ----
# img <- wcd_norm
# 
# # Take sample at random points within the region
# sample <-  img$sampleRegions(tropics_bb, NULL, scale = 2000)
# sample <-  img$sample(numPixels = 1e4) # couldn't export
# 
# # Export
# task_name <- 'sample_wcd_norm_1e4'
# task_vector <- sample %>% 
#   ee_table_to_drive(description = task_name,
#                     folder = basename(export_path),
#                     fileFormat = 'CSV', 
#                     timePrefix = FALSE)
# task_vector$start()
# 
# # Import CSV for plot
# samp_csv <- here::here(export_path, str_c(task_name, '.csv'))
# samp_df <- read_csv(samp_csv)
# n <- nrow(samp_df)
# 
# # Plot histogram
# samp_df %>% 
#   ggplot() +
#   geom_histogram() +
#   labs(y = str_glue('Number of pixels (N = {n})'), 
#        x = 'WCD (tC/ha)') +
#   theme_minimal()
# 
# # Save
# ggsave(here::here('outputs/fcm/histograms', str_c(task_name, '.png')),
#        width = 4, height = 3.5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SOC carbon density ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
soc_mgha <- ee$Image("users/sgorelik/WHRC_Global_Biomass_500m_V6/Current_SOC_Mgha")

# Convert to carbon density 
soc_mgcha <- soc_mgha$divide(2)$updateMask(c_mask)

# Normalize to 0-1 from 0-600 MgC/ha
soc_mgcha <- soc_mgcha$unitScale(0, 600)
soc_norm <- soc_mgcha$
  where(soc_mgcha$gt(1.0), 1.0)$
  where(soc_mgcha$lt(0.0), 0.0)

# # View
# map_norm_idx(soc_norm, 'Soil organic carbon', palette = pal_idx) +
#   Map$addLayer(c_mask)

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
flii <- flii$map(function(img) { img$updateMask(img$neq(-9999)) })

# Mosaic
flii <- flii$mosaic()$setDefaultProjection(crs = 'EPSG:4326', scale = 300)

# Normalize to 0-1 from 0-9666
flii_scale <- flii$unitScale(0, 10000)
flii_norm <- flii_scale$
  where(flii_scale$gt(1.0), 1.0)$
  where(flii_scale$lt(0.0), 0.0)$
  updateMask(c_mask)

# # View
# map_norm_idx(flii, 'FLII', palette = pal_idx) +
#   map_norm_idx(flii_norm, 'FLII normalized', palette = pal_idx, show = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Species richness 5km ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rich <- ee$Image(addm('IUCN_Richness_all_5km'))

# Rescale
rich_norm <- rescale_to_pctl(rich)

# Ventiles
rich_vent <- classify_percentiles(rich_norm)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Key Biodiversity Areas ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kba_id <- addm('KBAs_2021_Sep02_maskDHF')

kba_r <- ee$Image(kba_id)

# Reclass values outside of 0-1 range
kba_r <- kba_r$
  where(kba_r$lt(0.94), 0.8)$
  where(kba_r$lt(0.96), 0.9)

# View
# map_norm_idx(kba_r, "Key Biodiversity Areas", TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Deforestation Hot Spots (Harris et al. 2017 via GFW) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hs_id <- addm('Hotspots_GFW_2020')
hs_r <- ee$Image(hs_id)

# Reclass values outside of 0-1 range
hs_r <- hs_r$
  unmask()$
  where(hs_r$eq(2), 1)$    # Intensifying
  where(hs_r$eq(4), 0.9)$  # Persistent
  where(hs_r$eq(3), 0.8)$  # New
  where(hs_r$eq(5), 0.7)$  # Sporadic
  where(hs_r$eq(1), 0.6)   # Diminishing

map_norm_idx(hs_r, 'Hotspots', palette = pal_idx)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subnational Human Development Index ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shdi <- ee$Image(addm('shdi_2arcmin'))

shdi <- shdi$updateMask(shdi$gt(0))

# Normalize to 0-1 from 0-1
shdi_scale <- shdi$unitScale(0, 1) # The lowest value is around 0.5
shdi_norm <- shdi_scale$
  where(shdi_scale$gt(1.0), 1.0)$ 
  where(shdi_scale$lt(0.0), 0.0)$
  updateMask(c_mask)

# # View
# map_norm_idx(shdi_norm, 'Subnational Human Development Index', palette = pal_idx, show = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Development Potential Indices ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dpi_id <- addm('DPI/crop_dpi_geo_int')
dpi_crop <- ee$Image(dpi_id)
dpicrop_pctl <- rescale_to_pctl(dpi_crop, c(20,99))
dpicrop_pctl <- rescale_to_pctl(dpi_crop, c(0,100))

dpi_id <- addm('DPI/bio_dpi_geo_int')
dpi_bio <- ee$Image(dpi_id)
dpibio_pctl <- rescale_to_pctl(dpi_bio, c(20,99))
dpibio_pctl <- rescale_to_pctl(dpi_bio, c(0,100))

pal <- choose_palette(pal = sequential_hcl)
pal <- pal(7)
pal <- c("#FFCC58", "#FF9370", "#F06289", "#CB3598", "#9D0B9A", "#630391", "#001889")
map_norm_idx(dpicrop_pctl, name = 'Cropland DPI', palette = pal, shown = TRUE) +
  map_norm_idx(dpibio_pctl, name = 'Biofuelds DPI', palette = pal)


map_eq_int_10(dpicrop_pctl, name = 'Cropland DPI', palette = pal) +
  map_eq_int_10(dpibio_pctl, name = 'Biofuelds DPI', palette = pal)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Development Potential Indices ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dti_id <- addm('DTI/DTI_2016_pctls_maskDHF')

dti_norm <- ee$Image(dti_id)$updateMask(c_mask)

# Invert values
dti_inv <- dti_norm$multiply(-1)$add(1)

# Rescale to percentiles
dti_vent <- classify_percentiles(dti_inv)

# # View
# map_norm_idx(dti_norm, palette = pal_idx, 'Average and normalize') +
#   map_norm_idx(dti_inv, palette = pal_idx, 'Invert') +
#   map_norm_idx(dti_vent, palette = pal_idx, 'Percentiles')
# 
# map_eq_int(dti_norm, palette = pal_idx, 'Average and normalize') +
#   map_eq_int(dti_inv, palette = pal_idx, 'Invert') +
#   map_eq_int(dti_vent, palette = pal_idx, 'Percentiles')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EPI ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
epi_fc <- ee$FeatureCollection(addm('EPI_2020_LSIB'))

# Convert to raster
epi_ic <- epi_fc$
  reduceToImage(properties = list('EPI_new'), reducer = ee$Reducer$first())$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)

# Rescale
epi_norm <- rescale_to_pctl(epi_ic)$updateMask(c_mask)

# Ventiles
epi_vent <- classify_percentiles(epi_norm)

# # View
# map_norm_idx(epi_vent, 'EPI', palette = pal_idx, show = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EPI: Ecosystem Vitality ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
epi_fc <- ee$FeatureCollection(addm('EPI_2020_LSIB'))

# Convert to raster
epieco <- epi_fc$
  reduceToImage(properties = list('ECO_new'), reducer = ee$Reducer$first())$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)

# Rescale
eco_norm <- rescale_to_pctl(epieco)$updateMask(c_mask)

# Ventiles
eco_vent <- classify_percentiles(eco_norm)

# # View
# map_norm_idx(eco_vent, 'EPI', palette = pal_idx, show = TRUE)
