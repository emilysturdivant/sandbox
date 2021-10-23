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
library(tidyverse)

# Initialize variables 
viridis <- c('#440154', '#433982', '#30678D', '#218F8B', '#36B677', '#8ED542', '#FDE725')
BlueToRed <- c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c')
OrRd <- c('#fef0d9', '#fdcc8a', '#fc8d59', '#e34a33', '#b30000')
viz_idx_norm <- list(min = 0, max = 1, palette = BlueToRed)
viz_clssfd_idx <- list(min = 10, max = 90, palette = BlueToRed, 
                       values = c('0-20', '20-40', '40-60', '60-80', '80-100'))
viz_pctls_idx <- list(min = 0, max = 90, palette = BlueToRed,
                       values = c('0', '10', '20', '30', '40', '50', '60', '70', '80', '90'))

# Set a region of interest and center map display
region <- ee$Geometry$BBox(43, -22, 50, -20)
Map$centerObject(region, 6)

# Local paths
data_dir <- '/Users/emilysturdivant/data'
final_polys_dir <- '/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/data/hih_sites'

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

# Get 99th percentile
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

# Rescale to 99th percentile
rescale_to_pctl <- function(img, pctl = 99) {
  
  pctl_list <- c(0, 98, 100)
  if (!pctl %in% pctl_list) pctl_list <- sort(c(pctl_list, pctl))
  
  p <- img$
    rename('b1')$
    reduceRegion(
      reducer = ee$Reducer$percentile(pctl_list),
      geometry = tropics_bb,
      bestEffort = TRUE)
  
  pctl_val <- p$get(str_c('b1_p', pctl))$getInfo()
  min <- p$get(str_c('b1_p0'))$getInfo()
  
  # Divide by 99th percentile and reclass values outside of 0-1 range
  # img1 <- img$divide(ee$Image$constant(pctl_val))
  img1 <- img$unitScale(min, pctl_val)
  
  img <- img1$
    where(img1$gt(1.0), 1.0)$
    where(img1$lt(0.0), 0.0)
  
  return( img )
}

# Classify image to 10 equal-area ranked classes
classify_index_quants10 <- function(img) {
  qs <- get_quantiles(img, c(10, 20, 30, 40, 50, 60, 70, 80, 90))
  
  ee$Image(0)$
    where(img$lt(qs$p10), 0)$
    where(img$gte(qs$p10), 10)$
    where(img$gte(qs$p20), 20)$
    where(img$gte(qs$p30), 30)$
    where(img$gte(qs$p40), 40)$
    where(img$gte(qs$p50), 50)$
    where(img$gte(qs$p60), 60)$
    where(img$gte(qs$p70), 70)$
    where(img$gte(qs$p80), 80)$
    where(img$gte(qs$p90), 90)$
    updateMask(img$mask())
}

# Mask and rescale to 0-1
rescale_index_in_list <- function(lst) {
  
  # Get index and mask
  idx <- lst$index$
    updateMask(tropics_r$neq(0))
  
  # Rescale
  idx <- rescale_to_pctl(idx)
  
  # Return as list 
  return( list(name = lst$name, 
               index = idx))
}

# Classify index to equal intervals
classify_eq_int <- function(img) {
  ee$Image(0)$
    where(img$lt(0.2), 0)$
    where(img$gte(0.2), 20)$
    where(img$gte(0.4), 40)$
    where(img$gte(0.6), 60)$
    where(img$gte(0.8), 80)$
    where(img$gte(0.85), 85)$
    where(img$gte(0.9), 90)$
    where(img$gte(0.95), 95)$
    updateMask(img$mask())
}

# Classify to equal-area rank / percentiles
classify_index_quants <- function(img) {
  qs <- get_quantiles(img, c(20, 40, 60, 80))
  
  ee$Image(0)$
    where(img$lt(qs$p20), 10)$
    where(img$gte(qs$p20), 30)$
    where(img$gte(qs$p40), 50)$
    where(img$gte(qs$p60), 70)$
    where(img$gte(qs$p80), 90)$
    updateMask(img$mask())
}

# Apply classification to indices in list 
classify_index_in_list <- function(lst, classification = 'equal') {
  if(classification == 'equal')  {
    lst_out <- list(
      name = lst$name, 
      index = classify_index(lst$index)
    )
  }
  if(classification == 'quantile') {
    lst_out <- list(
      name = str_c(lst$name, ', ', classification, ' breaks'), 
      index = classify_index_quants(lst$index)
    )
  } 
  
  return(lst_out)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tropics extent ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create tropics extent rectangle ----
tropics_bb <- ee$Geometry$Rectangle(
  coords = c(-117, -26, 180, 26),
  proj = "EPSG:4326",
  geodesic = FALSE
)

# Convert to raster for masking ----
# Set property of 1 for the polygon
tropics_r <- ee$FeatureCollection(tropics_bb)$map(function(f) {f$set("tropics", 1)})

# Convert to raster
tropics_r <- tropics_r$
  reduceToImage(
    properties = list('tropics'),
    reducer = ee$Reducer$first()
  )

# # Test
# Map$addLayer(eeObject = tropics_r, visParams = viz_idx_norm, name = "Tropics", opacity = 0.5) +
#   Map$addLayer(eeObject = tropics_bb, name = "Tropics", opacity = 0.5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HIH sites ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
polys_fp <- here::here(final_polys_dir, 'hih_sites_polys.shp')
pts_fp <- here::here(final_polys_dir, 'hih_sites_pts.shp')

# Upload shapefile ----
hih_sites <- st_read(polys_fp) %>% select(-id) %>% sf_as_ee()
fc_pts <- st_read(pts_fp) %>% sf_as_ee()

# Paint all the polygon edges with the same number and width, display.
outline <- ee$Image()$byte()$paint(featureCollection = hih_sites, color = 1, width = 2)
hih_sites_lyr <- Map$addLayer(outline, name = 'HIH sites')
hih_pts_lyr <- Map$addLayer(fc_pts, name = 'HIH sites')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tropical biomes ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load
ecoregions <- ee$FeatureCollection("RESOLVE/ECOREGIONS/2017")

# Filter to tropical and merge
tropics <- ee$FeatureCollection(
  c(
    ecoregions$
      filter(ee$Filter$eq('BIOME_NAME', 'Tropical & Subtropical Coniferous Forests')),
    ecoregions$
      filter(ee$Filter$eq('BIOME_NAME', 'Tropical & Subtropical Dry Broadleaf Forests')),
    ecoregions$
      filter(ee$Filter$eq('BIOME_NAME', 'Tropical & Subtropical Grasslands, Savannas & Shrublands')),
    ecoregions$
      filter(ee$Filter$eq('BIOME_NAME', 'Tropical & Subtropical Moist Broadleaf Forests'))
  )
)$
  flatten()

# Dissolve
tropics <- tropics$union()

# View 
# Map$addLayer(eeObject = tropics, name = "Tropical biomes", opacity = 0.5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Protected Areas ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load
pas <- ee$FeatureCollection("WCMC/WDPA/current/polygons")$
  filter(ee$Filter(c(
    ee$Filter$neq('MARINE', '2'),
    ee$Filter$gt('REP_AREA', 0))))

# pas$size()$getInfo()

# Map$addLayer(eeObject = pas, name = "Protected areas", opacity = 0.5) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Key Biodiversity Areas ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load and convert to raster where KBA = 0.5 and AZE = 1
kbas <- ee$FeatureCollection(addm("KBAsGlobal_2021_September_02_POL"))

# Set sig_level to 1 or 0.9 to indicate significance level of site
aze_true <- kbas$
  filter(ee$Filter$eq('AzeStatus', 'confirmed'))$
  map(function(f) {f$set("sig_level", 1)})
aze_false <- kbas$
  filter(ee$Filter$neq('AzeStatus', 'confirmed'))$
  map(function(f) {f$set("sig_level", 0.75)})

# Merge the subsets
kbas <- aze_true$merge(aze_false)

# Convert to raster
kba_r <- kbas$
  reduceToImage(
    properties = list('sig_level'), 
    reducer = ee$Reducer$first()
  )

# View
# Map$addLayer(eeObject = kba_r, visParams = viz_idx_norm, name = "Key Biodiversity Areas")

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
flii <- flii$map(function(img) {img$updateMask(img$neq(-9999))})

# Mosaic
flii <- flii$mosaic()$
  setDefaultProjection(crs = 'EPSG:4326', scale = 300)

# get_pctl(flii) # 99th: 9995.569

# Scale values to 0-1 scale
flii <- rescale_to_pctl(flii)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Total biomass density ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Total biomass density
biomass_mgha <- ee$Image("users/sgorelik/WHRC_Global_Biomass_500m_V6/Current_AGB_BGB_SOC_Mgha")

# Convert to carbon density
carbon_mgcha <- biomass_mgha$divide(2)

# # Stretch and scale values to 0-1 scale, using 98th percentile as highest
# carbon_idx <- carbon_mgcha$divide(carbon_info$upper98)
# 
# # Reclass values above 1 to 1
# carbon_idx <- carbon_idx$where(carbon_idx$gt(1), 1)

get_pctl(carbon_mgcha) # 679.68 MgC/ha

carbon_idx <- rescale_to_pctl(carbon_mgcha)
# Map$addLayer(eeObject = carbon_idx, visParams = viz_idx_norm)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Human modification ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load
gHM <- ee$ImageCollection("CSP/HM/GlobalHumanModification")$
  first()$ # only image is '2016'
  rename('b1')

# Rescale
get_pctl(gHM) # 0.76
gHM <- rescale_to_pctl(gHM)

# View
# Map$addLayer(
#   eeObject = gHM, 
#   visParams = list(min = 0, max = 1, palette = c('0c0c0c', '071aff', 'ff0000', 'ffbd03', 'fbff05', 'fffdfd')),
#   name = "Human modification"
# )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Development Threat Index ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
dti <- ee$Image(addm("development-threat-index_geographic"))

# Mask each image
dti <- dti$updateMask(dti$neq(0))

# Rescale
dti <- rescale_to_pctl(dti)

# Test mask
# dti_m <- dti$updateMask(tropics_r$neq(0))
# Map$addLayer(eeObject = dti, visParams = viz_idx_norm) +
#   Map$addLayer(eeObject = dti_m, visParams = viz_idx_norm) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subnational Infant Mortality Rate ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
infant_mort <- ee$Image(addm("subnational_infant_mortality_rates_v2_01"))

# Mask 
infant_mort <- infant_mort$updateMask(infant_mort$gte(0))

# Rescale
# get_pctl(infant_mort) # 99.7
infant_mort <- rescale_to_pctl(infant_mort)
# Map$addLayer(eeObject = infant_mort, visParams = viz_idx_norm)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DALYs ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dalys_sf <- here::here(gbd_dir, 'processed', 'DALYs_2019.shp')
dalys_fc <- ee$FeatureCollection(addm('DALYs_2019_tropics'))

# Convert to raster
dalys <- dalys_fc$
  reduceToImage(
    properties = list('val'), 
    reducer = ee$Reducer$first()
  )$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)

# Rescale
# get_pctl(dalys, 99) # 83,090 disability-adjusted life years per 100,000 people
# get_pctl(dalys, 0) # 15,706 disability-adjusted life years per 100,000 people
dalys_norm <- rescale_to_pctl(dalys, 99)

# Rescale to Percentiles 
dalys_ea <- classify_index_quants10(dalys)

# # View
# Map$addLayer(eeObject = dalys, visParams = viz_idx_norm, name = "DALYs") +
#   Map$addLayer(eeObject = dalys_ea, visParams = viz_pctls_idx, name = "DALYs")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GDL Health indicators ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hdi_fc <- ee$FeatureCollection(addm('GDL_subnational_hdi_le_hi'))

# Convert to raster
le <- hdi_fc$
  reduceToImage(
  properties = list('LE'), 
  reducer = ee$Reducer$first()
  )$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)

# Rescale
# get_pctl(le, 1) # 51.9 years is the 1st percentile and 47.75 is the min
le <- rescale_to_pctl(le, 100)

# Invert values
le <- le$multiply(-1)$add(1)

# View
# Map$addLayer(eeObject = le, visParams = viz_idx_norm, name = "Life expectancy")

hi <- hdi_fc$reduceToImage(properties = list('HI'), reducer = ee$Reducer$first())
hdi <- hdi_fc$reduceToImage(properties = list('shdi'), reducer = ee$Reducer$first())

Map$addLayer(eeObject = hdi,
             visParams = list(min = 0, max = 1, palette = viridis), 
             name = "SHDI")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Accessibility to Healthcare ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
hc_access <- ee$Image("Oxford/MAP/accessibility_to_healthcare_2019")$
  select('accessibility_walking_only')$
  rename('b1')

# Rescale
# get_pctl(hc_access,  95) # 6380 min
hc_access <- rescale_to_pctl(hc_access, 95)
# Map$addLayer(eeObject = hc_access, visParams = viz_idx_norm)

# Load 
hc_motor <- ee$Image("Oxford/MAP/accessibility_to_healthcare_2019")$
  select('accessibility')$
  rename('b1')

# Rescale
# get_pctl(hc_motor,  95)
hc_motor <- rescale_to_pctl(hc_motor, 95)
# Map$addLayer(eeObject = hc_motor, visParams = viz)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Human footprint (from Wild Areas v3) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
hf <- ee$Image(addm("wildareas-v3-2009-human-footprint"))

# Rescale
# get_pctl(hf)
hf <- rescale_to_pctl(hf)
# Map$addLayer(eeObject = hf, visParams = viz_idx_norm)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Zoonotic spillover risk (from Allen et al. 2017) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
zoonotic_risk_all <- ee$Image(addm("zoonotic_eid_risk"))$
  setDefaultProjection(crs = 'EPSG:4326', scale = 100000)$
  resample() # Downsample zoonotic risk to smooth for display

# raw model output
zs_response <- zoonotic_risk_all$select('b1')$rename('b1') 
zs_resp_q10 <- classify_index_quants10(zs_response)
zs_resp_q10 <- zs_resp_q10$updateMask(tropics_r$neq(0))

# weighted by publications, not reweighted by population
zs_weight_pubs <- zoonotic_risk_all$select('b2')$rename('b1') 
zs_wpubs_q10 <- classify_index_quants10(zs_weight_pubs)
zs_wpubs_q10 <- zs_wpubs_q10$updateMask(tropics_r$neq(0))

# Reweighted by population
zs_weight_pop <- zoonotic_risk_all$select('b3')$rename('b1')
zs_wpop_q10 <- classify_index_quants10(zs_weight_pop)
zs_wpop_q10 <- zs_wpop_q10$updateMask(tropics_r$neq(0))

# # View
# viz_pctls_idx <- list(min = 0, max = 90, palette = BlueToRed, 
#                        values = c('0', '10', '20', '30', '40', '50', '60', '70', '80', '90'))
# Map$addLayer(eeObject = zs_wpop_q10, visParams = viz_pctls_idx) +
#   Map$addLayer(eeObject = zs_wpubs_q10, visParams = viz_pctls_idx) +
#   Map$addLayer(eeObject = zs_resp_q10, visParams = viz_pctls_idx) +
#   Map$addLegend( visParams = viz_pctls_idx, color_map = 'character', name = 'Percentile')

# # Create masks ----
# # Get only FLII > 0.96
# flii96 <- flii$updateMask(flii$gte(0.96))
# Map$addLayer(
#   eeObject = flii96,
#   visParams = viz_idx_norm,
#   name = "Sum"
# )
# 
# # Get only Human mod < 0.5
# gHM_lt50 <- gHM$updateMask(gHM$lt(0.5))
# Map$addLayer(
#   eeObject = gHM_lt50,
#   visParams = viz_idx_norm,
#   name = "Sum"
# )
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Forest Status (from WRI) ----
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Load 
# forest_stat <- ee$Image(addm("wri_foreststatus"))
# 
# # Create mask of potential closed forest: values 3, 6, 8, 9, 14 
# potential_forest <- forest_stat$remap(from = c(3, 6, 8, 9, 14,
#                                                1, 2, 4, 5, 7, 10, 11, 12, 13), 
#                                       to =   c(1, 1, 1, 1, 1,
#                                                0, 0, 0, 0, 0, 0, 0, 0, 0))
# potential_forest <- potential_forest$updateMask(potential_forest$neq(0))
# potential_forest_mask <- potential_forest$mask()
# 
# # Create mask of currently closed forest: values 2, 3, 9, 13, 14 
# # Intact closed forest: 3
# # Intact open forests: 2
# # Partially deforested closed forests: 9
# # Fragmented/managed closed forests: 14
# forest_stat <- forest_stat$remap(from = c(2, 9, 14, 3, 
#                                           8, 1, 6, 4, 5, 7, 10, 11, 12, 13), 
#                                  to =   c(1, 2, 3, 4, 
#                                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0))$
#   rename('b1')
# forest_stat <- forest_stat$updateMask(forest_stat$neq(0))
# 
# # Scale values to 0-1 scale
# forest_stat <- forest_stat$divide(fs_info$max)
# 
# # View
# Map$addLayer(
#   eeObject = potential_forest,
#   visParams = viz_idx_norm,
#   name = "Forest status"
# )
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Hansen tree cover ----
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Load 
# tree_cover <- ee$Image("UMD/hansen/global_forest_change_2020_v1_8")
# bands <- c('treecover2000', 'loss', 'gain', 'lossyear', 'datamask')
# 
# treecover2020 <- tree_cover$select('treecover2000')$
#   updateMask(tree_cover$select('loss')$selfMask())$
#   rename('b1')
# 
# # Get original CRS, resolution, and value distribution
# pctls <- treecover2020$
#   reduceRegion(reducer = ee$Reducer$percentile(c(0, 2, 50, 98, 100)),
#                geometry = tropics_bb,
#                bestEffort = TRUE)$
#   getInfo()
# tc_info <- list(
#   crs = treecover2020$projection()$getInfo()$crs, 
#   res = treecover2020$projection()$nominalScale()$getInfo(),
#   upper98 = signif(pctls$b1_p98, digits = 1),
#   max = pctls$b1_p100
# )
# 
# # Scale values to 0-1 scale
# treecover2020 <- treecover2020$divide(tc_info$max)
# 
# # View
# # Map$addLayer(
# #   eeObject = treecover2020,
# #   visParams = viz_idx_norm,
# #   name = "Tree cover"
# # )
