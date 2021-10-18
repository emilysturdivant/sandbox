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
BlueToRed <- c('#0571b0', '#92c5de', '#f7f7f7', '#f4a582', '#ca0020')
indexViz <- list(min = 0, max = 1, palette = viridis)

# Set a region of interest and center map display
region <- ee$Geometry$BBox(43, -22, 50, -20)
Map$centerObject(region, 6)

# Prep to get paths to assets
user <- ee_get_assethome()

data_dir <- '/Users/emilysturdivant/data'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Functions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
addm <- function(x) sprintf("%s/%s", user, x)

# Get quantiles
get_quantiles <- function(img) {
  
  quants <- img$
    rename('b1')$
    reduceRegion(
      reducer = ee$Reducer$percentile(c(0, 25, 50, 75, 100)),
      geometry = tropics_bb,
      bestEffort = TRUE)$
    getInfo()
  
  quants %>% setNames( str_remove_all(names(quants), 'b1_') )
}

# Get 99th percentile
get_pctl <- function(img, geometry, pctl = 99) {
  
  p <- img$
    rename('b1')$
    reduceRegion(
      reducer = ee$Reducer$percentile(c(98, pctl)),
      geometry = geometry,
      bestEffort = TRUE)$
    get(str_c('b1_p', pctl))$
    getInfo()
  
  return( p )
}

# Rescale to 99th percentile
rescale_to_pctl <- function(img, geometry, pctl = 99) {
  
  # Get 99th percentile
  pctl_val <- get_pctl(img, geometry, pctl)
  
  # Divide by 99th percentile and reclass values outside of 0-1 range
  img1 <- img$divide(ee$Image$constant(pctl_val))
  
  img <- img1$
    where(img1$gt(1.0), 1.0)$
    where(img1$lt(0.0), 0.0)
  
  return( img )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tropics extent ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Basic tropics
# e <- terra::ext(c(xmin = -180, xmax = 180, ymin = -23.3, ymax = 23.3))
# bb <- c(e$xmin, e$ymin, e$xmax, e$ymax)
# tropics_rect <- st_as_sfc(st_bbox(bb, crs = st_crs(4326)))
# 
# tropics_rect_shp <- file.path(data_dir, 'context', 'tropics_rect.shp')
# if(!file.exists(tropics_rect_shp)) {
#   tropics_rect %>% st_write(tropics_rect_shp)
# }
# 
# # Expanded tropics
# e <- terra::ext(c(xmin = -180, xmax = 180, ymin = -26, ymax = 26))
# bb <- c(e$xmin, e$ymin, e$xmax, e$ymax)
# tropics_rect <- sf::st_as_sfc(st_bbox(bb, crs = st_crs(4326)))
# 
# tropics_rect_shp <- file.path(data_dir, 'context', 'tropics_26.shp')
# if(!file.exists(tropics_rect_shp)) {
#   tropics_rect %>% st_write(tropics_rect_shp)
# }

# Get FC
tropics_bb <- ee$FeatureCollection(addm('tropics_26'))

# Convert to tropics mask
# Set property of 1 for the polygon
tropics_r <- tropics_bb$
  map(function(f) {f$set("tropics", 1)})

# Convert to raster
tropics_r <- tropics_r$
  reduceToImage(
    properties = list('tropics'), 
    reducer = ee$Reducer$first()
  )

# # Test
# Map$addLayer(eeObject = tropics_r, visParams = indexViz, name = "Tropics", opacity = 0.5) +
#   Map$addLayer(eeObject = tropics_bb, name = "Tropics", opacity = 0.5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HIH sites ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine FCs for all sites
tdm <- ee$FeatureCollection("users/esturdivant/HIH_sites/TdM_all_dissolved")
bbbr <- ee$FeatureCollection("users/esturdivant/HIH_sites/BBBR_divisions_v2")
gpnp <- ee$FeatureCollection("users/esturdivant/HIH_sites/GPNP_Border")
manombo <- ee$FeatureCollection("users/esturdivant/HIH_sites/ManomboNP_SRGpolys")

hih_sites <- ee$FeatureCollection(c(tdm, bbbr, gpnp, manombo))$flatten()

# Paint all the polygon edges with the same number and width, display.
outline <- ee$Image()$byte()$
  paint(featureCollection = hih_sites, color = 1, width = 2)
hih_sites_lyr <- Map$addLayer(outline, name = 'HIH sites')

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
# Map$addLayer(eeObject = kba_r, visParams = indexViz, name = "Key Biodiversity Areas")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Forest Landscape Integrity Index ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ee_manage_assetlist(path_asset = addm("flii"))

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

# Scale values to 0-1 scale
flii <- rescale_to_pctl(flii, tropics_bb)

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

carbon_idx <- rescale_to_pctl(carbon_mgcha, tropics_bb)
# Map$addLayer(eeObject = carbon_idx, visParams = indexViz)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Human modification ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load
gHM <- ee$ImageCollection("CSP/HM/GlobalHumanModification")$
  first()$ # only image is '2016'
  rename('b1')

# Rescale
gHM <- rescale_to_pctl(gHM, tropics_bb)

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
dti <- rescale_to_pctl(dti, tropics_bb)

# Test mask
# dti_m <- dti$updateMask(tropics_r$neq(0))
# Map$addLayer(eeObject = dti, visParams = indexViz) +
#   Map$addLayer(eeObject = dti_m, visParams = indexViz) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subnational Infant Mortality Rate ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
infant_mort <- ee$Image(addm("subnational_infant_mortality_rates_v2_01"))

# Mask 
infant_mort <- infant_mort$updateMask(infant_mort$gte(0))

# Rescale
infant_mort <- rescale_to_pctl(infant_mort, tropics_bb)
# Map$addLayer(eeObject = infant_mort, visParams = indexViz)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Accessibility to Healthcare ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
hc_access <- ee$Image("Oxford/MAP/accessibility_to_healthcare_2019")$
  select('accessibility_walking_only')$
  rename('b1')

# Rescale
get_pctl(hc_access, tropics_bb,  95)
hc_access <- rescale_to_pctl(hc_access, tropics_bb, 95)
# Map$addLayer(eeObject = hc_access, visParams = indexViz)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Human footprint (from Wild Areas v3) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
hf <- ee$Image(addm("wildareas-v3-2009-human-footprint"))

# Rescale
hf <- rescale_to_pctl(hf, tropics_bb)
# Map$addLayer(eeObject = hf, visParams = indexViz)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Zoonotic spillover risk (from Allen et al. 2017) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
zoonotic_risk <- ee$Image(addm("zoonotic_eid_risk"))$
  select('b3')$
  rename('b1')$
  setDefaultProjection(crs = 'EPSG:4326', scale = 100000)

# Rescale
zoonotic_risk <- rescale_to_pctl(zoonotic_risk, tropics_bb)

# Downsample zoonotic risk to smooth
zoonotic_risk <- zoonotic_risk$resample()

# Map$addLayer(eeObject = zoonotic_risk, visParams = indexViz)




# # Create masks ----
# # Get only FLII > 0.96
# flii96 <- flii$updateMask(flii$gte(0.96))
# Map$addLayer(
#   eeObject = flii96,
#   visParams = indexViz,
#   name = "Sum"
# )
# 
# # Get only Human mod < 0.5
# gHM_lt50 <- gHM$updateMask(gHM$lt(0.5))
# Map$addLayer(
#   eeObject = gHM_lt50,
#   visParams = indexViz,
#   name = "Sum"
# )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Forest Status (from WRI) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
forest_stat <- ee$Image(addm("wri_foreststatus"))

# Create mask of potential closed forest: values 3, 6, 8, 9, 14 
potential_forest <- forest_stat$remap(from = c(3, 6, 8, 9, 14,
                                               1, 2, 4, 5, 7, 10, 11, 12, 13), 
                                      to =   c(1, 1, 1, 1, 1,
                                               0, 0, 0, 0, 0, 0, 0, 0, 0))
potential_forest <- potential_forest$updateMask(potential_forest$neq(0))
potential_forest_mask <- potential_forest$mask()

# Create mask of currently closed forest: values 2, 3, 9, 13, 14 
# Intact closed forest: 3
# Intact open forests: 2
# Partially deforested closed forests: 9
# Fragmented/managed closed forests: 14
forest_stat <- forest_stat$remap(from = c(2, 9, 14, 3, 
                                          8, 1, 6, 4, 5, 7, 10, 11, 12, 13), 
                                 to =   c(1, 2, 3, 4, 
                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0))$
  rename('b1')
forest_stat <- forest_stat$updateMask(forest_stat$neq(0))

# Scale values to 0-1 scale
forest_stat <- forest_stat$divide(fs_info$max)

# View
Map$addLayer(
  eeObject = potential_forest,
  visParams = indexViz,
  name = "Forest status"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hansen tree cover ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
tree_cover <- ee$Image("UMD/hansen/global_forest_change_2020_v1_8")
bands <- c('treecover2000', 'loss', 'gain', 'lossyear', 'datamask')

treecover2020 <- tree_cover$select('treecover2000')$
  updateMask(tree_cover$select('loss')$selfMask())$
  rename('b1')

# Get original CRS, resolution, and value distribution
pctls <- treecover2020$
  reduceRegion(reducer = ee$Reducer$percentile(c(0, 2, 50, 98, 100)),
               geometry = tropics_bb,
               bestEffort = TRUE)$
  getInfo()
tc_info <- list(
  crs = treecover2020$projection()$getInfo()$crs, 
  res = treecover2020$projection()$nominalScale()$getInfo(),
  upper98 = signif(pctls$b1_p98, digits = 1),
  max = pctls$b1_p100
)

# Scale values to 0-1 scale
treecover2020 <- treecover2020$divide(tc_info$max)

# View
# Map$addLayer(
#   eeObject = treecover2020,
#   visParams = indexViz,
#   name = "Tree cover"
# )
