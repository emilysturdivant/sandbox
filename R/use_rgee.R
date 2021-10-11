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
library(tidyverse)

# Initialize variables 
viridis <- c('#440154', '#433982', '#30678D', '#218F8B', '#36B677', '#8ED542', '#FDE725')

# Set a region of interest and center map display
region <- ee$Geometry$BBox(43, -22, 50, -20)
Map$centerObject(region, 6)

# Prep to get paths to assets
user <- ee_get_assethome()
addm <- function(x) sprintf("%s/%s",user, x)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tropics ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tropics_bb <- ee$FeatureCollection('users/esturdivant/tropics_wide')
ee_print(tropics_bb)

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

# View 
Map$addLayer(eeObject = tropics, name = "Tropical biomes", opacity = 0.5) 

# Dissolve
tropics <- tropics$union()

# View 
Map$addLayer(eeObject = tropics, name = "Tropical biomes", opacity = 0.5) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Protected Areas ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load
pas <- ee$FeatureCollection("WCMC/WDPA/current/polygons")

Map$addLayer(eeObject = pas, name = "Protected areas", opacity = 0.5) 

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
  setDefaultProjection(crs = 'EPSG:4326')

# Scale values to 0-1 scale
flii <- flii$divide(10000)

flii$getInfo()

# Get original CRS, resolution, and area (ha)
(flii_crs <- flii$projection()$getInfo()$crs )
(flii_res <- flii$projection()$nominalScale()$getInfo())

flii_pctls <- flii$
  # reproject(crs = 'SR-ORG:6974', scale = 463.3127165275)$
  reduceRegion(
    reducer = ee$Reducer$percentile(c(0, 2, 50, 98, 100)),
    geometry = region,
    bestEffort = TRUE
  )$getInfo()

(upperlim <- signif(flii_pctls$b1_p98, digits = 2))

# Reproject

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

# Get original CRS, resolution, and value distribution
pctls <- forest_stat$
  reduceRegion(reducer = ee$Reducer$percentile(c(0, 2, 50, 98, 100)),
               geometry = tropics_bb,
               bestEffort = TRUE)$
  getInfo()
(fs_info <- list(
  crs = forest_stat$projection()$getInfo()$crs, 
  res = forest_stat$projection()$nominalScale()$getInfo(),
  upper98 = signif(pctls$b1_p98, digits = 1),
  max = pctls$b1_p100
))

# Scale values to 0-1 scale
forest_stat <- forest_stat$divide(fs_info$max)

# View
Map$addLayer(
  eeObject = forest_stat, 
  visParams = list(min = 0, max = 1, palette = viridis),
  name = "Forest status"
) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hansen tree cover ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
tree_cover <- ee$Image("UMD/hansen/global_forest_change_2020_v1_8")
bands <- c('treecover2000', 'loss', 'gain', 'lossyear', 'datamask')

# Get original CRS, resolution, and value distribution
pctls <- tree_cover$
  reduceRegion(reducer = ee$Reducer$percentile(c(0, 2, 50, 98, 100)),
               geometry = tropics_bb,
               bestEffort = TRUE)$
  getInfo()
(fs_info <- list(
  crs = tree_cover$projection()$getInfo()$crs, 
  res = tree_cover$projection()$nominalScale()$getInfo(),
  upper98 = signif(pctls$b1_p98, digits = 1),
  max = pctls$b1_p100
))

# Scale values to 0-1 scale
tree_cover <- tree_cover$divide(fs_info$max)

# View
Map$addLayer(
  eeObject = tree_cover, 
  visParams = list(min = 0, max = 1, palette = viridis),
  name = "Tree cover"
) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Total biomass density ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Total biomass density
biomass_mgha <- ee$Image("users/sgorelik/WHRC_Global_Biomass_500m_V6/Current_AGB_BGB_SOC_Mgha")

# Convert to carbon density
carbon_mgcha <- biomass_mgha$divide(2)

# Get original CRS, resolution, and area (ha)
ee_crs <- biomass_mgha$projection()$getInfo()$crs # should be 'SR-ORG:6974'
res <- biomass_mgha$projection()$nominalScale()$getInfo() # should be 463.3127165275
pxl_ha <- (res*res)/(1e4)
print(str_c('Pixel area (ha): ', pxl_ha)) # should be 21.4658673293776

# Aggregate/resample from 500m to 1km
carbon_mgcha_1km <- carbon_mgcha$
  
  # Explicitly specify the layer in MODIS projection and resolution
  # reproject(crs = 'SR-ORG:6974', scale = 463.3127165275)$
  setDefaultProjection(crs = ee_crs, scale = res)$
  
  # Specify that you want to sum 500m carbon stock
  # values within each of the 1km output pixels
  reduceResolution(reducer = ee$Reducer$mean(), maxPixels = 5)$
  
  # Finally, specify the output spatial resolution (~500m)
  reproject(crs = ee_crs, scale = 1000)

# # See if it makes a different to set projection before reducing
# carbon_mgcha_1km_v2 <- carbon_mgcha$
#   reduceResolution(reducer = ee$Reducer$mean(), maxPixels = 5)$
#   reproject(crs = ee_crs, scale = 1000)

# Get percentiles for plotting
pctls <- carbon_mgcha_1km$reduceRegion(
  reducer = ee$Reducer$percentile(c(0, 2, 50, 98, 100)),
  geometry = region,
  bestEffort = TRUE
)$getInfo()

(upperlim <- signif(pctls$b1_p98, digits = 2))

# View ----
# View 500 m and 1 km total C density
viz_mgcha <- list(min = 1, max = upperlim, palette = viridis)
Map$addLayer(
  eeObject = carbon_mgcha, visParams = viz_mgcha,
  name = "Total carbon density (MgC/ha)"
) + 
  Map$addLayer(
    eeObject = carbon_mgcha_1km, visParams = viz_mgcha,
    name = "Total carbon density (MgC/ha) 1km"
  ) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Human modification ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load
gHM <- ee$ImageCollection("CSP/HM/GlobalHumanModification")$first() # only image is '2016'
ee_print(gHM)

# Get original CRS, resolution, and area (ha)
(hm_crs <- gHM$projection()$getInfo()$crs )
(hm_res <- gHM$projection()$nominalScale()$getInfo())
hm_pctls <- gHM$
  reproject(crs = 'SR-ORG:6974', scale = 463.3127165275)$
  reduceRegion(
    reducer = ee$Reducer$percentile(c(0, 2, 50, 98, 100)),
    geometry = region,
    bestEffort = TRUE
  )$getInfo()

(upperlim <- signif(hm_pctls$gHM_p98, digits = 2))


# View
Map$addLayer(
  eeObject = gHM, 
  visParams = list(min = 0, max = 1, palette = c('0c0c0c', '071aff', 'ff0000', 'ffbd03', 'fbff05', 'fffdfd')),
  name = "Human modification"
) +
  Map$addLayer(
    eeObject = flii, 
    visParams = list(min = 0, max = 1, palette = viridis),
    name = "FLII"
  ) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Development Threat Index ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
dti <- ee$Image(addm("development-threat-index_geographic"))

# Mask each image
dti <- dti$updateMask(dti$neq(0))

# Scale values to 0-1 scale
dti <- dti$divide(4)

# Get original CRS, resolution, and value distribution
dti_pctls <- dti$
  reduceRegion(reducer = ee$Reducer$percentile(c(0, 2, 50, 98, 100)),
               geometry = tropics_bb, bestEffort = TRUE)$
  getInfo()
(dti_info <- list(
  crs = dti$projection()$getInfo()$crs, 
  res = dti$projection()$nominalScale()$getInfo(),
  upper98 = signif(dti_pctls$b1_p98, digits = 1)
  ))

# View
Map$addLayer(
  eeObject = dti, 
  visParams = list(min = 0, max = 1, palette = viridis),
  name = "DTI"
) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subnational Infant Mortality Rate ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
infant_mort <- ee$Image(addm("subnational_infant_mortality_rates_v2_01"))

# Mask 
infant_mort <- infant_mort$updateMask(infant_mort$gte(0))

# Get original CRS, resolution, and value distribution
pctls <- infant_mort$
  reduceRegion(reducer = ee$Reducer$percentile(c(0, 2, 50, 98, 100)),
               geometry = tropics_bb, bestEffort = TRUE)$
  getInfo()
(imr_info <- list(
  crs = infant_mort$projection()$getInfo()$crs, 
  res = infant_mort$projection()$nominalScale()$getInfo(),
  upper98 = signif(pctls$b1_p98, digits = 1),
  max = pctls$b1_p100
))

# Scale values to 0-1 scale
infant_mort <- infant_mort$divide(imr_info$max)

# View
Map$addLayer(
  eeObject = infant_mort, 
  visParams = list(min = 0, max = 1, palette = viridis),
  name = "Infant mortality rate"
) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subnational Infant Mortality Rate ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
hc_access <- ee$Image("Oxford/MAP/accessibility_to_healthcare_2019")$
  select('accessibility')$
  rename('b1')

# Get original CRS, resolution, and value distribution
pctls <- hc_access$
  reduceRegion(reducer = ee$Reducer$percentile(c(0, 2, 50, 98, 100)),
               geometry = tropics_bb, bestEffort = TRUE)$
  getInfo()
(hc_info <- list(
  crs = hc_access$projection()$getInfo()$crs, 
  res = hc_access$projection()$nominalScale()$getInfo(),
  upper98 = signif(pctls$b1_p98, digits = 1),
  max = pctls$b1_p100
))

# Scale values to 0-1 scale
hc_access <- hc_access$divide(hc_info$upper98)

# View
Map$addLayer(
  eeObject = hc_access, 
  visParams = list(min = 0, max = 1, palette = viridis),
  name = "Accessibility to Healthcare"
) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Human footprint (from Wild Areas v3) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
hf <- ee$Image(addm("wildareas-v3-2009-human-footprint"))

# Get original CRS, resolution, and value distribution
pctls <- hf$
  reduceRegion(reducer = ee$Reducer$percentile(c(0, 2, 50, 98, 100)),
               geometry = tropics_bb, bestEffort = TRUE)$
  getInfo()
(hf_info <- list(
  crs = hf$projection()$getInfo()$crs, 
  res = hf$projection()$nominalScale()$getInfo(),
  upper98 = signif(pctls$b1_p98, digits = 1),
  max = pctls$b1_p100
))

# Scale values to 0-1 scale
hf <- hf$divide(hf_info$max)

# View
Map$addLayer(
  eeObject = hf, 
  visParams = list(min = 0, max = 1, palette = viridis),
  name = "Human footprint"
) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Zoonotic spillover risk (from Allen et al. 2017) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
zoonotic_risk <- ee$Image(addm("zoonotic_eid_risk.tif"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
idx <- flii$add(gHM)

idx_pctls <- idx$
  reduceRegion(
    reducer = ee$Reducer$percentile(c(0, 2, 50, 98, 100)),
    geometry = region,
    bestEffort = TRUE
  )$getInfo()

(upperlim <- signif(idx_pctls$b1_p98, digits = 2))


Map$addLayer(
  eeObject = idx, 
  visParams = list(min = 0, max = upperlim, palette = viridis),
  name = "Sum"
) 
