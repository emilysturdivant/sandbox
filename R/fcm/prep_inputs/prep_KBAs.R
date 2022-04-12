#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Prep Key Biodiversity Areas for input to FCI
# Requires:
#     * 
# Author:
#     * esturdivant@woodwellclimate.org, 2022-04-11
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here::here('R/fcm/0_initialize.R'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# KBAs ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kba_id <- addm('KBAs_2021_Sep02')

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
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)

# Save image as EE asset
task_img <- ee_image_to_asset(kba_r,
                              basename(kba_id), 
                              assetId = kba_id,
                              crs = 'EPSG:4326',
                              scale = 1000,
                              maxPixels = 803042888,
                              overwrite = TRUE)
task_img$start()
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# WDPA ----
# IUCN_CAT = IUCN management category, one of: 
#   Ia (strict nature reserve), 
#   Ib (wilderness area), 
#   II (national park), 
#   III (natural monument or feature), 
#   IV (habitat/species management area), 
#   V (protected landscape/seascape), 
#   VI (PA with sustainable use of natural resources), 
#   not applicable, not assigned, or not reported.
# STATUS = Status of a PA, one of: 
#   proposed, inscribed, adopted, designated, or established. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pas_id <- addm('WDPA_polys_protected')
  
# Load PA polygons
pa_polys <- ee$FeatureCollection('WCMC/WDPA/current/polygons')
pa_pts <- ee$FeatureCollection('WCMC/WDPA/current/polygons')

# Set sig_level to 1 or 0.9 to indicate significance level of site
not_marine <- pa_polys$
  filter(ee$Filter$neq('MARINE', '2'))$
  filter(ee$Filter$neq('PA_DEF', '0'))$
  filter(ee$Filter$neq('IUCN_CAT', 'VI'))

# Filter by STATUS (seem to consistently use title case)
pas_filt <- not_marine$ 
  filter(ee$Filter$neq('STATUS', 'Proposed'))$
  filter(ee$Filter$neq('STATUS', 'Not Reported'))$
  map(function(f) {f$set("protection", 1)})

# pas_filt$filter(ee$Filter$eq('STATUS', 'Not Reported'))$first()$getInfo()

# # Reclassify by STATUS
# proposed <- not_marine$ 
#   filter(ee$Filter$eq('STATUS', 'proposed'))$
#   map(function(f) {f$set("protection", 0.5)})
# 
# established <- not_marine$ 
#   filter(ee$Filter$eq('STATUS', 'not reported'))$
#   map(function(f) {f$set("protection", 1)})
# 
# not_reported <- not_marine$ 
#   filter(ee$Filter$eq('STATUS', 'Not Reported'))$
#   map(function(f) {f$set("protection", 1)})
# 
# inscribed <- not_marine$ 
#   filter(ee$Filter$eq('STATUS', 'inscribed'))$
#   map(function(f) {f$set("protection", 1)})
# 
# adopted <- not_marine$ 
#   filter(ee$Filter$eq('STATUS', 'adopted'))$
#   map(function(f) {f$set("protection", 1)})
# 
# designated <- not_marine$ 
#   filter(ee$Filter$eq('STATUS', 'designated'))$
#   map(function(f) {f$set("protection", 1)})
# 
# # Merge the subsets
# not_marine <- inscribed$merge(adopted)$merge(designated)$merge(established)

# Convert to raster
pas_r <- pas_filt$
  reduceToImage(
    properties = list('protection'), 
    reducer = ee$Reducer$first()
  )$
  unmask()$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)

# Look
viz <- list(min = 0, max = 1, palette = c('red', 'black'))
Map$addLayer(pas_r, viz, 'PAs')

# Save image as EE asset
task_img <- ee_image_to_asset(pas_r,
                              basename(pas_id), 
                              assetId = pas_id,
                              crs = 'EPSG:4326',
                              scale = 1000,
                              maxPixels = 803042888,
                              overwrite = TRUE)
task_img$start()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# KBAs ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kba_id <- addm('KBAs_2021_Sep02_maskDHF')

alist <- ee_manage_assetlist(path_asset = addm(""))
if(!kba_id %in% alist$ID) {
  
  print("Complete previous section")
  
}

kba_r <- ee$Image(kba_id)

# Reclass values outside of 0-1 range
kba_r <- kba_r$
  where(kba_r$lt(0.94), 0.8)$
  where(kba_r$lt(0.96), 0.9)

# View
# map_norm_idx(kba_r, "Key Biodiversity Areas", TRUE)
