#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Combine inputs (prepped in 01...R) to create index options
# Requires:
#     * 01_use_rgee.R
#     * GEE account
# Author:
#     * esturdivant@woodwellclimate.org, 2021-10-22
# Differences from v1:
#     * reduce influence of KBAs
#     * different version of zoonotic spillover
#     * remove Healthcare accessibility
#     * add different health metric...
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source('R/where_next/01_use_rgee.R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine indicators in ventiles ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create components
i_forestbio <- flii_norm$multiply(0.45)$
  add(wcd_vent$multiply(0.225))$
  add(soc_vent$resample()$multiply(0.225))$
  add(kba_r$multiply(0.1))

i_humz <- imr_vent$multiply(0.33)$
  add(dti_vent$multiply(0.33))$
  add(zs_wpop_vent$multiply(0.33))$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)

# Create composite indicator
vent80 <- i_forestbio$multiply(0.8)$add(i_humz$multiply(0.2))
vent80_norm <- vent80 %>% rescale_to_pctl()

# Transform composite to ventiles
vent80_vents <- classify_ventiles(vent80)
vent80_dents <- classify_dentiles(vent80)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Look at it all together ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set center
Map$setCenter(30, 0, zoom = 3)
Map$setCenter(-53, -5, zoom = 5) # Brazil
Map$setCenter(46, -21, zoom = 7) # Manombo
Map$setCenter(112, 0, zoom = 7) # Borneo
Map$setCenter(136, -2, zoom = 6)  # Papua

# Get legend 
lgnd_80 <- lgnd_top_ventiles(80)
lgnd80_3clas <- lgnd_top_pctls_3class()
lgnd70_4clas <- lgnd_top_pctls()

# Input layers
map_norm_idx(zs_wpop_vent, 'Zoonotic spillover risk') +
  map_norm_idx(carbon_vent, 'Carbon ventiles') +
  map_norm_idx(wcd_idx, 'WCD') +
  map_norm_idx(soc_idx, 'SOC') +
  map_norm_idx(flii_norm, 'FLII') +
  map_norm_idx(kba_r, 'KBAs') +
  map_norm_idx(dti_vent, 'DTI ventiles') +
  map_norm_idx(imr_vent, 'Infant mortality rate ventiles') +
  
  # Components
  map_norm_idx(i_forestbio, 'Forest quality') +
  map_norm_idx(i_forest, 'Forest quality (w/o biodiv)') +
  map_norm_idx(i_humz, 'Human health and impacts') +
  
  # Final options
  map_norm_idx(vent80_vents, 'FQ + HHI (4:1) ventiles') + legend +
  map_top_pctls_3class(vent80_vents, 'FQ + HHI (4:1), top 80th') + lgnd80_3clas +
  map_top_pctls(vent80_vents, 'FQ + HHI (4:1), top 80th', TRUE) + lgnd70_4clas +
  
  hih_sites_lyr + hih_pts_lyr + 
  no_msf_lyr + msf_lyr + pas_lyr

# Slider views ----
map_top_pctls(i_forestbio, 'Forest quality') + lgnd70_4clas |
  map_top_pctls(vent80, 80, 'FQ + HHI (4:1)') + lgnd70_4clas

map_norm_idx(soc_idx, 'SOC') |
  map_norm_idx(flii_norm, 'FLII') + legend

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Aggregate ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Zonal stats for PAs
pas_idx_mean <- vent70$reduceRegions(
    collection = pas,
    reducer = ee$Reducer$mean()$unweighted(),
    scale = 10000
    )

# Create fill
pas_fill <- pas$draw(color = 'mean', strokeWidth = 0)
fills = ee$Image()$byte()$paint(pas_idx_mean, color = 'mean')
pas_mean_lyr <- Map$addLayer(fills, name = 'Normalized 70 mean', 
                             opacity = 0.5, shown = FALSE)

# Segmentation ----

# ee$Algorithms$Image$Segmentation$SNIC(image, size, compactness, connectivity, neighborhoodSize, seeds)

# From https://code.earthengine.google.com/24050f332c8072deebb7ec1fa71eeb1b
seeds <- ee$Algorithms$Image$Segmentation$seedGrid(35)
snic <- ee$Algorithms$Image$Segmentation$SNIC(
  image = vent70, 
  compactness = 0,
  connectivity = 4,
  neighborhoodSize = 128,
  size = 2,
  seeds = seeds
)


clusters_snic <- snic$select("clusters")

vectors <- clusters_snic$reduceToVectors(
  geometryType = 'polygon',
  reducer = ee$Reducer$countEvery(),
  scale = 10000,
  maxPixels = 1e13,
  geometry = tropics_bb
)

clusters_outline <- ee$Image()$byte()$paint(vectors, color = 1, width = 1)
seg_outlines_lyr <- Map$addLayer(clusters_outline, list(palette = 'FF0000'), 'segments')

means_snic <- snic$select("b1_mean")
Map$addLayer(means_snic, viz_idx_norm, 'Means') + seg_outlines_lyr

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# task_img <- vent80 %>%
#   ee_image_to_asset(assetId = addm('hih_index/PlanetaryHealthPriorities_index_v4_1km'),
#                     region = tropics_bb,
#                     scale = 1000,
#                     maxPixels = 312352344)
# 
# task_img$start()
# ee_monitoring(task_img)

# vent80v_eqint <- classify_eq_int_10(vent80_vents)
# vent80_eqint <- classify_eq_int_10(vent80)
# 
# map_norm_idx(vent80_eqint$divide(10), "Eq int") +
#   map_norm_idx(vent80v_eqint$divide(10), "Eq int") +
#   map_top_ventiles(vent80_vents, lower = 70, name = "Percentiles") +
#   map_norm_idx(vent80_vents, name = "Ventiles") +
#   map_top_pctls_3class(vent80, 'FQ + HHI (4:1), top 80th', TRUE)

task_img_to_drive <- vent80_vents$multiply(100) %>%
  ee_image_to_drive(description = 'HIH_PlanetaryHealthIndex_v4b_10km_ventiles',
                    folder = 'Earth Engine Exports',
                    region = tropics_bb,
                    scale = 10000)

task_img_to_drive$start()
ee_monitoring(task_img_to_drive)