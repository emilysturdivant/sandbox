#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Create composite indicator from layers (prepped in 01_standardize_layers.R)
# Requires:
#     * 01_standardize_layers.R
#     * GEE account
# Author:
#     * esturdivant@woodwellclimate.org, 2021-11-30
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source('R/where_next/01_standardize_layers.R')

# Palettes ----
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

demoplot(rev(pal_forestbio), type = 'heatmap')
demoplot(rev(pal_humz), type = 'heatmap')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine indicators ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create components
i_forestbio <- flii_norm$multiply(0.45)$
  add(wcd_vent$multiply(0.225))$
  add(soc_vent$multiply(0.225))$
  add(kba_r$multiply(0.1))$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)

i_humz <- imr_vent$multiply(0.33)$
  add(dti_vent$multiply(0.33))$
  add(zs_wpop_vent$multiply(0.33))$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)

# Classify to percentiles for display
i_forestbio_pctl <- classify_percentiles(i_forestbio)
i_humz_pctl <- classify_percentiles(i_humz)

# Create composite indicator and transform for display
v1_80 <- i_forestbio$multiply(0.8)$add(i_humz$multiply(0.2)) %>% classify_ventiles()

# Create composite indicator and transform for display
v1_70 <- i_forestbio$multiply(0.7)$add(i_humz$multiply(0.3)) %>% classify_ventiles()

# Experimental... ----
i_forestbio_v2 <- flii_norm$multiply(0.3)$
  add(wcd_vent$multiply(0.3))$
  add(soc_vent$multiply(0.2))$
  add(lbii_vent$multiply(0.1))$
  add(kba_r$multiply(0.1))

# Create composite indicator and transform for display
v2_80 <- i_forestbio_v2$multiply(0.8)$add(i_humz$multiply(0.2)) %>% classify_ventiles()
v2_70 <- i_forestbio_v2$multiply(0.7)$add(i_humz$multiply(0.3)) %>% classify_ventiles()

# Experimental... ----
i_humz_v2 <- dti_vent$multiply(0.5)$
  # add(hm_vent$multiply(0.25))$
  add(imr_vent$multiply(0.25))$
  add(zs_wpop_vent$multiply(0.25))$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)

# Create composite indicator and transform for display
v3_80 <- i_forestbio_v2$multiply(0.8)$add(i_humz_v2$multiply(0.2)) %>% classify_ventiles()
v3_70 <- i_forestbio_v2$multiply(0.7)$add(i_humz_v2$multiply(0.3)) %>% classify_ventiles()

# Break out HHI ---- 
i_health <- imr_vent$multiply(1)$
  add(zs_wpop_vent$multiply(1))$
  add(hcm_vent$multiply(1))$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000) %>%
  classify_ventiles()

i_threats <- dti_vent$multiply(1)$
  add(hm_vent$multiply(1))$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000) %>%
  classify_ventiles()

# i_humz <- i_threats$multiply(0.5)$
#   add(i_health$multiply(0.5))$
#   setDefaultProjection(crs = 'EPSG:4326', scale = 1000) %>% 
#   classify_ventiles()
  

# 
# vent80_pctls <- i_forestbio_pctl$multiply(0.8)$add(i_humz_pctl$multiply(0.2))$
#   setDefaultProjection(crs = 'EPSG:4326', scale = 1000)
# 
# # Transform composite to ventiles for display
# vent80_pctls <- classify_ventiles(vent80_pctls)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Look at it all together ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get legends 
legend <- lgnd_norm_idx()
lgnd_forestbio <- lgnd_norm_idx(pal_forestbio)
lgnd_humz <- lgnd_norm_idx(pal_humz)
lgnd80_3clas <- lgnd_top_pctls_3class()
lgnd70_4clas <- lgnd_top_pctls()

# Set center
Map$setCenter(30, 0, zoom = 3)
Map$setCenter(-53, -5, zoom = 5) # Brazil
Map$setCenter(46, -21, zoom = 7) # Manombo
Map$setCenter(112, 0, zoom = 7) # Borneo
Map$setCenter(136, -2, zoom = 6)  # Papua

# # Plot for comparison with RAISG 
# map_eq_int_10(hf_vent, 'Human footprint ventiles', palette = pal_raisg) +
#   map_eq_int_10(hm_vent, 'Human modification', show = TRUE, palette = pal_raisg) +
#   map_eq_int_10(dti_vent, 'DTI ventiles', show = TRUE, palette = pal_raisg) +
#   raisg_lyr

# Input layers
biomes_lyr +
  map_eq_int_10(flii_norm, 'FLII', palette = pal_forestbio) +
  map_eq_int_10(wcd_vent, 'WCD', palette = pal_forestbio) +
  map_eq_int_10(soc_vent, 'SOC', palette = pal_forestbio) +
  map_eq_int_10(carbon_vent, 'Carbon ventiles', palette = pal_forestbio) +
  map_norm_idx(kba_r, 'KBAs', palette = pal_forestbio) +
  map_eq_int_10(lbii_vent, 'LBII', palette = pal_forestbio) +
  map_eq_int_10(imr_vent, 'Infant mortality rate ventiles', palette = pal_humz) +
  map_eq_int_10(le_vent, 'Life expectancy ventiles', palette = pal_humz) +
  map_eq_int_10(hcw_vent, 'HC access walking ventiles', palette = pal_humz) +
  map_eq_int_10(hcm_vent, 'HC access motorized ventiles', palette = pal_humz) +
  map_eq_int_10(dti_vent, 'DTI ventiles', palette = pal_humz) +
  map_eq_int_10(hf_vent, 'Human footprint ventiles', palette = pal_humz) +
  map_eq_int_10(hm_vent, 'Human modification ventiles', palette = pal_humz) +
  map_eq_int_10(zs_wpop_vent, 'Zoonotic spillover risk', palette = pal_humz) +
  map_eq_int_10(i_threats, 'Threats component', palette = pal_carbontropics) +
  map_eq_int_10(i_health, 'Health component', palette = pal_carbontropics) +
  
  # Components
  map_norm_idx(i_forestbio_pctl, 'Forest quality v1', palette = pal_forestbio) +
  map_norm_idx(i_humz_pctl, 'Human health and impacts v1', palette = pal_humz) +
  
  # v1
  map_norm_idx(v1_80, 'FQ + HHI (4:1) ventiles') + 
  map_top_pctls(v1_80, 'FQ + HHI (4:1), top 20%', TRUE, palette = priorities_11) + 
  map_norm_idx(v1_70, 'FQ + HHI (7:3) ventiles', legend = FALSE, palette = priorities_11) + 
  map_top_pctls(v1_70, 'FQ + HHI (7:3), top 20%', legend = FALSE, palette = priorities_11) + 
  
  # v2
  map_norm_idx(v2_80, 'FQ + HHI (4:1) v2') + 
  map_top_pctls(v2_80, 'FQ + HHI (4:1) v2, top 20%', TRUE, palette = priorities_11) + 
  map_norm_idx(v2_70, 'FQ + HHI (7:3) v2', legend = FALSE, palette = priorities_11) + 
  map_top_pctls(v2_70, 'FQ + HHI (7:3) v2, top 20%', legend = FALSE, palette = priorities_11) + 
  
  # v3
  map_norm_idx(v3_80, 'FQ + HHI (4:1) v3') + 
  map_top_pctls(v3_80, 'FQ + HHI (4:1) v3, top 20%', TRUE, palette = priorities_11) + 
  map_norm_idx(v3_70, 'FQ + HHI (7:3) v3', legend = FALSE, palette = priorities_11) + 
  map_top_pctls(v3_70, 'FQ + HHI (7:3) v3, top 20%', legend = FALSE, palette = priorities_11) + 
  
  hih_sites_lyr + hih_pts_lyr + raisg_lyr +
  no_msf_lyr + msf_lyr + pas_lyr  + 
  # lgnd_forestbio +
  # lgnd_humz +
  legend

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

# Final index
task_img_to_drive <- vent80_pctl$multiply(100) %>%
  ee_image_to_drive(description = 'HIH_PlanetaryHealthIndex_v4b_1km_percentiles',
                    folder = 'Earth Engine Exports',
                    region = tropics_bb,
                    scale = 1000,
                    maxPixels = 312352344)
task_img_to_drive$start()

# HHI indicator
i_humz_pctl <- classify_percentiles(i_humz)
task_img_to_drive <- i_humz_pctl$multiply(100) %>%
  ee_image_to_drive(description = 'Indicator_HHI_v4b_10km_percentiles',
                    folder = 'Earth Engine Exports',
                    region = tropics_bb,
                    scale = 10000,
                    maxPixels = 312352344)
task_img_to_drive$start()

# FQ indicator
i_forestbio_pctl <- classify_percentiles(i_forestbio)
task_img_to_drive <- i_forestbio_pctl$multiply(100) %>%
  ee_image_to_drive(description = 'Indicator_FQ_v4b_10km_percentiles',
                    folder = 'Earth Engine Exports',
                    region = tropics_bb,
                    scale = 10000,
                    maxPixels = 312352344)
task_img_to_drive$start()

# FQ indicator
task_img_to_drive <- flii_norm$multiply(100) %>%
  ee_image_to_drive(description = 'FLII_v4b_10km_percentiles',
                    folder = 'Earth Engine Exports',
                    region = tropics_bb,
                    scale = 10000,
                    maxPixels = 312352344)
task_img_to_drive$start()

# FQ indicator
task_img_to_drive <- wcd_vent$multiply(100) %>%
  ee_image_to_drive(description = 'WCD_v4b_10km_percentiles',
                    folder = 'Earth Engine Exports',
                    region = tropics_bb,
                    scale = 10000,
                    maxPixels = 312352344)
task_img_to_drive$start()

# FQ indicator
task_img_to_drive <- kba_r$multiply(100) %>%
  ee_image_to_drive(description = 'KBA_v4b_10km_percentiles',
                    folder = 'Earth Engine Exports',
                    region = tropics_bb,
                    scale = 10000,
                    maxPixels = 312352344)
task_img_to_drive$start()

# FQ indicator
task_img_to_drive <- soc_vent$multiply(100) %>%
  ee_image_to_drive(description = 'SOC_v4b_10km_percentiles',
                    folder = 'Earth Engine Exports',
                    region = tropics_bb,
                    scale = 10000,
                    maxPixels = 312352344)
task_img_to_drive$start()

# HHI indicator
task_img_to_drive <- imr_vent$multiply(100) %>%
  ee_image_to_drive(description = 'IMR_v4b_10km_percentiles',
                    folder = 'Earth Engine Exports',
                    region = tropics_bb,
                    scale = 10000,
                    maxPixels = 312352344)
task_img_to_drive$start()

# HHI indicator
task_img_to_drive <- dti_vent$multiply(100) %>%
  ee_image_to_drive(description = 'DTI_v4b_10km_percentiles',
                    folder = 'Earth Engine Exports',
                    region = tropics_bb,
                    scale = 10000,
                    maxPixels = 312352344)
task_img_to_drive$start()

# HHI indicator
task_img_to_drive <- zs_wpop_vent$multiply(100) %>%
  ee_image_to_drive(description = 'ZS_v4b_10km_percentiles',
                    folder = 'Earth Engine Exports',
                    region = tropics_bb,
                    scale = 10000,
                    maxPixels = 312352344)
task_img_to_drive$start()
