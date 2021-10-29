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
# Combine normalized inputs  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
i_forestbio_norm <- flii_norm$multiply(0.45)$
  add(carbon_norm$multiply(0.45))$
  add(kba_r$multiply(0.1)) %>% 
  rescale_to_pctl()

i_humz_norm <- imr_norm$multiply(0.33)$
  add(dti_norm$multiply(0.33))$
  add(zoonotic_risk$multiply(0.33)) %>% 
  rescale_to_pctl()

norm_multiply <- i_forestbio_norm$multiply(i_humz_norm) %>% rescale_to_pctl()

norm50 <- i_forestbio_norm$multiply(0.5)$add(i_humz_norm$multiply(0.5)) %>% rescale_to_pctl()
norm60 <- i_forestbio_norm$multiply(0.6)$add(i_humz_norm$multiply(0.4)) %>% rescale_to_pctl()
norm70 <- i_forestbio_norm$multiply(0.7)$add(i_humz_norm$multiply(0.3)) %>% rescale_to_pctl()
norm80 <- i_forestbio_norm$multiply(0.8)$add(i_humz_norm$multiply(0.2)) %>% rescale_to_pctl()

norm50_vents <- classify_ventiles(norm50)
norm60_vents <- classify_ventiles(norm60)
norm70_vents <- classify_ventiles(norm70)
norm80_vents <- classify_ventiles(norm80)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine indicators in ventiles ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create components
i_forest <- flii_norm$multiply(0.5)$
  add(carbon_vent$multiply(0.5)) %>% 
  rescale_to_pctl(c(50, 99))

i_forestbio <- flii_norm$multiply(0.45)$
  add(carbon_vent$multiply(0.45))$
  add(kba_r$multiply(0.1)) %>% 
  rescale_to_pctl()

i_humz <- imr_vent$multiply(0.33)$
  add(dti_vent$multiply(0.33))$
  add(zoonotic_risk$multiply(0.33))$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)  %>% 
  rescale_to_pctl()

i_humz2 <- imr_vent$multiply(0.2)$
  add(hm_vent$multiply(0.2))$
  add(dti_vent$multiply(0.2))$
  add(zoonotic_risk$multiply(0.2))$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000) %>% 
  rescale_to_pctl()

# Create composite indicator
mult1 <- i_forestbio$multiply(i_humz) %>% rescale_to_pctl()

vent50 <- i_forestbio$multiply(0.5)$add(i_humz$multiply(0.5)) %>% rescale_to_pctl()
vent60 <- i_forestbio$multiply(0.6)$add(i_humz$multiply(0.4)) %>% rescale_to_pctl()
vent70 <- i_forestbio$multiply(0.7)$add(i_humz$multiply(0.3)) %>% rescale_to_pctl()
vent80 <- i_forestbio$multiply(0.8)$add(i_humz$multiply(0.2)) %>% rescale_to_pctl()

# Transform composite to ventiles
vent50_vents <- classify_ventiles(vent50)
vent60_vents <- classify_ventiles(vent60)
vent70_vents <- classify_ventiles(vent70)
vent80_vents <- classify_ventiles(vent80)

# Process without biodiversity
vent60_nobio <- i_forest$multiply(0.6)$add(i_humz$multiply(0.4)) %>% rescale_to_pctl()
vent60_nobio_vents <- classify_ventiles(vent60_nobio)

add1_top10 <- classify_top10pctl(additive1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Look at it all together ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set center
Map$setCenter(30, 0, zoom = 3)
Map$setCenter(-53, -5, zoom = 5) # Brazil
Map$setCenter(46, -21, zoom = 7) # Manombo
Map$setCenter(112, 0, zoom = 7) # Borneo
Map$setCenter(136, -2, zoom = 6)  # Papua

# Input layers
map_norm_idx(popd_norm, 'Population density') +
  map_norm_idx(dti_norm, 'DTI normalized') +
  map_norm_idx(imr_norm, 'Infant mortality rate normalized') +
  map_norm_idx(zoonotic_risk, 'Zoonotic Spillover') +
  map_norm_idx(carbon_norm, 'Carbon normalized') +
  map_norm_idx(flii_norm, 'FLII normalized') +
  map_norm_idx(kba_r, 'KBAs') +
  map_norm_idx(dti_vent, 'DTI ventiles') +
  map_norm_idx(imr_vent, 'Infant mortality rate ventiles') +
  map_norm_idx(carbon_vent, 'Carbon ventiles') +
  map_norm_idx(flii_vent, 'FLII ventiles') +
  
  # Components
  map_norm_idx(i_forestbio_norm, 'Forest quality normalized') +
  map_norm_idx(i_humz_norm, 'Human health and impacts normalized') +
  map_norm_idx(i_forestbio, 'Forest quality ventiles') +
  map_norm_idx(i_humz, 'Human health and impacts ventiles') +
  map_norm_idx(i_humz2, 'Human health and impacts ventiles (with HM)') +
  
  # Combinations, continuous, normalized
  map_norm_idx(norm_multiply, 'FQ * HHI') +
  map_norm_idx(norm50, 'FQ + HHI (1:1)') +
  map_norm_idx(norm60, 'FQ + HHI (3:2)') +
  map_norm_idx(norm70, 'FQ + HHI (7:3)') +
  map_norm_idx(norm80, 'FQ + HHI (4:1)') +
  
  # Combinations, continuous, ventiles
  map_norm_idx(mult1, 'FQ * HHI ventiles') +
  map_norm_idx(vent50, 'FQ + HHI (1:1) ventiles') +
  map_norm_idx(vent60, 'FQ + HHI (3:2) ventiles') +
  map_norm_idx(vent70, 'FQ + HHI (7:3) ventiles') +
  map_norm_idx(vent80, 'FQ + HHI (4:1) ventiles') +
  
  # Combinations, equal interval
  # map_eq_int(norm_multiply, 'FQ * HHI, eq. int.') +
  # map_eq_int(additive1, 'FQ + HHI (1:1), eq. int.') +
  # map_eq_int(additive2, 'FQ + HHI (3:2), eq. int.') +
  # map_eq_int(additive3, 'FQ + HHI (7:3), eq. int.') +
  # map_eq_int(additive4, 'FQ + HHI (4:1), eq. int.') +
  
  # Top 80th percentile, normalized
  map_top_ventiles(norm50_vents, 80, 'FQ + HHI (1:1), top 80th') +
  map_top_ventiles(norm60_vents, 80, 'FQ + HHI (3:2), top 80th') +
  map_top_ventiles(norm70_vents, 80, 'FQ + HHI (7:3), top 80th') +
  map_top_ventiles(norm80_vents, 80, 'FQ + HHI (4:1), top 80th') +
  
  # Top 80th percentile, ventiles
  map_top_ventiles(vent50_vents, 80, 'FQ + HHI (1:1) ventiles, top 80th') +
  map_top_ventiles(vent60_vents, 80, 'FQ + HHI (3:2) ventiles, top 80th') +
  map_top_ventiles(vent70_vents, 80, 'FQ + HHI (7:3) ventiles, top 80th') +
  map_top_ventiles(vent80_vents, 80, 'FQ + HHI (4:1) ventiles, top 80th') +
  
  map_norm_idx(vent60_nobio, 'FQb + HHI (3:2) ventiles') +
  map_top_ventiles(vent60_nobio_vents, 80, 'FQb + HHI (3:2) ventiles, top 80th') +
  
  hih_sites_lyr + hih_pts_lyr + msf_lyr + no_msf_lyr + pas_lyr + legend



map_top_ventiles(vent50_vents, 80, 'FQ + HHI (1:1), top 80th percentile') +
  map_top_10pctl(add1_top10, 'FQ + HHI (1:1), top 10th percentile') +
  map_top_10pctl(additive1, 'FQ + HHI (1:1), top 10th percentile 1') 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# task_img <- mltplctv_clas_eq$fb_humz$index %>% 
#   ee_image_to_asset(assetId = addm('hih_index/v1_mult_FCB_HmDtHcImZ'),
#                     region = geometry, 
#                     scale = 1000, 
#                     maxPixels = 191434780)
# 
# task_img$start()
# ee_monitoring(task_img)
# 
# task_img_to_drive <- mltplctv_clas_eq$fb_humz$index %>% 
#   ee_image_to_drive(description = 'v1_mult_FCB_HmDtHcImZ_10km',
#                     folder = 'Earth Engine Exports',
#                     region = geometry, 
#                     scale = 10000)
# 
# task_img_to_drive$start()
# ee_monitoring(task_img_to_drive)