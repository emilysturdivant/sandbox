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

# Set center
Map$setCenter(30, 0, zoom = 3)
Map$setCenter(-53, -5, 5) # Brazil
Map$setCenter(46, -21, 7) # Manombo
Map$setCenter(112, 0, 7) # Borneo
Map$setCenter(136, -2, 6)  # Papua

# Function to map layer ----
map_norm_idx <- function(img, name, shown = FALSE) {
  Map$addLayer(eeObject = img, 
               visParams = viz_idx_norm, 
               name = name, 
               shown = shown)
}

rescale_and_map <- function(img, name, shown = FALSE) {
  map_norm_idx(rescale_to_pctl(img), name, shown)
}

map_eq_int <- function(img, name, shown = FALSE) {
  img <- classify_eq_int(img)
  Map$addLayer(eeObject = img, 
               visParams = viz_clssfd_idx, 
               name = name, 
               shown = shown)
}

legend <- Map$addLegend(
  visParams = viz_idx_norm,
  name = NA,
  position = c("bottomright", "topright", "bottomleft", "topleft"),
  color_mapping = "numeric",
  opacity = 1
)

lgnd_eq_int <- Map$addLegend(
  visParams = viz_clssfd_idx,
  name = NA,
  position = c("bottomright", "topright", "bottomleft", "topleft"),
  color_mapping = "character",
  opacity = 1
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create indicators (combinations of inputs) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# (Forest + Biodiversity) ----
map_norm_idx(carbon_idx, 'Carbon') +
  map_norm_idx(flii_norm, 'FLII') +
  map_norm_idx(kba_r, 'KBAs') +
  map_norm_idx(carbon_idx$multiply(0.9)$add(kba_r$multiply(0.1)), '.9*Carbon + .1*KBA') +
  map_norm_idx(flii_norm$multiply(0.5)$add(carbon_idx$multiply(0.5)), '.5*FLII + .5*Carbon') +
  map_norm_idx(flii_norm$multiply(0.45)$add(carbon_idx$multiply(0.45))$add(kba_r$multiply(0.1)), 
               '.45*FLII + .45*Carbon + .1*Bio') +
  map_eq_int(flii_norm$multiply(0.45)$add(carbon_idx$multiply(0.45))$add(kba_r$multiply(0.1)), 
               '.45*FLII + .45*Carbon + .1*Bio (classified)') +
  hih_sites_lyr + hih_pts_lyr + legend + lgnd_eq_int

# Human impacts ----
map_norm_idx(dti, 'DTI') +
  map_norm_idx(gHM, 'Human Modification') +
  map_norm_idx(hf, 'Human Footprint') +
  map_norm_idx(dti$multiply(0.5)$add(gHM$multiply(0.5)), '.5*DTI + .5*HM') +
  map_norm_idx(dti$multiply(0.5)$add(hf$multiply(0.5)), '.5*DTI + .5*HF') +
  map_norm_idx(rescale_to_pctl(dti$add(0.001)$multiply(gHM)), 'DTI * HM') +
  map_norm_idx(rescale_to_pctl(dti$add(0.001)$multiply(hf)), 'DTI * HF') +
  hih_sites_lyr + hih_pts_lyr + legend

# Zoonotic spillover ----
map_norm_idx(zs_resp_ea$unitScale(0, .90), 'Spillover risk - raw model response') +
  map_norm_idx(zs_wpubs_norm, 'Spillover risk - weighted by publications') +
  map_norm_idx(zs_wpubs_ea$unitScale(0, .90), 'Spillover risk - weighted by pubs - equal area') +
  map_norm_idx(zs_wpop_norm, 'Spillover risk - reweighted by population') +
  map_norm_idx(zs_wpop_ea$unitScale(0, .90), 'Spillover risk - reweighted by pop - equal area')   +
  hih_sites_lyr + hih_pts_lyr + legend
  
# Human health ----
zoonotic_risk <- zs_wpop_q10$unitScale(0, .90)
dalys_ea_idx <- dalys_ea$unitScale(0, .90)
map_norm_idx(imr_norm, 'Infant mortality rate') +
  map_norm_idx(hc_access, 'HC access walking') +
  map_norm_idx(hc_motor, 'HC access motorized') +
  map_norm_idx(le_norm, 'Life expectancy') +
  map_norm_idx(le_ea, 'Life expectancy') +
  map_norm_idx(hi_norm, 'Health index') +
  map_norm_idx(hdi_norm, 'Human development index') +
  map_norm_idx(dalys_ea_idx, 'DALYs (equal-area rank)') +
  map_norm_idx(mortu5_ea, 'Under-5 mortality (equal-area rank)') +
  map_norm_idx(zoonotic_risk, 'Zoonotic Spillover') +
  map_norm_idx(imr_norm$multiply(0.5)$add(hc_access$multiply(0.5)), '.5*IMR + .5*HC access walking') +
  map_norm_idx(imr_norm$multiply(0.5)$add(le_norm$multiply(0.5)), '.5*IMR + .5*LE') +
  map_norm_idx(imr_norm$multiply(0.5)$add(dalys_ea_idx$multiply(0.5)), '.5*IMR + .5*DALYs') +
  map_norm_idx(imr_norm$multiply(0.5)$add(zoonotic_risk$multiply(0.5)), '.5*IMR + .5*ZS') +
  map_norm_idx(rescale_to_pctl(imr_norm$multiply(dalys_ea)), 'IMR * DALYs') +
  map_norm_idx(rescale_to_pctl(imr_norm$multiply(zoonotic_risk)), 'IMR * ZS') +
  hih_sites_lyr + hih_pts_lyr + legend

# Human health and impacts ----
map_norm_idx(popd_norm, 'Population density') +
  map_norm_idx(dti, 'DTI') +
  map_norm_idx(gHM, 'Human Modification') +
  map_norm_idx(hf, 'Human Footprint') +
  map_norm_idx(imr_norm, 'Infant mortality rate') +
  map_norm_idx(le, 'Life expectancy') +
  map_norm_idx(dalys_ea_idx, 'DALYs (equal-area rank)') +
  map_norm_idx(zoonotic_risk, 'Zoonotic Spillover') +
  map_norm_idx(le$multiply(0.33)$add(dti$multiply(0.33))$add(zoonotic_risk$multiply(0.33)), 
               '.33*DTI + .33*LE + .3*ZS') +
  map_norm_idx(imr_norm$multiply(0.33)$add(dti$multiply(0.33))$add(zoonotic_risk$multiply(0.33)), 
               '.33*DTI + .33*IMR + .3*ZS') +
  map_norm_idx(imr_norm$multiply(0.5)$add(dalys_ea_idx$multiply(0.5)), '.5*IMR + .5*DALYs') +
  map_norm_idx(imr_norm$multiply(0.5)$add(zoonotic_risk$multiply(0.5)), '.5*IMR + .5*ZS') +
  rescale_and_map(imr_norm$multiply(dalys_ea_idx), 'IMR * DALYs') +
  rescale_and_map(le$multiply(zoonotic_risk)$multiply(dti), 'LE * ZS * DTI') +
  rescale_and_map(imr_norm$multiply(zoonotic_risk)$multiply(dti), 'IMR * ZS * DTI') +
  hih_sites_lyr + hih_pts_lyr + legend

# Combine all ----
i_forestbio <- flii_norm$multiply(0.45)$
  add(carbon_idx$multiply(0.45))$
  add(kba_r$multiply(0.1)) %>% 
  rescale_to_pctl()

i_humz <- imr_norm$multiply(0.33)$
  add(dti$multiply(0.33))$
  add(zoonotic_risk$multiply(0.33)) %>% 
  rescale_to_pctl()

map_norm_idx(popd_norm, 'Population density') +
  map_norm_idx(dti, 'DTI') +
  map_norm_idx(imr_norm, 'Infant mortality rate') +
  map_norm_idx(zoonotic_risk, 'Zoonotic Spillover') +
  map_norm_idx(carbon_idx, 'Carbon') +
  map_norm_idx(flii_norm, 'FLII') +
  map_norm_idx(kba_r, 'KBAs') +
  rescale_and_map(i_forestbio$multiply(i_humz), 'Forest quality * Human health and impacts') +
  map_eq_int(i_forestbio$multiply(i_humz), 'Forest quality * Human health and impacts (classified)') +
  rescale_and_map(i_forestbio$add(i_humz), 'Forest quality + Human health and impacts') +
  map_eq_int(i_forestbio$add(i_humz), 'Forest quality + Human health and impacts (classified)') +
  rescale_and_map(i_forestbio$multiply(0.6)$add(i_humz$multiply(0.4)), 
                  'Forest quality + Human health and impacts') +
  map_eq_int(i_forestbio$multiply(0.6)$add(i_humz$multiply(0.4)), 
             'Forest Q + Human H&I (class)') +
  rescale_and_map(i_forestbio$multiply(0.7)$add(i_humz$multiply(0.3)), 
                  'Forest quality + Human health and impacts') +
  map_eq_int(i_forestbio$multiply(0.7)$add(i_humz$multiply(0.3)), 
             '7 Forest Q + 3 Human H&I (class)') +
  hih_sites_lyr + hih_pts_lyr + legend

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