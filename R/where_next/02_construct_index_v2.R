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
Map$setCenter(20, 0, zoom = 3)
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
  map_norm_idx(rescale_to_pctl(imr_norm$multiply(dalys_ea_idx)), 'IMR * DALYs') +
  map_norm_idx(rescale_to_pctl(le$multiply(zoonotic_risk)$multiply(dti)), 'LE * ZS * DTI') +
  map_norm_idx(rescale_to_pctl(imr_norm$multiply(zoonotic_risk)$multiply(dti)), 'IMR * ZS * DTI') +
  hih_sites_lyr + hih_pts_lyr + legend

# Combine all ----
i_forestbio <- flii_norm$multiply(0.45)$
  add(carbon_idx$multiply(0.45))$
  add(kba_r$multiply(0.1))

i_humz <- imr_norm$multiply(0.33)$
  add(dti$multiply(0.33))$
  add(zoonotic_risk$multiply(0.33))

map_norm_idx(popd_norm, 'Population density') +
  map_norm_idx(dti, 'DTI') +
  map_norm_idx(gHM, 'Human Modification') +
  map_norm_idx(hf, 'Human Footprint') +
  map_norm_idx(imr_norm, 'Infant mortality rate') +
  map_norm_idx(le_norm, 'Life expectancy') +
  map_norm_idx(dalys_ea_idx, 'DALYs (equal-area rank)') +
  map_norm_idx(zoonotic_risk, 'Zoonotic Spillover') +
  map_norm_idx(carbon_idx, 'Carbon') +
  map_norm_idx(flii_norm, 'FLII') +
  map_norm_idx(kba_r, 'KBAs') +
  map_norm_idx(carbon_idx$multiply(0.9)$add(kba_r$multiply(0.1)), '.9*Carbon + .1*KBA') +
  map_norm_idx(flii_norm$multiply(0.5)$add(carbon_idx$multiply(0.5)), '.5*FLII + .5*Carbon') +
  map_norm_idx(flii_norm$multiply(0.45)$add(carbon_idx$multiply(0.45))$add(kba_r$multiply(0.1)), 
               '.45*FLII + .45*Carbon + .1*Bio') +
  map_eq_int(flii_norm$multiply(0.45)$add(carbon_idx$multiply(0.45))$add(kba_r$multiply(0.1)), 
             '.45*FLII + .45*Carbon + .1*Bio (classified)') +
  map_norm_idx(flii_norm$multiply(0.45)$add(carbon_idx$multiply(0.45))$add(kba_r$multiply(0.1)), 
               '.45*FLII + .45*Carbon + .1*Bio') +
  map_eq_int(flii_norm$multiply(0.45)$add(carbon_idx$multiply(0.45))$add(kba_r$multiply(0.1)), 
             '.45*FLII + .45*Carbon + .1*Bio (classified)') +
  map_norm_idx(le$multiply(0.33)$add(dti$multiply(0.33))$add(zoonotic_risk$multiply(0.33)), 
               '.33*DTI + .33*LE + .3*ZS') +
  map_norm_idx(imr_norm$multiply(0.33)$add(dti$multiply(0.33))$add(zoonotic_risk$multiply(0.33)), 
               '.33*DTI + .33*IMR + .3*ZS') +
  map_eq_int(imr_norm$multiply(0.33)$add(dti$multiply(0.33))$add(zoonotic_risk$multiply(0.33)), 
               '.33*DTI + .33*IMR + .3*ZS (classified)') +
  map_norm_idx(rescale_to_pctl(rescale_to_pctl(i_forestbio)$add(rescale_to_pctl(i_humz)), 95), 
                'Forest quality * Human health and impacts') +
  map_eq_int(rescale_to_pctl(i_forestbio)$multiply(rescale_to_pctl(i_humz)), 
               'Forest quality * Human health and impacts (classified)') +
  hih_sites_lyr + hih_pts_lyr + legend

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create list of indicators ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
indicators <- list(
  flii_norm = list(
    name =  "FLII (forest landscape integrity)",
    index = flii_norm
  ),
  cabon = list(
    name =  "Carbon (above-ground, below-ground, soil)",
    index = carbon_idx
  ),
  kba = list(
    name =  "Key Biodiversity Areas",
    index = kba_r$updateMask(flii_norm$mask())
  ),
  dti = list(
    name =  "Development Threats Index",
    index = dti
  ),
  hm = list(
    name =  "Human modification",
    index = gHM
  ),
  hc = list(
    name =  "Healthcare accessibility (walking only)",
    index = hc_access
  ),
  imr = list(
    name =  "Infant Mortality Rate",
    index = imr_norm
  ),
  zs = list(
    name =  "Zoonotic spillover risk",
    index = zoonotic_risk
  ),
  forestbio = list(
    name =  "Forests indicator (FLII + C + Biodiv)",
    index = i_forestbio
  ), 
  tmiz = list(
    name =  "Humans indicator (HM + DTI + IMR + Zoonotic)",
    index = i_tmiz
  ),
  humz = list(
    name =  "Humans indicator (HM + DTI + IMR + Zoonotic)",
    index = i_humansz
  )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine indicators ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
multiplicative <- list(
  fb_tmiz = list(
    name = "ForestsBio * ThreatsHealthSpillover",
    name2 = "(FLII + C + Biodiv) * (HM + DTI + IMR + ZS)",
    index = i_forestbio$
      multiply(i_tmiz)$
      resample()
  ),
  fb_humz = list(
    name = "ForestsBio * ThreatsHealthSpillover",
    name2 = "(FLII + C + Biodiv) * (HM + DTI + LE + IMR + ZS)",
    index = i_forestbio$
      multiply(i_humansz)$
      resample()
  )
)

# Run
mltplctv_idx <- multiplicative %>% purrr::map(rescale_index_in_list)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Classify ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run
mltplctv_eq_int <- mltplctv_idx %>% purrr::map(classify_index_in_list)

# # Quantiles
# addtv_clas_quant <- additive_01 %>% purrr::map(classify_index_in_list, 'quantile')
# mltplctv_clas_quant <- multiplicative_01 %>% purrr::map(classify_index_in_list, 'quantile')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# View ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Display final options
Map$addLayer(eeObject = mltplctv_idx[[1]]$index, visParams = viz_idx_norm, 
             name = mltplctv_idx[[1]]$name, shown = TRUE) +
  Map$addLayer(eeObject = mltplctv_eq_int[[1]]$index, visParams = viz_clssfd_idx, 
               name = mltplctv_eq_int[[1]]$name, shown = TRUE) +
  Map$addLayer(eeObject = mltplctv_idx[[2]]$index, visParams = viz_idx_norm, 
               name = mltplctv_idx[[2]]$name, shown = TRUE) +
  Map$addLayer(eeObject = mltplctv_eq_int[[2]]$index, visParams = viz_clssfd_idx, 
               name = mltplctv_eq_int[[2]]$name, shown = TRUE) +
  hih_sites_lyr +
  Map$addLegend( visParams = viz_idx_norm, color_map = 'numeric', name = 'Index') +
  Map$addLegend( visParams = viz_clssfd_idx, color_map = 'character', name = 'Percentile')


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