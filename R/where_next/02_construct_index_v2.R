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
# Create indicators (combinations of inputs) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# (Forest + Biodiversity) ----
i_forestbio <- flii$
  add(carbon_idx)$
  add(kba_r$unmask()$multiply(0.25))$
  divide(2.25)

# Humans+ -----
zoonotic_risk <- zs_wpop_q10$unitScale(0, 50)

Map$addLayer(eeObject = zoonotic_risk, 
             visParams = list(min = 0, max = 1, palette = viridis), 
             name = 'ZS', 
             shown = TRUE) +
  Map$addLayer(eeObject = zoonotic_risk, 
               visParams = list(min = 0, max = 1.5, palette = viridis), 
               name = 'ZS', 
               shown = TRUE)

i_tmiz <- dti$unmask()$
  add(gHM)$
  add(infant_mort)$
  add(zoonotic_risk)$
  divide(4)

i_humansz <- dti$unmask()$
  add(gHM)$
  add(le)$
  add(infant_mort)$
  add(zoonotic_risk)$
  divide(5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create list of indicators ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
indicators <- list(
  flii = list(
    name =  "FLII (forest landscape integrity)",
    index = flii
  ),
  cabon = list(
    name =  "Carbon (above-ground, below-ground, soil)",
    index = carbon_idx
  ),
  kba = list(
    name =  "Key Biodiversity Areas",
    index = kba_r$updateMask(flii$mask())
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
    index = infant_mort
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
map_flii <- Map$addLayer(eeObject = flii, visParams = viz_idx_norm, name = "FLII (forest landscape integrity)", shown = FALSE)
map_carbon <- Map$addLayer(eeObject = carbon_idx, visParams = viz_idx_norm, name = "Carbon", shown = FALSE)
map_kba <- Map$addLayer(eeObject = kba_r, visParams = viz_idx_norm, name = "Biodiversity areas", shown = FALSE)
map_dti <- Map$addLayer(eeObject = dti, visParams = viz_idx_norm, name = "DTI", shown = FALSE)
map_hm <- Map$addLayer(eeObject = gHM, visParams = viz_idx_norm, name = "Human Mod", shown = FALSE)
map_hc <- Map$addLayer(eeObject = hc_access, visParams = viz_idx_norm, name = "Healthcare access", shown = FALSE)
map_imr <- Map$addLayer(eeObject = infant_mort, visParams = viz_idx_norm, name = "Infant Mortality", shown = FALSE)
map_spillover <- Map$addLayer(eeObject = zoonotic_risk, visParams = viz_idx_norm, name = "Zoonotic spillover risk", shown = FALSE)
map_dalys <- Map$addLayer(eeObject = dalys, visParams = viz_idx_norm, name = "DALYs") 
map_dalys <- Map$addLayer(eeObject = dalys_ea, visParams = viz_pctls_idx, name = "DALYs")

# Function to map layer ----
map_layer <- function(lst, shown = FALSE) {
  
  Map$addLayer(eeObject = lst$index, 
               visParams = viz_clssfd_idx, 
               name = lst$name, 
               shown = shown)
}

# Set center
Map$setCenter(20, 0, zoom = 3)

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