
source('R/01_use_rgee.R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sums ----
additive <- list(
  fi_hm = list(
    name = "FLII + HM",
    index = flii$
      add(gHM)
    ),
  fi_hm_c = list(
    name = "FLII  + C + HM",
    index = flii$
      add(carbon_idx)$
      add(gHM)
  ),
  fi_c_hm_dti = list(
    name = "FLII + C + HM + DTI",
    index = flii$
      add(gHM)$
      add(carbon_idx)$
      add(dti$unmask())
  ),
  fi_c_hm_dti_hc = list(
    name = "FLII + HM + C + DTI + HC",
    index = flii$
      add(gHM)$
      add(carbon_idx)$
      add(dti$unmask())$
      add(hc_access)
  ),
  fi_c_hm_dti_hc_zs = list(
    name = "FLII + C + HM + DTI + HC + ZS",
    index = flii$
      add(gHM)$
      add(carbon_idx)$
      add(dti$unmask())$
      add(hc_access)$
      add(zoonotic_risk)
  ),
  fi_c_bio_hm_dti_hc = list(
    name = "FLII + C + Biodiv + HM + DTI + HC",
    index = flii$
      add(gHM)$
      add(carbon_idx)$
      add(dti$unmask())$
      add(hc_access)$
      add(kba_r$unmask())
  ),
  fi_c_bio_hm_dti_hc_zs = list(
    name = "FLII+ C + Biodiv + HM + DTI + HC + ZS",
    index = flii$
      add(carbon_idx)$
      add(kba_r$unmask())$
      add(gHM)$
      add(dti$unmask())$
      add(hc_access)$
      add(zoonotic_risk)
  )
)

# Rescale to 0-1 and mask
rescale_index_in_list <- function(lst, tropics_bb) {
  idx <- lst$index$
    updateMask(tropics_r$neq(0))
  
  idx <- rescale_to_pctl(idx, tropics_bb)
  
  return( list(name = lst$name, 
               index = idx))
}

# Run
additive_01 <- additive %>% purrr::map(rescale_index_in_list, tropics_bb)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create indicators (combinations of inputs) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Forest ----
i_forest <- flii$
  add(carbon_idx)$
  divide(2)

# Threat ----
# i_threat <- dti$unmask()$
#   add(gHM$multiply(2))
i_threat <- dti$unmask()$
  add(gHM)$
  divide(2)

# Health ----
i_health <- hc_access$
  add(infant_mort)$
  divide(2)

# Biodiversity ----
i_biodiv <- kba_r$unmask()

# (Forest + Biodiversity) ----
i_forestbio <- flii$
  add(carbon_idx)$
  add(kba_r$unmask())$
  divide(3)

# Humans ----
i_humans <- dti$unmask()$
  add(gHM)$
  add(hc_access)$
  add(infant_mort)$
  divide(4)

# Zoonotic risk ----
i_zs <- zoonotic_risk$multiply(0.5)$add(0.5)

# Humans+ -----
i_humansz <- dti$unmask()$
  add(gHM)$
  add(hc_access)$
  add(infant_mort)$
  add(zoonotic_risk)$
  divide(5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine indicators ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
multiplicative <- list(
  f_t = list(
    name = "Forests * Threats",
    name2 = "(FLII + C) * (HM + DTI)",
    index = i_forest$
      multiply(i_threat)$
      resample()
  ),
  f_t_h = list(
    name = "Forests * Threats * Health",
    name2 = "(FLII + C) * (HM + DTI) * (HC + IMR)",
    index = i_forest$
      multiply(i_threat)$
      multiply(i_health)$
      resample()
  ),
  f_t_h_b = list(
    name = "Forests * Threats * Health * Biodiversity",
    name2 = "(FLII + C) * (HM + DTI) * (HC + IMR) * Biodiv",
    index = i_forest$
      multiply(i_threat)$
      multiply(i_health)$
      multiply(i_biodiv)$
      resample()
  ),
  fb_t_h = list(
    name = "ForestsBio * Threats * Health",
    name2 = "(FLII + C + Biodiv) * (HM + DTI) * (HC + IMR)",
    index = i_forestbio$
      multiply(i_threat)$
      multiply(i_health)$
      resample()
  ),
  fb_hum = list(
    name = "ForestsBio * ThreatsHealth",
    name2 = "(FLII + C + Biodiv) * (HM + DTI + HC + IMR)",
    index = i_forestbio$
      multiply(i_humans)$
      resample()
  ),
  fb_hum_z = list(
    name = "ForestsBio * ThreatsHealth * Spillover",
    name2 = "(FLII + C + Biodiv) * (HM + DTI + HC + IMR) * ZS",
    index = i_forestbio$
      multiply(i_humans)$
      multiply(i_zs)$
      resample()
  ),
  fb_humz = list(
    name = "ForestsBio * ThreatsHealthSpillover",
    name2 = "(FLII + C + Biodiv) * (HM + DTI + HC + IMR + ZS)",
    index = i_forestbio$
      multiply(i_humansz)$
      resample()
  ),
  f_humz = list(
    name = "Forests * ThreatsHealthSpillover",
    name2 = "(FLII + C) * (HM + DTI + HC + IMR + ZS)",
    index = i_forest$
      multiply(i_humansz)$
      resample()
  )
)

# Run
multiplicative_01 <- multiplicative %>% purrr::map(rescale_index_in_list, tropics_bb)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Classify ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
classify_index <- function(img) {
  ee$Image(0)$
    where(img$lt(0.2), 1)$
    where(img$gte(0.2), 2)$
    where(img$gte(0.4), 3)$
    where(img$gte(0.6), 4)$
    where(img$gte(0.8), 5)$
    where(img$gte(0.95), 6)$
    updateMask(img$mask())
}

classify_index_in_list <- function(lst) {
  idx <- classify_index(lst$index)
  
  return( list(name = lst$name, 
               index = idx)
          )
}

# Reclass ----
idx_ForestBioXThreatsHealthXSpillover <- classify_index(idx_ForestBioXThreatsHealthXSpillover)
idx_ForestBioXThreatsHealthSpillover <- classify_index(idx_ForestBioXThreatsHealthSpillover)

# Run
mltplctv_clas <- multiplicative_01 %>% purrr::map(classify_index_in_list)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# View ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Map$setCenter(zoom = 2)
legend <- Map$addLegend(
  visParams = indexViz,
  name = "Legend",
  position = c("bottomright", "topright", "bottomleft", "topleft"),
  color_mapping = "numeric",
  opacity = 1
)

Map$addLayer(eeObject = additive_01[[1]]$index, visParams = indexViz, name = additive_01[[1]]$name,
             shown = FALSE) +
  Map$addLayer(eeObject = additive_01[[2]]$index, visParams = indexViz, name = additive_01[[2]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = additive_01[[3]]$index, visParams = indexViz, name = additive_01[[3]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = additive_01[[4]]$index, visParams = indexViz, name = additive_01[[4]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = additive_01[[5]]$index, visParams = indexViz, name = additive_01[[5]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = additive_01[[6]]$index, visParams = indexViz, name = additive_01[[6]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = additive_01[[7]]$index, visParams = indexViz, name = additive_01[[7]]$name,
               shown = FALSE) +
  hih_sites_lyr +
  legend

Map$addLayer(eeObject = multiplicative_01[[1]]$index, visParams = indexViz, name = multiplicative_01[[1]]$name,
             shown = FALSE) +
  Map$addLayer(eeObject = multiplicative_01[[2]]$index, visParams = indexViz, name = multiplicative_01[[2]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = multiplicative_01[[3]]$index, visParams = indexViz, name = multiplicative_01[[3]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = multiplicative_01[[4]]$index, visParams = indexViz, name = multiplicative_01[[4]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = multiplicative_01[[5]]$index, visParams = indexViz, name = multiplicative_01[[5]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = multiplicative_01[[6]]$index, visParams = indexViz, name = multiplicative_01[[6]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = multiplicative_01[[7]]$index, visParams = indexViz, name = multiplicative_01[[7]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = multiplicative_01[[8]]$index, visParams = indexViz, name = multiplicative_01[[8]]$name,
               shown = FALSE) +
  hih_sites_lyr +
  legend

viz <- list(min = 0, max = 5, palette = BlueToRed, values = c('1', '2', '3', '4', '5'))
Map$addLayer(eeObject = mltplctv_clas[[1]]$index, visParams = viz, name = mltplctv_clas[[1]]$name,
             shown = FALSE) +
  Map$addLayer(eeObject = mltplctv_clas[[2]]$index, visParams = viz, name = mltplctv_clas[[2]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = mltplctv_clas[[3]]$index, visParams = viz, name = mltplctv_clas[[3]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = mltplctv_clas[[4]]$index, visParams = viz, name = mltplctv_clas[[4]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = mltplctv_clas[[5]]$index, visParams = viz, name = mltplctv_clas[[5]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = mltplctv_clas[[6]]$index, visParams = viz, name = mltplctv_clas[[6]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = mltplctv_clas[[7]]$index, visParams = viz, name = mltplctv_clas[[7]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = mltplctv_clas[[8]]$index, visParams = viz, name = mltplctv_clas[[8]]$name,
               shown = FALSE) +
  hih_sites_lyr +
  Map$addLegend( visParams = viz, color_mapp = 'character', name = NA)


# Map individual indicators ----
map_forests <- Map$addLayer(
  eeObject = i_forest, visParams = indexViz, name = "Forests indicator (FLII + Carbon)",
  shown = FALSE
)
map_dti <- Map$addLayer(
  eeObject = dti, visParams = indexViz, name = "DTI",
  shown = FALSE
)
map_threats <- Map$addLayer(
  eeObject = i_threat, visParams = indexViz, name = "Threats indicator (HumanModification + DTI)",
  shown = FALSE
)
map_idx_ForestXThreats <- Map$addLayer(
  eeObject = idx_multiply, visParams = indexViz,
  name = "(FLII + Carbon) * (HumanModification + DTI)",
  shown = FALSE
)
map_idx_sum<- Map$addLayer(
  eeObject = idx_sum, visParams = indexViz,
  name = "(FLII + Carbon) + (HumanModification + DTI)",
  shown = FALSE
)

# Map individual indicators ----
map_flii <- Map$addLayer(eeObject = flii, visParams = indexViz, name = "FLII (forest landscape integrity)", shown = FALSE)
map_carbon <- Map$addLayer(eeObject = carbon_idx, visParams = indexViz, name = "Carbon", shown = FALSE)
map_kba <- Map$addLayer(eeObject = kba_r, visParams = indexViz, name = "Biodiversity areas", shown = FALSE)
map_dti <- Map$addLayer(eeObject = dti, visParams = indexViz, name = "DTI", shown = FALSE)
map_hm <- Map$addLayer(eeObject = gHM, visParams = indexViz, name = "Human Mod", shown = FALSE)
map_hc <- Map$addLayer(eeObject = hc_access, visParams = indexViz, name = "Healthcare access", shown = FALSE)
map_imr <- Map$addLayer(eeObject = infant_mort, visParams = indexViz, name = "Infant Mortality", shown = FALSE)
map_spillover <- Map$addLayer(eeObject = zoonotic_risk, visParams = indexViz, name = "Zoonotic spillover risk", shown = FALSE)

map_forests <- Map$addLayer(eeObject = i_forest, visParams = indexViz, 
                            name = "Forests indicator (FLII + Carbon)", shown = FALSE)
map_threats <- Map$addLayer(eeObject = i_threat, visParams = indexViz, 
                            name = "Threats indicator (HumanModification + DTI)", shown = FALSE)
map_forestbio <- Map$addLayer(eeObject = i_forestbio, visParams = indexViz, 
                              name = "Forests indicator (FLII + Carbon + Biodiversity)", shown = FALSE)
map_health <- Map$addLayer(eeObject = i_health, visParams = indexViz, 
                           name = "Health indicator (HC + IMR)", shown = FALSE)

# Top picks
Map$addLayer(eeObject = additive_01[[2]]$index, visParams = indexViz, name = additive_01[[2]]$name,
             shown = FALSE) +
  Map$addLayer(eeObject = additive_01[[3]]$index, visParams = indexViz, name = additive_01[[3]]$name,
               shown = FALSE) +
  map_idx_ForestXThreats +
  map_idx_ForestXThreatsXHealth +
  map_idx_ForestBio_X_ThreatsHealth +
  map_idx_ForestBioXThreatsHealthXSpillover +
  hih_sites_lyr +
  legend

# Indicator layers
map_flii +
  map_carbon +
  map_kba +
  map_dti +
  map_hm +
  map_hc +
  map_imr +
  map_spillover +
  map_forests +
  map_forestbio +
  map_threats +
  map_health +
  hih_sites_lyr
