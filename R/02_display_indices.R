
source('R/01_use_rgee.R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sum FLII and human modification ----
indices_add <- list(
  fi_hm = list(
    name = "FLII + HM",
    index = flii$
      add(gHM)$
      divide(1.2)),
  fi_hm_c = list(
    name = "FLII  + C + HM",
    index = flii$
      add(carbon_idx)$
      add(gHM)$
      divide(2.2)
  ),
  fi_c_hm_dti = list(
    name = "FLII + C + HM + DTI",
    index = flii$
      add(gHM)$
      add(carbon_idx)$
      add(dti$unmask())$
      divide(2.9)
  ),
  fi_c_hm_dti_hc = list(
    name = "FLII + HM + C + DTI + HC",
    index = flii$
      add(gHM)$
      add(carbon_idx)$
      add(dti$unmask())$
      add(hc_access)$
      divide(3.5)
  ),
  fi_c_hm_dti_hc_zs = list(
    name = "FLII + C + HM + DTI + HC + ZS",
    index = flii$
      add(gHM)$
      add(carbon_idx)$
      add(dti$unmask())$
      add(hc_access)$
      add(zoonotic_risk)$
      divide(3.5)
  ),
  fi_c_bio_hm_dti_hc = list(
    name = "FLII + C + Biodiv + HM + DTI + HC",
    index = flii$
      add(gHM)$
      add(carbon_idx)$
      add(dti$unmask())$
      add(hc_access)$
      add(kba_r$unmask())$
      divide(4)
  ),
  fi_c_bio_hm_dti_hc_zs = list(
    name = "FLII+ C + Biodiv + HM + DTI + HC + ZS",
    index = flii$
      add(carbon_idx)$
      add(kba_r$unmask())$
      add(gHM)$
      add(dti$unmask())$
      add(hc_access)$
      add(zoonotic_risk)$
      divide(4.2)
  )
)

Map$setCenter(zoom = 2)
legend <- Map$addLegend(
  visParams = indexViz,
  name = "Legend",
  position = c("bottomright", "topright", "bottomleft", "topleft"),
  color_mapping = "numeric",
  opacity = 1
)

indices_add[[1]]

# Map$addLayer(eeObject = indices_add[[1]]$index, visParams = indexViz, name = indices_add[[1]]$name) +
#   Map$addLayer(eeObject = indices_add[[2]]$index, visParams = indexViz, name = indices_add[[2]]$name) +
#   Map$addLayer(eeObject = indices_add[[3]]$index, visParams = indexViz, name = indices_add[[3]]$name) +
#   Map$addLayer(eeObject = indices_add[[4]]$index, visParams = indexViz, name = indices_add[[4]]$name) +
#   Map$addLayer(eeObject = indices_add[[5]]$index, visParams = indexViz, name = indices_add[[5]]$name) +
#   Map$addLayer(eeObject = indices_add[[6]]$index, visParams = indexViz, name = indices_add[[6]]$name) +
#   Map$addLayer(eeObject = indices_add[[7]]$index, visParams = indexViz, name = indices_add[[7]]$name) +
#   hih_sites_lyr +
#   legend

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add and multiply ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Forest and Threat ----
i_forest <- flii$add(carbon_idx)$divide(2)
i_threat <- dti$unmask()$add(gHM$multiply(2))$divide(1.9)

# Combine
idx_multiply <- i_forest$multiply(i_threat)$resample()$divide(0.4)
idx_sum <- i_forest$add(i_threat)$resample()$divide(1.3)

# get percentiles
idx_pctls <- i_threat$reduceRegion(
  reducer = ee$Reducer$percentile(c(0, 2, 50, 98, 99, 100)),
  geometry = tropics_bb,
  bestEffort = TRUE)$
  getInfo()




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

# Forest * Threat * Health ----
i_health <- hc_access$add(infant_mort)$divide(1.4)

idx <- i_forest$
  multiply(i_threat)$
  multiply(i_health)$
  resample()$
  divide(0.22)

# Map
map_idx_ForestXThreatsXHealth <- Map$addLayer(
  eeObject = idx, visParams = indexViz,
  name = "(FLII + Carbon) * (HumanModification + DTI) * (HC + IMR)",
  shown = FALSE
)

# Forest * Threat * Health * Biodiversity ----
i_biodiv <- kba_r$unmask()

idx <- i_forest$
  multiply(i_threat)$
  multiply(i_health)$
  multiply(i_biodiv)$
  resample()$
  divide(0.12)

# Map
map_idx_ForestXThreatsXHealthXBio <- Map$addLayer(
  eeObject = idx, visParams = indexViz,
  name = "(FLII + Carbon) * (HumanModification + DTI) * (HC + IMR) * (Biodiversity)",
  shown = FALSE
)

# (Forest + Biodiversity) * Threat * Health ----
i_forestbio <- flii$add(carbon_idx)$add(kba_r$unmask())$divide(3)

idx <- i_forestbio$
  multiply(i_threat)$
  multiply(i_health)$
  resample()$
  divide(0.2)

# # get percentiles
# (idx_pctls <- idx$reduceRegion(
#   reducer = ee$Reducer$percentile(c(0, 2, 50, 98, 99, 100)),
#   geometry = tropics_bb,
#   bestEffort = TRUE)$
#     getInfo())

# get percentiles
map_idx_ForestBioXThreatsXHealth <- Map$addLayer(
  eeObject = idx, visParams = indexViz,
  name = "(FLII + C + Biodiv) * (HumanModification + DTI) * (HC + IMR)",
  shown = FALSE
)

# (Forest + Biodiversity) * (Threat + Health) ----
i_forestbio <- flii$add(carbon_idx)$add(kba_r$unmask())$divide(3)
i_humans <- dti$unmask()$add(gHM)$add(hc_access)$add(infant_mort)$divide(2.2)

idx <- i_forestbio$
  multiply(i_humans)$
  resample()$
  divide(0.67)

# # get percentiles
# (idx_pctls <- idx$reduceRegion(
#   reducer = ee$Reducer$percentile(c(0, 2, 50, 98, 99, 100)),
#   geometry = tropics_bb,
#   bestEffort = TRUE)$
#     getInfo())

# get percentiles
map_idx_ForestBio_X_ThreatsHealth <- Map$addLayer(
  eeObject = idx, visParams = indexViz,
  name = "(FLII + C + Biodiv) * (HumanModification + DTI + HC + IMR)",
  shown = FALSE
)

# (Forest + Biodiversity) * (Threat + Health) * Zoonotic ----
idx_ForestBioXThreatsHealthXSpillover <- i_forestbio$
  multiply(i_humans)$
  multiply(zoonotic_risk$multiply(0.5)$add(0.5))$
  resample()$
  divide(0.36)

# # get percentiles
# (idx_pctls <- idx$reduceRegion(
#   reducer = ee$Reducer$percentile(c(99, 100)),
#   geometry = tropics_bb,
#   bestEffort = TRUE)$
#     getInfo())

# get percentiles
map_idx_ForestBioXThreatsHealthXSpillover <- Map$addLayer(
  eeObject = idx_ForestBioXThreatsHealthXSpillover, visParams = indexViz,
  name = "(FLII + C + Biodiv) * (HumanMod + DTI + HC + IMR) * Spillover",
  shown = FALSE
)

# (Forest + Biodiversity) * (Threat + Health + Zoonotic) ----
i_humansz <- dti$unmask()$
  add(gHM)$
  add(hc_access)$
  add(infant_mort)$
  add(zoonotic_risk)$
  divide(2.4)

idx_ForestBioXThreatsHealthSpillover <- i_forestbio$
  multiply(i_humansz)$
  resample()$
  divide(0.66)

# # get percentiles
# (idx_pctls <- idx_ForestBioXThreatsHealthSpillover$reduceRegion(
#   reducer = ee$Reducer$percentile(c(99, 100)),
#   geometry = tropics_bb,
#   bestEffort = TRUE)$
#     getInfo())

# get percentiles
map_idx_ForestBioXThreatsHealthSpillover <- Map$addLayer(
  eeObject = idx_ForestBioXThreatsHealthSpillover, visParams = indexViz,
  name = "(FLII + C + Biodiv) * (HumanMod + DTI + HC + IMR + Spillover)",
  shown = FALSE
)


# Reclass ----
idx1 <- idx_ForestBioXThreatsHealthXSpillover
idx_reclas_1 <- ee$Image(0)$
  where(idx1$lt(0.2), 1)$
  where(idx1$gte(0.2), 2)$
  where(idx1$gte(0.4), 3)$
  where(idx1$gte(0.6), 4)$
  where(idx1$gte(0.8), 5)$
  where(idx1$gte(0.95), 6)$
  updateMask(flii$mask())

idx1 <- idx_ForestBioXThreatsHealthSpillover
idx_reclas_2 <- ee$Image(0)$
  where(idx1$lt(0.2), 1)$
  where(idx1$gte(0.2), 2)$
  where(idx1$gte(0.4), 3)$
  where(idx1$gte(0.6), 4)$
  where(idx1$gte(0.8), 5)$
  where(idx1$gte(0.95), 6)$
  updateMask(flii$mask())

viz <- list(min = 0, max = 5, palette = BlueToRed, values = c('1', '2', '3', '4', '5'))
Map$addLayer(
  eeObject = idx_reclas_1, visParams = viz,
  name = "(FLII + C + Biodiv) * (HumanMod + DTI + HC + IMR) * Spillover"
) + 
  Map$addLayer(
      eeObject = idx_reclas_2, visParams = viz,
      name = "(FLII + C + Biodiv) * (HumanMod + DTI + HC + IMR + Spillover"
    ) + 
    Map$addLegend( visParams = viz, color_mapp = 'character', name = NA)


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

# View ----
# All
map_forests +
  map_forestbio +
  map_threats +
  map_health +
  Map$addLayer(eeObject = indices_add[[1]]$index, visParams = indexViz, name = indices_add[[1]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = indices_add[[2]]$index, visParams = indexViz, name = indices_add[[2]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = indices_add[[3]]$index, visParams = indexViz, name = indices_add[[3]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = indices_add[[4]]$index, visParams = indexViz, name = indices_add[[4]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = indices_add[[5]]$index, visParams = indexViz, name = indices_add[[5]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = indices_add[[6]]$index, visParams = indexViz, name = indices_add[[6]]$name,
               shown = FALSE) +
  Map$addLayer(eeObject = indices_add[[7]]$index, visParams = indexViz, name = indices_add[[7]]$name,
               shown = FALSE) +
  map_idx_sum +
  map_idx_ForestXThreats +
  map_idx_ForestXThreatsXHealth +
  map_idx_ForestXThreatsXHealthXBio +
  map_idx_ForestBioXThreatsXHealth +
  map_idx_ForestBio_X_ThreatsHealth +
  map_idx_ForestBioXThreatsHealthXSpillover +
  hih_sites_lyr +
  legend

# Top picks
Map$addLayer(eeObject = indices_add[[2]]$index, visParams = indexViz, name = indices_add[[2]]$name,
             shown = FALSE) +
  Map$addLayer(eeObject = indices_add[[3]]$index, visParams = indexViz, name = indices_add[[3]]$name,
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
