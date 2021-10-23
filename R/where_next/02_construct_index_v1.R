#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Combine inputs (prepped in 01...R) to create index options
# Requires:
#     * 01_use_rgee.R
#     * GEE account
# Author:
#     * esturdivant@woodwellclimate.org, 2021-10-10
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


source('R/where_next/01_use_rgee.R')


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
    name = "FLII + C + HM",
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
    name = "FLII + C + HM + DTI + HC",
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
  fi_c_dti_hc_zs = list(
    name = "FLII + C + DTI + HC + ZS",
    index = flii$
      add(gHM)$
      add(carbon_idx)$
      add(dti$unmask())$
      add(hc_access)$
      add(zoonotic_risk)
  ),
  fi_c_hm_hc_zs = list(
    name = "FLII + C + HM + HC + ZS",
    index = flii$
      add(gHM)$
      add(carbon_idx)$
      add(gHM)$
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

# Run
additive_01 <- additive %>% purrr::map(rescale_index_in_list)

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

# Health+ -----
i_healthz <- hc_access$
  add(infant_mort)$
  add(zoonotic_risk)$
  divide(3)

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
  izs = list(
    name =  "Zoonotic spillover indicator",
    index = i_zs
  ),
  forests = list(
    name =  "Forests indicator (FLII + C)",
    index = i_forest
  ), 
  forestbio = list(
    name =  "Forests indicator (FLII + C + Biodiv)",
    index = i_forestbio
  ), 
  threats = list(
    name =  "Threats indicator (HM + DTI)",
    index = i_threat
  ), 
  health = list(
    name =  "Health indicator (HC + IMR)",
    index = i_health
  ), 
  threatshealth = list(
    name =  "ThreatsHealth indicator (HM + DTI + HC + IMR)",
    index = i_humans
  ), 
  healthz = list(
    name =  "Health indicator (HC + IMR + Zoonotic)",
    index = i_healthz
  ), 
  humz = list(
    name =  "Humans indicator (HM + DTI + HC + IMR + Zoonotic)",
    index = i_humansz
  )
)

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
  f_t_hz = list(
    name = "Forests * Threats * HealthSpillover",
    name2 = "(FLII + C) * (HM + DTI) * (HC + IMR + ZS)",
    index = i_forest$
      multiply(i_threat)$
      multiply(i_healthz)$
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
multiplicative_01 <- multiplicative %>% purrr::map(rescale_index_in_list)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Classify ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run
addtv_clas_eq <- additive_01 %>% purrr::map(classify_index_in_list)
mltplctv_clas_eq <- multiplicative_01 %>% purrr::map(classify_index_in_list)

# # Quantiles
# addtv_clas_quant <- additive_01 %>% purrr::map(classify_index_in_list, 'quantile')
# mltplctv_clas_quant <- multiplicative_01 %>% purrr::map(classify_index_in_list, 'quantile')


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