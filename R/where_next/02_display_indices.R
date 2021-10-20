
source('R/01_use_rgee.R')

# Function to map layer ----
map_layer <- function(lst, shown = FALSE) {
  
  Map$addLayer(eeObject = lst$index, 
               visParams = viz, 
               name = lst$name, 
               shown = shown)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# View ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set center
Map$setCenter(20, 0, zoom = 3)

# Display
viz <- list(min = 10, max = 90, 
            palette = BlueToRed, 
            values = c('5', '4', '3', '2', '1'))

# Display final options
map_layer(addtv_clas_eq$fi_c_bio_hm_dti_hc_zs, shown = FALSE) +
  map_layer(mltplctv_clas_eq$f_t_hz, shown = FALSE) +
  map_layer(mltplctv_clas_eq$fb_humz, shown = TRUE) +
  map_layer(mltplctv_clas_eq$f_humz, shown = FALSE) +
  hih_sites_lyr +
  Map$addLegend( visParams = viz, color_map = 'character', name = 'Priority level')

# Display final 3 for Borneo
Map$centerObject(bbbr, 6)
map_layer(addtv_clas_eq$fi_c_bio_hm_dti_hc_zs, shown = FALSE) +
  map_layer(mltplctv_clas_eq$f_t_hz, shown = FALSE) +
  map_layer(mltplctv_clas_eq$fb_humz, shown = TRUE) +
  hih_sites_lyr +
  Map$addLegend( visParams = viz, color_map = 'character', name = 'Priority level')

# Display final 3 for TdM
Map$centerObject(tdm, 5)
map_layer(addtv_clas_eq$fi_c_bio_hm_dti_hc_zs, shown = FALSE) +
  map_layer(mltplctv_clas_eq$f_t_hz, shown = FALSE) +
  map_layer(mltplctv_clas_eq$fb_humz, shown = TRUE) +
  hih_sites_lyr +
  Map$addLegend( visParams = viz, color_map = 'character', name = 'Priority level')

# Display final 3 for TdM
Map$centerObject(manombo, 7)
map_layer(addtv_clas_eq$fi_c_bio_hm_dti_hc_zs, shown = FALSE) +
  map_layer(mltplctv_clas_eq$f_t_hz, shown = FALSE) +
  map_layer(mltplctv_clas_eq$fb_humz, shown = TRUE) +
  hih_sites_lyr +
  Map$addLegend( visParams = viz, color_map = 'character', name = 'Priority level')

# Display final 3 for New Guinea
Map$setCenter(140.9, -5.6, 6)
map_layer(addtv_clas_eq$fi_c_bio_hm_dti_hc_zs, shown = FALSE) +
  map_layer(mltplctv_clas_eq$f_t_hz, shown = FALSE) +
  map_layer(mltplctv_clas_eq$fb_humz, shown = TRUE) +
  hih_sites_lyr +
  Map$addLegend( visParams = viz, color_map = 'character', name = 'Priority level')


# Display inputs/indicators ----
viz <- list(min = 0, max = 1, palette = BlueToRed)

# Display
map_layer(indicators[[1]]) +
  map_layer(indicators[[2]]) +
  map_layer(indicators[[3]]) +
  map_layer(indicators[[4]]) +
  map_layer(indicators[[5]]) +
  map_layer(indicators[[6]]) +
  map_layer(indicators[[7]]) +
  map_layer(indicators[[8]]) +
  map_layer(indicators[[9]]) +
  map_layer(indicators[[10]]) +
  map_layer(indicators[[11]]) +
  map_layer(indicators[[12]]) +
  map_layer(indicators[[13]]) +
  map_layer(indicators[[14]]) +
  map_layer(indicators[[15]]) +
  map_layer(indicators[[16]]) +
  hih_sites_lyr +
  Map$addLegend( visParams = viz, color_map = 'numeric', name = NA)


library(mapview)
Map$setCenter(140.9, -5.6, 2)
m <- map_layer(indicators[[1]], shown=TRUE)
mapshot(m, file = "indicator1.png", cliprect = c(320, 330, 380, 150), zoom = 2)

Map$setCenter(20, 0, zoom = 3)
m <- map_layer(indicators[[1]], shown=TRUE)
mapshot(m, file = "indicator1b.png", cliprect = c(300, -200, 600, 300), zoom = 2)

# https://csaybar.github.io/blog/2020/06/10/rgee_01_worldmap/
indicator1 <- ee_as_thumbnail(
  image = indicators[[1]]$index,  # Image to export
  region = geometry, # Region of interest
  dimensions = 4096, # output dimension
  raster = TRUE, # If FALSE returns a stars object. FALSE by default
  vizparams = viz[-3] # Delete the palette element
)

indicator1[] <- scales::rescale(
  raster::getValues(indicator1), c(viz$min, viz$max)
) 

indicator1 <- indicator1 %>% raster::mask(mask = indicator1, maskvalue = 0)

tm_shape(indicator1) +
  tm_raster(
    palette = cptcity::cpt("grass_bcyr", n = 100),
    stretch.palette = FALSE,
    style = "cont"
  )

r_df <- indicator1 %>% 
  terra::rast() %>% 
  as.data.frame(xy=TRUE) %>% 
  rename(index = 3)

(agb_map <- ggplot(r_df) +
    geom_raster(aes(x = x, y = y, fill = agb)) +
    geom_sf(data = xy_sf) + 
    geom_sf_label(data = xy_sf, aes(label = cell), nudge_x = 40000) +
    scale_fill_gradientn(name = agb_label, 
                         na.value = "transparent", 
                         colors = agb_palette,
                         limits = c(15, 150),
                         oob = scales::squish
    ) +
    scale_x_continuous(#limits = range(r_df$x), 
      expand = expansion()) +
    scale_y_continuous(#limits = range(r_df$y), 
      expand = expansion()) +
    ggthemes::theme_map() +
    ggtitle("Aboveground carbon density 2003") +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank()))





map_layer(addtv_clas_eq$fi_c_dti_hc_zs) +
  map_layer(addtv_clas_eq$fi_c_bio_hm_dti_hc_zs) +
  map_layer(mltplctv_clas_eq$f_t_hz) +
  map_layer(mltplctv_clas_eq$fb_humz) +
  hih_sites_lyr +
  Map$addLegend( visParams = viz, color_map = 'character', name = NA)

# Non-reclassed
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
