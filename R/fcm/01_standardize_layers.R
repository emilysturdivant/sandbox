#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Standardize layers in GEE for use in the forest carbon composite indicator
# Requires:
#     * GEE account
# Author:
#     * esturdivant@woodwellclimate.org, 2021-11-20
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here::here('R/fcm/0_initialize.R'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tropical boundaries ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Convert to raster for masking ----
# # Convert to raster
# tropics_r <- ee$FeatureCollection(tropics_bb)$
#   map(function(f) f$set("tropics", 1))$
#   reduceToImage(
#     properties = list('tropics'),
#     reducer = ee$Reducer$first()
#   )
# 
# # Dense humid forests biome ----
# biome_dhf_id <- addm('BIOME_Dense_Humid_Forests')
# 
# # Get dense humid forests biome
# alist <- ee_manage_assetlist(path_asset = addm(""))
# if(!biome_dhf_id %in% alist$ID) {
#   
#   # Load ecoregions and filter to dense humid forests biome
#   biome_dhf <- ee$FeatureCollection("RESOLVE/ECOREGIONS/2017")$
#     filter(
#       ee$Filter$eq('BIOME_NAME', 'Tropical & Subtropical Moist Broadleaf Forests')
#     )
#   
#   # Convert to raster
#   dense_humid_forests <- biome_dhf$
#     reduceToImage(
#       properties = list('BIOME_NUM'), 
#       reducer = ee$Reducer$first()
#     )$
#     setDefaultProjection(crs = 'EPSG:4326', scale = 1000)
#   
#   # Save image as EE asset
#   task_img <- ee_image_to_asset(dense_humid_forests,
#                                 'BIOME_Dense_Humid_Forests',
#                                 assetId = biome_dhf_id,
#                                 # region = tropics_bb,
#                                 crs = 'EPSG:4326',
#                                 scale = 1000,
#                                 maxPixels = 803042888)
#   task_img$start()
# }
# 
# dense_humid_forests <- ee$Image(biome_dhf_id)
# dhf_mask <- dense_humid_forests$mask()
# 
# # Create Biomes layer ----
# colorUpdates = list(
#   list(ECO_ID = 204, COLOR = '#B3493B'),
#   list(ECO_ID = 245, COLOR = '#267400'),
#   list(ECO_ID = 259, COLOR = '#004600'),
#   list(ECO_ID = 286, COLOR = '#82F178'),
#   list(ECO_ID = 316, COLOR = '#E600AA'),
#   list(ECO_ID = 453, COLOR = '#5AA500'),
#   list(ECO_ID = 317, COLOR = '#FDA87F'),
#   list(ECO_ID = 763, COLOR = '#A93800')
# )
# 
# ecoRegions = ee$FeatureCollection("RESOLVE/ECOREGIONS/2017")$
#   map(function(f) {
#     color = f$get('COLOR_BIO')
#     f$set(list(style = list(color = color, width = 0)))
#   })
# 
# ecoRegions = ecoRegions$
#   filter(ee$Filter$inList('BIOME_NUM', c(1,2,3,7,14)))$
#   merge(colorUpdates[[i]]$layer)
# 
# imageRGB = ecoRegions$style(styleProperty = 'style')
# biomes_lyr <- Map$addLayer(imageRGB, name = 'RESOLVE/ECOREGIONS/2017', show = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AGB+BGB carbon density ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wbd_mgha <- ee$Image("users/sgorelik/WHRC_Global_Biomass_500m_V6/Current_AGB_BGB_Mgha")

# Get mask
c_mask <- wbd_mgha$gt(0)

# Convert to carbon density 
wcd_mgcha <- wbd_mgha$divide(2)$updateMask(c_mask)

# Normalize WCD to 0-1 from 0-200 MgC/ha
wcd_mgcha <- wcd_mgcha$unitScale(0, 200)
wcd_norm <- wcd_mgcha$
  where(wcd_mgcha$gt(1.0), 1.0)$
  where(wcd_mgcha$lt(0.0), 0.0)

# # View
# map_norm_idx(wcd_norm, 'Woody biomass carbon', palette = pal_idx) +
#   Map$addLayer(c_mask)

# # Look at histogram ----
# img <- wcd_norm
# 
# # Take sample at random points within the region
# sample <-  img$sampleRegions(tropics_bb, NULL, scale = 2000)
# sample <-  img$sample(numPixels = 1e4) # couldn't export
# 
# # Export
# task_name <- 'sample_wcd_norm_1e4'
# task_vector <- sample %>% 
#   ee_table_to_drive(description = task_name,
#                     folder = basename(export_path),
#                     fileFormat = 'CSV', 
#                     timePrefix = FALSE)
# task_vector$start()
# 
# # Import CSV for plot
# samp_csv <- here::here(export_path, str_c(task_name, '.csv'))
# samp_df <- read_csv(samp_csv)
# n <- nrow(samp_df)
# 
# # Plot histogram
# samp_df %>% 
#   ggplot() +
#   geom_histogram() +
#   labs(y = str_glue('Number of pixels (N = {n})'), 
#        x = 'WCD (tC/ha)') +
#   theme_minimal()
# 
# # Save
# ggsave(here::here('outputs/fcm/histograms', str_c(task_name, '.png')),
#        width = 4, height = 3.5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SOC carbon density ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
soc_mgha <- ee$Image("users/sgorelik/WHRC_Global_Biomass_500m_V6/Current_SOC_Mgha")

# Convert to carbon density 
soc_mgcha <- soc_mgha$divide(2)$updateMask(c_mask)

# Normalize to 0-1 from 0-600 MgC/ha
soc_mgcha <- soc_mgcha$unitScale(0, 600)
soc_norm <- soc_mgcha$
  where(soc_mgcha$gt(1.0), 1.0)$
  where(soc_mgcha$lt(0.0), 0.0)

# # View
# map_norm_idx(soc_norm, 'Soil organic carbon', palette = pal_idx) +
#   Map$addLayer(c_mask)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Forest Landscape Integrity Index ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load 
flii <- ee$ImageCollection(c(
  "users/esturdivant/flii/flii_Africa",
  "users/esturdivant/flii/flii_Asia",
  "users/esturdivant/flii/flii_NorthAmerica",
  "users/esturdivant/flii/flii_Oceania",
  "users/esturdivant/flii/flii_SouthAmerica"
))

# Mask each image
flii <- flii$map(function(img) { img$updateMask(img$neq(-9999)) })

# Mosaic
flii <- flii$mosaic()$setDefaultProjection(crs = 'EPSG:4326', scale = 300)

# Normalize to 0-1 from 0-9666
flii_scale <- flii$unitScale(0, 10000)
flii_norm <- flii_scale$
  where(flii_scale$gt(1.0), 1.0)$
  where(flii_scale$lt(0.0), 0.0)$
  updateMask(c_mask)

# # View
# map_norm_idx(flii, 'FLII', palette = pal_idx) +
#   map_norm_idx(flii_norm, 'FLII normalized', palette = pal_idx, show = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Species richness 5km ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rich <- ee$Image(addm('IUCN_Richness_all_5km'))
rsr_id <- addm('IUCN_Richness_5km_2018/RangeSizeRarity_all')
rsr <- ee$Image(rsr_id)

# Rescale
rich_norm <- rescale_to_pctl(rsr)

# Ventiles
rich_vent <- classify_percentiles(rich_norm)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Key Biodiversity Areas ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kba_id <- addm('KBAs_2021_Sep02_maskDHF')

kba_r <- ee$Image(kba_id)

# Reclass values outside of 0-1 range
kba_r <- kba_r$
  where(kba_r$lt(0.94), 0.8)$
  where(kba_r$lt(0.96), 0.9)

# View
# map_norm_idx(kba_r, "Key Biodiversity Areas", TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Deforestation Hot Spots (Harris et al. 2017 via GFW) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hs_id <- addm('Hotspots_GFW_2020')
hs_r <- ee$Image(hs_id)

# Reclass values outside of 0-1 range
hs_r <- hs_r$
  unmask()$
  where(hs_r$eq(2), 1)$    # Intensifying
  where(hs_r$eq(4), 0.9)$  # Persistent
  where(hs_r$eq(3), 0.8)$  # New
  where(hs_r$eq(5), 0.7)$  # Sporadic
  where(hs_r$eq(1), 0.6)   # Diminishing

map_norm_idx(hs_r, 'Hotspots', palette = pal_idx)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subnational Human Development Index ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shdi <- ee$Image(addm('shdi_2arcmin'))

shdi <- shdi$updateMask(shdi$gt(0))

# Normalize to 0-1 from 0-1
shdi_scale <- shdi$unitScale(0, 1) # The lowest value is around 0.5
shdi_norm <- shdi_scale$
  where(shdi_scale$gt(1.0), 1.0)$ 
  where(shdi_scale$lt(0.0), 0.0)$
  updateMask(c_mask)

# # View
# map_norm_idx(shdi_norm, 'Subnational Human Development Index', palette = pal_idx, show = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Development Potential Indices ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dpi_id <- addm('DPI/crop_dpi_geo_int')
dpi_crop <- ee$Image(dpi_id)
dpicrop_pctl <- rescale_to_pctl(dpi_crop, c(20,99))
dpicrop_pctl <- rescale_to_pctl(dpi_crop, c(0,100))

dpi_id <- addm('DPI/bio_dpi_geo_int')
dpi_bio <- ee$Image(dpi_id)
dpibio_pctl <- rescale_to_pctl(dpi_bio, c(20,99))
dpibio_pctl <- rescale_to_pctl(dpi_bio, c(0,100))

pal <- choose_palette(pal = sequential_hcl)
pal <- pal(7)
pal <- c("#FFCC58", "#FF9370", "#F06289", "#CB3598", "#9D0B9A", "#630391", "#001889")
map_norm_idx(dpicrop_pctl, name = 'Cropland DPI', palette = pal, shown = TRUE) +
  map_norm_idx(dpibio_pctl, name = 'Biofuelds DPI', palette = pal)


map_eq_int_10(dpicrop_pctl, name = 'Cropland DPI', palette = pal) +
  map_eq_int_10(dpibio_pctl, name = 'Biofuelds DPI', palette = pal)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Development Potential Indices ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dti_id <- addm('DTI/DTI_2016_pctls_maskDHF')

dti_norm <- ee$Image(dti_id)$updateMask(c_mask)

# Invert values
dti_inv <- dti_norm$multiply(-1)$add(1)

# Rescale to percentiles
dti_vent <- classify_percentiles(dti_inv)

# # View
# map_norm_idx(dti_norm, palette = pal_idx, 'Average and normalize') +
#   map_norm_idx(dti_inv, palette = pal_idx, 'Invert') +
#   map_norm_idx(dti_vent, palette = pal_idx, 'Percentiles')
# 
# map_eq_int(dti_norm, palette = pal_idx, 'Average and normalize') +
#   map_eq_int(dti_inv, palette = pal_idx, 'Invert') +
#   map_eq_int(dti_vent, palette = pal_idx, 'Percentiles')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EPI ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
epi_fc <- ee$FeatureCollection(addm('EPI_2020_LSIB'))

# Convert to raster
epi_ic <- epi_fc$
  reduceToImage(properties = list('EPI_new'), reducer = ee$Reducer$first())$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)

# Rescale
epi_norm <- rescale_to_pctl(epi_ic)$updateMask(c_mask)

# Ventiles
epi_vent <- classify_percentiles(epi_norm)

# # View
# map_norm_idx(epi_vent, 'EPI', palette = pal_idx, show = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EPI: Ecosystem Vitality ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
epi_fc <- ee$FeatureCollection(addm('EPI_2020_LSIB'))

# Convert to raster
epieco <- epi_fc$
  reduceToImage(properties = list('ECO_new'), reducer = ee$Reducer$first())$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)

# Rescale
eco_norm <- rescale_to_pctl(epieco)$updateMask(c_mask)

# Ventiles
eco_vent <- classify_percentiles(eco_norm)

# # View
# map_norm_idx(eco_vent, 'EPI', palette = pal_idx, show = TRUE)
