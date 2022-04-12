#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Prep Development Potential Indices* for input to FCI
# Requires:
#     * 
# Author:
#     * esturdivant@woodwellclimate.org, 2021-09-30
# Reference/s:
#     * Oakleaf et al. 2019; Kennedy et al. 2021 (preprint)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here::here('R/fcm/0_initialize.R'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dev: Urban growth probability ---- 
# Urban DPI a la Kennedy et al. 2021: Masked out currently urban and non-urban built-up areas. 
# "We then summed urban growth probability values across the 31-year time 
# interval, resulting in values ranging from 1 (1% probability of expansion 
# in 2050) to 3100 (100% probability of expansion for all 31 years). Given 
# the right-skewed distribution of these data (skewness = 4.3619), values 
# were log transformed and scaled using min-max normalization."

# The raster has 102 categories. 
# Categories from 0 to 100 represent the percentage probability of urbanization in the area. 
# Category 111 refers to existing urban area in 2012, which is defined based 
# on population cutoff at 1000 people/km2 from the average of the LandScan 
# population datasets in 2012 and 2013. 
# Water bodies are masked as NA in the raster. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Probabilities of urban expansion by year from 2020–2050 based on the SLEUTH model.

# If already created:
ugp_id <- addm('GUGPS_2020_2050_logsum')
upgs_log <- ee$Image(ugp_id)
dpi_urb_id <- addm('DPI/urban')
urban_norm <- ee$Image(dpi_urb_id)

# To recreate:
# Load urban growth annual projections as ImageCollection
assets <- ee_manage_assetlist(addm(''))
ugp_ids <- assets$ID %>% str_subset('global_final_')
ugps <- ee$ImageCollection(ugp_ids)

# Mask out current urban areas (classified as 111 for urban in 2012)
ugps_mask <- ugps$map( function(img){ img$updateMask(img$neq(111)) })

# Sum and transform to logarithmic
ugps_sum <- ugps_mask$sum()
# ugps_sum <- ugps_sum$updateMask(c_mask)
# ugps_sum <- ugps_sum$updateMask(ugps_sum$neq(0)) 

# Log transform
upgs_log <- ugps_sum$log()

# Check skewness 
# skew_ugps <- get_skewness(ugps_sum)
# ugps$map(...updateMask($neq(111)) world bestEffort: 14.10031
# ugps$map(...updateMask($neq(111)) world: 14.50011
# ugps$map(...updateMask($neq(111)) sum()$updateMask(sum()$neq(0)) world: 1.349921
# ugps$map(...updateMask($neq(111)) sum()$updateMask(sum()$neq(0)) tropics: 1.444652
# ugps$map(...updateMask($neq(111)) sum()$updateMask(c_mask) world: 10.63062

# Check skewness 
# skew_ugpslog <- get_skewness(upgs_log)
# Skewness log: 0.3412639
# Skewness log10: 0.3412639

# Save image to EE asset
# task_img <- ee_image_to_asset(upgs_log,
#                               basename(ugp_id), 
#                               assetId = ugp_id,
#                               crs = 'EPSG:4326',
#                               scale = 1000,
#                               maxPixels = 803042888,
#                               overwrite = TRUE)
# task_img$start()

upgs_norm <- rescale_to_pctl(upgs_log, pctls = c(0, 100))

# Save image to EE asset
task_img <- ee_image_to_asset(upgs_norm,
                              basename(dpi_urb_id), 
                              assetId = dpi_urb_id,
                              crs = 'EPSG:4326',
                              scale = 1000,
                              maxPixels = 803042888,
                              overwrite = TRUE)
task_img$start()

# Look
Map$addLayer(upgs_log, list(min = 0, max = 8, palette = pal_idx)) +
  map_norm_idx(upgs_norm$unmask(), name = 'Urban DPI')

# # MODIS land cover ----
# lc <- ee$ImageCollection("MODIS/006/MCD12Q1")$
#   filter(ee$Filter$calendarRange(2020, 2020, 'year'))$
#   first()$
#   select('LC_Type1')
# 
# # Look
# Map$addLayer(upgs_log$unmask(), list(min = -0.1, max = 8, palette = pal_idx)) +
#   Map$addLayer(lc$updateMask(lc$eq(13)), list(min = 0, max = 13, palette = c('red', 'black')))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Development threat ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dpi_dir <- here::here(data_dir, 'Dev_Threat_Index', 'dev_potential_indices_2016')
fps <- list.files(dpi_dir, 'lulc.*dpi_g.*\\.tif$', full.names = TRUE, recursive = TRUE)

# Reduce file
(fp <- fps[[1]])
fp_out <- fp %>% 
  str_remove_all('.*lulc-development-potential-indices_') %>% 
  str_replace_all('_geographic', '_geo_int')
fp_out <- here::here(dpi_dir, fp_out)

r <- terra::rast(fp)
{r * 100} %>% terra::writeRaster(fp_out, datatype = 'INT1U', overwrite = TRUE)

# fps <- list.files(dpi_dir, '*dpi_g.*\\.tif$', full.names = TRUE, recursive = TRUE)
# (fp <- fps[[2]])
# r <- terra::rast(fp)
# r %>% terra::writeRaster(
#   "/Users/emilysturdivant/data/Dev_Threat_Index/dev_potential_indices_2016/convgas_dpi_geo_int.tif",
#   datatype = 'INT1U', overwrite = TRUE)
# 
# 
# # Unzip to temp dir
# miao <- tempfile()
# unzip(z, exdir = miao)
# 
# # Load shapefile (polygons)
# (shp_fp <- list.files(miao, pattern = "polygons\\.shp$", full.names=TRUE, recursive = TRUE))
# pa <- st_read(shp_fp[[1]])

# # Create ImageCollection
# ee_manage_create(
#   path_asset = addm("DPI"),
#   asset_type = "ImageCollection"
# )
# 
# dpi_eelist <- ee_manage_assetlist(path_asset = addm("DPI_v1"))
# 
# # Move images from DPI_v1 folder into ImageCollection
# ee_manage_move(
#   path_asset = addm("DPI_v1"),
#   final_path = addm("DPI")
# )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine Development Potential Indices into Threat Index ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exclude_list <- c('crop_dpi')

# Load ImageCollection 
dpi_eelist <- ee_manage_assetlist(path_asset = addm("DPI")) %>% 
  filter(str_detect(ID, exclude_list, negate = TRUE))

# Reclass each index
dpi <- dpi_eelist$ID %>% 
  purrr::map(function(x) {
    img <- ee$Image(x)$unmask()
    rescale_to_pctl(img, c(0, 100))
    # classify_percentiles(img)
  }) %>% 
  ee$ImageCollection()

# Additive / equal weights / simple average 
dti <- dpi$sum()$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)

# Normalize
dti_norm <- rescale_to_pctl(dti, c(0, 100))

# # Save image to EE asset
# task_img2 <- ee_image_to_asset(dti_norm,
#                                'DTI_2016', 
#                                assetId = dti_id,
#                                region = tropics_bb,
#                                crs = 'EPSG:4326',
#                                scale = 1000,
#                                maxPixels = 312352344, 
#                                overwrite = TRUE)
# task_img2$start()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add Protected Area constraint to DPIs ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exclude_list <- c('crop_dpi')

# Load ImageCollection 
dpi_eelist <- ee_manage_assetlist(path_asset = addm("DPI")) %>% 
  filter(str_detect(ID, exclude_list, negate = TRUE))

# Load PA polygons
pa_mask <- pas_r$neq(1)

# Look
img <- ee$Image(dpi_eelist$ID[[1]])$unmask()$updateMask(pa_mask)
map_norm_idx(img, name = 'DPIs')

# Reclass each index
dpi <- dpi_eelist$ID %>% 
  purrr::map(function(x) {
    img <- ee$Image(x)$unmask()$updateMask(pa_mask)
    rescale_to_pctl(img, c(0, 100))
    # classify_percentiles(img)
  }) %>% 
  ee$ImageCollection()

# Additive / equal weights / simple average 
dti <- dpi$sum()$
  setDefaultProjection(crs = 'EPSG:4326', scale = 1000)

# Normalize
dti_norm <- rescale_to_pctl(dti, c(0, 100))

# # Save image to EE asset
# task_img2 <- ee_image_to_asset(dti_norm,
#                                'DTI_2016', 
#                                assetId = dti_id,
#                                region = tropics_bb,
#                                crs = 'EPSG:4326',
#                                scale = 1000,
#                                maxPixels = 312352344, 
#                                overwrite = TRUE)
# task_img2$start()

# Look
map_norm_idx(dti_norm, name = 'DPIs')
