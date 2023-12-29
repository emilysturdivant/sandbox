
library(tidyverse)
library(sf)
library(terra)
library(units)
library(stars)

# "sudo gsutil -m cp -r gs://ejs-data/raw_inputs/Walker_etal_2022/ \
# ~/repos/sandbox/hih/data/Walker_etal_2022/"

# gdalbuildvrt ~/repos/sandbox/hih/data/from_gee_500m/MBF/alltiles.vrt ~/repos/sandbox/hih/data/from_gee_500m/MBF/*.tif
# gdalbuildvrt tropics.vrt *.tif

out_dir <- here::here('hih/data') 

# Walker AGB
agb_tif <- here::here(out_dir, 'Walker_etal_2022/Base_Cur_AGB_MgCha_500m.tif')
agbm_fp <- here::here(out_dir, 'Base_Cur_AGB_MgCha_500m_forest_MBF.tif')
agb_biome_fp <- here::here(out_dir, 'Base_Cur_AGB_MgCha_500m_MBF.tif')

# Biome
dhf_diss_fp <- here::here(out_dir, 'Ecoregions2017_MBF_realms.geojson')

iplcs_fp <- here::here('hih/data/IPLCs_RRI_tropics.shp')

# Forest
hansen_dir <- here::here(out_dir, 'from_gee_500m/MBF')
forest_tif <- here::here(hansen_dir, 'alltiles_mod.tif')
fmask_fp <- here::here(out_dir, 'forest_mask25.tif')

# Prep forest ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# sudo gsutil -m cp -r gs://ejs-data/processed_hansen/MBF \
# ~/repos/sandbox/hih/data/from_gee_500m/

if(!file.exists(forest_tif)){
  # VRT for Hansen forest cover - doesn't work on VM
  interp_tiles <- list.files(hansen_dir, '*\\.tif', full.names = TRUE)
  forest_vrt <- here::here(hansen_dir, 'alltiles.vrt')
  # gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, forest_vrt, overwrite=T)
  # cd ~/repos/sandbox/hih/data/from_gee_500m/MBF
  # sudo gdalbuildvrt alltiles.vrt *.tif  
  args <- c('gdalbuildvrt', forest_vrt, interp_tiles) 
  out <- system2('sudo', args = args)
  
  # Convert forest to correct MODIS crs
  args <- c('gdal_translate', '-a_srs', "'+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'", 
            forest_vrt, forest_tif) 
  out <- system2('sudo', args = args)
}

# Mask AGB to biome ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!file.exists(agb_biome_fp)){
  agb_mgcha <- terra::rast(agb_tif)
  
  # Load biome polygons and transform
  pols <- st_read(dhf_diss_fp) %>% 
    st_transform(st_crs(agb_mgcha)) 
  
  # Get area of biome
  pols %>% st_area() %>% units::set_units('ha') %>% sum()
  
  # Crop and mask to biome
  agb_mgcha_biome <- agb_mgcha %>% 
    crop(terra::vect(pols), mask=TRUE, 
         filename = agb_biome_fp,
         # datatype = 'INT1U',
         gdal=c("COMPRESS=LZW"),
         overwrite = FALSE)
  # plot(agb_mgcha_biome)
  
} else {
  agb_mgcha_biome <- terra::rast(agb_biome_fp)
}

pix_m <- res(agb_mgcha_biome)[1]
pix_ha <- pix_m * pix_m * 1e-4

# Get sum for biome
c_biome <- agb_mgcha_biome %>% global(fun='sum', na.rm=TRUE)
cstock_biome <- c_biome * pix_ha
cstock_biome

# # Convert C density to stock
# agb_mgc_biome <- agb_mgcha_biome * pix_ha
# plot(agb_mgc_biome)
# 
# # Get sum for biome
# cstock_biome <- agb_mgc_biome %>% global(fun='sum', na.rm=TRUE)
# cstock_biome

# Mask AGB to forest ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mask to forest
if(!file.exists(agbm_fp)){
  # Load biome polygons and transform
  pols <- st_read(dhf_diss_fp) %>% 
    st_transform(st_crs(agb_mgcha_biome)) 
  
  for_r <- terra::rast(forest_tif) %>% 
    crop(terra::vect(pols))
  
  # Reclassify treecover % to forest mask
  rclmat <- rbind(c(-Inf, 0.25, NA), 
                  c(0.25, Inf, 1))
  fmask <- for_r %>% 
    classify(rclmat, 
             include.lowest=TRUE, 
             filename = fmask_fp,
             datatype = 'INT1U',
             overwrite = TRUE)
  
  fmask <- fmask %>% crop(agb_mgcha_biome)
  agb_mgcha_biome <- agb_mgcha_biome %>% crop(fmask)
         
  # Mask to forest
  agb_mgcha_bf <- agb_mgcha_biome %>%
    mask(fmask, maskvalues=NA, filename=agbm_fp, 
         gdal=c("COMPRESS=LZW"),
         overwrite = TRUE)
}

# plot(fmask)

agb_mgcha_bf <- terra::rast(agbm_fp)
plot(agb_mgcha_bf)

# Get sum
c_bf <- agb_mgcha_bf %>% global(fun='sum', na.rm=TRUE)
cstock_bf <- c_bf * pix_ha
cstock_bf

# Mask AGB to IPLC ----

# Load biome polygons and transform
pols <- st_read(iplcs_fp) %>% 
  st_transform(st_crs(agb_mgcha_biome)) 

# Get area of biome
pols %>% st_area() %>% units::set_units('ha') %>% sum()

# Crop and mask to IPLC
agb_mgcha_bip <- agb_mgcha_biome %>% 
  crop(terra::vect(pols), mask=TRUE)
plot(agb_mgcha_bip)

# Get sum for biome
c_bip <- agb_mgcha_bip %>% global(fun='sum', na.rm=TRUE)
cstock_bip <- c_bip * pix_ha
cstock_bip

# Mask AGB to IPLC and forest ----

# Crop and mask to IPLC
agb_mgcha_bipf <- agb_mgcha_bf %>% 
  crop(terra::vect(pols), mask=TRUE)
plot(agb_mgcha_bipf)

# Get sum for biome
c_bipf <- agb_mgcha_bipf %>% global(fun='sum', na.rm=TRUE)
cstock_bipf <- c_bipf * pix_ha
cstock_bipf
