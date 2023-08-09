
library(tidyverse)
library(sf)
library(terra)
library(units)

# Path to ROI
fp_polys <- here::here(site_poly_dir, 'xingu_request_july2023.geojson')

# Create VRTs ----
# Directory with Hansen tiles of treecover2000 and lossyear
hansen_dir <- '/Volumes/ejs_storage/data/raw_data/Hansen_etal_2013/v1.10'

# Hansen 2000 tree cover
interp_tiles <- list.files(hansen_dir, '*\\.tif', full.names = TRUE) %>% 
  str_subset('treecover2000')
tc2000_vrt <- here::here(hansen_dir, 'alltiles_treecover.vrt')
gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, tc2000_vrt, overwrite=T)

# Hansen loss year
interp_tiles <- list.files(hansen_dir, '*\\.tif', full.names = TRUE) %>% 
  str_subset('lossyear')
lossyr_vrt <- here::here(hansen_dir, 'alltiles_lossyear.vrt')
gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, lossyr_vrt, overwrite=T)

# fp_polys = params$polys
# lossyr_vrt = params$lossyear_fp
# tc2000_vrt = params$tc2000_fp
# agb_vrt = params$agb30_fp

# Load and prep data ----
# Load and reproject polygons
pols <- st_read(fp_polys) %>% # Load from shapefile
  st_transform(st_crs('EPSG:4326')) %>% 
  mutate(area_ha = st_area(geometry) %>% set_units('ha') %>% drop_units())

# Load, clip, and mask raster to AOI polygons
r_lossyr <- terra::rast(lossyr_vrt, win=pols)
r_tc2000 <- terra::rast(tc2000_vrt, win=pols)

# Area of each pixel
r_areaha <- cellSize(r_lossyr, unit='ha')

# Apply the 25% threshold to get forest area in 2000
r_fc00 <- compare(r_tc2000, 25, ">", falseNA=TRUE)
r_areaha_fc00 <- r_areaha * r_fc00
names(r_areaha_fc00) <- 'fc_area_2000'

# Create annual loss rasters ----
# Convert loss year and 2000 carbon to annual layers of area and carbon lost
yrvals <- r_lossyr %>% unique() %>% slice(-1) %>% pull(alltiles_lossyear)
r_annloss <- yrvals %>% 
  purrr::map(function(x){
    r_loss_ann <- compare(r_lossyr, x, "==", falseNA=TRUE)
    
    # Total area
    r_area <- r_areaha %>% mask(r_loss_ann)
    names(r_area) <- paste0('a', 2000+x)
    
    # Forested area
    r_fc_area <- r_areaha_fc00 %>% mask(r_loss_ann)
    names(r_fc_area) <- paste0('f', 2000+x)
    
    # Return all three
    return(c(r_area, r_fc_area))
  }) %>% 
  rast()
