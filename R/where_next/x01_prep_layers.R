#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Prep layers for HIH scaling
# Requires:
#     * 
# Author:
#     * esturdivant@woodwellclimate.org, 2021-09-30
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(sf)
library(terra)
library(tmap)
tmap_mode('view')
library(tidyverse)

# Initialize ----
data_dir <- '/Users/emilysturdivant/data'

# Create tropics extent ----
e <- ext(c(xmin = -180, xmax = 180, ymin = -23.3, ymax = 23.3))
bb <- c(e$xmin, e$ymin, e$xmax, e$ymax)
tropics_rect <- st_as_sfc(st_bbox(bb, crs = st_crs(4326)))

tropics_rect_shp <- file.path(data_dir, 'context', 'tropics_rect.shp')
if(!file.exists(tropics_rect_shp)) {
  tropics_rect %>% st_write(tropics_rect_shp)
}

# Create tropics extent ----
e <- ext(c(xmin = -180, xmax = 180, ymin = -26, ymax = 26))
bb <- c(e$xmin, e$ymin, e$xmax, e$ymax)
tropics_26 <- st_as_sfc(st_bbox(bb, crs = st_crs(4326)))

# Extract countries in tropics and add MSF flag ----
standardize_text <- function(x){
  x %>% as.character() %>% 
    str_trim() %>% 
    str_to_upper() %>% 
    stringi::stri_trans_general(str=., id='Latin-ASCII') %>% 
    str_replace_all(' +', ' ') %>% 
    str_replace_all('(, )+', ', ') %>% 
    str_replace_all('( ,)+', ',') %>% 
    str_remove_all('^,') %>% 
    str_trim()
}

# MSF countries
msf_countries <- read_csv(file.path('data', 'MSF_countries.csv'))
msf_countries <- msf_countries %>% 
  mutate(country = standardize_text(country), 
         MSF = 1) %>% 
  arrange(country)

# Load GADM country boundaries as singlepart
countries <- st_read(file.path(data_dir, 'gadm', 'gadm36_0.shp'),
                     promote_to_multi = TRUE) %>% 
  st_cast("POLYGON")

# Simplify and subset to those that intersect tropics
countries <- countries %>% st_simplify(dTolerance = 0.001)
countries <- countries[unlist(st_intersects(tropics_rect, countries)),]

# Remove parts of countries < 2km2
countries['area'] <- countries %>% st_area()
countries <- countries %>% 
  filter(area > units::set_units(4, 'km^2')) %>% 
  group_by(NAME_0) %>% 
  summarize()

# Join with MSF
countries <- countries %>% 
  mutate(country = standardize_text(NAME_0)) %>% 
  left_join(msf_countries, by = 'country')

# Save
countries %>% st_write(file.path(data_dir, 'gadm', 'gadm36_0_tropics_simp001big3.shp'))

tm_shape(countries) + tm_polygons(col = 'MSF')

# Extract Protected Areas in the tropics ----
# Protected area polygons ----
pa_zips <- list.files(
  file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_Public_shp'),
  pattern = ".zip$", full.names=TRUE, recursive = TRUE)
# 
# # Unzip
# unzip_and_filter_pa_polygons <- function(z) {
#   # z <- pa_zips[[1]]
#   
#   # Unzip to temp dir
#   miao <- tempfile()
#   unzip(z, exdir = miao)
#   
#   # Load shapefile (polygons)
#   (shp_fp <- list.files(miao, pattern = ".shp$", full.names=TRUE, recursive = TRUE))
#   pa <- st_read(shp_fp[[2]])
#   
#   # Filter
#   pa <- pa %>% 
#     filter(MARINE != 2) %>% 
#     st_simplify(dTolerance = 0.001) 
#   
#   # subset polygons to those that intersect tropics rectangle
#   pa_tropics <- pa[unlist(st_intersects(tropics_rect, pa)),]
#   
#   return(pa_tropics)
# }
# 
# pa_tropics <- pa_zips %>% purrr::map_dfr(unzip_and_filter_pa_polygons)
# pa_tropics %>% 
#   st_write(file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_polygons_tropics_simp001.gpkg'))
# 
# # Protected area points ----
# # pa_zip <- file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_Public_shp', 'WDPA_Oct2021_Public_shp.zip')
# pa_zips <- list.files(
#   file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_Public_shp'),
#   pattern = ".zip$", full.names=TRUE, recursive = TRUE)
# 
# # Unzip
# unzip_and_filter_pa_points <- function(z) {
#   # z <- pa_zips[[1]]
#   
#   # Unzip to temp dir
#   miao <- tempfile()
#   unzip(z, exdir = miao)
#   
#   # Load shapefile (polygons)
#   (shp_fp <- list.files(miao, pattern = "points\\.shp$", full.names=TRUE, recursive = TRUE))
#   pa <- st_read(shp_fp[[1]])
#   
#   pa <- pa[unlist(st_intersects(tropics_rect, pa)),]
#   pa <- pa %>% filter(REP_AREA > 0, MARINE != 2)
#   pa_buff <- pa %>% st_buffer(8.5*0.001)
#   
#   return(pa_buff)
# }
# 
# pa_tropics <- pa_zips %>% purrr::map_dfr(unzip_and_filter_pa_points)
# pa_tropics %>% 
#   st_write(file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_points_tropics_buff0085.gpkg'))
# 
# # Merge
# pa_polys <- st_read(file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_polygons_tropics_simp001.gpkg'))
# pa_tropics2 <- bind_rows(pa_polys, pa_tropics)
# 
# pa_tropics2 %>% object.size() %>% print(units = "MB")
# pa_tropics2 %>% 
#   st_write(file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_tropics_simp001_buff0085.gpkg'))

# OECM polygons ----
# pa_zip <- file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_Public_shp', 'WDPA_Oct2021_Public_shp.zip')
pa_zips <- list.files(
  file.path(data_dir, 'protected_areas', 'WDOECM_Oct2021_Public_shp'),
  pattern = ".zip$", full.names=TRUE, recursive = TRUE)

# Unzip
unzip_and_filter_pas <- function(z) {
  # z <- pa_zips[[1]]
  
  # Unzip to temp dir
  miao <- tempfile()
  unzip(z, exdir = miao)
  
  # Load shapefile (polygons)
  (shp_fp <- list.files(miao, pattern = "polygons\\.shp$", full.names=TRUE, recursive = TRUE))
  pa <- st_read(shp_fp[[1]])
  
  # Filter
  pa <- pa %>% 
    filter(MARINE != 2) %>% 
    st_simplify(dTolerance = 0.001) 
  
  # subset polygons to those that intersect tropics rectangle
  pa_polys <- pa[unlist(st_intersects(tropics_rect, pa)),]
  
  # Load points
  (shp_fp <- list.files(miao, pattern = "points\\.shp$", full.names=TRUE, recursive = TRUE))
  pa <- st_read(shp_fp[[1]])
  pa <- pa[unlist(st_intersects(tropics_rect, pa)),]
  pa <- pa %>% filter(REP_AREA > 0, MARINE != 2)
  pa_points <- pa %>% st_buffer(8.5*0.001)
  
  # Combine
  pa_tropics <- bind_rows(pa_polys, pa_points)
  
  # Return
  return(pa_tropics)
}

pa_tropics <- pa_zips %>% purrr::map_dfr(unzip_and_filter_pas)
tm_shape(pa_tropics) + tm_polygons()
pa_tropics %>% 
  st_write(file.path(data_dir, 'protected_areas', 'WDOECM_Oct2021_tropics_simp001_buff0085.gpkg'))


# Convert Allen zoonotic spillover predictions to GeoTIFFs ----
load('data/predictions.RData')

pred_list <- c('bsm_response', 'bsm_weight_pubs', 'bsm_weight_pop') %>% 
  purrr::map(function(lyr_name){
    # Convert to raster
    pred <- predictions %>% 
      dplyr::select(lon, lat, matches(lyr_name)) %>% 
      dplyr::rename(x=lon, y=lat)
    
    # Convert to SpatialPixelsDataFrame
    sp::coordinates(pred) = ~ x + y
    sp::proj4string(pred) = sp::CRS("+init=epsg:4326") # set it to lat-long
    sp::gridded(pred) <- TRUE
    
    # Convert to SpatRaster
    predr <-  raster::raster(pred) %>% rast()
  }
  )

pred_stack <- terra::rast(pred_list)
plot(pred_stack)

# Save
writeRaster(pred_stack, str_c('data/zoonotic_eid_risk.tif'))


lyr_name <- 'bsm_response'
lyr_name <- 'bsm_weight_pubs'
lyr_name <- 'bsm_weight_pop'

# Convert to raster
pred <- predictions %>% 
  dplyr::select(lon, lat, matches(lyr_name)) %>% 
  dplyr::rename(x=lon, y=lat)

# Convert to SpatialPixelsDataFrame
sp::coordinates(pred) = ~ x + y
sp::proj4string(pred) = sp::CRS("+init=epsg:4326") # set it to lat-long
sp::gridded(pred) <- TRUE

# Convert to SpatRaster
predr <-  raster::raster(pred) %>% rast()
plot(predr)

# Save
writeRaster(predr, str_c('data/allen_', lyr_name, '.tif'))

# Prep Global Safety Net ----
# Global Safety Net ----
gsn_shps <- list.files(
  file.path(data_dir, 'global_safety_net', 'global_safety_net'),
  pattern = ".shp$", full.names=TRUE, recursive = TRUE)

# Unzip
subset_to_tropics <- function(shp_fp) {
  # shp_fp <- gsn_shps[[1]]
  
  # Load data
  pa <- st_read(shp_fp)
  
  # Filter
  pa <- pa %>% 
    st_simplify(dTolerance = 0.001) 
  
  # subset polygons to those that intersect tropics rectangle
  pa_polys <- pa[unlist(st_intersects(tropics_rect, pa)),]
  
  (fn <- str_split(shp_fp, '/') %>% last() %>% last())
  pa_polys %>% 
    st_write(file.path(data_dir, 'global_safety_net', 'tropics_simp001', 
                       str_c(tools::file_path_sans_ext(fn), '.gpkg')))
}

gsn_shps[[1]] %>% purrr::walk(subset_to_tropics)

# Convert Allen zoonotic spillover predictions to GeoTIFFs ----
load('data/predictions.RData')

lyr_name <- 'bsm_response'
lyr_name <- 'bsm_weight_pubs'
lyr_name <- 'bsm_weight_pop'

# Convert to raster
pred <- predictions %>% 
  dplyr::select(lon, lat, matches(lyr_name)) %>% 
  dplyr::rename(x=lon, y=lat)

# Convert to SpatialPixelsDataFrame
sp::coordinates(pred) = ~ x + y
sp::proj4string(pred) = sp::CRS("+init=epsg:4326") # set it to lat-long
sp::gridded(pred) <- TRUE

# Convert to SpatRaster
predr <-  raster::raster(pred) %>% rast()
plot(predr)

# Save
writeRaster(predr, str_c('data/allen_', lyr_name, '.tif'))

# Intact Forest Landscape ----
in_dir <- file.path(data_dir, 'forests', 'IFL_2016')
out_dir <- file.path(data_dir, 'forests', 'ifl_2016_tropics')
ifl_shp <- list.files(in_dir, pattern = ".shp$", full.names=TRUE, recursive = TRUE)

# Unzip
subset_to_tropics <- function(shp_fp) {
  # shp_fp <- gsn_shps[[1]]
  
  # Load data
  pa <- st_read(shp_fp)
  
  # Filter
  pa <- pa %>% 
    st_simplify(dTolerance = 0.001) 
  
  # subset polygons to those that intersect tropics rectangle
  pa_polys <- pa[unlist(st_intersects(tropics_rect, pa)),]
  
  (fn <- str_split(shp_fp, '/') %>% last() %>% last())
  pa_polys %>% 
    st_write(out_dir, str_c(tools::file_path_sans_ext(fn), '_simp001.gpkg'))
}

ifl_shp[[1]] %>% purrr::walk(subset_to_tropics)

# FLII ----
out_dir <- file.path(data_dir, 'forests', 'flii_tropics2')
(flii_tifs <- list.files(file.path(data_dir, 'forests', 'flii'), 
                         pattern = 'flii_[^earth].*\\.tif$', 
                         full.names = TRUE))

crop_and_agg <- function(fp) {
  # Get output name
  fn_ac <- file.path(out_dir, str_c(basename(tools::file_path_sans_ext(fp)), '_agg3.tif'))
  
  # Load raster
  r <- terra::rast(fp)
  
  # Get intersection of the bounding boxes of the two rasters 
  # (didn't work with terra::intersect)
  r_bbox <- terra::ext(r) %>% as.vector() %>% sf::st_bbox() %>% sf::st_as_sfc()
  e_bbox <- e %>% as.vector() %>% sf::st_bbox() %>% sf::st_as_sfc()
  bb <- sf::st_intersection(r_bbox, e_bbox)
  if(length(bb) == 0) {
    print('Input raster does not include tropics.')
    return()
  }
  
  bb <- bb %>% sf::st_bbox() %>% as.vector()
  
  # Convert to terra extent object
  bbex <- terra::ext(bb[c(1, 3, 2, 4)])
  
  # Crop
  NAflag(r) <- -9999
  r <- terra::crop(r, bbex)
  
  # Aggregate and save
  terra::aggregate(r, fact = 3, na.rm = TRUE, filename = fn_ac, overwrite = TRUE, 
                   datatype = 'INT2U')
}

flii_tifs %>% purrr::walk(crop_and_agg)

# gdalUtils::gdalwarp(srcfile = fp, 
#                     dstfile = fn_ac, 
#                     te = bb, 
#                     tr = c(xres(flii), yres(flii)), 
#                     tap = TRUE, 
#                     overwrite = TRUE)

# Mosaic resulting rasters
in_dir <- file.path(data_dir, 'forests', 'flii_tropics2')
out_dir <- file.path(data_dir, 'forests', 'flii_tropics3')
(flii_tifs <- list.files(in_dir, 
                         pattern = 'flii_[^earth].*\\.tif$', 
                         full.names = TRUE))

gdalUtils::mosaic_rasters(flii_tifs, file.path(out_dir, 'flii_tropics_agg3.tif'))

# Human modification ----
in_dir <- file.path(data_dir, 'human_influence', 'gHM')
out_dir <- in_dir
(tifs <- list.files(in_dir, pattern = 'gHM\\.tif$', full.names = TRUE))
fp <- tifs[[1]]

# Get output name
fn_ac <- file.path(out_dir, str_c(basename(tools::file_path_sans_ext(fp)), '_wgs_tropics.tif'))

# Load raster ----
r <- terra::rast(fp)

# Reproject from Mollweide to WGS84
crs(r)
r_p <- terra::project(r, "epsg:4326")

# Get intersection of the bounding boxes of the two rasters
r_bbox <- terra::ext(r_p) %>% as.vector() %>% sf::st_bbox() %>% sf::st_as_sfc()
e_bbox <- e %>% as.vector() %>% sf::st_bbox() %>% sf::st_as_sfc()
bb <- sf::st_intersection(r_bbox, e_bbox)
if(length(bb) == 0) {
  print('Input raster does not include tropics.')
  return()
}

# Convert to terra extent object
bb <- bb %>% sf::st_bbox() %>% as.vector()
bbex <- terra::ext(bb[c(1, 3, 2, 4)])

# Crop and save
terra::crop(r_p, bbex, filename = fn_ac, overwrite = TRUE)

# Combine ----
# Load Human modification
hm_fp <- file.path(data_dir, 'human_influence', 'gHM', 'gHM_wgs_tropics.tif')
hm <- terra::rast(fn_ac)

# Load FLII
out_dir <- file.path(data_dir, 'forests', 'flii_tropics3')
flii_fp <- file.path(data_dir, 'forests', 'flii_tropics3', 'flii_tropics_agg3.tif')
flii <- terra::rast(flii_fp)

# Resample to common grid
hm

# Adjust range
flii2 <- flii / 10000


# Plotting (old) ----
# Forest Landscape Integrity Index
flii_dir <- "/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/data/flii"
(flii_tifs <- list.files(flii_dir, '*.tif', full.names = TRUE))

# Load data
flii_africa <- terra::rast(flii_tifs[[1]])
flii_africa <- flii_africa / 1000
summary(flii_africa)
NAflag(flii_africa) <- -9999

plot(flii_africa)

# make test area
library(tmap)
tmap_mode('view')

e <- ext(c(xmin = 22, xmax = 41, ymin = -12, ymax = 2))
bb <- c(e$xmin, e$ymin, e$xmax, e$ymax)

tm_shape(st_as_sfc(st_bbox(bb))) + tm_borders()

flii_sub <- flii_africa %>% crop(e)
plot(flii_sub)

flii_subr <- as(flii_sub, 'Raster')
tm_shape(flii_subr) + tm_raster()

library(mapview)
mapview::mapview(flii_sub)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Global Data Lab ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gdl_dir <- file.path(data_dir, 'human_health', 'GDL_2019')
shp <- file.path(gdl_dir, 'GDL Shapefiles V4', 'GDL Shapefiles V4.shp')
sf <- st_read(shp)

# Simplify and subset to those that intersect tropics
sfs <- sf %>% st_simplify(dTolerance = 0.01)
sf.st <- sfs[unlist(st_intersects(tropics_26, sfs)),]

tm_shape(sf.st) + tm_polygons(col = 'shdi')

# Load other indicators
csv <- file.path(gdl_dir, 'GDL-Life-expectancy-data.csv')
le <- read_csv(csv) %>% 
  select(GDLcode = GDLCODE,
         LE = `2019`)

csv <- file.path(gdl_dir, 'GDL-Health-index-data.csv')
hi <- read_csv(csv) %>% 
  select(GDLcode = GDLCODE,
         HI = `2019`)

# Join to GDL subnational units ----
sf.le <- sf.st %>% left_join(le, by = 'GDLcode')
sf.ind <- sf.le %>% left_join(hi, by = 'GDLcode')

tm_shape(sf.ind) + tm_polygons(col = 'shdi', lwd = NA)
tm_shape(sf.ind) + tm_polygons(col = 'LE', lwd = NA)
tm_shape(sf.ind) + tm_polygons(col = 'HI', lwd = NA)

sf.ind %>% st_write(file.path(gdl_dir, 'GDL_subnational_hdi_le_hi.shp'), 
                    append = FALSE)
# sf.ind %>% sf_as_ee('getInfo_to_asset', addm('GDL_subnational_hdi_le_hi'))
# ee_monitoring()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Global Burden of Disease ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gbd_dir <- file.path(data_dir, 'human_health', 'GBD_2019')
adml1_simp_fp <- file.path(data_dir, 'gadm', 'gadm36_1_tropics_simp01big4.rds')

# Load GADM country boundaries as singlepart
prep_gadm <- function(fp, grp_var = 'NAME_0') {
  # Load
  adm <- st_read(fp, promote_to_multi = TRUE) %>%
    st_cast("POLYGON")
  
  # Simplify and subset to those that intersect tropics
  adm <- adm %>% st_simplify(dTolerance = 0.01)
  adm <- adm[unlist(st_intersects(tropics_26, adm)),]
  
  # Remove parts of countries < 2km2
  adm['area'] <- adm %>% st_area()
  adm <- adm %>% 
    filter(area > units::set_units(2, 'km^2')) %>% 
    group_by(.dots = grp_var) %>% 
    summarize()

}

# Load CSVs
dalys <- read_csv(file.path(gbd_dir, 'DALYs_2019_tropics_subnational.csv')) %>% 
  select(location_id, location_name, 
         dalys = val)
mortU5_noshocks <- read_csv(file.path(gbd_dir, 'MORTALITY_1950_2019_5Q0_NOSHOCK_Y2020M07D31.CSV')) %>% 
  filter(sex_name == 'both', year_id == 2019) %>% 
  select(location_id, location_name, 
         u5mortNS = val)
mortU5 <- read_csv(file.path(gbd_dir, 'MORTALITY_1950_2019_5Q0_WSHOCK_Y2020M11D13.CSV')) %>% 
  filter(sex_name == 'both', year_id == 2019) %>% 
  select(location_id, location_name, 
         u5mort = val)

# Join
gbd_metrics <- mortU5 %>% 
  full_join(select(mortU5_noshocks, -location_name), by = 'location_id') %>% 
  full_join(select(dalys, -location_name), by = 'location_id') %>% 
  select(-location_id)

# Adjust certain country names to match GADM
gbd_metrics <- gbd_metrics %>% 
  mutate(location_name = location_name %>% 
           str_replace_all('United Republic of Tanzania', 'Tanzania') %>% 
           str_replace_all('Bolivia \\(P.*', 'Bolivia') %>% 
           str_replace_all('Venezuela \\(.*', 'Venezuela') %>% 
           str_replace_all('^Congo$', 'Republic of Congo') %>% 
           str_replace_all('^Viet Nam$', 'Vietnam') %>% 
           str_replace_all("^Lao People's Democratic Republic$", 'Laos'))

# Join to administrative units level 1 
if(!file.exists(adml1_simp_fp)) {
  adm_l1_fp <- file.path(data_dir, 'gadm', 'gadm36_1.shp')
  adm_l1 <- prep_gadm(adm_l1_fp, c('NAME_0', 'NAME_1'))
  adm_l1 %>% saveRDS(adml1_simp_fp)
} else {
  adm_l1 <- readRDS(adml1_simp_fp)
}

# Left join DALYs to countries and subnational units
adm1_gbd <- adm_l1 %>% 
  inner_join(gbd_metrics, by = c('NAME_1' = 'location_name'))

adm0_gbd <- adm_l1 %>% 
  inner_join(gbd_metrics, by = c('NAME_0' = 'location_name')) %>% 
  anti_join(st_drop_geometry(adm1_gbd), by = 'NAME_1')

# Combine
gbd_sf <- bind_rows(adm0_gbd, adm1_gbd)
gbd_sf <- gbd_sf %>% 
  group_by(NAME_0, u5mort, u5mortNS, dalys) %>% 
  summarize() %>%
  nngeo::st_remove_holes(100000)

fp_out <- here::here(gbd_dir, 'processed', 'GBD_2019_tropics.shp')
gbd_sf %>% st_write(fp_out, append = FALSE)

gbd_sf <- st_read(fp_out)
tm_shape(st_as_sf(as.data.frame(gbd_sf))) + tm_polygons(col = 'dalys')

