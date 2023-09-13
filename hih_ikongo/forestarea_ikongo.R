#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Calculate annual forest and carbon losses for input polygons from 30 m data
# Requires:
#     * site polygon
#     * Hansen tree cover and lossyear
# Author:
#     * esturdivant@woodwellclimate.org, 2023-07
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here::here('R/carbon_report_utils.R'))

library(tmap)
tmap_mode('view')

home_dir <- here::here('hih_ikongo')
out_dir <- here::here(home_dir, 'outputs_forestarea')
dir.create(out_dir, recursive=TRUE)

site_poly_dir <- '~/data/hih_sites/Madagascar_Ikongo'
fp_polys <- here::here(site_poly_dir, 'Ikongo_communes_BNRGC2018_MSF17.geojson')

# Create VRTs ----
# 30m AGB
agb_dir <- '/Volumes/ejs_storage/data/raw_data/biomass/global_30m_year2000_v5'
interp_tiles <- list.files(agb_dir, '*\\.tif', full.names = TRUE)
agb30m_vrt <- here::here(agb_dir, 'alltiles.vrt')
gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, agb30m_vrt, overwrite=T) # dryrun=TRUE

# Hansen 2000 tree cover
hansen_dir <- '/Volumes/ejs_storage/data/raw_data/Hansen_etal_2013/v1.10'
interp_tiles <- list.files(hansen_dir, '*\\.tif', full.names = TRUE) %>% 
  str_subset('treecover2000')
tc2000_vrt <- here::here(hansen_dir, 'alltiles_treecover.vrt')
gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, tc2000_vrt, overwrite=T)

# Hansen loss year
interp_tiles <- list.files(hansen_dir, '*\\.tif', full.names = TRUE) %>% 
  str_subset('lossyear')
hansen_vrt <- here::here(hansen_dir, 'alltiles_lossyear.vrt')
gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, hansen_vrt, overwrite=T)

# Prep df ----
params <-
  list(
    filter = c(ADM0_EN = 'Madagascar'),
    polys = fp_polys,
    site_var = 'ADM2_EN',
    subdiv_var='ADM3_EN',
    years = c(2003, 2020),
    single_site = FALSE,
    agb30_fp = agb30m_vrt, 
    tc2000_fp = tc2000_vrt,
    lossyear_fp = hansen_vrt
  )

overwrite <- FALSE
loss_csv <- here::here(out_dir, 'loss_30m_Hansen_v1.10_expanded.csv')

# Get zonal sums from 30m data----
if( !file.exists(loss_csv) | overwrite ){
  loss_df <- extract_zonal_sums_30m(params, out_csv=loss_csv, only_losses=FALSE)
}
loss_df <- readr::read_csv(loss_csv)

# Calculate forest area in most recent year ----
tc_22 <- loss_df %>% 
  select(div_name, site, Year, fc_area_2000, loss_fc_ha) %>% 
  group_by(div_name) %>% 
  mutate(loss_since_2000 = cumsum(loss_fc_ha), 
         fc_area_ha = fc_area_2000 - loss_since_2000) %>% 
  select(-loss_fc_ha) %>% 
  slice_max(Year)

# Primary forest ----
primforest_tif = '/Volumes/ejs_storage/data/raw_data/Turubanova_etal_2018_primaryforest2001/Madagsacar_2001_primary.tif'

fp_polys = params$polys
hansen_vrt = params$lossyear_fp
tc2000_vrt = params$tc2000_fp
agb_vrt = params$agb30_fp

# Load and prep data ----
# Load and reproject polygons
pols <- st_read(fp_polys) %>% # Load from shapefile
  st_transform(st_crs('EPSG:4326')) %>% 
  mutate(area_ha = st_area(geometry) %>% set_units('ha') %>% drop_units())

# Load, clip, and mask raster to AOI polygons
r_lossyr <- terra::rast(hansen_vrt, win=pols) %>% mask(pols)
r_tc2000 <- terra::rast(tc2000_vrt, win=pols) %>% mask(pols)
r_mgha <- terra::rast(agb_vrt, win=pols) %>% mask(pols)
r_f1_01 <- terra::rast(primforest_tif, win=pols) %>% mask(pols)

# Area of each pixel
r_areaha <- cellSize(r_lossyr, unit='ha')

# convert biomass density (Mg/ha) to carbon stock (MgC)
r_mgc <- r_mgha * 0.5 * r_areaha; names(r_mgc) <- 'mgc_2000'

# Apply the 25% threshold to get forest area in 2000
r_fc00 <- compare(r_tc2000, 25, ">", falseNA=TRUE)
r_areaha_fc00 <- r_areaha * r_fc00
names(r_areaha_fc00) <- 'fc_area_2000'

# Get primary forest area in 2001
r_areaha_f1_01 <- r_areaha * r_f1_01
names(r_areaha_f1_01) <- 'f1_area_2001'

# Create annual loss rasters ----
# Convert loss year and 2000 carbon to annual layers of area and carbon lost
yrvals <- r_lossyr %>% unique() %>% slice(-1) %>% pull(alltiles_lossyear)
r_annloss <- yrvals %>% 
  purrr::map(function(x){
    r_loss_ann <- compare(r_lossyr, x, "==", falseNA=TRUE)
    
    # Carbon stock
    r_stock <- r_mgc %>% mask(r_loss_ann)
    names(r_stock) <- paste0('c', 2000+x)
    
    # Total area
    r_area <- r_areaha %>% mask(r_loss_ann)
    names(r_area) <- paste0('a', 2000+x)
    
    # # Forested area
    # r_fc_area <- r_areaha_fc00 %>% mask(r_loss_ann)
    # names(r_fc_area) <- paste0('f', 2000+x)
    
    # Primary forest loss
    r_f1_area <- r_areaha_f1_01 %>% mask(r_loss_ann)
    names(r_f1_area) <- paste0('f', 2000+x)
    
    # Return all three
    return(c(r_stock, r_area, r_f1_area))
  }) %>% 
  rast()

# Sum annual losses ----
# Add 2000 carbon stock and area to polygons
pols <- r_mgc %>% extract(pols, fun=sum, na.rm=TRUE, bind=TRUE)
# pols <- r_areaha_fc00 %>% extract(pols, fun=sum, na.rm=TRUE, bind=TRUE)
pols <- r_areaha_f1_01 %>% extract(pols, fun=sum, na.rm=TRUE, bind=TRUE)
pols %>% 
  st_as_sf() %>% 
  st_drop_geometry() %>% 
  rename(div_name=params$subdiv_var, 
         site=params$site_var)

# Extract
sums <- r_annloss %>% 
  extract(pols, fun=sum, na.rm=TRUE, bind=TRUE) %>% 
  st_as_sf() %>% 
  st_drop_geometry() %>% 
  rename(div_name=params$subdiv_var, 
         site=params$site_var) %>% 
  mutate(across(mgc_2000:last_col(), ~ replace_na(.x, 0)))

# Tidy area lost
loss_ha <- sums %>% 
  dplyr::select(div_name, area_ha, contains('a20')) %>% 
  pivot_longer(starts_with('a20'), 
               names_to='Year', names_prefix='a', 
               values_to='loss_ha') %>% 
  mutate(loss_pct_area = loss_ha / area_ha)# %>% 
  # dplyr::select(-area_ha)

# Tidy forest area lost
loss_fc_ha <- sums %>% 
  dplyr::select(div_name, f1_area_2001, contains('f20')) %>% 
  pivot_longer(starts_with('f20'), 
               names_to='Year', names_prefix='f', 
               values_to='loss_fc_ha') %>% 
  mutate(loss_pct_fc_area = loss_fc_ha / f1_area_2001)# %>% 
# dplyr::select(-f1_area_2001)

# Tidy carbon lost and combine with area
loss_df <- sums %>% 
  dplyr::select(div_name, site, mgc_2000, contains('c20')) %>%
  pivot_longer(starts_with('c20'), 
               names_to='Year', names_prefix='c', 
               values_to='loss_mgc') %>% 
  mutate(loss_pct_carbon = loss_mgc / mgc_2000) %>% 
  dplyr::select(-mgc_2000) %>% 
  full_join(loss_ha) %>% 
  full_join(loss_fc_ha) %>% 
  mutate(Year = as.numeric(Year))
  

# Calculate forest area in most recent year ----
primfor_22 <- loss_df %>% 
  select(div_name, site, Year, f1_area_2001, loss_fc_ha, area_ha) %>% 
  group_by(div_name) %>% 
  mutate(loss_fc_cum = cumsum(loss_fc_ha), 
         f1_area_ha = f1_area_2001 - loss_fc_cum) %>% 
  slice_max(Year)

# Compare to area of TC>25%
for_22 <- primfor_22 %>% 
  full_join(tc_22, by=c('div_name', 'site', 'Year'))

for_22 %>% 
  transmute(div_name, loss_fc_cum.x, loss_fc_cum.y, area_ha, diff_area = loss_fc_cum.x - loss_fc_cum.y)

for_22 %>% 
  transmute(div_name, f1_area_ha, fc_area_ha, area_ha, diff_area = fc_area_ha - f1_area_ha)

# Save primary forest area in most recent year ----
primfor_22 %>% 
  ungroup() %>% 
  select(div_name, f1_area_ha) %>% 
  mutate(across(where(is.numeric),~ round(.x, 1))) %>% 
  write_csv(here::here(out_dir, 'primaryforest_area_ha_2022.csv'))
