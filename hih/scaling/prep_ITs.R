# Compare polygons showing IT land globally

library(tidyverse)
library(sf)
sf_use_s2(FALSE) # Turn off spherical geometry (https://r-spatial.org/book/04-Spherical.html#validity-on-the-sphere)
library(tmap)
tmap_mode('view')
Sys.setenv(OGR_GEOJSON_MAX_OBJ_SIZE=500)

wcmc_gdb <- '/Volumes/ejs_storage/data/raw_data/WCMC/IPLC_WWF_2021_split_version.gdb'
rri18_gdb <- '/Volumes/ejs_storage/data/raw_data/RRI_2018/2018_06_12_clean.gdb'

out_dir <- here::here('hih/data') 

# Load and prep DHF boundary ----
# Download ecoregions
dl_url <- 'https://storage.googleapis.com/teow2016/Ecoregions2017.zip'
local_zip_fp <- here::here(out_dir, 'Dinerstein_etal_2017/Ecoregions2017.zip')
download.file(dl_url, destfile = local_zip_fp)

# Unzip to temp dir
eco_dir <- here::here(out_dir, 'Dinerstein_etal_2017')
unzip(local_zip_fp, exdir = eco_dir)
dhf_shp <- list.files(eco_dir, '.shp', full.names=TRUE)

dhf_diss_fp <- here::here(out_dir, 'Ecoregions2017_MBF_realms.geojson')
dhf <- st_read(dhf_shp) %>% 
  filter(str_detect(BIOME_NAME, 'Moist Broadleaf Forests')) %>% 
  st_make_valid() %>% 
  group_by(BIOME_NAME, BIOME_NUM, REALM) %>% 
  summarize() %>% 
  st_simplify(dTolerance = 0.01) %>% 
  ungroup()
dhf %>% st_write(dhf_diss_fp, delete_dsn=T)

dhf_diss_fp <- here::here('hih/data/Ecoregions2017_MBF.geojson')
dhf %>% summarize() %>% st_write(dhf_diss_fp, delete_dsn=T)

bb_wkt = dhf %>% st_bbox() %>% st_as_sfc() %>% st_as_text()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prep RRI data for the entire tropics ----
## ITs from RRI 2021 ----
# (use DHF extent, but don't limit to intersection with DHF)
its_rri21_fp <- here::here('hih/data/ITs_RRI21_tropics.shp')

layers <- st_layers(wcmc_gdb)$name
its_rri21 <- st_read(wcmc_gdb, layer=layers[1], wkt_filter = bb_wkt) %>% 
  st_make_valid() %>% 
  group_by() %>% 
  summarize()  %>%
  st_simplify(dTolerance = 0.01)
its_rri21 %>% st_write(its_rri21_fp, delete_dsn = TRUE)
its_rri21 <- st_read(its_rri21_fp)

## ITs from RRI 2018 ----
its_rri18_fp <- here::here('hih/data/ITs_RRI18_tropics.shp')

layers <- st_layers(rri18_gdb)$name
its_rri18 <- st_read(rri18_gdb, layer=layers[1]) %>% 
  st_transform(crs = st_crs(dhf)) %>% 
  st_make_valid() %>% 
  # Select those within tropics
  group_by(Identity) %>% 
  summarize() %>%
  st_simplify(dTolerance = 0.01)

its_rri18 %>% st_write(its_rri18_fp, delete_dsn = TRUE)
its_rri18 <- st_read(its_rri18_fp)

## Combine ----
its_rri1821_fp <- here::here('hih/data/ITs_RRI_1821_tropics.shp')

its_rri18 <- its_rri18 %>% filter(Identity == 'Indigenous')
its_union <- st_union(its_rri21, its_rri18) %>% 
  st_union() %>% 
  st_buffer(0.001) %>% st_buffer(-0.001) %>% 
  st_simplify(dTolerance = 0.01)

tm_shape(its_union) + tm_polygons()

its_union %>% st_write(its_rri1821_fp, delete_dsn = TRUE)
its_union <- st_read(its_rri1821_fp)

## LCs from RRI 2021 ----
lcs_trop_fp <- here::here('hih/data/LCs_RRI21_tropics.shp')

layers <- st_layers(wcmc_gdb)$name
lcs_rri21 <- st_read(wcmc_gdb, layer=layers[2], wkt_filter = bb_wkt) %>% 
  st_make_valid() %>% 
  group_by() %>% 
  summarize()  %>%
  st_simplify(dTolerance = 0.01)
lcs_rri21 %>% st_write(lcs_trop_fp, delete_dsn = TRUE)
lcs_rri21 <- st_read(lcs_trop_fp)

## Combine ITs and LCs ----
iplcs_trop_fp <- here::here('hih/data/IPLCs_RRI_tropics.shp')

iplcs <- st_union(its_union, lcs_rri21) %>% 
  st_union() %>% 
  st_buffer(0.001) %>% st_buffer(-0.001) %>% 
  st_simplify(dTolerance = 0.01)

iplcs %>% st_write(iplcs_trop_fp, delete_dsn = TRUE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Intersect with DHF ----
its_diss_fp <- here::here('hih/data/IPLC_WWF_2021_DHF_IPs.geojson')
lcs_diss_fp <- here::here('hih/data/IPLC_WWF_2021_DHF_LCs.geojson')

## Get intersection with DHF ----
get_intersection <- function(realm, dhf, lyr_idx=1) {
  # Filter biome to realm and convert bounding box to WKT
  dhf_r1 <- dhf %>% filter(REALM == realm)
  wkt = dhf_r1 %>% st_bbox() %>% st_as_sfc() %>% st_as_text()
  
  # Load polygons from WCMC
  layers <- st_layers(wcmc_gdb)$name
  p1 <- st_read(wcmc_gdb, layer=layers[lyr_idx], wkt_filter = wkt) %>% 
    st_make_valid()
  
  if(nrow(p1) > 0) {
    p1 <- p1 %>% 
      group_by() %>% 
      summarize()  %>%
      st_simplify(dTolerance = 0.01) %>%
      st_intersection(dhf_r1)
  }
  
}

## IP ----
# Get IP_lands_only from 2021 
it_nt <- get_intersection(realm='Neotropic', dhf, in_fp=wcmc_gdb)
it_o <- get_intersection(realm='Oceania', dhf, in_fp=wcmc_gdb)
it_at <- get_intersection(realm='Afrotropic', dhf, in_fp=wcmc_gdb)
it_aa <- get_intersection(realm='Australasia', dhf, in_fp=wcmc_gdb)
it_im <- get_intersection(realm='Indomalayan', dhf, in_fp=wcmc_gdb)
it_pa <- get_intersection(realm='Palearctic', dhf, in_fp=wcmc_gdb)

# Combine realms into one and save
its_wcmc21 <- bind_rows(it_nt, it_o, it_at, it_aa, it_im, it_pa)
its_wcmc21 %>% st_write(its_diss_fp, delete_dsn = TRUE)

## LC ----
# Get LC_lands_only from 2021 
it_nt <- get_intersection(realm='Neotropic', dhf, in_fp=wcmc_gdb, lyr_idx=2)
it_o <- get_intersection(realm='Oceania', dhf, in_fp=wcmc_gdb, lyr_idx=2)
it_at <- get_intersection(realm='Afrotropic', dhf, in_fp=wcmc_gdb, lyr_idx=2)
it_aa <- get_intersection(realm='Australasia', dhf, in_fp=wcmc_gdb, lyr_idx=2)
it_im <- get_intersection(realm='Indomalayan', dhf, in_fp=wcmc_gdb, lyr_idx=2)
it_pa <- get_intersection(realm='Palearctic', dhf, in_fp=wcmc_gdb, lyr_idx=2)

# Combine all realms into one and save
lcs <- bind_rows(it_nt, it_o, it_at, it_aa, it_im, it_pa)
lcs %>% st_write(lcs_diss_fp, delete_dsn = TRUE)


# 2018 polygons for RRI analysis ----
rri18_diss_fp <- here::here('hih/data/2018_06_12_clean.geojson')

# Run by realm
get_intersection_2018 <- function(dhf, in_fp, grp_col=NULL, lyr_idx=1) {
  
  # Load polygons from WCMC
  layers <- st_layers(in_fp)$name
  p1 <- st_read(in_fp, layer=layers[lyr_idx]) %>% 
    st_transform(crs = st_crs(dhf)) %>% 
    st_make_valid()
  
  if(nrow(p1) > 0) {
    p1 <- p1 %>% 
      group_by({{grp_col}}) %>% 
      summarize() %>%
      st_simplify(dTolerance = 0.01) %>%
      st_intersection(dhf)
  }
  
}

rri18 <- get_intersection_2018(dhf, in_fp=rri18_gdb, grp_col=Identity)
rri18 %>% st_write(rri18_diss_fp, delete_dsn = TRUE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Union 2018 and 2021 DHF polygons ----
its_out_fp <- here::here('hih/data/ITs_RRI_2018_2021.geojson')
lcs_out_fp <- here::here('hih/data/LCs_RRI_2018_2021.geojson')

# Load all 
its_wcmc21 <- st_read(its_diss_fp)
lcs_wcmc21 <- st_read(lcs_diss_fp)
rri18 <- st_read(rri18_diss_fp)

## ITs ----
its_rri18 <- rri18 %>% filter(Identity == 'Indigenous')
its_union <- st_union(its_wcmc21, its_rri18) %>% 
  st_union() %>% 
  st_buffer(0.001) %>% st_buffer(-0.001) %>% 
  st_simplify(dTolerance = 0.01)

its_union %>% st_write(its_out_fp, delete_dsn = TRUE)

## LCs ----
lcs_rri18 <- rri18 %>% filter(Identity != 'Indigenous')
lcs_union <- st_union(lcs_wcmc21, lcs_rri18) %>% 
  st_union() %>% 
  st_buffer(0.001) %>% st_buffer(-0.001) %>% 
  st_simplify(dTolerance = 0.01)

tm_shape(its_union) + tm_polygons() +
  tm_shape(lcs_union) + tm_polygons()

lcs_union %>% st_write(lcs_out_fp, delete_dsn = TRUE)

# Get areas of intersection and difference ----

# Load
its_union <- st_read(its_out_fp)

# Areas
(dhf_area <- dhf %>% st_area() %>% 
   units::set_units('ha') %>% units::drop_units() %>% 
   sum())
(its_area <- its_union %>% st_area() %>% 
   units::set_units('ha') %>% units::drop_units() %>% 
   sum())
(rri18_area <- its_rri18 %>% st_area() %>% 
  units::set_units('ha') %>% units::drop_units() %>% 
  sum())
(rri21_area <- its_wcmc21 %>% st_area() %>% 
  units::set_units('ha') %>% units::drop_units() %>% 
  sum())

# Intersection
rri_intersect <- its_wcmc21 %>% st_intersection(its_rri18)
(intxn_area <- rri_intersect %>% st_area() %>% 
    units::set_units('ha') %>% units::drop_units() %>% 
    sum())

# Difference
rri21_diff <- its_wcmc21 %>% st_union() %>% 
  st_difference(st_union(its_rri18))
(only21_area <- rri21_diff %>% st_area() %>% 
    units::set_units('ha') %>% units::drop_units() %>% 
    sum())

# Difference
rri18_diff <- its_rri18 %>% st_union() %>% 
  st_difference(st_union(its_wcmc21))
(only18_area <- rri18_diff %>% st_area() %>% 
    units::set_units('ha') %>% units::drop_units() %>% 
    sum())



# Only LCs that are not already covered by ITs ----

# Load
its_union <- st_read(its_out_fp)
lcs_union <- st_read(lcs_out_fp)

# Where LC is not overlapped by IT
st_erase = \(x, y) st_difference(x, st_union(st_combine(y)))
lcs_no_overlap <- st_erase(lcs_union, its_union)


tm_shape(its_union) + tm_polygons() +
  tm_shape(lcs_union) + tm_polygons() +
  tm_shape(lcs_no_overlap) + tm_polygons()

its_out_shp <- here::here('hih/data/ITs_RRI_2018_2021.shp')
lcs_out_shp <- here::here('hih/data/LCs_RRI_2018_2021_no_IToverlap.shp')
its_union %>% st_write(its_out_shp, delete_dsn = TRUE)
lcs_no_overlap %>% st_write(lcs_out_shp, delete_dsn = TRUE)



# WDPA ----
wdpa_dir <- '/Volumes/ejs_storage/data/raw_data/WDPA/WDPA_Mar2023_Public_shp'
oecm_dir <- '/Volumes/ejs_storage/data/raw_data/WDPA/WDOECM_Nov2023_Public_shp'

wdpa_its_fp <- here::here('hih/data/WDPA_Mar2023_Public_ITs.shp')
wdpa_lcs_fp <- here::here('hih/data/WDPA_Mar2023_Public_LCs.shp')

# Filter biome to realm and convert bounding box to WKT
get_intersection_wdpa <- function(realm, dhf, in_fp, lyr_idx=1) {
  
  # Load polygons from WCMC
  layers <- st_layers(in_fp)$name
  p1 <- st_read(in_fp, layer=layers[lyr_idx]) %>% 
    filter(MARINE != 2) %>% 
    filter(str_detect(DESIG_ENG, regex('indig|communit', ignore_case=T)) | 
             str_detect(GOV_TYPE, regex('indig|communit', ignore_case=T))) %>% 
    st_transform(crs = st_crs(dhf)) %>% 
    st_make_valid()
  
  if(nrow(p1) > 0) {
    p1 <- p1 %>% 
      group_by(DESIG_ENG, GOV_TYPE, MARINE) %>% 
      summarize() %>%
      st_simplify(dTolerance = 0.01) %>%
      st_intersection(filter(dhf, REALM == realm))
  }
}

# Run for all realms and all WDPA shapefiles
realms <- dhf %>% st_drop_geometry() %>% distinct(REALM) %>% pull()
wdpa_shps <- c(here::here(wdpa_dir, 'WDPA_Mar2023_Public_shp_0'),
               here::here(wdpa_dir, 'WDPA_Mar2023_Public_shp_1'),
               here::here(wdpa_dir, 'WDPA_Mar2023_Public_shp_2'))
itlc_wdpa <- realms %>% 
  purrr::map_dfr( \(r) {
    wdpa_shps %>% purrr::map_dfr( \(x) {
      get_intersection_wdpa(realm=r, dhf, in_fp=x)
    } )
  } )

# Separate into ITs and LCs
its_wdpa <- itlc_wdpa %>% 
  filter(str_detect(GOV_TYPE, regex('indigenous', ignore_case=T)) |
           str_detect(DESIG_ENG, regex('indigenous', ignore_case=T)))
lcs_wdpa <- itlc_wdpa %>% 
  filter(!(str_detect(GOV_TYPE, regex('indigenous', ignore_case=T)) |
             str_detect(DESIG_ENG, regex('indigenous', ignore_case=T))))

# Save
its_wdpa %>% st_write(wdpa_its_fp, delete_dsn = TRUE)
lcs_wdpa %>% st_write(wdpa_lcs_fp, delete_dsn = TRUE)


in_fp <- here::here(oecm_dir, 'WDOECM_Nov2023_Public_shp_0')
lyr_idx <- 1
# Load polygons from WCMC
layers <- st_layers(in_fp)$name
p1 <- st_read(in_fp, layer=layers[lyr_idx]) %>% 
  filter(MARINE != 2) %>% 
  filter(str_detect(DESIG_ENG, regex('indig|communit', ignore_case=T)) | 
           str_detect(GOV_TYPE, regex('indig|communit', ignore_case=T))) %>% 
  st_transform(crs = st_crs(dhf)) %>% 
  st_make_valid()

p1 %>% tbl_vars()
p1 %>% st_drop_geometry() %>% distinct(GOV_TYPE,OWN_TYPE, DESIG_ENG, STATUS, DESIG_TYPE)
p1 %>% st_drop_geometry() %>% distinct(OWN_TYPE)
p1 %>% st_drop_geometry() %>% distinct(DESIG_ENG)
p1 %>% st_drop_geometry() %>% filter(GOV_TYPE == 'Collaborative governance')
