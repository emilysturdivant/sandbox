# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Supplement ITs from 2021 data with those from 2018 (2024-01-24) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(sf)
sf_use_s2(FALSE) # Turn off spherical geometry (https://r-spatial.org/book/04-Spherical.html#validity-on-the-sphere)
library(tmap)
tmap_mode('view')
Sys.setenv(OGR_GEOJSON_MAX_OBJ_SIZE=500)

wcmc_gdb <- '/Volumes/ejs_storage/data/raw_data/WCMC/IPLC_WWF_2021_split_version.gdb'
rri18_gdb <- '/Volumes/ejs_storage/data/raw_data/RRI_2018/2018_06_12_clean.gdb'

out_dir <- here::here('hih/data') 

# Load DHF boundary for CRS and bounding box ----
dhf_diss_fp <- here::here(out_dir, 'Ecoregions2017_MBF_realms.geojson')
dhf <- st_read(dhf_diss_fp)
dhf <- st_read('/Volumes/ejs_storage/data/raw_data/Ecoregions2017/Ecoregions2017_DHF.shp')
bb_wkt <- dhf %>% st_bbox() %>% st_as_sfc() %>% st_as_text()

# ## ITs from RRI 2018 
# its_rri18_fp <- here::here('hih/data/ITs_RRI18_tropics.shp')
# if(!file.exists(its_rri18_fp)){
#   layers <- st_layers(rri18_gdb)$name
#   its_rri18 <- st_read(rri18_gdb, layer=layers[1]) %>% 
#     st_transform(crs = st_crs(dhf)) %>% 
#     st_make_valid() %>% 
#     # Select those within tropics
#     group_by(Identity) %>% 
#     summarize() %>%
#     st_simplify(dTolerance = 0.01)
#   
#   its_rri18 %>% st_write(its_rri18_fp, delete_dsn = TRUE)
#   
# }
# its_rri18 <- st_read(its_rri18_fp, wkt_filter = bb_wkt) %>%
#   filter(Identity == 'Indigenous')
# qtm(its_rri18)
# 
# ## Filter to only polygons that intersect biome
# its18 <- its_rri18 %>% st_cast('POLYGON') %>% st_filter(dhf %>% st_bbox() %>% st_as_sfc())
# qtm(its18)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save countries ----
# Directory for country shps
it_countries_dir <- here::here('hih/data/country_IT_shps')
dir.create(it_countries_dir, recursive=T)

reduce_poly_size <- function(polys) {
  polys %>% 
    group_by() %>% 
    summarize() %>% 
    nngeo::st_remove_holes(max_area = 2e5) %>%
    st_simplify(dTolerance = 0.01)
}

## 2018 ----
load_by_iso18 <- function(iso_code) {
  in_fp <- rri18_gdb
  lyr_idx <- 1
  
  # Load polys for given country
  out_shp <- here::here(it_countries_dir, 
                        paste0(str_remove_all(iso_code, '/'), '_ITs_18.shp'))
  
  if(file.exists(out_shp)){
    cat(out_shp, 'already exists. Skipping. \n')
    return()
  } 
  
  layers <- st_layers(in_fp)$name
  query <- str_c("SELECT * FROM \"", layers[lyr_idx], "\" WHERE ISO_Code = '", iso_code, "'")
  p1 <- st_read(in_fp, query=query) %>% 
    st_make_valid() %>% 
    st_transform(crs = st_crs(dhf))
  
  # Dissolve, remove holes, and simplify
  p1 <- p1 %>% reduce_poly_size()
  
  # Save
  p1 %>% st_write(out_shp)
}

# Save simplified ITs for countries not fully represented in 2021 data
c('GUY', 'PHL', 'NZL') %>% 
  purrr::walk(load_by_iso18)

# # Load all country files into one SF
# its_rri18 <- list.files(it_countries_dir, '18.shp', full.names=T) %>% 
#   purrr::map_dfr(function(fp) {
#     iso_code <- str_extract(fp, '(?<=country_IT_shps/).*(?=_ITs_)')
#     p1 <- sf::st_read(fp) %>% mutate(ISO3=iso_code)
#   })
# qtm(its_rri18)

## 2021 ----
load_by_iso <- function(iso_code, in_fp, lyr_idx, iso_var='ISO_Code') {
  out_shp <- here::here(it_countries_dir, 
                        paste0(str_remove_all(iso_code, '/'), '_ITs_21.shp'))
  
  if(file.exists(out_shp)){
    cat(out_shp, 'already exists. Skipping. \n')
    return()
  } 
  
  # Load polys for given country
  lyr <- st_layers(in_fp)$name[lyr_idx]
  query <- str_c("SELECT * FROM \"", lyr, "\" WHERE ", iso_var, " = '", iso_code, "'")
  p1 <- st_read(in_fp, query=query) %>% 
    st_make_valid() %>% 
    st_transform(crs = st_crs(dhf))
  
  # Dissolve, remove holes, and simplify
  p1 <- p1 %>% reduce_poly_size()
  
  # Save
  p1 %>% st_write(out_shp)
}

# Get ISO codes for countries within biome
lyr <- st_layers(wcmc_gdb)$name[1]
bb_sf <- dhf %>% st_bbox() %>% st_as_sfc()
dhf_iso_codes <- st_read(wcmc_gdb, layer=lyr, wkt_filter = st_as_text(bb_sf)) %>% 
  st_drop_geometry() %>% 
  distinct(ISO3) %>% 
  pull(ISO3)

# Create polygon file for each country
its21_sub1 <- dhf_iso_codes %>% 
  purrr::walk(load_by_iso, in_fp=wcmc_gdb, lyr_idx=1, iso_var='ISO3')

# its21_all <- list.files(it_countries_dir, '21.shp', full.names=T) %>% 
#   purrr::map_dfr(function(fp) {
#     iso_code <- str_extract(fp, '(?<=country_IT_shps/).*(?=_ITs_21)')
#     p1 <- sf::st_read(fp) %>% mutate(ISO3=iso_code)
#   })
# qtm(its21_all)

# Dissolve all country files ----
its_all <- list.files(it_countries_dir, '.shp', full.names=T) %>% 
  purrr::map_dfr(function(fp) {
    iso_code <- str_extract(fp, '(?<=country_IT_shps/).*(?=_ITs_)')
    p1 <- sf::st_read(fp) %>% mutate(ISO3=iso_code)
  })

# Dissolve
its_union <- st_union(its_all)

# Filter to biome
its_dhf <- its_union %>% 
  st_cast('POLYGON') %>% 
  st_as_sf() %>% 
  st_as_sf() %>% 
  st_filter(dhf)

# Save
its_fp <- here::here(out_dir, 'ITs_inMBF.shp')
its_dhf %>% st_write(its_fp)


