#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Package HIH site polygons
# Requires:
#     * input files indicated
# Author:
#     * esturdivant@woodwellclimate.org, 2022-05-19
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(sf)
library(tidyverse)
library(tmap)
tmap_mode('view')


sites_dir <- '~/data/hih_sites'

# Shapefiles - used for forest cover reports and sent to Nina ----
polys_dir <- file.path(sites_dir, 'forest_cover_report_and_shps_20220320',
                       'hih_sites_shps')
list.files(polys_dir, '.*\\.shp')
shps <- list.files(polys_dir, '.*\\.shp', full.names = TRUE)

p <- st_read(shps[[1]])
qtm(p)

p <- st_read(shps[[3]])
qtm(p)

fn <- shps[[4]] 
p <- st_read(fn) %>% 
  group_by() %>% 
  summarize()
qtm(p)
out_dir <- file.path(sites_dir, 'boundaries_20220519')
out_fname <- basename(fn) %>% str_remove_all('_3')
p %>% st_write(here::here(out_dir, out_fname))

p <- st_read(shps[[5]]) %>% 
  group_by() %>% 
  summarize()
qtm(p)

p <- st_read(shps[[6]])
qtm(p)

# GeoJSONs - simplified for Rainforest Exchange ----
polys_dir <- file.path(sites_dir, 'HIH_boundaries_simplified_geojson')
list.files(polys_dir, '.*\\.geojson')
shps <- list.files(polys_dir, '.*\\.geojson', full.names = TRUE)

p <- st_read(shps[[1]])
qtm(p)

p <- st_read(shps[[2]])
qtm(p)

p <- st_read(shps[[3]])
qtm(p)

fn <- shps[[4]]
p <- st_read(fn)
qtm(p)

p <- st_read(shps[[5]]) %>% 
  st_make_valid()
qtm(p)
