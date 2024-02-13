#!/usr/bin/env Rscript
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Script:
#   create_realm_tiles.R
#
# Description:
#   Create shapefile for tiling by continental region in GEE. 

# Author:
#   Emily Sturdivant - Jan 2024
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(sf)
sf_use_s2(F)
library(tmap)
tmap_mode('view')

# Create shapefile of buffered and diced continental regions ----
# ~~~~~~~~~~~~~~~~
# Load continents that were created by buffering World_Continents.shp in QGIS
continents <- st_read('/Users/esturdivant/data/misc/World_Continents/continents_buffered.geojson') 
qtm(continents)

# Parameters
lon_step = 15
lat_step = 15
crs_dst <- st_crs('EPSG:4326')

# Create DF with tile corner coordinates
coords <- tibble(x1=NA, y1=NA, x2=NA, y2=NA,
                 x3=NA, y3=NA, x4=NA, y4=NA)
for( lon in seq(-180, 180, lon_step) ){
  for( lat in seq(-35, 31, lat_step) ){
    coords <- coords %>% add_row(x1=lon, y1=lat, 
                                 x2=lon + lon_step, y2=y1, 
                                 x3=x2, y3=lat + lat_step,
                                 x4=x1, y4=y3)
  }
} 

# Use corner coordinates to create SF polygons
tiles_sf <- coords %>% 
  dplyr::filter(!is.na(x1)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(geometry = 
                  sf::st_polygon(
                    list(matrix(c(x1, y1, x2, y2, x3, y3, x4, y4, x1, y1), 
                                ncol=2, byrow=TRUE))
                  ) %>% 
                  sf::st_sfc(crs=crs_dst)) %>% 
  sf::st_as_sf(crs=crs_dst) %>% 
  dplyr::select(-c(x1, y1, x2, y2, x3, y3, x4, y4))

# Intersect with continents
diced <- tiles_sf %>% 
  st_intersection(continents) %>% 
  select(CONTINENT)

## write shapefile to disk ----
polys_shp <- here::here('/Users/esturdivant/data/misc/World_Continents/tropics_continents_diced.shp')
diced %>% sf::st_write(polys_shp, delete_dsn=TRUE)


# Create shapefile of buffered and diced countries in tropics ----
# ~~~~~~~~~~~~~~~~
# Load continents that were created by buffering World_Continents.shp in QGIS
# polys <- readRDS('/Volumes/ejs_storage/data/gadm/gadm36_0_tropics.rds') 
polys <- st_read('/Volumes/ejs_storage/data/gadm/gadm36_0_tropics_simp001big3.shp')
qtm(polys)

# Parameters
lon_step = 15
lat_step = 15
crs_dst <- st_crs('EPSG:4326')

# Create DF with tile corner coordinates
coords <- tibble(x1=NA, y1=NA, x2=NA, y2=NA,
                 x3=NA, y3=NA, x4=NA, y4=NA)
for( lon in seq(-180, 180, lon_step) ){
  for( lat in seq(-35, 31, lat_step) ){
    coords <- coords %>% add_row(x1=lon, y1=lat, 
                                 x2=lon + lon_step, y2=y1, 
                                 x3=x2, y3=lat + lat_step,
                                 x4=x1, y4=y3)
  }
} 

# Use corner coordinates to create SF polygons
tiles_sf <- coords %>% 
  dplyr::filter(!is.na(x1)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(geometry = 
                  sf::st_polygon(
                    list(matrix(c(x1, y1, x2, y2, x3, y3, x4, y4, x1, y1), 
                                ncol=2, byrow=TRUE))
                  ) %>% 
                  sf::st_sfc(crs=crs_dst)) %>% 
  sf::st_as_sf(crs=crs_dst) %>% 
  dplyr::select(-c(x1, y1, x2, y2, x3, y3, x4, y4))

# Intersect with countries
diced <- tiles_sf %>% 
  st_intersection(polys) %>% 
  select(NAME_0)

diced <- diced %>% 
  st_collection_extract(type='POLYGON')

# Look
dhf <- st_read('/Volumes/ejs_storage/data/ecoregions/Ecoregions2017_DHF_dissolved1.shp')
tm_shape(polys) + tm_polygons() +
  tm_shape(tiles_sf) + tm_polygons() +
  tm_shape(diced) + tm_polygons() +
  tm_shape(dhf) + tm_polygons()
  
## write shapefile to disk ----
polys_shp <- here::here('/Users/esturdivant/data/misc/GADM/tropics_countries_diced.shp')
diced %>% sf::st_write(polys_shp, delete_dsn=TRUE)
