# 
library(tidyverse)
library(sf)
library(terra)
library(tmap)
tmap_mode('view')

man_poly <- st_read('/Users/esturdivant/data/hih_sites/Manombo/ManomboNP_all.shp')
alerts_tif <- '/Volumes/ejs_storage/data/raw_data/gfw_deforestation_alerts/20S_040E.tif'
alerts_r <- rast(alerts_tif, win=man_poly) %>% mask(man_poly)

# Filter to high confidence alerts in 2023
library(tidyterra)
alerts_high <- alerts_r %>% 
  transmute(value = 
           case_when(
             (`20S_040E` >= 32923 & `20S_040E` < 39999) |
               (`20S_040E` >= 42923 & `20S_040E` < 49999) ~ 1, 
             TRUE ~ 0)
  ) %>% 
  filter(value==1) 
alerts_high <- alerts_r %>% 
  transmute(value = 
              case_when(
                (`20S_040E` >= 32558 & `20S_040E` < 39999) |
                  (`20S_040E` >= 42558 & `20S_040E` < 49999) ~ 1, 
                TRUE ~ 0)
  ) %>% 
  filter(value==1) 

# qtm(man_poly) + qtm(poly_alerts) 

poly_alerts <- alerts_high %>% 
  as.polygons(values = FALSE, dissolve=TRUE, extent=FALSE) %>% 
  st_as_sf() %>% 
  st_cast('POLYGON') %>% 
  rowid_to_column("ID")

# Get centroid coordinates
centroids <- poly_alerts %>% 
  st_centroid() 
coords <- centroids %>% 
  st_coordinates() %>% 
  bind_cols(st_drop_geometry(centroids))

# Join back to polys
poly_alerts <- poly_alerts %>% 
  full_join(coords, by='ID') %>% 
  mutate(area_m2 = st_area(geometry) %>% units::drop_units())

poly_alerts <- poly_alerts %>% 
  mutate(coords_wgs84 = paste0(round(X, 6), ', ', round(Y, 6)))

poly_alerts %>% 
  st_write(here::here('hih_manombo/data/alerts_highconf_202201_202309.kml'))

poly_alerts %>% 
  st_drop_geometry() %>% 
  write_csv(here::here('hih_manombo/data/alerts_highconfidence_2023_centroids.csv'))

tm_shape(poly_alerts) + tm_polygons()  +
  tm_text('coords_wgs84')
