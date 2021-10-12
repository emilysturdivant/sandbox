
library(sf)
library(terra)
library(tidyverse)

# Get area for site polygons ----
polys_dir <- '/Volumes/GoogleDrive-105942041423621298895/My Drive/2_Work/Woodwell/HIH/site_polys'
tdm_shp <- file.path(polys_dir,
                     'Xingu/final_divisions/TdM_all_dissolved_gapfilled.shp')
tdm <- st_read(tdm_shp)
tdm %>% tbl_vars()

tdm_union <- tdm %>% 
  st_transform(st_crs(5641)) %>% # projected CRS for South America
  group_by(type1) %>% 
  summarize()
tdm_union %>% 
  mutate(area_ha = st_area(geometry) %>% units::set_units('ha')) %>% 
  st_drop_geometry() %>% 
  mutate(area_ha = format(area_ha, big.mark = ','))

tdm_union %>% 
  summarize() %>% 
  mutate(area_ha = st_area(geometry) %>% units::set_units('ha')) %>% 
  st_drop_geometry() %>% 
  mutate(area_ha = format(area_ha, big.mark = ','))
