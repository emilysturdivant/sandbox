library(tmap)
tmap_mode('view')
library(sf)
library(terra)
library(tidyverse)


sites_dir <- '/Volumes/GoogleDrive-105942041423621298895/My Drive/2_Work/Woodwell/HIH/site_polys'
polys_dir <- file.path(sites_dir, 'final_sites')

# Get area for site polygons ----
tdm_shp <- file.path(polys_dir, 'TdM_all_dissolved_gapfilled.shp')
tdm <- st_read(tdm_shp)
tdm %>% tbl_vars()

tdm <- tdm %>% mutate(HIH_site = 'Terra do Meio') %>% st_zm(drop = TRUE)
tdm %>% st_write(tdm_shp, append = FALSE)

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


# Troubleshoot Manombo polygon ----
site <- 'Manombo'

# Original Special Reserve ---
manombo_shp <- file.path(polys_dir, 'ManomboNP_SRGpolys.shp')
manombo_sr <- st_read(manombo_shp) 

manombo_sr$area_ha <-  manombo_sr %>%
  st_area() %>%
  units::set_units('ha') %>%
  units::set_units(NULL)

# GADM 
# mdg <- st_read('/Users/emilysturdivant/data/context/gadm36_MDG_gpkg/gadm36_MDG.gpkg')
# 
# tm_shape(df) + tm_polygons(alpha = 0.5) +
# tm_shape(mdg) + tm_polygons(alpha = 0.5)
# 
# # Protected areas
# pas_wpts <- st_read('/Users/emilysturdivant/data/protected_areas/WDPA/WDPA_Oct2021_tropics_simp001_buff0085.gpkg') %>%
#   filter(ISO3 == 'MDG')
# # including points adds 4 PAs in Madagascar
# oecm <- st_read('/Users/emilysturdivant/data/protected_areas/WDOECM/WDOECM_Oct2021_tropics_simp001_buff0085.gpkg') %>%
#   filter(ISO3 == 'MDG')
# tm_shape(df) + tm_polygons(alpha = 0.5) +
# tm_shape(pas_wpts) + tm_polygons(alpha = 0.5)
# 
# man_pa <- pas_wpts %>%
#   filter(str_detect(NAME, regex('manombo', ignore_case = TRUE))) %>%
#   select(NAME, DESIG)
# man_pa$area_ha <- man_pa%>%
#   st_area() %>%
#   units::set_units('ha') %>%
#   units::set_units(NULL)

# KBAs ----
man_kbas <- st_read('/Users/emilysturdivant/data/biodiversity/KBAsGlobal_2021_September_02/KBAsGlobal_2021_September_02_POL.shp') %>%
  filter(ISO3 == 'MDG') %>%
  filter(str_detect(IntName, regex('manombo', ignore_case = TRUE)) |
           str_detect(IntName, regex('efatsy', ignore_case = TRUE))) %>%
  select(IntName)

# Efatsy
efatsy <- man_kbas %>% 
  filter(str_detect(IntName, regex('efatsy', ignore_case = TRUE))) %>% 
  mutate(name = 'Manombo Classified Forest',
         HIH_site = 'Manombo',
         ID = 3) %>% 
  select(-IntName) %>% 
  st_cast('POLYGON')

efatsy$area_ha <-  efatsy %>%
  st_area() %>%
  units::set_units('ha') %>%
  units::set_units(NULL)

efatsy <- select(efatsy, ID, name, HIH_site, area_ha)
manombo_sr <- select(manombo_sr, ID, name, HIH_site, area_ha)

manombo <- rbind(manombo_sr, efatsy)

manombo %>% st_write(file.path(polys_dir, 'ManomboNP_all.shp'))

#
tm_shape(manombo) + tm_polygons(alpha = 0.5)

manombos_sr <- man_kbas %>% 
  st_difference() %>% 
  filter(str_detect(IntName, regex('special reserve', ignore_case = TRUE)))
manombos_cf <- man_kbas %>% filter(str_detect(IntName, regex('classified forest', ignore_case = TRUE)))
tm_shape(manombos_sr) + tm_polygons(alpha = 0.5, col = 'IntName') +
  tm_shape(efa_kbas) + tm_polygons(alpha = 0.5, col = 'IntName')

manombos_cf$area_ha <- manombos_cf %>%
  st_area() %>%
  units::set_units('ha') %>%
  units::set_units(NULL)

manombos_sr$area_ha <- manombos_sr %>%
  st_area() %>%
  units::set_units('ha') %>%
  units::set_units(NULL)

efatsy$area_ha <- efatsy %>%
  st_area() %>%
  units::set_units('ha') %>%
  units::set_units(NULL)

tm_shape(manombo1) + tm_polygons(alpha = 0.5) +
tm_shape(man_pa) + tm_polygons(alpha = 0.5) +
tm_shape(manombos_sr) + tm_polygons(alpha = 0.5) +
tm_shape(efatsy) + tm_polygons(alpha = 0.5) +
  tm_shape(manombos_cf) + tm_polygons(alpha = 0.5)


# GERP ----
fps <- list.files(file.path(sites_dir, 'Manombo', 'GERP'), 'shp$', full.names = TRUE)
sf_list <- fps %>% purrr::map(st_read)


tm_shape(sf_list[[1]]) + tm_borders() +
  tm_shape(sf_list[[2]]) + tm_borders() +
  # tm_shape(sf_list[[3]]) + tm_borders() +
  tm_shape(sf_list[[4]]) + tm_borders() +
  tm_shape(manombo) + tm_borders()
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge all ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fps <- list.files(polys_dir, '*\\.shp$', full.names = TRUE)
df <- fps %>% purrr::map_dfr(function(x) {
  df <- st_read(x)
  df <- st_zm(df, drop = TRUE)
  df %>% select(any_of(c('HIH_site', 'name', 'type')))
  })

df <- df %>% 
  mutate(type = case_when(
    str_detect(name, regex('special reserve', ignore_case = TRUE)) ~ 'SR', 
    str_detect(name, regex('classified forest', ignore_case = TRUE)) ~ 'CF', 
    str_detect(name, regex('control', ignore_case = TRUE)) ~ 'NP',
    str_detect(name, regex('experiment', ignore_case = TRUE)) ~ 'NP',
    str_detect(name, regex('national park', ignore_case = TRUE)) ~ 'NP',
    TRUE ~ type))

df %>% st_write(file.path(polys_dir, 'all_sites.shp'))
