#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Pre-process HIH site polygons
# Requires:
#     * input files indicated
# Author:
#     * esturdivant@woodwellclimate.org, 2021-10-10
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(raster)
library(tmap)
tmap_mode('view')
library(sf)
library(terra)
library(tidyverse)

sites_dir <- '~/data/hih_sites'
polys_dir <- file.path(sites_dir, 'final_sites')
final_polys_dir <- '/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/data/hih_sites'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Estonia ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # ~~ TESTING ~~ ----
# agb_500m_2003_vrt <- here::here('~/data/raw_data/biomass/2003_2018/global.vrt')
# agb_500m <- raster::stack(agb_500m_2003_vrt, bands = 1:16)
# agb_500m %>% object.size() %>% print(units = 'MB')
# agb_500m_2003 <- subset(agb_500m, 1)
# 
# agb_500m_2003 <- raster::raster(agb_500m_2003_vrt, bands = 1)
# crs(agb_500m_2003) <- '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'
# agb_500m_2003 %>% object.size() %>% print(units = 'MB')
#   raster::writeRaster(here::here('~/data/raw_data/biomass/2003_2018/global_biomass_500m_2003.tif'))

# # Prep to get paths to assets
# user <- ee_get_assethome()
# addm <- function(x) sprintf("%s/%s", user, x)
# 
# agb_500m_2003 %>% raster_as_ee(assetId = addm("global_biomass_500m_2003"))



# GADM ----
countries_shp <- here::here('~/data', 'raw_data', 'world_context', 'gadm', 'gadm36_1.shp')
estonia <- st_read(countries_shp) %>% 
  filter(str_detect(NAME_0, regex('estonia', ignore_case = TRUE)),
         NAME_1 != 'Peipsi') %>% 
  mutate(div1_lat = stringi::stri_trans_general(str=NAME_1, id='Latin-ASCII')) %>% 
  select(name = NAME_0, 
         div1 = NAME_1) 

# Diva lakes
lakes_shp <- here::here('~/data/raw_data/world_context/diva', 'EST_wat', 
                        'EST_water_areas_dcw.shp')
lakes <- st_read(lakes_shp) %>% st_make_valid()

# Clip out lakes
est_nolks <- estonia %>% st_difference(st_union(lakes))

# Simplify
est_nolks_simp <- est_nolks %>% st_simplify(dTolerance = 0.01)

# Save
estonia_shp <- here::here('~/data', 'sites_for_c_report', 'estonia_div_nolakes_simp01.shp')
est_nolks_simp %>% st_write(estonia_shp, append = FALSE)

# Dissolve
estonia_dissolve <- estonia %>% st_union()
est_diss_nolks <- estonia_dissolve %>% st_difference(st_union(lakes))
est_diss_nolks_simp <- est_diss_nolks %>% st_simplify(dTolerance = 0.01)

# Save
estonia_shp <- here::here('~/data', 'sites_for_c_report', 'estonia_nolakes_simp01.shp')
est_diss_nolks_simp %>% st_write(estonia_shp, append = FALSE)

# Rasterize (at Hansen resolution) ----
# Set ID field 
est_nolks <- est_nolks %>% rownames_to_column('ID')
est_nolks %>% terra::rasterize()



# Natural Earth ----
lakes <- ne_download(scale = 'small', type = 'lakes', category = 'physical', 
            destdir = here::here('~/data', 'raw_data', 'world_context', 'natural_earth'), 
            load = TRUE, returnclass = 'sf')



# Get area for site polygons ----
tdm_shp <- file.path(final_polys_dir, 'TdM_all_dissolved_gapfilled.shp')
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
manombo_shp_new <- file.path(final_polys_dir, 'ManomboNP_all.shp')

# Original Special Reserve ---
manombo_shp <- file.path(sites_dir, 'Manombo', 'ManomboNP_SRGpolys.shp')
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
# Protected areas
pas_wpts <- st_read('/Users/emilysturdivant/data/protected_areas/WDPA/WDPA_Oct2021_tropics_simp001_buff0085.gpkg') %>%
  filter(ISO3 == 'MDG')
# including points adds 4 PAs in Madagascar
tm_shape(df) + tm_polygons(alpha = 0.5) +
tm_shape(pas_wpts) + tm_polygons(alpha = 0.5)

man_pa <- pas_wpts %>%
  filter(str_detect(NAME, regex('manombo', ignore_case = TRUE))) %>%
  select(NAME, DESIG)
man_pa$area_ha <- man_pa%>%
  st_area() %>%
  units::set_units('ha') %>%
  units::set_units(NULL)

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

manombo <- rbind(manombo_sr, efatsy) %>% st_difference()

manombo %>% st_write(manombo_shp_new, append = FALSE)

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


# GERP ----
fps <- list.files(file.path(sites_dir, 'Manombo', 'GERP'), 'shp$', full.names = TRUE)
sf_list <- fps %>% purrr::map(st_read)
names <- fps %>% purrr::map(function(x) {
  x %>% basename() 
})
areas <- sf_list %>% purrr::map(function(x) {
  x %>% st_union() %>% 
    st_area() %>%
    units::set_units('ha') %>%
    units::set_units(NULL)
})

manombo <- st_read(manombo_shp_new)


man_diff <- manombo %>% st_difference()

tm_shape(man_diff) + tm_polygons(alpha = 0.5)

names(sf_list) <- flatten_chr(names)

limite_AP_manombo <- sf_list[[3]] %>% 
  filter(NOM_AP == 'Manombo') %>% 
  select(-TITRE)
limite_AP_manombo %>% 
  st_union() %>% 
  st_area() %>%
  units::set_units('ha') %>%
  units::set_units(NULL)

tm_shape(sf_list[[1]]) + tm_polygons(alpha = 0.4, col = 'purple') + # GERP
  # tm_shape(sf_list[[2]]) + tm_borders() +
  # tm_shape(sf_list[[3]]) + tm_borders() +
  # tm_shape(sf_list[[4]]) + tm_borders() +
  tm_shape(limite_AP_manombo) + tm_borders() +
  tm_shape(manombo) + tm_borders(col = 'black')
  

tm_shape(limite_AP_manombo) + tm_polygons(col = 'seagreen3') + 
  tm_shape(man_pa) + tm_borders(lwd = 3) +
  tm_shape(manombos_sr) + tm_polygons(alpha = 0.4, col = 'purple') +
  tm_shape(manombo_sr) + tm_borders(lwd = 3) +
  tm_shape(efatsy) + tm_borders()

# Manombo from Centre ValBio ----
dir <- '/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/data/Manombo/Centre_ValBio'
fps <- list.files(dir, 'kml$', full.names = TRUE)

sf_list <- fps %>% purrr::map_dfr(st_read)

sf_list$area_ha <- sf_list  %>% 
    st_area() %>%
    units::set_units('ha') %>%
    units::set_units(NULL)

out <- sf_list %>% 
  st_drop_geometry %>% 
  select(Name, area_ha)
  
sf_list <- sf_list %>% mutate(name_abbr = abbreviate(Name))
tm_shape(sf_list) + tm_polygons(alpha = 0.5, col = 'Name') +
  tm_shape(sf_list) + tm_text('name_abbr', shadow = TRUE)
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BBBR ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fp <- list.files(file.path(sites_dir, 'BBBR_divisions', 'final_divisions'), 
                 'BBBR_divisions_v2\\.shp', full.names = TRUE)
new_fp <- file.path(final_polys_dir, 'BBBR_divisions_v3.shp')

bbbr <- st_read(fp) %>% filter(zone != '')
bbbr$area_ha <- st_area(bbbr) %>%
  units::set_units('ha') %>%
  units::set_units(NULL)
bbbr <- bbbr %>% 
  rename(name = zone) %>% 
  mutate(HIH_site = 'Bukit Baka Bukit Raya National Park', 
         site_code = 'BBBRNP',
         type = 'NP') %>% 
  select(HIH_site, site_code, name, type, area_ha)

tmap_mode('view')
tm_shape(bbbr) + tm_polygons()
bbbr %>% st_write(new_fp, append = FALSE)

bbbr <- st_read(new_fp)
tm_shape(bbbr) + tm_polygons(alpha = 0.5) +
  tm_shape(bbbr) + tm_text('name')

bbbr %>% 
  st_drop_geometry %>% 
  select(name, area_ha) %>% 
  clipr::write_clip()

st_crs(bbbr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Papua ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
papua <- st_read(file.path(sites_dir, 'Papua', 'papua_rough_sketch.shp')) %>% 
  select(-id)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge all ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
out_fp <- file.path(final_polys_dir, 'all_sites.shp')

# List all shapefiles in final_sites dir except for all_sites
fps <- list.files(final_polys_dir, '*\\.shp$', full.names = TRUE) %>% 
  .[str_detect(., 'all_sites', negate = TRUE)] %>% 
  .[str_detect(., 'hih_sites', negate = TRUE)]

# Merge them into one
df <- fps %>% purrr::map_dfr(function(x) {
  df <- st_read(x)
  df <- st_zm(df, drop = TRUE)
  df %>% select(any_of(c('HIH_site', 'name', 'type')))
  }) %>% 
  mutate(HIH_site = str_replace_all(HIH_site, 'Gunung Palung', 'Gunung Palung National Park'))

# View
tm_shape(df) + tm_polygons(alpha = 0.5)

# Set types
df <- df %>% 
  mutate(type = case_when(
    str_detect(name, regex('special reserve', ignore_case = TRUE)) ~ 'SR', 
    str_detect(name, regex('classified forest', ignore_case = TRUE)) ~ 'CF', 
    str_detect(name, regex('control', ignore_case = TRUE)) ~ 'NP',
    str_detect(name, regex('experiment', ignore_case = TRUE)) ~ 'NP',
    str_detect(name, regex('national park', ignore_case = TRUE)) ~ 'NP',
    TRUE ~ type))

# Save
df %>% st_write(out_fp, append = FALSE)


# Read and add Papua scouting sites ----
hih_sites_shp <- file.path(final_polys_dir, 'hih_sites_polys.shp')
df <- st_read(out_fp)

df_out <- bind_rows(df, papua)

tm_shape(df_out) + tm_polygons(alpha = 0.5)

df_out %>% st_write(hih_sites_shp, append = FALSE)
pts <- df_out %>% 
  group_by(HIH_site) %>% summarize() %>%
  st_centroid()

pts %>% st_write(file.path(final_polys_dir, 'hih_sites_pts.shp'), append = FALSE)
# tm_shape(df_out) + tm_polygons() +
#   tm_shape(pts) + tm_dots()

# Convert to json ----
hih_sites_shp <- file.path(final_polys_dir, 'hih_sites_polys.shp')
df <- st_read(hih_sites_shp)

hih_sites_json <- file.path(final_polys_dir, 'hih_sites_polys.geojson')
df %>% st_write(hih_sites_json)
