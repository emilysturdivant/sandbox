#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Extract Keystone Forests and AGB values for PAs - Indonesia
# Requires:
#     * 
# Author:
#     * esturdivant@woodwellclimate.org, 2023-03
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(stars)
library(dplyr)
library(ggplot2)
library(units)

poly_dir <- '~/data/misc/CreeNation/Eeyou_Istchee'
fp_polys <- here::here(poly_dir, 'EI_Community_extent_2015.shp')

kfph_tif <- here::here(
  '/Volumes/ejs_storage/data/hih_scaling_ee_exports', 
  'HIH_PlanetaryHealthIndex_v4b_1km_percentiles_2021_11_02_19_05_31-0000000000-0000023296.tif')
agbbgb_tif <- here::here('~/data/Walker_etal_2022/Base_Cur_AGB_BGB_MgCha_500m.tif')
soc_tif <- here::here('~/data/Walker_etal_2022/Base_Cur_SOC_MgCha_500m.tif')
agbbgbsoc_tif <- here::here('~/data/Walker_etal_2022/Base_Cur_AGB_BGB_SOC_MgCha_500m.tif')

polys_out <- here::here(poly_dir, 'Eeyou_Istchee_KFPH.shp')

# Calculate zonal statistics ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read stars (proxy)
agb <- stars::read_stars(agb_tif)

dims <- st_dimensions(agb)
pix_ha <- units::set_units(dims$x$delta**2, 'm^2') %>% units::set_units('ha')

# Load and reproject polygons
pols <- st_read(fp_polys) %>%
  filter(!st_is_empty(.)) %>%
  st_make_valid() %>% #tbl_vars()
  select(COMUNITY, Area_Km) %>% 
  mutate(Area_ha = st_area(geometry) %>% units::set_units('ha'))

## AGB ----
# Load and reproject polygons
pols_geom <- pols %>% 
  st_transform(st_crs(agb)) %>% 
  st_geometry()

# Get mean
agb_mean <- aggregate(agb, by = pols_geom, FUN = sum, na.rm=TRUE) %>% 
  pull() %>% 
  tibble::as_tibble_col(column_name = 'Carbon_dens_tCha')

## Add stats to sf ----
pols <- pols %>% bind_cols(kf_mean, kf_qs, agb_mean)
pols <- pols %>% 
  mutate(Carbon_stock_MtC = Carbon_dens_tCha * Area_ha * 1e-6,
         PA_Name = stringr::str_c(ORIG_NAME, DESIG_ENG, sep=' '))

# Save GeoJSON
pols %>% st_write(polys_out, append=FALSE, delete_dsn=TRUE)
pols <- st_read(polys_out)

# Save as CSV
df <- pols %>%
  st_drop_geometry() %>% 
  filter(as.numeric(Area_ha) > as.numeric(pix_ha*10), 
         !is.na(KF_mean)) %>%
  transmute(
    PA_Name = as.character(PA_Name),
    priority = ifelse(priority > 14, NA, priority),
    Wiratno_rank = as.character(priority) %>% 
      stringr::str_replace_all('10', 'other') %>% 
      tidyr::replace_na(''),
    Area_ha = as.numeric(Area_ha) %>% round(digits = 1), 
    KFPH_mean = as.numeric(KF_mean) %>% round(digits = 1), 
    KFPH_IQR = stringr::str_glue('{round(KF_q25)} - {round(KF_q75)}'),
    Carbon_dens_tCha = as.numeric(Carbon_dens_tCha) %>% round(digits = 1), 
    Carbon_stock_MtC = as.numeric(Carbon_stock_MtC) %>% round(digits = 2)
    ) %>% 
  select(PA_Name, Area_ha, Wiratno_rank, Carbon_dens_tCha, Carbon_stock_MtC,
         KFPH_mean, KFPH_IQR)

df %>% 
  readr::write_csv(paste0(tools::file_path_sans_ext(polys_out), '.csv'), 
                   na = '')

# Analyze - moved to explore_priority_pas.Rmd ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df <- readr::read_csv(paste0(tools::file_path_sans_ext(polys_out), '.csv'))
# 
# df %>% 
#   tidyr::pivot_longer(cols = any_of(c('KF_mean', 'KF_q50', 'KF_q25'))) %>% 
#   ggplot(aes(value)) +
#   geom_histogram(binwidth = 2) +
#   theme_minimal() +
#   facet_wrap(vars(name), ncol = 1)
# 
# mean_gt95 <- df %>% 
#   filter(KF_mean > 97)
# print(paste('Number of PAs with mean Keystone Forests value >96:', nrow(mean_gt95)))
# 
# top20_by_kf <- df %>% 
#   arrange(desc(KF_mean)) %>% 
#   slice_max(KF_mean, n = 20)
# 
# top20_by_kf %>% 
#   select(NAME, DESIG_ENG, priority, KF_mean, KF_q50, AGB_mean_mgcha, area_ha, carbon_stock_Mmgc)
# 
# priorities <- df %>% 
#   filter(priority < 14) %>% 
#   arrange(desc(KF_mean)) %>% 
#   select(NAME, DESIG_ENG, priority, KF_mean, KF_q50, AGB_mean_mgcha, area_ha, carbon_stock_Mmgc)
# print('Keystone Forests values for sites recommended by Mr. Wiratno: ')
# priorities
# 
# 
# 
# 
# 
# 
# # Plots... 
# med_gt95 <- df %>% 
#   filter(KF_q50 > 95)
# print(paste('Number of PAs with median Keystone Forests value >95:', nrow(med_gt95)))
# 
# mean_gt95 <- df %>% 
#   filter(KF_mean > 95)
# print(paste('Number of PAs with mean Keystone Forests value >95:', nrow(mean_gt95)))
# 
# p75_gt95 <- df %>% 
#   filter(KF_q25 > 95)
# print(paste('Number of PAs with 25th percentile Keystone Forests value >95:', nrow(p75_gt95)))
# 
# top10_by_kf <- df %>% 
#   arrange(desc(KF_mean)) %>% 
#   slice_max(KF_mean, n = 10)
#   
# print('Top 10 PAs by mean Keystone Forests value:')
# top10_by_kf %>% select(NAME, DESIG_ENG, priority, KF_mean, KF_q50, AGB_mean_mgcha, area_ha, carbon_stock_Mmgc)
# 
# # Plots... 
# pols %>% 
#   filter(priority < 14) %>% 
#   ggplot(aes(priority, KF_q50)) +
#   geom_point()
# 
# pols %>% 
#   filter(priority < 14) %>% 
#   ggplot(aes(AGB_mean_mgcha, KF_q50, color=priority)) +
#   geom_point() +
#   scale_color_viridis_c(name = "Priority", option = "H" )
# 
# pols %>% 
#   filter(priority < 14) %>% 
#   ggplot(aes(KF_mean, KF_q50)) +
#   geom_point()
# 
# pols %>% 
#   filter(priority < 14) %>% 
#   arrange(desc(KF_mean))
# 
# 
# # Extract Protected Areas in the tropics ----
# raw_dir <- '/Volumes/ejs_storage/data/raw_data'
# data_dir <- '/Volumes/ejs_storage/data'
# 
# # Protected area polygons ----
# pa_dir <- here::here(raw_dir, 'WDPA', 'WDPA_Mar2023_Public_shp')
# (pa_zips <- list.files(pa_dir, pattern = ".zip$", full.names=TRUE, recursive = TRUE))
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
#   pa <- st_read(shp_fp[[2]]) |> st_make_valid()
#   
#   # Filter
#   pa <- pa %>%
#     filter(MARINE != 2) %>%
#     filter(ISO3 != 'IDN') %>%
#     st_simplify(dTolerance = 0.001)
#   
#   # subset polygons to those that intersect tropics rectangle
#   pa_tropics <- pa[unlist(st_intersects(tropics_rect, pa)),]
#   
#   return(pa_tropics)
# }
# 
# pa_tropics <- pa_zips %>% purrr::map_dfr(unzip_and_filter_pa_polygons)
# out_fp <- here::here(data_dir, 'protected_areas', 
#                      paste0(basename(tools::file_path_sans_ext()), '_simp001.gpkg'))s
# pa_tropics %>% st_write(out_fp)
# # 
# # # Protected area points ----
# # # pa_zip <- file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_Public_shp', 'WDPA_Oct2021_Public_shp.zip')
# # pa_zips <- list.files(
# #   file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_Public_shp'),
# #   pattern = ".zip$", full.names=TRUE, recursive = TRUE)
# # 
# # # Unzip
# # unzip_and_filter_pa_points <- function(z) {
# #   # z <- pa_zips[[1]]
# #   
# #   # Unzip to temp dir
# #   miao <- tempfile()
# #   unzip(z, exdir = miao)
# #   
# #   # Load shapefile (polygons)
# #   (shp_fp <- list.files(miao, pattern = "points\\.shp$", full.names=TRUE, recursive = TRUE))
# #   pa <- st_read(shp_fp[[1]])
# #   
# #   pa <- pa[unlist(st_intersects(tropics_rect, pa)),]
# #   pa <- pa %>% filter(REP_AREA > 0, MARINE != 2)
# #   pa_buff <- pa %>% st_buffer(8.5*0.001)
# #   
# #   return(pa_buff)
# # }
# # 
# # pa_tropics <- pa_zips %>% purrr::map_dfr(unzip_and_filter_pa_points)
# # pa_tropics %>% 
# #   st_write(file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_points_tropics_buff0085.gpkg'))
# # 
# # # Merge
# # pa_polys <- st_read(file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_polygons_tropics_simp001.gpkg'))
# # pa_tropics2 <- bind_rows(pa_polys, pa_tropics)
# # 
# # pa_tropics2 %>% object.size() %>% print(units = "MB")
# # pa_tropics2 %>% 
# #   st_write(file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_tropics_simp001_buff0085.gpkg'))
# 
# # OECM polygons ----
# # pa_zip <- file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_Public_shp', 'WDPA_Oct2021_Public_shp.zip')
# pa_zips <- list.files(
#   file.path(data_dir, 'protected_areas', 'WDOECM_Oct2021_Public_shp'),
#   pattern = ".zip$", full.names=TRUE, recursive = TRUE)
# 
# # Unzip
# unzip_and_filter_pas <- function(z) {
#   # z <- pa_zips[[1]]
#   
#   # Unzip to temp dir
#   miao <- tempfile()
#   unzip(z, exdir = miao)
#   
#   # Load shapefile (polygons)
#   (shp_fp <- list.files(miao, pattern = "polygons\\.shp$", full.names=TRUE, recursive = TRUE))
#   pa <- st_read(shp_fp[[1]])
#   
#   # Filter
#   pa <- pa %>% 
#     filter(MARINE != 2) %>% 
#     st_simplify(dTolerance = 0.001) 
#   
#   # subset polygons to those that intersect tropics rectangle
#   pa_polys <- pa[unlist(st_intersects(tropics_rect, pa)),]
#   
#   # Load points
#   (shp_fp <- list.files(miao, pattern = "points\\.shp$", full.names=TRUE, recursive = TRUE))
#   pa <- st_read(shp_fp[[1]])
#   pa <- pa[unlist(st_intersects(tropics_rect, pa)),]
#   pa <- pa %>% filter(REP_AREA > 0, MARINE != 2)
#   pa_points <- pa %>% st_buffer(8.5*0.001)
#   
#   # Combine
#   pa_tropics <- bind_rows(pa_polys, pa_points)
#   
#   # Return
#   return(pa_tropics)
# }
# 
# pa_tropics <- pa_zips %>% purrr::map_dfr(unzip_and_filter_pas)
# tm_shape(pa_tropics) + tm_polygons()
# pa_tropics %>% 
#   st_write(file.path(data_dir, 'protected_areas', 'WDOECM_Oct2021_tropics_simp001_buff0085.gpkg'))
