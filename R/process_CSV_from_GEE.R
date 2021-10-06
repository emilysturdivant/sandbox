
library(sf)
library(terra)
library(tidyverse)

# Convert Allen zoonotic spillover predictions to GeoTIFFs ----
load('data/predictions.RData')

lyr_name <- 'bsm_response'
lyr_name <- 'bsm_weight_pubs'
lyr_name <- 'bsm_weight_pop'

# Convert to raster
pred <- predictions %>% 
  dplyr::select(lon, lat, matches(lyr_name)) %>% 
  dplyr::rename(x=lon, y=lat)

# Convert to SpatialPixelsDataFrame
sp::coordinates(pred) = ~ x + y
sp::proj4string(pred) = sp::CRS("+init=epsg:4326") # set it to lat-long
sp::gridded(pred) <- TRUE

# Convert to SpatRaster
predr <-  raster::raster(pred) %>% rast()
plot(predr)

# Save
writeRaster(predr, str_c('data/allen_', lyr_name, '.tif'))


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

# Function ----
tidy_forest_loss_df <- function(df) {
  df_carbon <- df %>% 
    select(starts_with('carbon_loss'), zone) %>% 
    pivot_longer(-zone, 
                 names_to = 'year',
                 names_prefix = 'carbon_loss_', 
                 values_to = 'carbon_loss_MgC') %>% 
    mutate(year = str_c(str_extract(year, '\\d{4}'), '-01-01') %>% 
             lubridate::year(), )
  
  df_area <- df %>% 
    select(starts_with('forest'), zone) %>% 
    pivot_longer(-zone, 
                 names_to = 'year',
                 names_prefix = 'forest_loss_', 
                 values_to = 'forest_loss_ha') %>% 
    mutate(year = str_c(str_extract(year, '\\d{4}'), '-01-01') %>% 
             lubridate::year())
  
  # Join forest carbon and area loss values
  df_join <- df_carbon %>% 
    left_join(df_area) %>% 
    mutate(across(carbon_loss_MgC:forest_loss_ha, ~ .x * -1)) %>% 
    pivot_longer(cols = carbon_loss_MgC:forest_loss_ha) %>% 
    mutate(yr_label = str_c(substr(year-1, 3,4), '-', substr(year, 3,4)))
  
  return(df_join)
}

# BBBR ----
df <- read_csv('data/BBBR_annual_deforestation_by_district.csv') %>% 
  mutate(zone = c('control 1', 'control 2', 'experiment'))

df_join <- tidy_forest_loss_df(df)

# Plot
yr_labels <- df_join %>% distinct(yr_label) %>% deframe
df_join %>% 
  filter(!str_detect(name, 'forest')) %>% 
  ggplot(aes(x = year, y = value)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 2001:2020, 
                     labels = yr_labels) +
  labs(x = 'Year', y = 'Aboveground carbon (metric tons)') +
  facet_grid(rows = vars(zone),
             cols = vars(name), 
             scales = 'fixed') +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Cumulative loss
# (df_cumsum <- df_join %>% 
#     arrange(year) %>% 
#     group_by(zone, name) %>% 
#     mutate(cum_loss = cumsum(value)))
# 
# # Plot
# df_cumsum %>% 
#   ggplot(aes(x = year, y = cum_loss)) +
#   geom_point() +
#   geom_line() +
#   scale_x_continuous(breaks = 2001:2020) +
#   xlab("Year") +
#   facet_grid(rows = vars(name),
#              cols = vars(zone), 
#              scales = 'free_y')

# Terra do Meio ----
df_tdm <- read_csv('data/TdM_RESEX_annual_deforestation.csv') %>% 
  mutate(zone = c('Terra do Meio', 'RESEX Rio Iriri', 
                  'RESEX Rio Xingu', 'RESEX Riosinho do Anfrisio'))

df_tdm <- tidy_forest_loss_df(df_tdm) %>% 
  filter(!str_detect(name, 'forest'))

# Plot
yr_labels <- df_tdm %>% distinct(yr_label) %>% deframe
df_tdm %>% 
  filter(zone == 'Terra do Meio') %>% 
  ggplot(aes(x = year, y = value)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 2001:2020, 
                     labels = yr_labels) +
  labs(x = 'Year', y = 'Aboveground carbon (metric tons)') +
  facet_grid(rows = vars(zone),
             # cols = vars(name), 
             scales = 'fixed') +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Plot
yr_labels <- df_tdm %>% distinct(yr_label) %>% deframe
df_tdm %>% 
  filter(zone != 'Terra do Meio') %>% 
  ggplot(aes(x = year, y = value)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 2001:2020, 
                     labels = yr_labels) +
  labs(x = 'Year', y = 'Aboveground carbon (metric tons)') +
  facet_grid(rows = vars(zone),
             # cols = vars(name), 
             scales = 'fixed') +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Plot
# df_join %>% 
#   filter(zone == 'Terra do Meio') %>% 
#   ggplot(aes(x = year, y = value)) +
#   geom_point() +
#   geom_line() +
#   scale_x_continuous(breaks = 2001:2020) +
#   xlab("Year") +
#   facet_grid(rows = vars(name),
#              cols = vars(zone), 
#              scales = 'free_y')

# Gunung Palung ----
df_gp <- read_csv('data/GPNP_annual_deforestation.csv') %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(zone = 'Gunung Palung')

df_gp <- tidy_forest_loss_df(df_gp) %>% 
  filter(!str_detect(name, 'forest'))

# Plot
yr_labels <- df_gp %>% distinct(yr_label) %>% deframe
df_gp %>% 
  ggplot(aes(x = year, y = value)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 2001:2020, 
                     labels = yr_labels) +
  labs(x = 'Year', y = 'Aboveground carbon (metric tons)') +
  facet_grid(rows = vars(zone),
             # cols = vars(name),
             scales = 'fixed') +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Manombo ----
df_manombo <- read_csv('data/Manombo_annual_deforestation.csv') %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(zone = 'Manombo')

df_manombo <- tidy_forest_loss_df(df_manombo) %>% 
  filter(!str_detect(name, 'forest'))

# Plot
yr_labels <- df_manombo %>% distinct(yr_label) %>% deframe
df_manombo %>% 
  ggplot(aes(x = year, y = value)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 2001:2020, 
                     labels = yr_labels) +
  labs(x = 'Year', y = 'Aboveground carbon (metric tons)') +
  facet_grid(rows = vars(zone),
             # cols = vars(name),
             scales = 'fixed') +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
