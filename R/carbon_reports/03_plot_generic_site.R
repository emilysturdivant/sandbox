#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Calculate annual biomass loss for input polygons
# Requires:
#     * GEE account with access to global_AGB_2000_30m_Mgha_V4
#     * site polygon
#     * 02_report_from_Hansen_data.R for functions following the GEE section
# Author:
#     * esturdivant@woodwellclimate.org, 2021-10-10
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load libraries 
library(sf)
library(patchwork)
library(segmented)
library(tidyverse)
library(tmap)

# Prep paths
export_path <- '/Volumes/GoogleDrive/My Drive/Earth Engine Exports'
polys_fp <- here::here('~/data', 'sites_for_c_report', 'estonia_div_nolakes.shp')
task_name <- tools::file_path_sans_ext(basename(polys_fp))
out_fp <- file.path(export_path, str_c(task_name, '.geojson'))

# Run GEE process
source('R/carbon_reports/01_calculate_loss_with_rgee.R')

# Load functions 
source('R/carbon_reports/02_report_from_Hansen_data.R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get original polygons ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
countries_shp <- here::here('~/data', 'raw_data', 'world_context', 
                            'gadm', 'gadm36_1.shp')
estonia <- st_read(countries_shp) %>% 
  filter(str_detect(NAME_0, regex('estonia', ignore_case = TRUE))) %>% 
  mutate(div1_lat = stringi::stri_trans_general(str=NAME_1, id='Latin-ASCII')) %>% 
  rename(name = NAME_0, 
         div1 = NAME_1)

names_lu <- estonia %>% 
  st_drop_geometry() %>% 
  select(name, div1, div1_lat)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load new sf for all sites ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data
df <- st_read(out_fp) %>% filter(div1 != 'Peipsi')

# Tidy 
df_tidy <- tidy_forest_loss_df(df)

# # Add accents back
# df_tidy <- df_tidy %>% 
#   left_join(names_lu, by = c('name', div1 = 'div1_lat'), suffix = c('', '_accents'))

# Get site and division names 
(site <- df_tidy %>% distinct(name) %>% deframe())
site_code <- abbreviate(site, minlength = 3)
df_site <- df_tidy

# Create output table of 20-year loss ----
# Sum losses over 20-year period
df_sums <- df_site %>% 
  group_by(div1, carbon_2000_mgc, area_ha) %>% 
  summarize(across(any_of(c('carbon_loss_MgC', 'forest_loss_ha')), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup() %>% 
  arrange(desc(carbon_loss_MgC)) 

# Totals row
df_total <- df_sums %>% 
  summarize(across(where(is.double), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(div1 = 'Total')

# Combine and change column names
df_out <- df_sums %>% 
  bind_rows(df_total) %>% 
  select(County = div1, 
         C_stock_2000_MgC = carbon_2000_mgc, 
         Area_ha = area_ha, 
         C_loss_MgC = carbon_loss_MgC,
         Forest_loss_ha = forest_loss_ha) %>% 
  mutate(County = stringi::stri_trans_general(str=County, id='Latin-ASCII'))

# Save DF to CSV
sums_csv <- here::here('outputs', site, 'loss_by_county.csv')
df_out %>% write_csv(sums_csv)

# Create piecewise regression plots ----
(div_names <- df_sums$div1)
df_site_t <- df_site %>% 
  mutate(carbon_loss_MgC = carbon_loss_MgC / 1000)

plots <- list()
for (i in 1:length(div_names)) {
  div_name <-  div_names[[i]]
  print('')
  print(div_name)
  
  df_zone <- filter(df_site_t, div1 == div_name)
  
  try(rm(pw_fit))
  pw_fit <- get_piecewise_line(df_zone)
  p <- plot_pw_fit(df_zone, div_name, pw_fit)
  try(rm(pw_fit))
  plots[[i]] <- p
  
}

# plots <- create_pw_plot_list(div_names, df_site_t)
params <- get_params(div_names)
formatted_plots <- layout_plots(plots, params)

ggsave(file.path('outputs', site, str_c(site_code, '_counties_2001_2020_pw.png')), 
       plot = formatted_plots,
       width = params$png_width,
       height = params$png_height)

# Break into two groups
pset1 <- plots[1:8]
params <- get_params(length(pset1))
pset1_formatted <- layout_plots(pset1, params)

ggsave(file.path('outputs', site, str_c(site_code, '_counties_2001_2020_pw_1to8.png')), 
       plot = pset1_formatted,
       width = params$png_width,
       height = params$png_height)

# Break into two groups
pset2 <- plots[9:15]
params <- get_params(length(pset2))
pset2_formatted <- layout_plots(pset2, params)

ggsave(file.path('outputs', site, 
                 str_c(site_code, '_counties_2001_2020_pw_9to15.png')), 
       plot = pset2_formatted,
       width = params$png_width,
       height = params$png_height)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Total piecewise regression plot ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_tot <- df_site %>% 
  group_by(year) %>% 
  select(-ends_with('pct')) %>% 
  summarize(across(where(is.double), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup()

df_tot_t <- df_tot %>% 
  mutate(carbon_loss_MgC = carbon_loss_MgC / 1000)

try(rm(pw_fit))
df_zone <- df_tot_t
pw_fit <- get_piecewise_line(df_tot_t)
p <- plot_pw_fit(df_tot_t, site, pw_fit)
try(rm(pw_fit))

# plots <- create_pw_plot_list(div_names, df_tot_t)
params <- get_params(1)
formatted_plots <- layout_plots(p, params, fix_y = FALSE)

ggsave(file.path('outputs', site, str_c(site_code, '_total_2001_2020_pw.png')), 
       plot = formatted_plots,
       width = params$png_width,
       height = params$png_height)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Map, choropleth by carbon loss ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c_loss_sf <- df %>% 
  select(div1) %>% 
  inner_join(
    df_sums %>% 
      mutate(
        c_loss_pct = carbon_loss_MgC / carbon_2000_mgc * 100,
        c_loss_dens = carbon_loss_MgC / area_ha,
        c_loss_str = round(carbon_loss_MgC / 1e6, 2) %>% 
          format(., big.mark = ',') %>% 
          str_c(., ' m MgC'),
        label = str_c(div1, '\n', c_loss_str))
  )

c_loss_sf %>% 
  st_write(here::here('outputs', site, 'carbon_loss_counties.gpkg'),
           append = FALSE)


tmap_mode('view')
tm_shape(c_loss_sf2) + tm_polygons(col = 'carbon_loss_MgC') +
  tm_shape(c_loss_sf2) + tm_text('label')
