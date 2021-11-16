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
# source('R/carbon_reports/01_calculate_loss_with_rgee.R')

# Load functions 
source('R/carbon_reports/02_functions.R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get original polygons ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# countries_shp <- here::here('~/data', 'raw_data', 'world_context', 
#                             'gadm', 'gadm36_1.shp')
# estonia <- st_read(countries_shp) %>% 
#   filter(str_detect(NAME_0, regex('estonia', ignore_case = TRUE))) %>% 
#   mutate(div1_lat = stringi::stri_trans_general(str=NAME_1, id='Latin-ASCII')) %>% 
#   rename(name = NAME_0, 
#          div1 = NAME_1)
# 
# names_lu <- estonia %>% 
#   st_drop_geometry() %>% 
#   select(name, div1, div1_lat)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load new sf for all sites ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data
df_sf <- st_read(out_fp) %>% filter(div1 != 'Peipsi')

# Get site and division names 
(site <- df %>% distinct(name) %>% deframe())
site_code <- abbreviate(site, minlength = 3)

# Create output table of 20-year loss ----
# Get values by county
df_sf_sums <- df_sf %>% 
  mutate(c_2000_MtC = carbon_2000_mgc / 1e6, 
         c_loss_MtC = c_loss_mgc / 1e6,
         c_loss_pct = c_loss_MtC / c_2000_MtC * 100, 
         c_loss_dens_tCha = c_loss_mgc / area_ha
         ) %>% 
  select(div1, 
         area_ha,
         forest_2000_ha, 
         forest_loss_ha,
         c_2000_MtC,
         c_loss_MtC, 
         c_loss_pct,
         c_dens_2000_tCha = c_dens_2000_mgcha, 
         c_dens_loss_tCha = c_loss_dens,
         c_loss_dens_tCha) %>% 
  arrange(desc(c_loss_MtC))

# Totals row
df_total <- df_sf_sums %>% 
  st_drop_geometry() %>% 
  summarize(
    across(any_of(c('area_ha', 'forest_2000_ha', 'forest_loss_ha',
                  'c_2000_MtC', 'c_loss_MtC')), 
           ~ sum(.x, na.rm = TRUE)),
    across(any_of(c('c_loss_pct', 'c_dens_2000_tCha', 'c_dens_loss_tCha', 'c_loss_dens_tCha')), 
           ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(div1 = 'Estonia')

# Combine and change column names
df_out <- df_sf_sums %>% 
  st_drop_geometry() %>% 
  bind_rows(df_total) %>% 
  mutate(div1 = stringi::stri_trans_general(str=div1, id='Latin-ASCII'))

# Save DF to CSV
sums_csv <- here::here('outputs', site, 'loss_by_county.csv')
df_out %>% write_csv(sums_csv)


# Display table ----
library(flextable)
library(officer)
df_out2 <- df_out %>% 
  rename(County = div1) %>%
  select(-c_dens_loss_tCha) 

ft <- df_out2 %>% 
  mutate(across(where(is.double), ~ round(.x, 1))) %>%
  flextable() %>% 
  colformat_num(digits = 1) %>% 
  font(fontname = 'Times New Roman', part = 'all') %>% 
  font(fontname = 'Times New Roman Bold', part = 'header') %>% 
  fontsize(size = 10, part = 'all') %>% 
  # set_header_labels(site = paste0('Division')) %>% 
  compose(j = 'area_ha', part = 'header', 
          value = as_paragraph('Land area (ha)')) %>% 
  compose(j = 'forest_2000_ha', part = 'header', 
          value = as_paragraph('Forest area in 2000 (ha)')) %>% 
  compose(j = 'forest_loss_ha', part = 'header', 
          value = as_paragraph('Forest area loss (ha)')) %>% 
  compose(j = 'c_2000_MtC', part = 'header', 
          value = as_paragraph('Carbon stock in 2000 (MtC)')) %>% 
  compose(j = 'c_loss_MtC', part = 'header', 
          value = as_paragraph('Carbon stock loss (MtC)')) %>% 
  compose(j = 'c_loss_pct', part = 'header', 
          value = as_paragraph('Carbon stock loss (%)')) %>% 
  compose(j = 'c_dens_2000_tCha', part = 'header', 
          value = as_paragraph('Carbon density in 2000 (tC/ha)')) %>% 
  compose(j = 'c_loss_dens_tCha', part = 'header', 
          value = as_paragraph('Carbon density loss (tC/ha)')) %>% 
  hline_top(border = fp_border(width = 1), part = 'all') %>% 
  hline_bottom(border = fp_border(width = 1), part = 'all') %>% 
  hline_top(border = fp_border(width = 1), part = 'body') %>% 
  hline_bottom(border = fp_border(width = 1), part = 'body') %>% 
  set_table_properties(layout = "autofit", width = 0.7)
ft

ft_forest <- df_out2 %>% 
  select(County, area_ha, forest_2000_ha, forest_loss_ha) %>% 
  mutate(across(where(is.double), ~ round(.x, 1))) %>%
  flextable() %>% 
  colformat_num(digits = 1) %>% 
  font(fontname = 'Times New Roman', part = 'all') %>% 
  fontsize(size = 10, part = 'all') %>% 
  # set_header_labels(site = paste0('Division')) %>% 
  compose(j = 'area_ha', part = 'header', 
          value = as_paragraph('Land area (ha)')) %>% 
  compose(j = 'forest_2000_ha', part = 'header', 
          value = as_paragraph('Forest area in 2000 (ha)')) %>% 
  compose(j = 'forest_loss_ha', part = 'header', 
          value = as_paragraph('Forest area loss (ha)')) %>% 
  hline_top(border = fp_border(width = 1), part = 'all') %>% 
  hline_bottom(border = fp_border(width = 1), part = 'all') %>% 
  set_table_properties(layout = "autofit", width = 0.7)
ft_forest

ft_carbon <- df_out2 %>% 
  select(-forest_2000_ha, -forest_loss_ha) %>% 
  mutate(across(where(is.double), ~ round(.x, 1))) %>%
  flextable() %>% 
  colformat_num(digits = 1) %>% 
  font(fontname = 'Times New Roman', part = 'all') %>% 
  fontsize(size = 10, part = 'all') %>% 
  # set_header_labels(site = paste0('Division')) %>% 
  compose(j = 'area_ha', part = 'header', 
          value = as_paragraph('Land area (ha)')) %>% 
  compose(j = 'c_2000_MtC', part = 'header', 
          value = as_paragraph('Carbon stock in 2000 (MtC)')) %>% 
  compose(j = 'c_loss_MtC', part = 'header', 
          value = as_paragraph('Carbon stock loss (MtC)')) %>% 
  compose(j = 'c_loss_pct', part = 'header', 
          value = as_paragraph('Carbon stock loss (%)')) %>% 
  compose(j = 'c_dens_2000_tCha', part = 'header', 
          value = as_paragraph('Carbon density in 2000 (tC/ha)')) %>% 
  compose(j = 'c_loss_dens_tCha', part = 'header', 
          value = as_paragraph('Carbon density loss (tC/ha)')) %>% 
  hline_top(border = fp_border(width = 1), part = 'all') %>% 
  hline_bottom(border = fp_border(width = 1), part = 'all') %>% 
  set_table_properties(layout = "autofit", width = 0.7)
ft_carbon

sums_doc <- here::here('outputs', site, 'loss_tables.docx')
save_as_docx(forest = ft_forest, carbon = ft_carbon, path = sums_doc)

# Create piecewise regression plots ----
# Tidy 
df <- df_sf %>% st_drop_geometry()
df_site <- tidy_forest_loss_df(df)

(div_names <- df_sums$div1)
df_site_t <- df_site %>% 
  mutate(c_loss_MtC = carbon_loss_MgC / 1e6)

plots <- list()
for (i in 1:length(div_names)) {
  div_name <-  div_names[[i]]
  print('')
  print(paste0(i, ': ', div_name))
  
  df_zone <- filter(df_site_t, div1 == div_name)
  
  try(rm(pw_fit))
  pw_fit <- get_piecewise_line(df_zone)
  p <- plot_pw_fit(df_zone, div_name, pw_fit)
  try(rm(pw_fit))
  plots[[i]] <- p
  
}

# plots <- create_pw_plot_list(div_names, df_site_t)
params <- get_params(length(div_names))
formatted_plots <- layout_plots(plots, params)

ggsave(file.path('outputs', site, str_c(site_code, '_counties_2001_2020_pw.png')), 
       plot = formatted_plots,
       width = params$png_width,
       height = params$png_height)

# # Break into two groups
# pset1 <- plots[1:8]
# params <- get_params(length(pset1))
# pset1_formatted <- layout_plots(pset1, params)
# 
# ggsave(file.path('outputs', site, str_c(site_code, '_counties_2001_2020_pw_1to8.png')), 
#        plot = pset1_formatted,
#        width = params$png_width,
#        height = params$png_height)
# 
# # Break into two groups
# pset2 <- plots[9:15]
# params <- get_params(length(pset2))
# pset2_formatted <- layout_plots(pset2, params)
# 
# ggsave(file.path('outputs', site, 
#                  str_c(site_code, '_counties_2001_2020_pw_9to15.png')), 
#        plot = pset2_formatted,
#        width = params$png_width,
#        height = params$png_height)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Total piecewise regression plot ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_tot <- df_site %>% 
  group_by(year) %>% 
  select(any_of(c('carbon_loss_MgC', 'forest_loss_ha'))) %>% 
  summarize(across(where(is.double), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup()

df_tot_t <- df_tot %>% 
  mutate(c_loss_MtC = carbon_loss_MgC / 1e6)

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
# c_loss_sf <- df %>% 
#   select(div1) %>% 
#   inner_join(
#     df_sums %>% 
#       mutate(
#         c_loss_pct = carbon_loss_MgC / carbon_2000_mgc * 100,
#         c_loss_dens = carbon_loss_MgC / area_ha,
#         c_loss_str = round(carbon_loss_MgC / 1e6, 2) %>% 
#           format(., big.mark = ',') %>% 
#           str_c(., ' m MgC'),
#         label = str_c(div1, '\n', c_loss_str))
#   )
# 
# c_loss_sf %>% 
#   st_write(here::here('outputs', site, 'carbon_loss_counties.gpkg'),
#            append = FALSE)

df_sf_sums %>% 
  st_write(here::here('outputs', site, 'counties_carbon_loss.gpkg'),
           append = FALSE)

tmap_mode('plot')
p1 <- tm_shape(df_sf_sums) + 
  tm_polygons(col = 'c_loss_MtC', palette = 'Reds', title = 'Carbon stock loss (MtC)')
p2 <- tm_shape(df_sf_sums) + 
  tm_polygons(col = 'c_loss_pct', palette = 'Reds', title = 'Carbon stock loss (%)')
