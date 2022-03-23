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
library(flextable)
library(officer)
library(sf)
library(patchwork)
library(segmented)
library(tidyverse)
library(tmap)
library(lubridate)

# Prep paths
export_path <- '/Volumes/GoogleDrive/My Drive/Earth Engine Exports'
# polys_fp <- here::here('~/data', 'sites_for_c_report', 'estonia_div_nolakes.shp')
task_name <- tools::file_path_sans_ext(basename(polys_fp))
out_fp <- file.path(export_path, 'v1', str_c(task_name, '.geojson'))

site_name_var <- 'HIH_site' # Estonia: 'name'
site_div_var <- 'name' # Estonia: 'div1'
div_column_name <- 'Zone'
  
# Run GEE process
# source('R/carbon_reports/01_calculate_loss_with_rgee.R')

# Load functions 
source('R/carbon_reports/02_functions.R')

# Site parameters ----
# BBBR
params <- list(
  hih_site = 'BBBR',
  shp = "/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/data/hih_sites/BBBR_dissolved_v3.shp",
  dates = tibble(year = as_date(c(ym('2017-01'), ym('2018-09'), ym('2019-06'), 
                                  ym('2021-01'))), 
                 event = c('Radical Listening', 'Healthcare', 
                           'Alternative Livelihoods', 
                           'Reciprocity Agreements / Incentive System'))
)
hih_start <- params$dates[1,1]
# 
# # GPNP
# params <- list(
#   hih_site = 'GPNP',
#   shp = "/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/data/hih_sites/GPNP_dissolved.shp",
#   dates = tibble(year = as_date(c(ym('2006-01'), ym('2007-06'), ym('2008-01'), 
#                                   ym('2008-09'), ym('2009-11'), ym('2017-01'))), 
#                  event = c('Radical Listening', 'Healthcare', 
#                            'Incentive System', 'Alternative Livelihoods', 
#                            'Reforestation', 'Chainsaw Buyback'))
# )
# 
# # Manombo
# params <- list(
#   hih_site = 'Manombo',
#   shp = "/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/data/hih_sites/ManomboNP_all.shp",
#   dates = tibble(year = as_date(c(ym('2019-09'), ym('2020-02'), ym('2020-09'), 
#                                   ym('2020-10'), ym('2021-05'))), 
#                  event = c('Radical Listening', 
#                            'reciprocity agreements / incentive system',
#                            'alternative livelihoods (agriculture training)',
#                            'healthcare',
#                            'reforestation'))
# )
# 
# # Xingu
# params <- list(
#   hih_site = 'TdM',
#   shp = "/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/data/hih_sites/TdM_all_dissolved_gapfilled.shp",
#   dates = tibble(year = as_date(c(ym('2020-07'), ym('2020-09'))), 
#                  event = c('Radical Listening', 'Healthcare'))
# )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load new sf for all sites ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data
df_sf <- st_read(out_fp)# %>% filter(div1 != 'Peipsi')

# Get site and division names 
(site <- unique(df_sf[[site_name_var]]))
(site <- str_split(basename(out_fp), '[_\\W]', simplify = TRUE)[,1])
# (site_code <- abbreviate(site, minlength = 3))
site_code <- site

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create output table of 20-year loss ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate totals ----
# Drop geometry
df <- df_sf %>% st_drop_geometry() 

if( nrow(df_sf) > 1 ) {

  # Totals row
  df_total <- df %>% 
    summarize(
      across(any_of(ends_with('_ha')), ~ sum(.x, na.rm = TRUE)),
      across(any_of(ends_with('_mgc')), ~ sum(.x, na.rm = TRUE))) %>% 
    mutate({{site_div_var}} := 'Total')
  
  # Combine and change column names
  df_sums <- df %>% 
    bind_rows(df_total)
  
} else {
  
  df_sums <- df %>% mutate(name = 'Total')
}

# Get values by county ----
df_out <- df_sums %>% 
  mutate(c_2000_MtC = carbon_2000_mgc / 1e6, 
         c_loss_MtC = c_loss_mgc / 1e6,
         c_loss_pct = c_loss_MtC / c_2000_MtC * 100, 
         c_dens_2000_tCha = carbon_2000_mgc / forest_2000_ha,
         c_dens_loss_tCha = c_loss_mgc / forest_2000_ha,
         c_dens_loss_pct = c_dens_loss_tCha / c_dens_2000_tCha * 100,
         forest_loss_pct = forest_loss_ha / forest_2000_ha * 100
         ) %>% 
  select(any_of(site_div_var), 
         area_ha,
         forest_2000_ha, 
         forest_loss_ha,
         forest_loss_pct,
         c_2000_MtC,
         c_loss_MtC, 
         c_loss_pct,
         c_dens_2000_tCha, 
         c_dens_loss_tCha, 
         c_dens_loss_pct) %>% 
  arrange(desc(c_loss_MtC))

# Get total areas ----
area_totals <- df_out %>% 
  select(name, area_ha, forest_2000_ha, forest_loss_ha) %>% 
  mutate(approx_forest_2020_ha = forest_2000_ha - forest_loss_ha) %>% 
  filter(name == 'Total')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot time series ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tidy 
df_total <- df_sums %>% 
  filter(.data[[site_div_var]] == 'Total')
df_site <- tidy_forest_loss(df_total) %>% 
  filter(!is.na(year))
(div_names <- df_total[[site_div_var]])

df_site_t <- df_site
div_name <- div_names[[1]]
df_zone <- filter(df_site_t, .data[[site_div_var]] == div_name)

yrvec <- min(df_zone$year):max(df_zone$year)
labels <- str_c(str_sub(yrvec-1, 3,4), str_sub(yrvec, 3,4), sep = '-')

val_var <- 'forest_loss_ha'
y_name <- str_c('Forest loss (ha)')

params$dates

# Plot
p <- df_zone %>% 
  mutate(year = year %>% str_c('0101') %>% as_date()) %>% 
  ggplot(aes(x = year, y = .data[[val_var]])) +
  geom_point(size = .3, color = 'grey40') +
  geom_line(color = 'grey40', size = 1) + 
  # geom_vline(mapping = aes(x = year), data = params$dates) + 
  geom_vline(xintercept = hih_start[[1]], color = 'dodgerblue3', size = .6) + 
  geom_text(aes(x = hih_start[[1]], y = 120, label = 'HIH activity starts'), 
            nudge_x = -100, show.legend = FALSE, 
            color = 'dodgerblue4', hjust = 1, 
            family = 'Helvetica') + 
  # geom_line(data = pw_fit, aes(x = x, y = y), color = 'firebrick3', size = .6) +
  # scale_x_continuous(name = "Year",
  #                    breaks = 2001:2020,
  #                    expand = c(0.01, 0.01),
  #                    labels = labels) +
  scale_x_date(name = "Year", expand = c(0.01, 0.01)) +
  scale_y_continuous(name = y_name,
                     labels = scales::comma
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = 'Helvetica'), # Times
    axis.text = element_text(family = 'Helvetica'),
    # axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    # axis.title.x = element_blank(),
    axis.title.y = element_text(angle = 0, hjust = 1),
    panel.grid.minor = element_blank()) 

p

params <- get_params(length(div_names))
ggsave(here::here('outputs', site, str_c(site_code, '_fcloss20yr_hih.png')), 
       plot = p,
       width = 4,
       height = 2.4
       # width = params$png_width,
       # height = params$png_height
       )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create piecewise regression plots ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tidy 
df_total <- df_sums %>% 
  filter(.data[[site_div_var]] == 'Total')
df_site <- tidy_forest_loss(df_total) %>% 
  filter(!is.na(year))
(div_names <- df_total[[site_div_var]])

plots <- list()
for (i in 1:length(div_names)) {
  div_name <-  div_names[[i]]
  print('')
  print(paste0(i, ': ', div_name))
  
  df_zone <- filter(df_site_t, .data[[site_div_var]] == div_name)
  
  try(rm(pw_fit))
  pw_fit <- get_piecewise_line(df_zone)
  p <- plot_pw_fit(df_zone, div_name, pw_fit)
  try(rm(pw_fit))
  plots[[i]] <- p
  
}

params <- get_params(length(div_names))
y_max <- max(df_site_t$c_loss_flex)
y_maxr <- signif(y_max, 2) 
y_min <- round(y_maxr - y_max) * -2
formatted_plots <- layout_plots(plots, params, y_lim = c(y_min, y_maxr))

ggsave(file.path('outputs', site, str_c(site_code, '_zones_2001_2020_pw.png')), 
       plot = formatted_plots,
       width = params$png_width,
       height = params$png_height)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Total piecewise regression plot ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_tot <- df_site %>% 
  group_by(year) %>% 
  select(any_of(c('carbon_loss_MgC', 'forest_loss_ha'))) %>% 
  summarize(across(where(is.double), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup()

if (round(max(df_tot$carbon_loss_MgC)/1e6) > 1) {
  df_tot_t <- df_site %>% 
    mutate(c_loss_flex = carbon_loss_MgC / 1e6)
}

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
df_sf_sums %>% 
  st_write(here::here('outputs', site, 'counties_carbon_loss.gpkg'),
           append = FALSE)

tmap_mode('plot')
p1 <- tm_shape(df_sf_sums) + 
  tm_polygons(col = 'c_loss_MtC', palette = 'Reds', title = 'Carbon stock loss (MtC)')
p2 <- tm_shape(df_sf_sums) + 
  tm_polygons(col = 'c_loss_pct', palette = 'Reds', title = 'Carbon stock loss (%)')

# 