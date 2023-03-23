
library(tidyverse)

export_path <- '/Volumes/GoogleDrive/My Drive/Earth Engine Exports'
out_csv <- file.path(export_path, str_c('forest_loss_to_2021_telgap', '.csv'))

df <- read_csv(out_csv)
df_sums <- df %>% mutate(name = 'Total')

df %>% tbl_vars()
carbon_loss <- df %>%
  select(starts_with('carbon'))

carbon_loss %>% tbl_vars()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Initialize variables for scripts in this folder
# Requires:
#     * GEE account with access to global_AGB_2000_30m_Mgha_V4
#     * site polygon
#     * 02_report_from_Hansen_data.R for functions following the GEE section
# Author:
#     * esturdivant@woodwellclimate.org, 2022-04-01
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
final_polys_dir <- '/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/data/hih_sites'
export_path <- '/Volumes/GoogleDrive/My Drive/Earth Engine Exports'

shps <- list.files(final_polys_dir, 'shp$', full.names = TRUE)
(polys_fp <- shps[[3]])
polys_fp <- '/Users/emilysturdivant/data/hih_sites/Borneo_GunungNyiut/GnNyiut.geojson'
polys_fp <- '/Users/emilysturdivant/data/hih_sites/Borneo_GunungNaning/GnNaning_v0.geojson'
polys_fp <- '/Users/emilysturdivant/data/hih_sites/Borneo_KubuRaya/Hutan_Desa_Kubu_Raya_Intervensi_YPI.shp'
(site <- str_split(basename(polys_fp), '[_\\W]', simplify = TRUE)[,1])

task_name <- tools::file_path_sans_ext(basename(polys_fp))
out_fp <- file.path(export_path, str_c(task_name, '.geojson'))

site_name_var <- 'HIH_site' # Estonia: 'name'
site_div_var <- 'name' # Estonia: 'div1'
div_column_name <- 'Zone'

# Load functions 
source('R/carbon_reports/02_functions.R')

# Kubu Raya
params <- list(
  hih_site = 'IRPBoundary',
  site_name = 'IRP Boundary',
  shp = '~/data/misc/TelGap/TelGap_IRPBoundary.shp',
  crs = 4326,
  dates = NA
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load new sf for all sites ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data
df_sf <- st_read(out_fp)

# Get site and division names 
(site <- unique(df_sf[[site_name_var]]))
(site <- str_split(basename(out_fp), '[_\\W]', simplify = TRUE)[,1])
# (site_code <- abbreviate(site, minlength = 3))
site_code <- params$hih_site

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create output table of 20-year loss ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate totals ----

# Get area with sf
# df_sf %>%
#   st_transform(st_crs(params$crs)) %>%
#   mutate(area_ha_2 = st_area(geometry) %>% units::set_units('ha')) %>%
#   select(area_ha, area_ha_2)

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
