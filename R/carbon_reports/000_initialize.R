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
# final_polys_dir <- '/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/data/hih_sites'
# export_path <- '/Volumes/GoogleDrive/My Drive/Earth Engine Exports'
# final_polys_dir <- '~/Downloads/hih_sites'
# export_path <- here::here('data/gee_exports')
final_polys_dir <- '~/data/misc/Kepos_examplecarbonoffsetproject'
final_polys_dir <- '~/data/hih_sites/final_sites_202301'
export_path <- '~/Library/CloudStorage/GoogleDrive-esturdivant@woodwellclimate.org/My Drive/Earth Engine Exports'

# shps <- list.files(final_polys_dir, 'shp$', full.names = TRUE)
# (polys_fp <- shps[[3]])
# polys_fp <- '/Users/emilysturdivant/data/hih_sites/Borneo_GunungNyiut/GnNyiut.geojson'
# polys_fp <- '/Users/emilysturdivant/data/hih_sites/Borneo_GunungNaning/GnNaning_v0.geojson'
# polys_fp <- '/Users/emilysturdivant/data/hih_sites/Borneo_KubuRaya/Hutan_Desa_Kubu_Raya_Intervensi_YPI.shp'
polys_fp <- here::here(final_polys_dir, 'envira_polys.geojson')
polys_fp <- here::here('~/data/hih_sites/Borneo_GunungNaning/GnNaning_v0.geojson')
(site <- str_split(basename(polys_fp), '[_\\W]', simplify = TRUE)[,1])

task_name <- tools::file_path_sans_ext(basename(polys_fp))
out_fp <- file.path(export_path, str_c(task_name, '.geojson'))

site_name_var <- 'hih_site' # Estonia: 'name'
site_div_var <- 'name' # Estonia: 'div1'
div_column_name <- 'name'
# site_name_var <- 'filename' # Estonia: 'name'
# site_div_var <- 'name' # Estonia: 'div1'
# div_column_name <- 'Zone'

# Load functions 
source('R/carbon_reports/02_functions.R')
