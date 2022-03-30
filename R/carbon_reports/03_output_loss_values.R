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
final_polys_dir <- '/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/data/hih_sites'
export_path <- '/Volumes/GoogleDrive/My Drive/Earth Engine Exports'
final_polys_dir <- '~/Downloads/hih_sites'
export_path <- here::here('data/gee_exports')

shps <- list.files(final_polys_dir, 'shp$', full.names = TRUE)
(polys_fp <- shps[[9]])

task_name <- tools::file_path_sans_ext(basename(polys_fp))
out_fp <- file.path(export_path, str_c(task_name, '.geojson'))

site_name_var <- 'HIH_site' # Estonia: 'name'
site_div_var <- 'name' # Estonia: 'div1'
div_column_name <- 'Zone'
  
# Run GEE process
# source('R/carbon_reports/01_calculate_loss_with_rgee.R')

# Load functions 
source('R/carbon_reports/02_functions.R')

# Site parameters ----
# # BBBR
# params <- list(
#   hih_site = 'BBBR',
#   shp = "/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/data/hih_sites/BBBR_dissolved_v3.shp",
#   dates = tibble(year = as_date(c(ym('2017-01'), ym('2018-09'), ym('2019-06'),
#                                   ym('2021-01'))),
#                  event = c('Radical Listening', 'Healthcare',
#                            'Alternative Livelihoods',
#                            'Reciprocity Agreements / Incentive System'),
#                  label = c('RL', 'HC', 'AL', 'RA/IS'),
#                  y_pos = c(98, 98, 98, 98))
# )

# # GPNP
# params <- list(
#   hih_site = 'GPNP',
#   shp = "/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/data/hih_sites/GPNP_dissolved.shp",
#   dates = tibble(year = as_date(c(ym('2006-01'), ym('2007-06'), ym('2008-01'),
#                                   ym('2008-09'), ym('2009-11'), ym('2017-01'))),
#                  event = c('Radical Listening', 'Healthcare',
#                            'Incentive System', 'Alternative Livelihoods',
#                            'Reforestation', 'Chainsaw Buyback'),
#                  label = c('RL', 'HC', 'IS', 'AL', 'RF', 'CB'),
#                  y_pos = c(620, 580, 620,
#                            580, 620, 580))
# )

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
df_sf <- st_read(out_fp)

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
df_total <- df_sums %>% slice(3) #slice_tail()
df_total <- df_sums %>% slice_tail() #GPNP, TdM, Manombo
df_site <- tidy_forest_loss(df_total) %>% 
  filter(!is.na(year))
(div_names <- df_total[[site_div_var]])

df_site_t <- df_site
div_name <- div_names[[1]]
df_zone <- df_site_t %>% 
  mutate(year = year %>% str_c('0101') %>% as_date(),
         # Get forest area for each year based on loss
         fc_loss_post00_ha = cumsum(forest_loss_ha),
         fc_ha = forest_2000_ha - cumsum(forest_loss_ha))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create piecewise regression plots ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_pw_line_fc <- function(df_zone) {
  
  # Get linear regression
  out.lm <- lm(forest_loss_ha ~ year, data = df_zone)
  dat2 = data.frame(x = df_zone$year, y = out.lm$fitted.values)
  
  # BIC-based selection - throws error when 0 breakpoints are found
  try(dev.off)
  try(rm(os_bic))
  set.seed(1)
  os_bic <- try(selgmented(out.lm, Kmax=4, type="bic", 
                           return.fit = TRUE, msg = FALSE))
  
  if(any(class(os_bic) == 'try-error')) {
    print('**** BIC-based selection threw error so running Score-based. ****')
    
    # Score-based breakpoint selection - returns 0 breakpoints without error
    set.seed(1)
    os <- selgmented(out.lm)  
    
    # Use selgmented fit if there are breakpoints
    if( os$selection.psi$npsi > 0 ) {
      print('**** Getting line from score-based breakpoints. ****')
      dat2 = data.frame(x = df_zone$year, y = os$fitted.values)
    } 
    
  } else {
    print('**** Getting line from BIC-based breakpoints. ****')
    # Get piecewise trend from os_bic BIC results
    # npsi <- nrow(os_bic$psi)
    # dat2 = data.frame(x = df_zone$year, y = broken.line(os_bic)$fit)
    dat2 = data.frame(x = df_zone$year, y = os_bic$fitted.values)
  }
  
  pw_fit <- dat2
  return(pw_fit)
}

pw_fit <- get_pw_line_fc(df_zone)

# get DF of HIH start dates for vertical lines
hih_pts <- params$dates %>% slice(2)
hih_start <- hih_pts$year 

df_zone %>% select(area_ha, forest_2000_ha) %>% slice(1)
df_zone %>% select(year, fc_ha) %>% 
  filter(str_sub(year, 1, 4) == str_sub(hih_start, 1, 4) |
         str_sub(year, 1, 4) == '2020')

df_zone2 <- df_zone %>% 
  left_join(rename(pw_fit, year = 'x', pw_fit = 'y'))

# Save as CSV ----
df_zone2 %>% 
  select(year, fc_loss_ha = forest_loss_ha, pw_fit) %>% 
  mutate(year = year %>% str_sub(1,4)) %>% 
  write_csv(here::here('outputs', 'forest_cover', site, 
                       str_c(site_code, '_FCloss_20yr_pw.csv')))

# Plot ----
val_var <- 'forest_loss_ha'
y_name <- str_c('Forest loss (ha)')# from previous year
ymax <- max(df_zone[[val_var]])

p <- df_zone2 %>% 
  ggplot(aes(x = year, y = .data[[val_var]])) +
  geom_line(color = 'grey80', size = 1) + 
  # Add vertical line/s
  geom_linerange(aes(x = year, ymin = -Inf, ymax = Inf), 
                 data = hih_pts, color = 'grey30', size = .6,
                 linetype = 'dashed',
                 inherit.aes = FALSE) + 
  # geom_point(data = hih_0pts, aes(x = year, y = y), size = .6, color = 'grey10') +
  # geom_vline(xintercept = hih_start[[1]], color = 'grey10', size = .6) + 
  geom_text(aes(x = hih_start[[1]]), label = 'HIH activity starts',
            y = ymax*1.02, hjust = 1, nudge_x = -100,
            show.legend = FALSE, 
            color = 'grey30', family = 'Helvetica') +
  # geom_text(aes(x = year, label = label, y = y_pos), 
  #           data = hih_pts,
  #           hjust = 0, nudge_x = 20, 
  #           show.legend = FALSE, 
  #           color = 'grey30', family = 'Helvetica', size = 2) + 
  geom_line(aes(y = pw_fit), color = 'firebrick3', size = 1.2) +
  scale_x_date(name = "Year", expand = c(0.01, 0.01), breaks = '3 years', 
               labels = function(x) year(x)) +
  scale_y_continuous(name = y_name,
                     labels = function(x) str_c(format(x, big.mark = ','), ' ha'), 
                     limits = c(0, ymax*1.1), 
                     expand = c(0, 0.1)
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = 'Helvetica'), # Times
    axis.text = element_text(family = 'Helvetica', color = 'grey30'),
    axis.title = element_text(family = 'Helvetica', color = 'grey30'),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank()) 
p

# Save as PNG ----
ggsave(here::here('outputs', 'forest_cover', site, 
                  str_c(site_code, '_fcloss20yr_hih_pw.png')), 
       plot = p,
       width = 4.5,
       height = 2.9,
       dpi = 150)

