library(tmap)
tmap_mode('view')
library(sf)
library(patchwork)
library(segmented)
library(tidyverse)

# Function ----
tidy_forest_loss_df <- function(df) {
  # Get DF with annual carbon loss
  df_carbon <- df %>% 
    st_drop_geometry() %>% 
    dplyr::select(starts_with('carbon_loss'), name) %>% 
    pivot_longer(-name, 
                 names_to = 'year',
                 values_to = 'carbon_loss_MgC') %>% 
    mutate(year = str_c(str_extract(year, '\\d{4}'), '-01-01') %>% 
             lubridate::year(), )
  
  # Get DF with annual forest area loss
  df_area <- df %>% 
    st_drop_geometry() %>% 
    dplyr::select(starts_with('forest'), name) %>% 
    pivot_longer(-name, 
                 names_to = 'year',
                 values_to = 'forest_loss_ha') %>% 
    mutate(year = str_c(str_extract(year, '\\d{4}'), '-01-01') %>% 
             lubridate::year())
  
  # Join forest carbon and area loss values
  df_join <- df_carbon %>% 
    left_join(df_area)
  
  # Get carbon stock in 2000
  mgc_2000 <- df %>% dplyr::select(name, sum)
  
  # Get area of each polygon
  mgc_2000$area_ha  <- mgc_2000 %>% 
    st_transform(23836) %>% 
    st_area() %>% 
    units::set_units('ha') %>% 
    units::set_units(NULL)
  
  # Get percent of carbon lost annually and percent of the region deforested 
  df_tidy <- df_join %>% 
    left_join(st_drop_geometry(mgc_2000)) %>% 
    mutate(carbon_loss_pct = carbon_loss_MgC / sum,
           forest_loss_pct = forest_loss_ha / area_ha)
  
  # Return
  return(df_tidy)
}

get_piecewise_line <- function(df_zone) {
  
  # Get linear regression
  # df_for_lm <- data.frame(x = df_zone$year, y = df_zone$carbon_loss_MgC)
  out.lm <- lm(carbon_loss_MgC ~ year, data = df_zone)
  dat2 = data.frame(x = df_zone$year, y = out.lm$fitted.values)
  
  set.seed(12)
  
  # BIC-based selection - throws error when 0 breakpoints are found
  os <- try(selgmented(out.lm, Kmax=3, type="bic"), silent = TRUE)
  
  if(any(class(os) == 'try-error')) {
    print('**** BIC-based selection threw error so running Score-based. ****')
    # Score-based breakpoint selection - returns 0 breakpoints without error
    os <- selgmented(out.lm)  
    npsi <- os$selection.psi$npsi
    
    # Use linear regression if there are no breakpoints
    if( npsi > 0 ) {
      print('**** Getting line from score-based breakpoints. ****')
      dat2 = data.frame(x = df_zone$year, y = os$fitted.values)
    } 
    
  } else {
    print('**** Getting line from BIC-based breakpoints. ****')
    # Get piecewise trend from os BIC results
    npsi <- nrow(os$psi)
    # dat2 = data.frame(x = df_zone$year, y = broken.line(os)$fit)
    dat2 = data.frame(x = df_zone$year, y = os$fitted.values)
  }
  
  # Use linear regression if there are no breakpoints
  if( npsi == 0 ) {
    print('**** Using linear regression because no breakpoints selected. ****')
    dat2 = data.frame(x = df_zone$year, y = out.lm$fitted.values)
  } 
  
  return(dat2)
}

plot_pw_fit <- function(df_zone, div_name, pw_fit, y_name = 'AGC loss (metric tons C)') {
  
  # Plot
  p <- df_zone %>% 
    ggplot(aes(x = year, y = carbon_loss_MgC)) +
    geom_point(size = .3, color = 'grey30') +
    geom_line(color = 'grey30', size = .5) + 
    geom_line(data = pw_fit, aes(x = x, y = y), color = 'steelblue3', size = .5) +
    scale_x_continuous(name = "Year",
                       breaks = 2001:2020,
                       expand = c(0.01, 0.01)) +
    scale_y_continuous(name = y_name,
                       labels = scales::comma) +
    labs(title = div_name) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      panel.grid.minor = element_blank()) 
  
  # p
}

cr_hansen_dir <- '/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/carbon_reports/2001_2020_withHansen'


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Terra do Meio ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site <- 'Terra do Meio'
site_code <- 'TdM'
df_tdm <- st_read(here::here('data/gee_exports/TdM_annual_deforestation.geojson')) %>% 
  rename(zone = nombre)

# Tidy
df_tidy <- tidy_forest_loss_df(df_tdm)

# TI sites
(div_names <- df_tidy %>% 
    filter(str_detect(zone, '^TI')) %>%
    distinct(zone) %>% deframe())

div_name <- div_names[[1]]
df_zone <- df_tidy %>% filter(zone == div_name)
pw_fit <- df_zone %>% get_piecewise_line()
p1 <- plot_pw_fit(df_zone, div_name, pw_fit)
  
plots <- list()
for (i in 1:length(div_names)) {
  div_name <-  div_names[[i]]
  df_zone <- df_tidy %>% filter(zone == div_name)
  pw_fit <- df_zone %>% get_piecewise_line()
  p <- plot_pw_fit(df_zone, div_name, pw_fit)
  plots[[i]] <- p
}

(tdm_ti_plots <- plots %>% wrap_plots(ncol = 2))
ggsave(file.path(cr_hansen_dir, str_c(site_code, '_TI_2001_2020_piecewise.png')), 
       plot = tdm_ti_plots,
       width = 9, height = 12)

# Non-TI sites
(div_names <- df_tidy %>% 
    filter(str_detect(zone, '^TI', negate = TRUE)) %>%
    distinct(zone) %>% deframe())

plots_noTI <- list()
for (i in 1:length(div_names)) {
  div_name <-  div_names[[i]]
  df_zone <- df_tidy %>% filter(zone == div_name)
  pw_fit <- df_zone %>% get_piecewise_line()
  p <- plot_pw_fit(df_zone, div_name, pw_fit)
  plots_noTI[[i]] <- p
}

(tdm_noTI_plots <- plots_noTI %>% wrap_plots(ncol = 2))
ggsave(file.path(cr_hansen_dir, str_c(site_code, '_noTI_2001_2020_piecewise.png')), 
       plot = tdm_noTI_plots,
       width = 9, height = 9)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BBBR ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site <- 'Bukit Baka Bukit Raya National Park'
site_code <- 'BBBR'
df_bbbr <- st_read(here::here('data/gee_exports/BBBR_annual_deforestation.geojson')) %>% 
  filter(zone != '')

# Tidy
df_tidy <- tidy_forest_loss_df(df_bbbr)

# Sites
(div_names <- df_tidy %>% 
    distinct(zone) %>% deframe())

div_name <- div_names[[1]]
df_zone <- df_tidy %>% filter(zone == div_name)
pw_fit <- df_zone %>% get_piecewise_line()
p1 <- plot_pw_fit(df_zone, div_name, pw_fit)

plots_bbbr <- list()
for (i in 1:length(div_names)) {
  div_name <-  div_names[[i]]
  df_zone <- df_tidy %>% filter(zone == div_name)
  pw_fit <- df_zone %>% get_piecewise_line()
  p <- plot_pw_fit(df_zone, div_name, pw_fit)
  plots_bbbr[[i]] <- p
}

(bbbr_plots <- plots_bbbr %>% wrap_plots(ncol = 2))
ggsave(file.path(cr_hansen_dir, str_c(site_code, '_2001_2020_piecewise.png')), 
       plot = bbbr_plots,
       width = 9, height = 6)

# TEST BFAST -----
# (df_ts <- df_zone %>% 
#     pull(carbon_loss_MgC) %>% 
#     ts(frequency = 20) %>% 
#     bfast::bfast(h = 0.15, season = 'none'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GPNP ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site <- 'Gunung Palung'
site_code <- 'GPNP'
df_gpnp <- st_read(here::here('data/gee_exports/GPNP_annual_deforestation.geojson')) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(zone = site)

# Tidy
df_tidy <- tidy_forest_loss_df(df_gpnp)

# Sites
(div_names <- df_tidy %>% 
    distinct(zone) %>% deframe())

div_name <- div_names[[1]]
df_zone <- df_tidy %>% filter(zone == div_name)
pw_fit <- df_zone %>% get_piecewise_line()
p1 <- plot_pw_fit(df_zone, div_name, pw_fit)

(gpnp_plots <- p1 %>% wrap_plots(ncol = 1))
ggsave(file.path(cr_hansen_dir, str_c(site_code, '_2001_2020_piecewise.png')), 
       plot = gpnp_plots,
       width = 5, height = 3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Manombo ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site <- 'Manombo'
df_manombo <- st_read(here::here('data/gee_exports/Manombo_annual_deforestation.geojson')) %>% 
  # summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(zone = c('Parcel 2', 'Parcel 1'))

# Tidy
df_tidy <- tidy_forest_loss_df(df_manombo)

# Sites
(div_names <- df_tidy %>% 
    distinct(zone) %>% deframe())

plots_man <- list()
for (i in 1:length(div_names)) {
  div_name <-  div_names[[i]]
  df_zone <- df_tidy %>% filter(zone == div_name)
  pw_fit <- df_zone %>% get_piecewise_line()
  p <- plot_pw_fit(df_zone, div_name, pw_fit)
  plots_man[[i]] <- p
}

(manombo_plots <- plots_man %>% wrap_plots(ncol = 2))
ggsave(file.path(cr_hansen_dir, str_c(site, '_2001_2020_piecewise.png')), 
       width = 9, height = 3)

