#!/usr/bin/env Rscript
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Purpose:
#     * Functions for extracting raster values for input polygons
# History: 
#     * Copied from biomass_team/hih/carbon_utils.R on 3/15/2023
# Author:
#     * esturdivant@woodwellclimate.org, 2023-03
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(terra)
library(dplyr)
library(tibble)
library(reshape2)
# library(foreach)
# library(doMC)
library(ggplot2)
library(patchwork)
library(stringi)
library(stringr)
library(tidyr)

# Functions ----
get_c_for_poly <- function(sp.poly, agb.mgha, px.ha) {
  # crop/mask raster stack
  agb.mgha.c <- crop(agb.mgha, sp.poly)
  agb.mgha.cm <- mask(agb.mgha.c, sp.poly)
  
  # Area of masked pixels
  # Get count of pixels where AGB > 0
  clip_pix <- reclassify(subset(agb.mgha.c, 1), rcl = c(-Inf, Inf, 1))
  clip_mask <- mask(clip_pix, sp.poly)
  poly.px.ct <- cellStats(clip_mask, stat='sum', na.rm = TRUE)
  area_ha <- px.ha * poly.px.ct
  
  # Annual sums (stock, gain, loss, net) ----
  # Total stock and net change
  ann.stock.tc <- cellStats(agb.mgha.cm * px.ha, 'sum', na.rm = T) / 2
  ann.netc.tc.s <- diff(ann.stock.tc)
  
  # Annual losses and gains
  agb.mgha.cm.diff <- calc(agb.mgha.cm, fun = diff)
  
  ## Gains
  agb.mgha.cm.gain <- reclassify(agb.mgha.cm.diff, c(-Inf, 0, 0))
  ann.gain.tc <- cellStats(agb.mgha.cm.gain * px.ha, 'sum', na.rm = T) / 2
  ann.gain.tc <- c(d02.03 = NA, ann.gain.tc)
  
  ## Losses
  agb.mgha.cm.loss <- reclassify(agb.mgha.cm.diff, c(0, Inf, 0))
  ann.loss.tc <- cellStats(agb.mgha.cm.loss * px.ha, 'sum', na.rm = T) / 2
  ann.loss.tc <- c(d02.03 = NA, ann.loss.tc)
  
  ## Net change
  ann.netc.tc.gl <- ann.gain.tc + ann.loss.tc
  
  # Compare and warn
  sums_diff <- ann.netc.tc.s - ann.netc.tc.gl[-1]
  if(any(abs(sums_diff) > 1e-8)) {
    warning(paste("Annual net changes aggregated to the polygon are different", 
                  "when calculated from stock vs. from gains and losses.", 
                  "Using net changes calculated from stock totals."))
  }
  
  # combine into table ----
  ann.change.tc <- rbind(ann.stock.tc, ann.gain.tc, ann.loss.tc, ann.netc.tc.gl) %>% 
    t() %>% 
    as_tibble(rownames = 'year') %>%
    mutate(area_ha = area_ha,
           stock_year = as.numeric(paste0(20, substr(year, 5, 6))),
           change_year = paste0(as.numeric(paste0(20, substr(year, 2, 3))), '-', stock_year),
           across(ends_with('tc'), ~ round(.x, 2))) %>% 
    select(stock_year,
           area_ha,
           carbon_stock_tC = ann.stock.tc,
           change_year,
           annual_gain_tC = ann.gain.tc,
           annual_loss_tC = ann.loss.tc,
           annual_net_change_tC = ann.netc.tc.gl) %>%
    mutate(across(ends_with('tC'), ~ round(.x, 2)))
}

get_c_change_df <- function(params) {
  
  # Get parameters ----
  single <- ifelse(is.null(params$single_site), TRUE, FALSE)
  
  # input shapefile
  shp.file <- params$shp
  
  # input AGB 
  if(is.null(params$agb_ras)) {
    # Most recent global file
    tif.file <- '/mnt/data3/biomass/final/bio_change/2003_2018/v1/annfit/c95/f03/global.vrt'
    params$years <- c(2003, 2018)
  } else {
    tif.file <- params$agb_ras
  }
  
  # time period
  start.year <- params$years[1] # 2003
  end.year <- params$years[2] # 2017 or 2018
  years <- seq(from = start.year, to = end.year, by = 1)
  y <- seq(from = start.year-1, to = end.year, by = 1)
  d.yrs <- paste0('d', substr(y[-length(y)], start = 3, stop = 4), '.',
                  substr(y[-1], start = 3, stop = 4))
  
  # Prep raster stack ----
  # 2003-2018 (V1)
  agb.mgha <- stack(tif.file, bands = 1:length(years))
  names(agb.mgha) <- d.yrs
  
  # compute pixel area in hectares
  x.res.m <- xres(agb.mgha)
  y.res.m <- yres(agb.mgha)
  px.ha <- x.res.m * y.res.m * 1e-4
  
  # reproject shapefile to match modis
  sp <- shapefile(here::here(shp.file))
  sp.p <- spTransform(sp, crs(agb.mgha))
  subdiv_var <- grep(pattern = 'name|div|NAMA_DESA', names(sp), ignore.case = TRUE, value = TRUE) %>% 
    first()
  
  # Treat all polygons as one
  if(single | nrow(sp.p) == 1) {
    
    # Get annual gain, loss, net change, and total stock as DF
    df <- get_c_for_poly(sp.poly = sp.p, agb.mgha, px.ha, site_name = params$site) %>% 
      tibble::add_column(location = params$location, .before = 1)
    
    return(df)
  }
  
  # Process each polygon individually
  dflist <- foreach(i = 1:nrow(sp.p)) %dopar% {
    
    # Get annual gain, loss, net change, and total stock as DF
    sp.poly <- sp.p[i,]
    get_c_for_poly(sp.poly, agb.mgha, px.ha, site_name = sp.poly[[subdiv_var]])
    
  }
  
  # Condense to single dataframe and return
  df <- bind_rows(dflist) %>% 
    tibble::add_column(location = params$location, .before = 1)
  
  return(df)
}

get_ann_carb_df <- function(params) {
  
  # Get parameters ----
  var1 <- params$subdiv_var # usually div_id
  var0 <- params$site_var # usually site_id
  
  # process the input polygons as one unit or separate out?
  single <- ifelse(is.null(params$single_site), TRUE, FALSE)
  
  # input shapefile
  polys.file <- params$polys
  
  # input AGB 
  tif.file <- params$agb_ras
  
  # time period
  start.year <- params$years[1] # 2003
  end.year <- params$years[2] # 2017 or 2018
  
  # Set up year vectors ----
  years <- seq(from = start.year, to = end.year, by = 1)
  y <- seq(from = start.year-1, to = end.year, by = 1)
  d.yrs <- paste0('d', substr(y[-length(y)], start = 3, stop = 4), '.',
                  substr(y[-1], start = 3, stop = 4))
  
  # Prep raster stack ----
  # 2003-2018 (V1)
  agb.mgha <- stack(tif.file, bands = 1:length(years))
  names(agb.mgha) <- d.yrs
  
  # compute pixel area in hectares
  x.res.m <- xres(agb.mgha)
  y.res.m <- yres(agb.mgha)
  px.ha <- x.res.m * y.res.m * 1e-4
  
  # Load and filter polygons
  sp <- rgdal::readOGR(polys.file, verbose=FALSE) 
  sp.f <- sp[sp[[names(params$filter)]] == params$filter, ]
  # sp$id <- seq_len(nrow(sp))
  
  # reproject polygons to match modis
  sp.f <- spTransform(sp.f, crs(agb.mgha))
  
  # Treat all polygons as one
  if(single | nrow(sp.f) == 1) {
    
    # Get annual gain, loss, net change, and total stock as DF
    df <- get_c_for_poly(sp.poly = sp.f, agb.mgha, px.ha, site_name = params$site) %>% 
      tibble::add_column(location = params$location, .before = 1)
    
    return(df)
  }
  
  # Process each subdiv individually
  divs <- sp.f[c(var0, var1)] %>% data.frame() %>% unique()
  divs$id <- seq_len(nrow(divs))
  
  dflist <- foreach(i = 1:nrow(divs)) %dopar% {
    
    div <- divs[i,]
    
    # Filter to the given division
    sp.poly <- sp.f[sp.f[[var0]] == div[[var0]] & sp.f[[var1]] == div[[var1]], ]
    
    # Get annual gain, loss, net change, and total stock as DF
    df <- get_c_for_poly(sp.poly, agb.mgha, px.ha)
    
    df %>% mutate(id = div$id)
    
  }
  
  sites_df <- sp %>% as_tibble() %>% 
    distinct(country, site_id, site_code, site_name, div_id, div_name, rx_id)
  
  # Condense to single dataframe and return
  df <- bind_rows(dflist) %>% 
    left_join(divs, by = 'id') %>% 
    left_join(sites_df, by = c(var0, var1)) %>% 
    select(-id)
  
  return(df)
}