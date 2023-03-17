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
library(sf)

# Functions ----
get_c_for_poly <- function(sp.poly, agb.mgha, px.ha) {
  # clip stars proxy to polygon
  rr.cm <- st_crop(rr, sp.poly)
  
  # extract values
  ar <- rr.cm |> 
    stars::st_as_stars() |> 
    pull() |> 
    as.vector() |> 
    na.omit()
  
  ar |> hist()
  
  # Extract values and get histogram
  ar <- rr[sp.poly] |> 
    stars::st_as_stars() |> 
    pull() |> 
    as.vector() |> 
    na.omit()
  
  ar |> 
    hist()
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
  
  # Read stars (proxy)
  rr <- stars::read_stars(params$rast_fp)
  
  # Load and reproject polygons
  sp.f <- st_read(polys.file) |> st_transform(st_crs(rr))
  
  # # Process each subdiv individually
  # divs <- sp.f[c(var0, var1)] %>% unique()
  # divs$id <- seq_len(nrow(divs))
  
  # dflist <- foreach(i = 1:nrow(divs)) %dopar% {
  i <- 3
  df <- seq_len(nrow(sp.f)) |> purrr::map_dfr(function(i) {
    sp.poly <- sp.f[i,]
    
    # Extract values
    ar <- rr[sp.poly] |> 
      stars::st_as_stars() |> 
      pull() |> 
      as.vector() |> 
      na.omit() |> 
      as_tibble() |> 
      mutate(name = sp.poly$name)
  })
    
    
  library(ggplot2)
  
  df_p <- df |> 
    mutate(value = value * 0.01, 
           name = factor(name, levels = c("Leakage area 4", "Leakage area 3",
                                          "Leakage area 2", "Leakage area 1", 
                                          "Project area", "Feijó", "Acre")) ) 
  
  df_p |> 
    ggplot(aes(x=value, y = name, fill = stat(x))) +
    geom_density_ridges_gradient(rel_min_height = 0.0005, show.legend = F) +
    scale_fill_viridis_c(name = "Value", option = "C", limits = c(0,100)) +
    scale_x_continuous(limits = c(0,100)) + 
    scale_y_discrete(expand = expansion(mult = c(0, .05))) + 
    theme_ridges() +
    theme(axis.title = element_blank()) +
    ggtitle("Composite Index")
  
  df_p |> 
    ggplot(aes(x=value, y = name)) +
    stat_density_ridges(quantile_lines = TRUE, 
                        alpha = 0.75,
                        quantiles = c(0.05, 0.5, 0.95),
                        rel_min_height = 0.0005) +
    # scale_fill_viridis_d(name = "Quantile", option = "C") +
    theme_ridges()
  
  df_p |> 
    ggplot(aes(value, y = name, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
    scale_fill_gradient(low = "white", high = "#87CEFF",
                        name = "Tail prob.") +
    theme_ridges()
  
  df_p |> 
    ggplot(aes(x=value, y = ..density..)) +
    geom_histogram() +
    facet_wrap('name')
    
  
  library(ggridges)
  
  # }
    
  
  
  sites_df <- sp %>% as_tibble() %>% 
    distinct(country, site_id, site_code, site_name, div_id, div_name, rx_id)
  
  # Condense to single dataframe and return
  df <- bind_rows(dflist) %>% 
    left_join(divs, by = 'id') %>% 
    left_join(sites_df, by = c(var0, var1)) %>% 
    select(-id)
  
  return(df)
# }