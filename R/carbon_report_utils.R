library(segmented)
# library(dplyr)
library(tibble)
# library(ggplot2)
library(patchwork)
# library(stringr)
library(tidyverse)
library(sf)
# library(stars)
# library(ggridges)
# library(flextable)
# library(officer)
# library(janitor)
library(terra)
library(units)
library(scales)


label_yearintervals <- function(x) {
  str_glue("'{str_sub(x-1, 3)}-'{str_sub(x, 3)}")
}

abbreviate_year <- function(x){
  str_glue("'{x}") %>% str_remove_all('(?<!\\d)20')
}

extract_zonal_sums_30m <- function(params, out_csv=NULL, only_losses=TRUE){
  
  fp_polys = params$polys
  hansen_vrt = params$lossyear_fp
  tc2000_vrt = params$tc2000_fp
  agb_vrt = params$agb30_fp
  
  # Load and prep data ----
  # Load and reproject polygons
  pols <- st_read(fp_polys) %>% # Load from shapefile
    st_transform(st_crs('EPSG:4326')) %>% 
    mutate(area_ha = st_area(geometry) %>% set_units('ha') %>% drop_units())
  
  if(params$subdiv_var == params$site_var) {
    pols$div_name = pols[[params$site_var]]
    params$subdiv_var = 'div_name'
  }
  
  # Load, clip, and mask raster to AOI polygons
  r_lossyr <- terra::rast(hansen_vrt, win=pols) %>% mask(pols)
  r_tc2000 <- terra::rast(tc2000_vrt, win=pols) %>% mask(pols)
  r_mgha <- terra::rast(agb_vrt, win=pols) %>% mask(pols)
  
  # Area of each pixel
  r_areaha <- cellSize(r_lossyr, unit='ha')
  
  # convert biomass density (Mg/ha) to carbon stock (MgC)
  r_mgc <- r_mgha * 0.5 * r_areaha; names(r_mgc) <- 'mgc_2000'
  
  # Apply the 25% threshold to get forest area in 2000
  r_fc00 <- compare(r_tc2000, 25, ">", falseNA=TRUE)
  r_areaha_fc00 <- r_areaha * r_fc00
  names(r_areaha_fc00) <- 'fc_area_2000'
  
  # Create annual loss rasters ----
  # Convert loss year and 2000 carbon to annual layers of area and carbon lost
  yrvals <- r_lossyr %>% unique() %>% slice(-1) %>% pull(alltiles_lossyear)
  yrvals <- seq(1, 22, 1)
  r_annloss <- yrvals %>% 
    purrr::map(function(x){
      r_loss_ann <- compare(r_lossyr, x, "==", falseNA=TRUE)
      
      # Carbon stock
      r_stock <- r_mgc %>% mask(r_loss_ann)
      names(r_stock) <- paste0('c', 2000+x)
      
      # Total area
      r_area <- r_areaha %>% mask(r_loss_ann)
      names(r_area) <- paste0('a', 2000+x)
      
      # Forested area
      r_fc_area <- r_areaha_fc00 %>% mask(r_loss_ann)
      names(r_fc_area) <- paste0('f', 2000+x)
      
      # Return all three
      return(c(r_stock, r_area, r_fc_area))
    }) %>% 
    rast()
  
  # Sum annual losses ----

  # Add 2000 carbon stock and area to polygons
  pols <- r_mgc %>% extract(pols, fun=sum, na.rm=TRUE, bind=TRUE)
  pols <- r_areaha_fc00 %>% extract(pols, fun=sum, na.rm=TRUE, bind=TRUE)
 
  # Extract
  sums <- r_annloss %>% 
    extract(pols, fun=sum, na.rm=TRUE, bind=TRUE) %>% 
    st_as_sf() %>% 
    st_drop_geometry() %>% 
    rename(div_name=params$subdiv_var, 
           site=params$site_var) %>% 
    mutate(across(mgc_2000:last_col(), ~ replace_na(.x, 0)))
  
  # Tidy area lost
  loss_ha <- sums %>% 
    dplyr::select(div_name, area_ha, contains('a20')) %>% 
    pivot_longer(starts_with('a20'), 
                 names_to='Year', names_prefix='a', 
                 values_to='loss_ha') %>% 
    mutate(loss_pct_area = loss_ha / area_ha) #%>% 
    # dplyr::select(-area_ha)
  
  # Tidy forest area lost
  loss_fc_ha <- sums %>% 
    dplyr::select(div_name, fc_area_2000, contains('f20')) %>% 
    pivot_longer(starts_with('f20'), 
                 names_to='Year', names_prefix='f', 
                 values_to='loss_fc_ha') %>% 
    mutate(loss_pct_fc_area = loss_fc_ha / fc_area_2000)# %>% 
    # dplyr::select(-fc_area_2000)
  
  # Tidy carbon lost and combine with area
  loss_df <- sums %>% 
    dplyr::select(div_name, site, mgc_2000, contains('c20')) %>%
    pivot_longer(starts_with('c20'), 
                 names_to='Year', names_prefix='c', 
                 values_to='loss_mgc') %>% 
    mutate(loss_pct_carbon = loss_mgc / mgc_2000) %>% 
    # dplyr::select(-mgc_2000) %>% 
    full_join(loss_ha) %>% 
    full_join(loss_fc_ha) %>% 
    mutate(Year = as.numeric(Year))
  
  if(only_losses){
    loss_df %>% 
      select(-any_of(c('area_ha', 'fc_area_2000', 'mgc_2000')))
  }
  
  if( !is.null(out_csv) ){
    loss_df %>% readr::write_csv(out_csv)
  }
  
  return(loss_df)
}

extract_zonal_sums_500m <- function(params, out_csv=NULL){
  
  fp_polys = params$polys
  agb_vrt = params$agb500_fp
  
  # Set up year vectors
  years <- seq(from = params$years[1], to = params$years[2], by = 1)
  y <- seq(from = params$years[1]-1, to = params$years[2], by = 1)
  d.yrs <- paste0('d', substr(y[-length(y)], start = 3, stop = 4), '.',
                  substr(y[-1], start = 3, stop = 4))
  
  # Load and prep data ----
  # Load and reproject polygons to match MODIS
  pols <- st_read(fp_polys) %>% # Load from shapefile
    st_transform(st_crs(terra::rast(agb_vrt))) %>%
    # select(!starts_with('auxiliary')) %>%
    select(div_name=params$subdiv_var,
           site=params$site_var) %>% 
    mutate(area_ha = st_area(geometry) %>% set_units('ha') %>% drop_units())
  
  # Load, clip, and mask raster to AOI polygons\
  r_mgha <- terra::rast(agb_vrt, win=pols) %>% mask(pols)
  names(r_mgha) <- str_c('y', years)
  
  # Convert biomass density (Mg/ha) to carbon stock (MgC)
  r_mgc <- r_mgha * 0.5 * cellSize(r_mgha, unit='ha')
  
  # Annual change in carbon stock per pixel
  r_mgc_diff <- diff(r_mgc)
  
  # Annual losses and gains ----
  
  ## Gains
  r_mgc_gains <- classify(r_mgc_diff, matrix(c(-Inf, 0, 0), ncol=3, byrow=TRUE))
  p_mgc_gains <- r_mgc_gains %>% extract(pols, fun=sum, na.rm=TRUE, bind=TRUE)
  df_gains <- p_mgc_gains %>%
    st_as_sf() %>%
    st_drop_geometry() %>% 
    pivot_longer(starts_with('y'), 
                 names_to='Year', names_prefix='y', 
                 values_to='annual_gain_tC')
  
  ## Losses
  r_mgc_losses <- classify(r_mgc_diff, matrix(c(0, Inf, 0), ncol=3, byrow=TRUE))
  p_mgc_losses <- r_mgc_losses %>% extract(pols, fun=sum, na.rm=TRUE, bind=TRUE)
  df_losses <- p_mgc_losses %>%
    st_as_sf() %>%
    st_drop_geometry() %>% 
    pivot_longer(starts_with('y'), 
                 names_to='Year', names_prefix='y', 
                 values_to='annual_loss_tC')
  
  df_change <- df_gains %>% 
    full_join(df_losses) %>% 
    mutate(netchange2 = annual_gain_tC + annual_loss_tC)
  
  # Add 2000 carbon stock and area to polygons
  p_mgc <- r_mgc %>% extract(pols, fun=sum, na.rm=TRUE, bind=TRUE)
  df_stock <- p_mgc %>%
    st_as_sf() %>%
    st_drop_geometry() %>% 
    pivot_longer(starts_with('y'), 
                 names_to='Year', names_prefix='y', 
                 values_to='carbon_stock_tC') %>% 
    group_by(div_name) %>% 
    mutate(netchange1 = carbon_stock_tC - lag(carbon_stock_tC)) %>% 
    ungroup()
  
  # Compare and warn
  flag_df <- df_stock %>% 
    full_join(df_change) %>% 
    mutate(flag = netchange1 - netchange2) %>% 
    filter(!is.na(flag))
  
  if(any(abs(flag_df$flag) > 1e-8)) {
    warning(paste("Net changes aggregated to the polygon are different", 
                  "when calculated from stock vs. from gains and losses.", 
                  "Using net changes calculated from stock totals."))
  }
  
  df_out <- df_stock %>% 
    full_join(df_change) %>% 
    mutate(stock_year = as.numeric(Year),
           change_year = paste0(stock_year-1, '-', stock_year)) %>% 
    select(site, 
           div_name, 
           stock_year,
           area_ha,
           carbon_stock_tC,
           change_year,
           annual_gain_tC,
           annual_loss_tC,
           annual_net_change_tC = netchange1) %>%
    mutate(across(ends_with('tC'), ~ round(.x, 2)))
  
  if( !is.null(out_csv) ){
    df_out %>% readr::write_csv(out_csv)
  }
  
  return(df_out)
}

get_pw_line_fc <- function(df_zone, y_var='forest_area_ha', x_var='year') {
  
  # df <- df_zone %>% 
  #   rename(all_of(c(y_var=y_var,
  #                   x_var=x_var)))
  
  df = data.frame(x_var = df_zone[[x_var]], y_var = df_zone[[y_var]])
  
  # Get linear regression
  out.lm <- lm(y_var ~ x_var, data = df)
  dat2 = data.frame(x = df$x_var, y = out.lm$fitted.values)
  
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
      dat2 = data.frame(x = df$x_var, y = os$fitted.values)
    } 
    
  } else {
    print('**** Getting line from BIC-based breakpoints. ****')
    # Get piecewise trend from os_bic BIC results
    dat2 = data.frame(x = df$x_var, y = os_bic$fitted.values)
  }
  
  pw_fit <- dat2
  return(pw_fit)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_gross_changes <- function(df, 
                               facet_scales='free', 
                               fp=NULL, 
                               facet_ncol=NULL, 
                               facets_on=TRUE,
                               use_percents=FALSE, 
                               include_net_line=FALSE,
                               ...) { 
  
  # labels
  lines_order <- c('gain_500', 'loss_500')
  cat_labels <- c(gain_500='Net change in carbon\n(Woodwell 500 m)', 
                  loss_500='Carbon loss\n(Woodwell 500 m)')
  
  # Optionally plot as percents
  if(use_percents){
    
    names_prefix <- 'pct_'
    y_title <- 'Carbon stock change (%)'
    y_lab_fun <- percent
    
  } else {
    
    names_prefix <- 'mgc_'
    y_title <- 'Carbon stock change (tC)'
    y_lab_fun <- label_number(scale_cut=cut_short_scale())
    
  }
  
  # Pivot longer for plotting
  var_names <- str_c(names_prefix, names(cat_labels)) 
  df_pivot <- df %>% 
    filter( !(is.na(mgc_gain_500) & is.na(mgc_loss_500)) ) %>% 
    pivot_longer(any_of(var_names), names_to='var', values_to='value', 
                 names_prefix = names_prefix) %>% 
    mutate(var = factor(var, levels=lines_order))
  
  p <- df_pivot %>% 
    ggplot(aes(x = stock_year, y = value, color=var, fill=var)) +
    geom_line(show.legend = FALSE) +
    geom_area(alpha = 0.7, show.legend=FALSE) + 
    scale_y_continuous(label = y_lab_fun, name=y_title) +
    scale_x_continuous(labels = label_yearintervals, name=NULL,
                       minor_breaks=seq(2004, 2020, 1)) +
    scale_color_manual(name = NULL, values = c(gain_500='#30C9A0', loss_500='#F5A011')) +
    scale_fill_manual(name = NULL, values = c(gain_500='#30C9A0', loss_500='#F5A011')) +
    theme_bw() 
  
  if(include_net_line) {
    p <- p +
      geom_line(inherit.aes=FALSE, data=df, 
                aes(x=stock_year, y=.data[[str_c(names_prefix, 'netchange_500')]]),
                linetype=2,
                color='slategray'
      )
  }
  
  if(facets_on){
    p <- p +
      facet_wrap(vars(div_name),
                 scales = facet_scales,
                 ncol=facet_ncol)
  }
  
  if(!is.null(fp)){
    ggsave(fp, plot=p, ...)
  }
  
  return(p)
  
}

plot_net_changes <- function(df, 
                             facet_scales='free', 
                             fp=NULL, 
                             facet_ncol=NULL, 
                             facets_on=TRUE, 
                             use_percents=FALSE, 
                             ...) { 
  
  # labels
  lines_order <- c('netchange_500')
  cat_labels <- c(netchange_500='Net change in carbon\n(Woodwell 500 m)')
  
  # Optionally plot as percents
  if(use_percents){
    
    names_prefix <- 'pct_'
    y_title <- 'Carbon stock change (%)'
    y_lab_fun <- percent
    
  } else {
    
    names_prefix <- 'mgc_'
    y_title <- 'Carbon stock change (tC)'
    y_lab_fun <- label_number(scale_cut=cut_short_scale())
    
  }
  
  # Pivot longer for plotting
  var_names <- str_c(names_prefix, names(cat_labels)) 
  df_pivot <- df %>% 
    pivot_longer(any_of(var_names), names_to='var', values_to='value', 
                 names_prefix = names_prefix) %>% 
    mutate(var = factor(var, levels=lines_order))
  
  p <- df_pivot %>% 
    ggplot(aes(x = stock_year, y = value)) +
    geom_bar(stat = 'identity', fill='grey50') + 
    scale_y_continuous(label = y_lab_fun, name=y_title) +
    scale_x_continuous(labels=label_yearintervals, 
                       breaks=seq(2001, 2022, 6),
                       minor_breaks=seq(2001, 2023, 2),
                       name=NULL) + 
    theme_bw() 
  
  if(facets_on){
    p <- p +
      facet_wrap(vars(div_name), scales = facet_scales, ncol=facet_ncol)
  }
  
  if(!is.null(fp)){
    ggsave(fp, plot=p, ...)
  }
  
  return(p)
}

plot_ann_stock <- function(df, facet_scales='free', fp=NULL, facet_ncol=NULL, facets_on=TRUE, ...) { 
  df1 <- df %>% 
    # mutate(pct_change = mgc_netchange_500 / lag(carbon_stock_tC) ) %>% 
    mutate(pct_change = pct_netchange_500 ) %>%
    select(site, div_name, stock_year, carbon_stock_tC, pct_change) %>% 
    gather(key = 'key', value = 'value', carbon_stock_tC, 
           factor_key = T)
  
  fy <- df1 %>% slice_min(stock_year) %>% pull(stock_year)
  
  p <- df1 %>% 
    ggplot(aes(x = stock_year, y = value, fill = pct_change)) +
    geom_bar(stat = 'identity') + 
    scale_y_continuous(label = label_number(scale_cut=cut_short_scale()), 
                       expand = expansion(mult=c(0, 0.05))) +
    scale_x_continuous(label = abbreviate_year, name=NULL) +
    scale_fill_gradient2(name = str_wrap(str_glue('% change relative to {fy}'), 11),
                         low = '#F5A011', 
                         mid = 'grey90', 
                         high = '#30C9A0', 
                         na.value = 'grey80',
                         labels = label_percent(),
                         limits = c(-0.005, 0.005), 
                         oob = squish) +
    ylab('Standing stock (tC)') +
    theme_bw()
  
  if(facets_on){
    p <- p +
      facet_wrap(vars(div_name), scales = facet_scales, ncol=facet_ncol) +
      theme(strip.text = element_text(size=7))
  }
  
  if(!is.null(fp)){
    ggsave(fp, plot=p, ...)
  }
  
  return(p)
}


# Compare 30m to 500m 
plot_comp_30mpw_500m <- function(df, facet_scales='free', 
                                 fp=NULL, facet_ncol=NULL, facets_on=TRUE, 
                                 use_percents=FALSE, include_pw=TRUE, ...) {
  
  # labels
  legend_order <- c('netchange_500', 'loss_500', 'loss_30')
  lines_order <- c('loss_30', 'netchange_500', 'loss_500')
  cat_labels <- c(netchange_500='Net change in carbon\n(Woodwell 500 m)', 
                  loss_500='Carbon loss\n(Woodwell 500 m)', 
                  loss_30='Carbon loss\n(Hansen + Woodwell 30 m)')
  cat_colors=c(loss_500='darkblue', netchange_500='darkblue', 
               loss_30='#edca55') #'F5A011'
  cat_linetype=c(loss_500=1, netchange_500=2, loss_30=1)
  cat_lw=c(loss_500=0.5, netchange_500=0.5, loss_30=0.5)
  
  if(include_pw) {
    
    legend_order <- c(legend_order, 'pw_fit')
    lines_order <- c('loss_30', 'pw_fit', 'netchange_500', 'loss_500')
    cat_labels <- c(cat_labels, 
                    pw_fit='Loss trend line\n(Hansen + Woodwell 30 m)')
    cat_colors=c(cat_colors, pw_fit='#de8f07') #'F5A011'
    cat_linetype=c(cat_linetype, pw_fit=1)
    cat_lw=c(cat_lw, pw_fit=0.75)
  }
  
  # Optionally plot as percents
  if(use_percents){
    
    names_prefix <- 'pct_'
    y_title <- 'Carbon stock loss (%)'
    y_lab_fun <- percent
    
  } else {
    
    names_prefix <- 'mgc_'
    y_title <- 'Carbon stock loss (tC)'
    y_lab_fun <- label_number(scale_cut=cut_short_scale())
    
  }
  
  # Pivot longer for plotting
  var_names <- str_c(names_prefix, names(cat_labels)) 
  df_pivot <- df %>% 
    pivot_longer(any_of(var_names), names_to='var', values_to='value', 
                 names_prefix = names_prefix) %>% 
    mutate(var = factor(var, levels=lines_order))
  
  # Plot lines 
  p <- df_pivot %>% 
    ggplot(aes(x=stock_year, y=value, color=var, lty=var, linewidth=var)) +
    geom_hline(aes(yintercept=0), color='black') +
    geom_line() +
    scale_color_manual(limits=legend_order,
                       values=cat_colors,
                       labels=cat_labels,
                       name=NULL) + 
    scale_linetype_manual(limits=legend_order,
                          values=cat_linetype,
                          labels=cat_labels,
                          name=NULL) + 
    scale_linewidth_manual(limits=legend_order,
                      values=cat_lw,
                      labels=cat_labels,
                      name=NULL) + 
    scale_x_continuous(labels=label_yearintervals, 
                       breaks=seq(2001, 2022, 6),
                       minor_breaks=seq(2001, 2023, 2),
                       name=NULL) + 
    scale_y_continuous(labels=y_lab_fun, 
                       name=y_title) +
    theme_bw() +
    theme(legend.key.size = unit(1.75, 'lines'), 
          legend.key.width = unit(0.03, 'snpc'))
  
  if(facets_on) {
    p <- p + 
      facet_wrap('div_name', scales=facet_scales, ncol=facet_ncol)
  }
  
  if(!is.null(fp)){
    ggsave(fp, plot=p, ...)
  }
  
  return(p)
  
}
