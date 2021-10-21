
library(sf)
library(patchwork)
library(segmented)
library(tidyverse)

# Function ----
tidy_forest_loss_df <- function(df) {
  # Improve name of sum column
  df <- df %>% rename(carbon_2000_mgc = sum)
  
  # Get DF with annual carbon loss
  df_carbon <- df %>% 
    st_drop_geometry() %>% 
    dplyr::select(!starts_with('forest')) %>%
    # dplyr::select(starts_with('carbon_loss'), HIH_site, type, name) %>% 
    pivot_longer(starts_with('carbon_loss'), 
                 names_to = 'year',
                 values_to = 'carbon_loss_MgC') %>% 
    mutate(year = str_c(str_extract(year, '\\d{4}'), '-01-01') %>% 
             lubridate::year(), )
  
  # Get DF with annual forest area loss
  df_area <- df %>% 
    st_drop_geometry() %>% 
    dplyr::select(!starts_with('carbon_loss')) %>%
    # dplyr::select(starts_with('forest'), name) %>% 
    pivot_longer(starts_with('forest'), 
                 names_to = 'year',
                 values_to = 'forest_loss_ha') %>% 
    mutate(year = str_c(str_extract(year, '\\d{4}'), '-01-01') %>% 
             lubridate::year())
  
  # Join forest carbon and area loss values
  df_join <- df_carbon %>% 
    left_join(df_area)
  
  # Get carbon stock in 2000
  mgc_2000 <- df %>% dplyr::select(name, carbon_2000_mgc)
  
  # Get area of each polygon
  mgc_2000$area_ha  <- mgc_2000 %>% 
    st_area() %>% 
    units::set_units('ha') %>% 
    units::set_units(NULL)
  
  # Get percent of carbon lost annually and percent of the region deforested 
  df_tidy <- df_join %>% 
    left_join(st_drop_geometry(mgc_2000)) %>% 
    mutate(carbon_loss_pct = carbon_loss_MgC / carbon_2000_mgc,
           forest_loss_pct = forest_loss_ha / area_ha)
  
  # Return
  return(df_tidy)
}

get_piecewise_line <- function(df_zone) {
  
  # Get linear regression
  out.lm <- lm(carbon_loss_MgC ~ year, data = df_zone)
  dat2 = data.frame(x = df_zone$year, y = out.lm$fitted.values)
  
  # BIC-based selection - throws error when 0 breakpoints are found
  try(dev.off)
  try(rm(os_bicc))
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

  return(dat2)
}

get_piecewise_line_scorebased <- function(df_zone) {
  
  # Get linear regression
  out.lm <- lm(carbon_loss_MgC ~ year, data = df_zone)
  dat2 = data.frame(x = df_zone$year, y = out.lm$fitted.values)
  
  # Score-based breakpoint selection - returns 0 breakpoints without error
  set.seed(12)
  os <- selgmented(out.lm)  
  npsi <- os$selection.psi$npsi
    
  # Use selgmented fit if there are breakpoints
  if( npsi > 0 ) {
    print('**** Getting line from score-based breakpoints. ****')
    dat2 = data.frame(x = df_zone$year, y = os$fitted.values)
  } 

  return(dat2)
}

plot_pw_fit <- function(df_zone, div_name, pw_fit, y_name = 'AGC loss (metric tons C)') {
  
  # Plot
  p <- df_zone %>% 
    ggplot(aes(x = year, y = carbon_loss_MgC)) +
    geom_point(size = .3, color = 'grey30') +
    geom_line(color = 'grey30', size = .5) + 
    geom_line(data = pw_fit, aes(x = x, y = y), color = 'firebrick3', size = .6) +
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
 
}

create_pw_plot_list <- function(div_names, df_site) {
  
  if (length(div_names) == 1) {
    df_zone <- df_site %>% filter(name == div_names)
    pw_fit <- df_zone %>% get_piecewise_line()
    p <- plot_pw_fit(df_zone, NULL, pw_fit)
    plots <- p
    return(plots)
  } 
  
  plots <- list()
  for (i in 1:length(div_names)) {
    div_name <-  div_names[[i]]
    print('')
    print('')
    print(div_name)
    df_zone <- filter(df_site, name == div_name)
    try(rm(pw_fit))
    pw_fit <- get_piecewise_line(df_zone)
    p <- plot_pw_fit(df_zone, div_name, pw_fit)
    try(rm(pw_fit))
    plots[[i]] <- p
  }
  
  return(plots)
}

get_params <- function(div_names) {
  if(length(div_names) < 4) {
    
    ncol <- 1
    png_width <- 4.5
    png_height <- length(div_names) * 2
    
  } else {
    
    ncol <- 2
    png_width <- 9
    png_height <- length(div_names)
    
  }
  
  return(list(ncol=ncol, png_width=png_width, png_height=png_height))
}

layout_plots <- function(plots, params, title = TRUE) {
  
  ps <- plots %>% 
    wrap_plots(ncol = params$ncol) +
    plot_annotation(
      title = site,
      # caption = cap_text,
      theme = theme(plot.title = element_text(size = 14))
    ) &
    theme(title = element_text(size = 10), 
          axis.title = element_blank())
  gt <- patchwork::patchworkGrob(ps)
  
  if(params$png_height < 3) {
    y_lab <- grid::textGrob("AGC loss (tC x 1000)", 
                            gp = grid::gpar(fontsize=10), 
                            rot = 90)
  } else {
    y_lab <- grid::textGrob("Aboveground carbon loss (tC x 1000)", 
                            gp = grid::gpar(fontsize=10), 
                            rot = 90)
  }
  x_lab <- grid::textGrob("Year", gp = grid::gpar(fontsize=10))
  gta <- gridExtra::grid.arrange(gt, 
                                 left = y_lab, 
                                 bottom = x_lab)
}
