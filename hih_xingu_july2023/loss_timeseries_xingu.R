#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Calculate annual forest and carbon losses for input polygons from 30 m data
# Requires:
#     * site polygon
#     * Hansen tree cover and lossyear
#     * CSV of annual carbon change from 500 m data
# Author:
#     * esturdivant@woodwellclimate.org, 2023-07
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here::here('R/carbon_report_utils.R'))

library(tmap)
tmap_mode('view')

home_dir <- here::here('hih_xingu_july2023')
out_dir <- here::here(home_dir, 'outputs')
dir.create(out_dir, recursive=TRUE)

site_poly_dir <- here::here(home_dir, 'data')
fp_polys <- here::here(site_poly_dir, 'xingu_request_july2023.geojson')

# Create VRTs ----
# 30m AGB
agb_dir <- '/Volumes/ejs_storage/data/raw_data/biomass/global_30m_year2000_v5'
interp_tiles <- list.files(agb_dir, '*\\.tif', full.names = TRUE)
agb30m_vrt <- here::here(agb_dir, 'alltiles.vrt')
gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, agb30m_vrt, overwrite=T) # dryrun=TRUE

# Hansen 2000 tree cover
hansen_dir <- '/Volumes/ejs_storage/data/raw_data/Hansen_etal_2013/v1.10'
interp_tiles <- list.files(hansen_dir, '*\\.tif', full.names = TRUE) %>% 
  str_subset('treecover2000')
tc2000_vrt <- here::here(hansen_dir, 'alltiles_treecover.vrt')
gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, tc2000_vrt, overwrite=T)

# Hansen loss year
interp_tiles <- list.files(hansen_dir, '*\\.tif', full.names = TRUE) %>% 
  str_subset('lossyear')
hansen_vrt <- here::here(hansen_dir, 'alltiles_lossyear.vrt')
gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, hansen_vrt, overwrite=T)

# 500m AGB
agb_dir <- '/Volumes/ejs_storage/data/raw_data/biomass/agb500m_y03_20'
interp_tiles <- list.files(agb_dir, '*\\.tif', full.names = TRUE)
agb500m_vrt <- here::here(agb_dir, 'alltiles.vrt')
gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, agb500m_vrt, overwrite=T) # dryrun=TRUE

# Prep df ----
params <-
  list(
    filter = c(country = 'Brazil'),
    polys = fp_polys,
    site_var = 'site_name',
    subdiv_var='div_name',
    years = c(2003, 2020),
    single_site = FALSE,
    agb30_fp = agb30m_vrt, 
    agb500_fp = agb500m_vrt, 
    tc2000_fp = tc2000_vrt,
    lossyear_fp = hansen_vrt
  )

overwrite <- FALSE
loss_csv <- here::here(out_dir, 'loss_30m_Hansen_v1.10.csv')

# Get zonal sums from 30m data----
if( !file.exists(loss_csv) | overwrite ){
  loss_df <- extract_zonal_sums_30m(params, out_csv=loss_csv)
}
loss_df <- readr::read_csv(loss_csv)

## Plot ---- 
figwidth = 7
figheight = 6

# Percents
loss_df %>% 
  select(site, Year, contains('pct')) %>% 
  pivot_longer(starts_with('loss'), names_to = 'var_name', 
               names_prefix='loss_pct_') %>% 
  mutate(var_name = var_name %>% 
           factor(levels=c('area','carbon','fc_area'))) %>% 
  ggplot(aes(x=Year, y=value, color=var_name)) +
  geom_line() +
  scale_color_discrete(name = '') +
  scale_y_continuous(labels=percent, name='Loss') +
  scale_x_continuous(labels=label_yearintervals, 
                     n.breaks=6, name='') + 
  facet_wrap(vars(site), scales='fixed') +
  theme_bw() 

fp <- here::here(out_dir, paste0('plots_30m_loss_pct.png'))
ggsave(fp, width=figwidth, height=figheight)

# Carbon and area and tree cover area lost
loss_df %>% 
  select(-contains('pct')) %>% 
  pivot_longer(starts_with('loss')) %>% 
  ggplot(aes(x=Year, y=value, fill=name)) +
  geom_bar(stat='identity', show.legend = FALSE) +
  scale_y_continuous(labels=scales::comma, name='') + 
  scale_x_continuous(labels=label_yearintervals, name='') + 
  facet_grid(vars(name), vars(site), scales='free') +
  theme_bw() 

# Carbon lost
loss_df %>% 
  mutate(loss_mgc = loss_mgc * -1) %>% 
  ggplot(aes(x=Year, y=loss_mgc)) +
  geom_bar(stat='identity', alpha = 0.7, fill='#F5A011', show.legend=FALSE) + 
  scale_y_continuous(labels=label_number(scale_cut=cut_short_scale()), name='Carbon stock lost (tC)') + 
  scale_x_continuous(labels=label_yearintervals, n.breaks=6, name='') + 
  facet_grid(rows=vars(site), scales='free_y') +
  theme_bw() 

# Percent carbon lost
p <- loss_df %>% 
  mutate(loss_pct_carbon = loss_pct_carbon * -1) %>% 
  ggplot(aes(x=Year, y=loss_pct_carbon)) +
  geom_line(color='#F5A011', show.legend = FALSE) +
  geom_area(alpha = 0.7, fill='#F5A011', show.legend=FALSE) +
  scale_y_continuous(labels=scales::percent, name='Carbon stock lost (% of stock in 2000)') + 
  scale_x_continuous(labels=label_yearintervals, name='') + 
  facet_wrap(vars(site), scales='fixed') +
  theme_bw() 

p

fp <- here::here(out_dir, paste0('plots_30m_carbon_lost_pct.png'))
ggsave(fp, plot=p, width=figwidth, height=figheight)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create piecewise regression plots ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

y_var='loss_mgc'
x_var='Year'

div_names <- loss_df %>% distinct(site) %>% pull(site)
loss_df_inv <- loss_df %>% 
  mutate(!!y_var := .data[[y_var]] * -1)
df_pw <- div_names %>% purrr::map_dfr(
  function(x){
    df_zone <- loss_df_inv %>% filter(site == x)
    pw_fit <- get_pw_line_fc(df_zone, y_var=y_var, x_var=x_var)
    df_zone2 <- pw_fit %>% 
      right_join(df_zone, by=c(x = x_var)) %>% 
      rename(all_of(c(Year = 'x', pw_fit = 'y')))
  }
)

# Save as CSV 
df_pw %>% 
  # dplyr::select(Year, loss_mgc, pw_fit) %>% 
  # mutate(Year = Year %>% str_sub(1,4)) %>% 
  readr::write_csv(here::here(out_dir, 'piecewise_loss_mgc.csv'))

## Plot ----
val_var <- y_var
y_name <- str_c('Loss in carbon (tC)')# from previous year
ymax <- max(df_pw[[val_var]])

p <- df_pw %>% 
  ggplot(aes(x = Year, y = .data[[val_var]])) +
  geom_hline(aes(yintercept=0), color='darkgray') +
  geom_line(color = 'grey80', linewidth = 1) + 
  geom_line(aes(y = pw_fit), color = '#F5A011', linewidth = 1.2) +
  scale_x_continuous(name = "", expand = c(0.01, 0.01)) +
  scale_y_continuous(name = y_name,
                     labels = label_number(big.mark = ',', scale_cut=cut_short_scale()),
                     # limits = c(0, ymax*1.1), 
                     expand = c(0, 0.1)) +
  theme_bw() +
  facet_wrap(vars(div_name), scales='free_y')
p

# Save as PNG 
fp <- here::here(out_dir, 'piecewise_loss_mgc.png')
ggsave(fp, plot=p,
       width = figwidth,
       height = figheight)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot 30 m and 500 m data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

agb_id <- 'agb500m_y03_20'
df_csv <- here::here(home_dir, 'co_outputs', str_c(agb_id, '.csv'))
df <- readr::read_csv(df_csv)

## Prep 500m df ----
# Standardize column names and calculate density
site_df <- df %>% 
  rename(site = params$site_var, 
         div_name = params$subdiv_var) %>%
  mutate(Dens_tCha = carbon_stock_tC / area_ha, 
         Stock_MtC = carbon_stock_tC * 1e-6, 
         Area_Mha = area_ha * 1e-6
  )

# Sort subdivisions by carbon stock in last year
endyr <- site_df %>% slice_max(stock_year) %>% distinct(stock_year) %>% pull(stock_year)
div_order <- site_df %>% 
  filter(stock_year == endyr) %>% 
  arrange(desc(Dens_tCha)) %>% 
  distinct(div_name) %>% 
  pull(div_name)

## Join 30m piecewise ----
df <- site_df %>% 
  full_join(df_pw, by=c(stock_year='Year', div_name='div_name', site='site')) %>% 
  mutate(div_name = factor(div_name, levels=div_order))

## Plot ----
# Stock from 500m colored by percent net change
facet_ncol = 7
fp <- here::here(out_dir, 'annual_stock_fixed.png')
site_df %>% plot_ann_stock(fp = fp, width=figwidth+2, height=figheight+2, 
                           facet_scales='fixed', 
                           facet_ncol = facet_ncol)

plot_comp_30mpw_500m <- function(df, facet_scales='free', fp=NULL, facet_ncol=NULL, facets_on=TRUE, ...) {
  
  # labels
  cat_labels <- c(net_change_tC='Net change from\n500-m carbon', 
                  loss_tC='Loss from\n500-m carbon', 
                  loss_mgc='Loss from\n30-m tree cover', 
                  pw_fit='Piecewise trend\n(30-m)')
  
  # Plot lines 
  p <- df %>% 
    # mutate(loss_mgc = loss_mgc * -1) %>% 
    pivot_longer(any_of(c('annual_loss_tC', 'annual_net_change_tC', 'loss_mgc', 'pw_fit')), 
                 names_to='var', values_to='tC', names_prefix='annual_') %>% 
    ggplot(aes(x=stock_year, y=tC, color=var, lty=var)) +
    geom_hline(aes(yintercept=0), color='darkgray') +
    geom_line() +
    scale_color_manual(values=c(loss_tC='darkblue', 
                                net_change_tC='darkblue', 
                                loss_mgc='grey80', 
                                pw_fit='#F5A011'),
                       labels=cat_labels,
                       name='') + 
    scale_linetype_manual(values=c(loss_tC=1, 
                                   net_change_tC=2, 
                                   loss_mgc=1, 
                                   pw_fit=1),
                          labels=cat_labels,
                          name='') + 
    scale_x_continuous(labels=label_yearintervals, name='') + 
    scale_y_continuous(labels=label_number(scale_cut=cut_short_scale()), 
                       name='Carbon stock lost (tC)') +
    theme_bw() +
    theme(legend.key.size = unit(2, 'lines'))
  
  if(facets_on) {
    p <- p + 
      facet_wrap('div_name', scales=facet_scales, ncol=facet_ncol)
  }
  
  if(!is.null(fp)){
    ggsave(fp, plot=p, ...)
  }
  
  return(p)
  
}

fp <- here::here(out_dir, paste0('plots_carbon_lost_30m_v_500m_pw.png'))
plot_comp_30mpw_500m(df, fp=fp, facet_scales='free_y', width=figwidth+2, height=figheight)

# One site only ----
plot_site <- function(div) {
  df_sub <- df %>% filter(str_detect(div_name, div))
  
  p_gl <- df_sub %>% plot_gross_changes(facets_on=F)
  p_nc <- df_sub %>% plot_net_changes(facets_on=F)
  p_as <- df_sub %>% plot_ann_stock(facets_on=F)
  p_comp30 <- df_sub %>% plot_comp_30mpw_500m(facets_on=F)
  
  # Layout and save
  p <- ((p_comp30 / p_nc ) | p_as) + plot_layout(guides='collect')
  
  site_code <- df_sub %>% distinct(site_code) %>% pull(site_code)
  fp <- here::here(out_dir, paste0('plots_', site_code, '.png'))
  ggsave(fp, plot=p, width=7, height=5)
  
  return(p)
}

div_names <- site_df %>% distinct(div_name) %>% pull() %>% as.vector()
for( div in div_names ){
  print(div)
  plot_site(div)
}

# UNTESTED - Get zonal sums from 500m data ----
extract_zonal_sums_500m <- function(params, out_csv=NULL){
  
  fp_polys = params$polys
  agb_vrt = params$agb500_fp
  
  # Load and prep data ----
  # Load and reproject polygons
  pols <- st_read(fp_polys) %>% # Load from shapefile
    st_transform(st_crs('SR-ORG:6842')) %>% 
    mutate(area_ha = st_area(geometry) %>% set_units('ha') %>% drop_units())
  
  # Load, clip, and mask raster to AOI polygons\
  r_mgha <- terra::rast(agb_vrt, win=pols) %>% mask(pols)
  
  # Area of each pixel
  r_areaha <- cellSize(r_lossyr, unit='ha')
  
  # convert biomass density (Mg/ha) to carbon stock (MgC)
  r_mgc <- r_mgha * 0.5 * r_areaha; names(r_mgc) <- 'mgc_2000'
  
  # Apply the 25% threshold to get forest area in 2000
  r_fc00 <- compare(r_tc2000, 25, ">", falseNA=TRUE)
  r_areaha_fc00 <- r_areaha * r_fc00
  names(r_areaha_fc00) <- 'fc_area_2000'
  
  # Add 2000 carbon stock and area to polygons
  pols <- r_mgc %>% extract(pols, fun=sum, na.rm=TRUE, bind=TRUE)
  pols <- r_areaha_fc00 %>% extract(pols, fun=sum, na.rm=TRUE, bind=TRUE)
  
  # Create annual loss rasters ----
  # Convert loss year and 2000 carbon to annual layers of area and carbon lost
  yrvals <- r_lossyr %>% unique() %>% slice(-1) %>% pull(alltiles_lossyear)
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
  # Extract
  sums <- r_annloss %>% 
    extract(pols, fun=sum, na.rm=TRUE, bind=TRUE) %>% 
    st_as_sf() %>% 
    st_drop_geometry() %>% 
    rename(div_name=params$subdiv_var, 
           site=params$site_var)
  
  # Tidy area lost
  loss_ha <- sums %>% 
    select(div_name, area_ha, contains('a20')) %>% 
    pivot_longer(starts_with('a20'), 
                 names_to='Year', names_prefix='a', 
                 values_to='loss_ha') %>% 
    mutate(loss_pct_area = loss_ha / area_ha) %>% 
    select(-area_ha)
  
  # Tidy forest area lost
  loss_fc_ha <- sums %>% 
    select(div_name, fc_area_2000, contains('f20')) %>% 
    pivot_longer(starts_with('f20'), 
                 names_to='Year', names_prefix='f', 
                 values_to='loss_fc_ha') %>% 
    mutate(loss_pct_fc_area = loss_fc_ha / fc_area_2000) %>% 
    select(-fc_area_2000)
  
  # Tidy carbon lost and combine with area
  loss_df <- sums %>% 
    select(div_name, site, mgc_2000, contains('c20')) %>%
    pivot_longer(starts_with('c20'), 
                 names_to='Year', names_prefix='c', 
                 values_to='loss_mgc') %>% 
    mutate(loss_pct_carbon = loss_mgc / mgc_2000) %>% 
    select(-mgc_2000) %>% 
    full_join(loss_ha) %>% 
    full_join(loss_fc_ha) %>% 
    mutate(Year = as.numeric(Year))
  
  if( !is.null(out_csv) ){
    loss_df %>% readr::write_csv(out_csv)
  }
  
  return(loss_df)
}
