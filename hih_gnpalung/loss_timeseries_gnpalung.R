#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Calculate annual forest and carbon losses for input polygons from 30 m data and compare to 500 m data
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

home_dir <- here::here('hih_gnpalung')
out_dir <- here::here(home_dir, 'outputs')
dir.create(out_dir, recursive=TRUE, showWarnings = F)

working_dir <- here::here(home_dir, 'working_data')
site_poly_dir <- here::here('~/data/hih_sites', 'Borneo_GunungPalung')
fp_polys <- here::here(site_poly_dir, 'GPNP_divided_4area.geojson')

# Create VRTs ----
# AGB
agb_dir <- '/Volumes/ejs_storage/data/raw_data/biomass/global_30m_year2000_v5'
agb_vrt <- here::here(agb_dir, 'alltiles.vrt')
interp_tiles <- list.files(agb_dir, '*\\.tif', full.names = TRUE)
gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, agb_vrt, overwrite=T) # dryrun=TRUE

# Hansen 2000 tree cover
hansen_dir <- '/Volumes/ejs_storage/data/raw_data/Hansen_etal_2013/v1.10'
tc2000_vrt <- here::here(hansen_dir, 'alltiles_treecover.vrt')
interp_tiles <- list.files(hansen_dir, '*\\.tif', full.names = TRUE) %>% 
  str_subset('treecover2000')
gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, tc2000_vrt, overwrite=T)

# Hansen loss year
hansen_vrt <- here::here(hansen_dir, 'alltiles_lossyear.vrt')
interp_tiles <- list.files(hansen_dir, '*\\.tif', full.names = TRUE) %>% 
  str_subset('lossyear')
gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, hansen_vrt, overwrite=T)

# 500m AGB
agb_dir <- '/Volumes/ejs_storage/data/raw_data/biomass/agb500m_y03_20'
interp_tiles <- list.files(agb_dir, '*\\.tif', full.names = TRUE)
agb500m_vrt <- here::here(agb_dir, 'alltiles.vrt')
gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, agb500m_vrt, overwrite=T) # dryrun=TRUE

# Processing parameters ----
params <-
  list(
    filter = c(name = 'Gunung Palung'),
    polys = fp_polys,
    site_var = 'HIH_site',
    subdiv_var='name',
    agb30_fp = agb_vrt, 
    tc2000_fp = tc2000_vrt,
    lossyear_fp = hansen_vrt,
    agb500_fp = agb500m_vrt, 
    years = c(2003, 2020),
    single_site = FALSE
  )

overwrite <- FALSE
loss_csv <- here::here(out_dir, 'loss_30m_Hansen_v1.10.csv')

# Get zonal sums from 30m data----
if( !file.exists(loss_csv) | overwrite ){
  loss_df <- extract_zonal_sums_30m(params, out_csv=loss_csv)
}

loss_df <- readr::read_csv(loss_csv) %>% 
  mutate(div_name = div_name %>% 
           str_remove_all('Gunung Palung - ') %>% 
           str_to_title())
divnames_ordered <- c('West','East','North','Center')

## Plot ---- 
loss_df <- loss_df %>% 
  mutate(div_name = factor(div_name, levels=divnames_ordered))

# Percents
loss_df %>% 
  select(div_name, Year, contains('pct')) %>% 
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
  facet_wrap(vars(div_name), scales='fixed') +
  theme_bw() 

fp <- here::here(out_dir, paste0('plots_30m_loss_pct_fixed.png'))
ggsave(fp, width=7, height=6)

# Carbon and area and tree cover area lost
loss_df %>% 
  select(-contains('pct')) %>% 
  pivot_longer(starts_with('loss')) %>% 
  ggplot(aes(x=Year, y=value, fill=name)) +
  geom_bar(stat='identity', show.legend = FALSE) +
  scale_y_continuous(labels=scales::comma, name='') + 
  scale_x_continuous(labels=label_yearintervals, name='') + 
  facet_grid(vars(name), vars(div_name), scales='free') +
  theme_bw() 

# Carbon lost
loss_df %>% 
  mutate(loss_mgc = loss_mgc * -1) %>% 
  ggplot(aes(x=Year, y=loss_mgc)) +
  geom_bar(stat='identity', alpha = 0.7, fill='#F5A011', show.legend=FALSE) + 
  scale_y_continuous(labels=label_number(scale_cut=cut_short_scale()), name='Carbon stock lost (tC)') + 
  scale_x_continuous(labels=label_yearintervals, n.breaks=6, name='') + 
  facet_grid(rows=vars(div_name), scales='fixed') +
  theme_bw() 

# Percent carbon lost
p <- loss_df %>% 
  mutate(loss_pct_carbon = loss_pct_carbon * -1) %>% 
  ggplot(aes(x=Year, y=loss_pct_carbon)) +
  geom_line(color='#F5A011', show.legend = FALSE) +
  geom_area(alpha = 0.7, fill='#F5A011', show.legend=FALSE) +
  scale_y_continuous(labels=scales::percent, name='Carbon stock lost (% of stock in 2000)') + 
  scale_x_continuous(labels=label_yearintervals, name='') + 
  facet_wrap(vars(div_name), scales='fixed') +
  theme_bw() 
p

fp <- here::here(out_dir, paste0('plots_30m_carbon_lost_pct_fixed.png'))
ggsave(fp, plot=p, width=7, height=6)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create piecewise regression plots ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

y_var='loss_mgc'
x_var='Year'

div_names <- loss_df %>% distinct(div_name) %>% pull(div_name)
loss_df_inv <- loss_df %>% 
  mutate(!!y_var := .data[[y_var]] * -1)
df_pw <- div_names %>% purrr::map_dfr(
  function(x){
    df_zone <- loss_df_inv %>% filter(div_name == x)
    pw_fit <- get_pw_line_fc(df_zone, y_var=y_var, x_var=x_var)
    df_zone2 <- pw_fit %>% 
      right_join(df_zone, by=c(x = x_var)) %>% 
      rename(all_of(c(Year = 'x', pw_fit = 'y')))
  }
)

## Save as CSV ----
pw_csv <- here::here(out_dir, 'piecewise_loss_mgc.csv')
df_pw %>% 
  # dplyr::select(Year, loss_mgc, pw_fit) %>% 
  # mutate(Year = Year %>% str_sub(1,4)) %>% 
  readr::write_csv(pw_csv)

## Read
df_pw <- read_csv(pw_csv)

## Plot ----
val_var <- y_var
y_name <- str_c('Loss in carbon (tC)')# from previous year
ymax <- max(df_pw[[val_var]])

p <- df_pw %>% 
  mutate(div_name = factor(div_name, levels=divnames_ordered)) %>% 
  ggplot(aes(x = Year, y = .data[[val_var]])) +
  geom_hline(aes(yintercept=0), color='darkgray') +
  geom_line(color = 'grey80', linewidth = 1) + 
  geom_line(aes(y = pw_fit), color = '#F5A011', linewidth = 1.2) +
  # geom_smooth(method='lm', se=FALSE, linewidth = 1.2) +
  scale_x_continuous(name = "", expand = c(0.01, 0.01)) +
  scale_y_continuous(name = y_name,
                     labels = label_number(big.mark = ',', scale_cut=cut_short_scale()),
                     # limits = c(0, ymax*1.1), 
                     expand = c(0, 0.1)) +
  theme_bw() +
  facet_wrap(vars(div_name))
p

# Save as PNG 
fp <- here::here(out_dir, 'piecewise_loss_mgc.png')
ggsave(fp, plot=p, width=7, height=6)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot 30 m and 500 m data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get zonal sums from 500m data ----
agb_id <- 'agb500m_y03_20'
agb500_csv <- here::here(out_dir, str_c(agb_id, '.csv'))

# Process
agb500_df <- extract_zonal_sums_500m(params, agb500_csv)

# Read CSV
agb500_df <- readr::read_csv(df_csv)

# Tidy DF
agb500_df <- agb500_df %>% 
  mutate(div_name = div_name %>% 
           str_remove_all('Gunung Palung - ') %>% 
           str_to_title())
divnames_ordered <- c('West','East','North','Center')

## Prep df ----
# Calculate density
site_df <- agb500_df %>% 
  mutate(Dens_tCha = carbon_stock_tC / area_ha, 
         Stock_MtC = carbon_stock_tC * 1e-6, 
         Area_Mha = area_ha * 1e-6
  )

## Join 30m piecewise ----
# Calculate percents and rename columns prior to joining
site_df <- site_df %>% 
  group_by(div_name) %>% 
  mutate(pct_gain_500 = annual_gain_tC / first(carbon_stock_tC),
         pct_loss_500 = annual_loss_tC / first(carbon_stock_tC),
         pct_netchange_500 = annual_net_change_tC / first(carbon_stock_tC)
  ) %>% 
  rename(mgc_gain_500 = annual_gain_tC, 
         mgc_loss_500 = annual_loss_tC, 
         mgc_netchange_500 = annual_net_change_tC
  ) %>% 
  ungroup()

# # Sort subdivisions by carbon stock in last year
# endyr <- site_df %>% slice_max(stock_year) %>% distinct(stock_year) %>% pull(stock_year)
# div_order <- site_df %>%
#   filter(stock_year == endyr) %>%
#   arrange(desc(Dens_tCha)) %>%
#   distinct(div_name) %>%
#   pull(div_name)
# 
# site_df <- site_df %>%
#   mutate(div_name = factor(div_name, levels=div_order))
# 
# site_df <- site_df %>%
#   group_by(div_name) %>%
#   mutate(pct_gain_500 = annual_gain_tC / first(carbon_stock_tC),
#          pct_loss_500 = annual_loss_tC / first(carbon_stock_tC),
#          pct_netchange_500 = annual_net_change_tC / first(carbon_stock_tC)
#   ) %>%
#   rename(mgc_gain_500 = annual_gain_tC,
#          mgc_loss_500 = annual_loss_tC,
#          mgc_netchange_500 = annual_net_change_tC
#   ) %>%
#   ungroup()

## Stock from 500m colored by pct net change ----
figwidth = 5
figheight = 6

# Y Fixed
facet_ncol = 3
fp <- here::here(out_dir, 'annual_stock_fixed_3divs.png')
site_df %>% 
  filter(div_name != 'Center') %>% 
  plot_ann_stock(fp = fp, width=figwidth+4, height=figheight+1, 
                 facet_scales='fixed', 
                 facet_ncol = facet_ncol)

# Y Free
facet_ncol = 2
fp <- here::here(out_dir, 'annual_stock_free_3divs.png')
site_df %>% 
  filter(div_name != 'Center') %>% 
  plot_ann_stock(fp = fp, width=figwidth+4, height=figheight+1, 
                 facet_scales='free_y', 
                 facet_ncol = facet_ncol)

## Gains and losses ----
facet_ncol = 2
fp <- here::here(out_dir, 'annual_gain_loss_fixed_pct.png')
site_df %>% 
  filter(div_name != 'Center') %>% 
  plot_gross_changes(fp = fp, width=figwidth+4, height=figheight+1, 
                     facet_scales='fixed', 
                     facet_ncol = facet_ncol,
                     use_percents=T)

facet_ncol = 2
fp <- here::here(out_dir, 'annual_gain_loss_free_mgc.png')
site_df %>% 
  plot_gross_changes(fp = fp, width=figwidth+4, height=figheight+1, 
                     facet_scales='free_y', 
                     facet_ncol = facet_ncol,
                     use_percents=F)

facet_ncol = 2
fp <- here::here(out_dir, 'annual_gain_loss_fixed_mgc_3divs.png')
site_df %>% 
  filter(div_name != 'Center') %>% 
  plot_gross_changes(fp = fp, width=figwidth+4, height=figheight+1, 
                     facet_scales='fixed', 
                     facet_ncol = facet_ncol,
                     use_percents=F)

## Net change ----
facet_ncol = 2
fp <- here::here(out_dir, 'annual_netchange_fixed_pct.png')
site_df %>% 
  filter(div_name != 'Center') %>% 
  plot_net_changes(fp = fp, width=figwidth+4, height=figheight+1, 
                   facet_scales='fixed', 
                   facet_ncol = facet_ncol,
                   use_percents=T)

facet_ncol = 2
fp <- here::here(out_dir, 'annual_netchange_free_mgc.png')
site_df %>% 
  filter(div_name != 'Center') %>% 
  plot_net_changes(fp = fp, width=figwidth+4, height=figheight+1, 
                   facet_scales='free_y', 
                   facet_ncol = facet_ncol,
                   use_percents=F)

# Rename 30m columns prior to joining
df_pw <- df_pw %>% 
  rename(
    mgc_loss_30 = loss_mgc,
    pct_loss_30 = loss_pct_carbon,
    # mgc_loss_30 = loss_30_tC,
    # pct_loss_30 = loss_30_pct,
    # pct_pw_fit = pw_fit_pct,
    mgc_pw_fit = pw_fit
  ) %>%
  mutate(mgc_loss_30 = mgc_loss_30,
         pct_loss_30 = pct_loss_30)

# Join
df <- site_df %>% 
  full_join(df_pw, by=c(stock_year='Year', div_name='div_name', site='site')) %>% 
  mutate(div_name = factor(div_name, levels=div_order))

# Plot lines 
df %>%
  filter(div_name != 'Center') %>% 
  plot_comp_30mpw_500m(facet_scales='fixed', 
                       width=figwidth+2, 
                       height=figheight,
                       facet_ncol = facet_ncol,
                       use_percents = F)

fp <- here::here(out_dir, paste0('plots_carbon_lost_30m_v_500m_pw.png'))
ggsave(fp, width=7, height=4)


# Table of stock and density in most recent year ----
library(flextable)
library(officer)

ft <- site_df %>% 
  filter(stock_year == endyr) %>%
  mutate(Stock_MtC = round(Stock_MtC, 1), 
         area_ha = round(area_ha),
         Area_Mha = round(Area_Mha, 1)) %>% 
  select(div_name, Dens_tCha, Stock_MtC, area_ha) %>% 
  arrange(desc(Dens_tCha)) %>% 
  flextable() %>% 
  mk_par(j = 'div_name', part = 'header', value = as_paragraph('')) %>% 
  mk_par(j = 'Stock_MtC', part = 'header', value = as_paragraph('Carbon stock (MtC)')) %>% 
  mk_par(j = 'Dens_tCha', part = 'header', value = as_paragraph('Carbon density (tC/ha)')) %>% 
  align(j = 'Dens_tCha', align = 'right', part = 'body') %>% 
  mk_par(j = 'area_ha', part = 'header', value = as_paragraph('Area (ha)')) %>% 
  align(j = 'area_ha', align = 'right', part = 'body') %>% 
  width(j = 'area_ha', width = 0.8, unit = 'in') %>% 
  font(fontname = 'Helvetica Neue', part = 'all') %>%
  fontsize(size = 9, part = 'all') %>%
  bold(part = 'header') %>% 
  align(part = 'header', align = 'center') %>% 
  valign(part = 'header', valign = 'bottom') %>% 
  align(j = 'div_name', align = 'left', part = 'header') %>% 
  bold(j = 'div_name') %>% 
  set_formatter(Dens_tCha=function(x) sprintf("%.01f", x)) %>%
  hline_top(border = fp_border(width = 1), part = 'all') %>% 
  hline_bottom(border = fp_border(width = 1), part = 'all') %>% 
  width(width = 1.1, unit = 'in') %>% 
  width(j = 'Dens_tCha', width = 1.5, unit = 'in')
ft

png_fp <- here::here(out_dir, paste0('table_stock_', endyr, '.png'))
save_as_image(ft, png_fp)

# All plots grouped by site/division ----
plot_site <- function(div) {
  df_sub <- df %>% filter(str_detect(div_name, div))
  
  p_gl <- df_sub %>% plot_gross_changes(facets_on=F)
  p_nc <- df_sub %>% plot_net_changes(facets_on=F)
  p_as <- df_sub %>% plot_ann_stock(facets_on=F)
  p_comp30 <- df_sub %>% plot_comp_30mpw_500m(facets_on=F)
  
  # Layout and save
  p <- ((p_comp30 / p_nc ) | p_as) + plot_layout(guides='collect')
  
  site_code <- df_sub %>% distinct(div_name) %>% 
    filter(!is.na(div_name)) %>% pull(div_name)
  fp <- here::here(out_dir, paste0('plots_', site_code, '.png'))
  ggsave(fp, plot=p, width=7, height=5)
  
  return(p)
}

div_names <- site_df %>% distinct(div_name) %>% pull() %>% as.vector()
for( div in div_names ){
  print(div)
  plot_site(div)
}
