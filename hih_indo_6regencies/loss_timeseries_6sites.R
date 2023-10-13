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

home_dir <- here::here('hih_indo_6regencies')
out_dir <- here::here(home_dir, 'outputs'); dir.create(out_dir, recursive=TRUE)

fp_polys <- '/Users/esturdivant/data/hih_sites/Indonesia_request/adm2_selected_regencies.geojson'

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
agb500m_vrt <- here::here(agb_dir, 'alltiles_agb500.vrt')
gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, agb500m_vrt, overwrite=T) # dryrun=TRUE

# Prep df ----
params <-
  list(
    filter = c(COUNTRY = 'Indonesia'),
    polys = fp_polys,
    site_var = 'NAME_1',
    subdiv_var='NAME_2',
    years = c(2003, 2020),
    single_site = FALSE,
    agb30_fp = agb30m_vrt, 
    agb500_fp = agb500m_vrt, 
    tc2000_fp = tc2000_vrt,
    lossyear_fp = hansen_vrt
  )

overwrite <- FALSE
loss_csv <- here::here(out_dir, 'loss_30m_Hansen_v1.10_expanded.csv')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get zonal sums from 500m data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
agb_id <- 'agb500m_y03_20'
agb500_csv <- here::here(out_dir, str_c(agb_id, '.csv'))

# Process
if( !file.exists(agb500_csv) | overwrite ){
  agb500_df <- extract_zonal_sums_500m(params, agb500_csv)
}

# Read CSV
agb500_df <- readr::read_csv(agb500_csv)

## Prep df ----
# Calculate density
site_df <- agb500_df %>% 
  mutate(Dens_tCha = carbon_stock_tC / area_ha, 
         Stock_MtC = carbon_stock_tC * 1e-6, 
         Area_Mha = area_ha * 1e-6
  )

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

# Sort subdivisions by carbon stock in last year
endyr <- site_df %>% slice_max(stock_year) %>% distinct(stock_year) %>% pull(stock_year)
div_order <- site_df %>%
  filter(stock_year == endyr) %>%
  arrange(desc(Dens_tCha)) %>%
  distinct(div_name) %>%
  pull(div_name)

site_df <- site_df %>%
  mutate(div_name = factor(div_name, levels=div_order))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get zonal sums from 30m data----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if( !file.exists(loss_csv) | overwrite ){
  loss_df <- extract_zonal_sums_30m(params, out_csv=loss_csv)
}

loss_df <- readr::read_csv(loss_csv) 
# divnames_ordered <- c('West','East','North','Center')

## Plot ---- 
loss_df <- loss_df %>% 
  mutate(div_name = factor(div_name, levels=div_order))

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

pw_csv <- here::here(out_dir, 'piecewise_loss_mgc.csv')

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
  mutate(div_name = factor(div_name, levels=div_order)) %>%
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

# # Get piecewise regression for percents
# y_var='loss_pct_carbon'
# div_names <- loss_df %>% distinct(div_name) %>% pull(div_name)
# loss_df_inv <- loss_df %>% 
#   mutate(!!y_var := .data[[y_var]] * -1)
# df_pw_pct <- div_names %>% purrr::map_dfr(
#   function(x){
#     df_zone <- loss_df_inv %>% filter(div_name == x)
#     pw_fit <- get_pw_line_fc(df_zone, y_var=y_var, x_var='Year')
#     pw_fit[['div_name']] <- x
#     df_zone2 <- pw_fit %>% 
#       # right_join(df_zone, by=c(x = x_var)) %>%
#       rename(all_of(c(Year = 'x', pw_fit = 'y')))
#   }
# )
# 
# df_pw <- df_pw_pct %>% 
#   full_join(df_pw_mgc, by=c('Year', 'div_name'), suffix=c('_pct', '_mgc')) %>% 
#   full_join(loss_df, by=c('Year', 'div_name'))
# 
# # Save as CSV 
# df_pw %>% 
#   readr::write_csv(here::here(out_dir, 'piecewise_loss.csv'))
# df_pw <- readr::read_csv(here::here(out_dir, 'piecewise_loss.csv'))
# 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot 500 m data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Stock from 500m colored by pct net change ----
figwidth = 7
figheight = 6
facet_ncol = 3

# Y Fixed
fp <- here::here(out_dir, 'annual_stock_fixed.png')
site_df %>% 
  plot_ann_stock(fp = fp, width=figwidth+4, height=figheight+1, 
                 facet_scales='fixed', 
                 facet_ncol = 6)

# Y Free
fp <- here::here(out_dir, 'annual_stock_free.png')
site_df %>% 
  plot_ann_stock(fp = fp, width=figwidth+4, height=figheight+1, 
                 facet_scales='free_y', 
                 facet_ncol = facet_ncol)

## Gains and losses ----
fp <- here::here(out_dir, 'annual_gain_loss_fixed_pct.png')
site_df %>% 
  plot_gross_changes(fp = fp, width=figwidth+4, height=figheight+1, 
                     facet_scales='fixed', 
                     facet_ncol = facet_ncol,
                     use_percents=T)

fp <- here::here(out_dir, 'annual_gain_loss_free_mgc.png')
site_df %>% 
  plot_gross_changes(fp = fp, width=figwidth+4, height=figheight+1, 
                     facet_scales='free_y', 
                     facet_ncol = facet_ncol,
                     use_percents=F)

fp <- here::here(out_dir, 'annual_gain_loss_fixed_mgc.png')
site_df %>% 
  plot_gross_changes(fp = fp, width=figwidth+4, height=figheight+1, 
                     facet_scales='fixed', 
                     facet_ncol = facet_ncol,
                     use_percents=F)

## Net change ----
fp <- here::here(out_dir, 'annual_netchange_fixed_pct.png')
site_df %>% 
  plot_net_changes(fp = fp, width=figwidth+4, height=figheight+1, 
                   facet_scales='fixed', 
                   facet_ncol = facet_ncol,
                   use_percents=T)

fp <- here::here(out_dir, 'annual_netchange_free_mgc.png')
site_df %>% 
  plot_net_changes(fp = fp, width=figwidth+4, height=figheight+1, 
                   facet_scales='free_y', 
                   facet_ncol = facet_ncol,
                   use_percents=F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot 500 with 30 m data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Join 30m with piecewise ----
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
  full_join(df_pw, by=c(stock_year='Year', div_name='div_name', site='site'))

# Plot lines 
df %>%
  plot_comp_30mpw_500m(facet_scales='fixed', 
                       width=figwidth+2, 
                       height=figheight,
                       facet_ncol = facet_ncol,
                       use_percents = F)

fp <- here::here(out_dir, paste0('plots_carbon_lost_30m_v_500m_pw.png'))
ggsave(fp, width=7, height=4)



df_pw %>% 
  filter(div_name == 'Aceh Selatan') %>% head(3)

site_df %>% 
  filter(div_name == 'Aceh Selatan') %>% 
  select(-any_of(c('site', 'div_name', 'area_ha', 'stock_year',
                   'Dens_tCha', 'Stock_MtC', 'Area_Mha'))) %>% 
  select(change_year, mgc_loss_500, mgc_)
head(3)

df %>% 
  filter(div_name == 'Aceh Selatan') %>% 
  select(-any_of(c('site', 'div_name', 'area_ha', 'stock_year',
                   'Dens_tCha', 'Stock_MtC', 'Area_Mha'))) %>% 
  tbl_vars()

df %>% 
  filter(div_name == 'Aceh Selatan') %>% 
  select(stock_year, mgc_loss_500, pct_loss_500, mgc_loss_30, pct_loss_30) %>% 
  pivot_longer(mgc_loss_500:mgc_pw_fit) %>% 
  ggplot(aes(x=stock_year, y=value)) +
  geom_line() +
  facet_wrap(vars(name), scales='free_y')





# Table of stock and density in most recent year ----
library(flextable)
library(officer)

## Get mean loss rate for most recent years -----
lastyr <- 2020
loss_15to20_df <- df %>% 
  group_by(div_name) %>% 
  filter(stock_year > lastyr-5 & stock_year <= lastyr) %>% 
  summarize(across(where(is.double), ~ mean(.x, na.rm=T))) %>% 
  select(div_name, 
         mgc_loss_500_mean = mgc_loss_500, 
         loss_fc_ha_mean_15to20 = loss_fc_ha)

lastyr <- 2022
loss_17to22_df <- df %>% 
  group_by(div_name) %>% 
  filter(stock_year > lastyr-5 & stock_year <= lastyr) %>% 
  summarize(across(where(is.double), ~ mean(.x, na.rm=T))) %>% 
  select(div_name, 
         loss_fc_ha_mean_17to22 = loss_fc_ha)

summary_df <- site_df %>% 
  filter(stock_year == endyr) %>% 
  mutate(Stock_MtC = round(Stock_MtC, 1), 
         area_ha = round(area_ha),
         Area_Mha = round(Area_Mha, 1)) %>% 
  arrange(desc(Dens_tCha)) %>% 
  left_join(loss_15to20_df, by='div_name') %>%
  left_join(loss_17to22_df, by='div_name') %>%
  select(site, div_name, Dens_tCha, Stock_MtC, area_ha, 
         mgc_loss_500_mean, 
         loss_fc_ha_mean_15to20, 
         loss_fc_ha_mean_17to22) %>% 
  mutate(mgc_loss_500_mean = round(mgc_loss_500_mean), 
         loss_fc_ha_mean_15to20 = round(loss_fc_ha_mean_15to20), 
         loss_fc_ha_mean_17to22 = round(loss_fc_ha_mean_17to22))

ft <- summary_df %>% 
  flextable() %>% 
  font(fontname = 'Helvetica Neue', part = 'all') %>%
  fontsize(size = 9, part = 'all') %>%
  bold(part = 'header') %>% 
  align(part = 'header', align = 'center') %>% 
  valign(part = 'header', valign = 'bottom') %>%
  width(width = 1.1, unit = 'in') %>% 
  line_spacing(part='header', space=1.5) %>%
  # Column names
  mk_par(j = 'site', part = 'header', value = as_paragraph('Province')) %>% 
  mk_par(j = 'div_name', part = 'header', value = as_paragraph('Regency')) %>% 
  mk_par(j = 'Stock_MtC', part = 'header', value = as_paragraph('Carbon stock\nin 2020\n(MtC)')) %>% 
  mk_par(j = 'Dens_tCha', part = 'header', value = as_paragraph('Carbon density\nin 2020\n(tC/ha)')) %>% 
  mk_par(j = 'mgc_loss_500_mean', part = 'header', 
         value = as_paragraph(str_glue('Carbon loss\n2015-2020 (tC/yr)'))) %>% 
  mk_par(j = 'loss_fc_ha_mean_15to20', part = 'header', 
         value = as_paragraph(str_glue('Forest area loss\n2015-2020 (ha/yr)'))) %>% 
  mk_par(j = 'loss_fc_ha_mean_17to22', part = 'header', 
         value = as_paragraph(str_glue('Forest area loss\n2017-2022 (ha/yr)'))) %>% 
  mk_par(j = 'area_ha', part = 'header', value = as_paragraph('Area\n(ha)')) %>% 
  # Alignment
  align(j = 'Dens_tCha', align = 'right', part = 'body') %>% 
  align(j = 'mgc_loss_500_mean', align = 'right', part = 'body') %>%
  align(j = 'loss_fc_ha_mean_15to20', align = 'right', part = 'body') %>%
  align(j = 'loss_fc_ha_mean_17to22', align = 'right', part = 'body') %>%
  align(j = 'area_ha', align = 'right', part = 'body') %>% 
  align(j = 'site', align = 'left', part = 'header') %>% 
  align(j = 'div_name', align = 'left', part = 'header') %>% 
  bold(j = 'div_name') %>% 
  set_formatter(Dens_tCha=function(x) sprintf("%.01f", x)) %>%
  hline_top(border = fp_border(width = 1), part = 'all') %>%
  hline_bottom(border = fp_border(width = 1), part = 'all')
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

