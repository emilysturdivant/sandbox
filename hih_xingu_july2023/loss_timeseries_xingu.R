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

# Get piecewise regression for total loss
y_var='loss_mgc'
div_names <- loss_df %>% distinct(site) %>% pull(site)
loss_df_inv <- loss_df %>% 
  mutate(!!y_var := .data[[y_var]] * -1)
df_pw_mgc <- div_names %>% purrr::map_dfr(
  function(x){
    df_zone <- loss_df_inv %>% filter(site == x)
    pw_fit <- get_pw_line_fc(df_zone, y_var=y_var, x_var='Year')
    pw_fit[['div_name']] <- x
    df_zone2 <- pw_fit %>% 
      # right_join(df_zone, by=c(x = x_var)) %>%
      rename(all_of(c(Year = 'x', pw_fit = 'y')))
  }
)

# Get piecewise regression for percents
y_var='loss_pct_carbon'
div_names <- loss_df %>% distinct(div_name) %>% pull(div_name)
loss_df_inv <- loss_df %>% 
  mutate(!!y_var := .data[[y_var]] * -1)
df_pw_pct <- div_names %>% purrr::map_dfr(
  function(x){
    df_zone <- loss_df_inv %>% filter(div_name == x)
    pw_fit <- get_pw_line_fc(df_zone, y_var=y_var, x_var='Year')
    pw_fit[['div_name']] <- x
    df_zone2 <- pw_fit %>% 
      # right_join(df_zone, by=c(x = x_var)) %>%
      rename(all_of(c(Year = 'x', pw_fit = 'y')))
  }
)

df_pw <- df_pw_pct %>% 
  full_join(df_pw_mgc, by=c('Year', 'div_name'), suffix=c('_pct', '_mgc')) %>% 
  full_join(loss_df, by=c('Year', 'div_name'))

# Save as CSV 
df_pw %>% 
  readr::write_csv(here::here(out_dir, 'piecewise_loss.csv'))
df_pw <- readr::read_csv(here::here(out_dir, 'piecewise_loss.csv'))

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

site_df <- site_df %>% 
  mutate(div_name = factor(div_name, levels=div_order)) 

# Plot 500m data ----
## Stock ----
# Stock from 500m colored by percent net change
facet_ncol = 7
fp <- here::here(out_dir, 'annual_stock_fixed_rel03.png')
site_df %>% 
  plot_ann_stock(fp = fp, width=figwidth+4, height=figheight+1, 
                           facet_scales='fixed', 
                           facet_ncol = facet_ncol)

# Stock from 500m colored by percent net change
facet_ncol = 3
fp <- here::here(out_dir, 'annual_stock_free.png')
site_df %>% 
  plot_ann_stock(fp = fp, width=figwidth+4, height=figheight+1, 
                 facet_scales='free_y', 
                 facet_ncol = facet_ncol)

## Gains and losses ----
facet_ncol = 3
fp <- here::here(out_dir, 'annual_gain_loss_fixed_pct.png')
site_df %>% 
  plot_gross_changes(fp = fp, width=figwidth+4, height=figheight+1, 
                 facet_scales='fixed', 
                 facet_ncol = facet_ncol,
                 use_percents=T)

p_small <- site_df %>% 
  filter(str_detect(div_name, 'RESEX|Xipaya|Trincheira')) %>% 
  plot_gross_changes(width=figwidth+4, height=figheight+1, 
                     facet_scales='fixed', 
                     facet_ncol = facet_ncol,
                     use_percents=T)

p_big <- site_df %>% 
  filter(str_detect(div_name, 'RESEX|Xipaya|Trincheira', negate=T)) %>% 
  plot_gross_changes(width=figwidth+4, height=figheight+1, 
                     facet_scales='fixed', 
                     facet_ncol = 4,
                     use_percents=T)
# Layout and save
design <- '1111
2223'
p <- p_big + p_small + guide_area() + 
  plot_layout(guides='collect', design=design)
p
fp <- here::here(out_dir, paste0('annual_gain_loss_pct_2fixed.png'))
ggsave(fp, plot=p, width=figwidth+2, 
       height=figheight)

# Subset with larger changes
df_subset <- site_df %>% filter(str_detect(div_name, 'RESEX|Xipaya|Paquiçamba', negate=T))
p_large <- plot_gross_changes(df_subset, 
                                facet_scales='fixed', 
                                width=figwidth+2, 
                                height=figheight,
                                facet_ncol = 4)

# Subset with smaller changes
df_subset <- site_df %>% filter(str_detect(div_name, 'RESEX|Xipaya|Paquiçamba'))
p_small <- plot_gross_changes(df_subset, 
                                facet_scales='fixed', 
                                width=figwidth+2, 
                                height=figheight,
                                facet_ncol = 3,
                                use_percents = T)

# Layout and save
design <- '1111
2223'
p <- p_large + p_small + guide_area() + 
  plot_layout(guides='collect', design=design)
p
fp <- here::here(out_dir, paste0('annual_gain_loss_2fixed.png'))
ggsave(fp, plot=p, width=figwidth+2, 
       height=figheight)

## Net change ----
facet_ncol = 3
fp <- here::here(out_dir, 'annual_netchange_fixed_pct.png')
site_df %>% 
  plot_net_changes(fp = fp, width=figwidth+4, height=figheight+1, 
                     facet_scales='fixed', 
                     facet_ncol = facet_ncol,
                     use_percents=T)

p_small <- site_df %>% 
  filter(str_detect(div_name, 'RESEX|Xipaya|Trincheira')) %>% 
  plot_net_changes(width=figwidth+4, height=figheight+1, 
                     facet_scales='fixed', 
                     facet_ncol = facet_ncol,
                     use_percents=T)

p_big <- site_df %>% 
  filter(str_detect(div_name, 'RESEX|Xipaya|Trincheira', negate=T)) %>% 
  plot_net_changes(width=figwidth+4, height=figheight+1, 
                     facet_scales='fixed', 
                     facet_ncol = 4,
                     use_percents=T)
# Layout and save
design <- '1111
2223'
p <- p_big + p_small + guide_area() + 
  plot_layout(guides='collect', design=design)
p
fp <- here::here(out_dir, paste0('annual_netchange_pct_2fixed.png'))
ggsave(fp, plot=p, width=figwidth+2, 
       height=figheight)

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

# Rename 30m columns prior to joining
df_pw <- df_pw %>% 
  rename(
    mgc_loss_30 = loss_mgc,
         pct_loss_30 = loss_pct_carbon,
         # mgc_loss_30 = loss_30_tC,
         # pct_loss_30 = loss_30_pct,
         pct_pw_fit = pw_fit_pct,
         mgc_pw_fit = pw_fit_mgc) %>%
  mutate(mgc_loss_30 = mgc_loss_30 * -1,
         pct_loss_30 = pct_loss_30 * -1)

# Join
df <- site_df %>% 
  full_join(df_pw, by=c(stock_year='Year', div_name='div_name', site='site')) %>% 
  mutate(div_name = factor(div_name, levels=div_order))

## Plot ----
# Change in metric tons
fp <- here::here(out_dir, paste0('plot_carbon_30m_v_500m_pw_mgc.png'))
plot_comp_30mpw_500m(df, fp=fp, facet_scales='free_y', 
                     width=figwidth+2, 
                     height=figheight,
                     use_percents = F)

# Change in percents
# Change in percents: subset with larger changes
df_subset <- df %>% filter(str_detect(div_name, 'Xingu|Cachoeira|Apyterewa|Paquiçamba'))
p_large <- plot_comp_30mpw_500m(df_subset, 
                     facet_scales='fixed', 
                     width=figwidth+2, 
                     height=figheight,
                     facet_ncol = 4,
                     use_percents = T)

# Change in percents: subset with smaller changes
df_subset <- df %>% 
  filter(str_detect(div_name, 'Xingu|Cachoeira|Apyterewa|Paquiçamba', negate=T))
p_small <- plot_comp_30mpw_500m(df_subset, 
                     facet_scales='fixed', 
                     width=figwidth+2, 
                     height=figheight,
                     facet_ncol = 3,
                     use_percents = T)

# Layout and save
design <- '1111
2223'
p <- p_large + p_small + guide_area() + 
  plot_layout(guides='collect', design=design)
p
fp <- here::here(out_dir, paste0('plot_carbon_30m_v_500m_pw_pcts.png'))
ggsave(fp, plot=p, width=figwidth+2, 
       height=figheight)

# 
# df_subset <- df %>% filter(str_detect(div_name, 'TI Xingu'))
# p1 <- plot_comp_30mpw_500m(df_subset, facet_scales='fixed')
# p1
# 
# df_subset <- df %>% filter(str_detect(div_name, 'TI Apyterewa|TI Cachoeira|TI Trincheira'))
# p2 <- plot_comp_30mpw_500m(df_subset, facet_scales='fixed')
# p2
# 
# df_subset <- df %>% filter(str_detect(div_name, 'RESEX|TI Trincheira'))
# p3 <- plot_comp_30mpw_500m(df_subset, facet_scales='fixed')
# p3
# 
# df_subset <- df %>% filter(str_detect(div_name, 'TI Xipaya|TI Paquiçamba'))
# p4 <- plot_comp_30mpw_500m(df_subset, facet_scales='fixed')
# p4
# 
# (p1 / p2 / p3 / p4) + plot_layout(guides = 'collect')
# 
# #
# df_subset <- df %>% filter(str_detect(div_name, 'TI Xingu'))
# p1 <- plot_comp_30mpw_500m(df_subset, facet_scales='fixed')
# p1
# 
# df_subset <- df %>% filter(str_detect(div_name, 'TI Apyterewa|TI Cachoeira|TI Trincheira'))
# p2 <- plot_comp_30mpw_500m(df_subset, facet_scales='fixed')
# p2
# 
# df_subset <- df %>% filter(str_detect(div_name, 'RESEX|TI Xipaya|TI Paquiçamba'))
# p3 <- plot_comp_30mpw_500m(df_subset, facet_scales='fixed')
# p3
# # 
# # df_subset <- df %>% filter(str_detect(div_name, 'TI Xipaya|TI Paquiçamba'))
# # p4 <- plot_comp_30mpw_500m(df_subset, facet_scales='fixed')
# # p4
# 
# (p1 / p2 / p3 / p4) + plot_layout(guides = 'collect')

# One site only ----
plot_site <- function(div) {
  df_sub <- df %>% filter(str_detect(div_name, div))
  
  p_gl <- df_sub %>% plot_gross_changes(facets_on=F)
  p_nc <- df_sub %>% plot_net_changes(facets_on=F)
  p_as <- df_sub %>% plot_ann_stock(facets_on=F)
  p_comp30 <- df_sub %>% plot_comp_30mpw_500m(facets_on=F)
  
  # Layout and save
  p <- ((p_comp30 / p_nc ) | p_as) + plot_layout(guides='collect')
  
  site_code <- df_sub %>% distinct(site_code) %>% filter(!is.na(site_code)) %>% pull(site_code)
  fp <- here::here(out_dir, paste0('plots_', site_code, '.png'))
  ggsave(fp, plot=p, width=7, height=5)
  
  return(p)
}

site_df %>% 
  rename(site=params$site_var)

div_names <- site_df %>% distinct(div_name) %>% pull() %>% as.vector()
for( div in div_names ){
  print(div)
  plot_site(div)
}
