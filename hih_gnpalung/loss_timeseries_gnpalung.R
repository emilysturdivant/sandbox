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
dir.create(out_dir, recursive=TRUE)

working_dir <- here::here(home_dir, 'working_data')
site_poly_dir <- working_dir
fp_polys <- here::here(site_poly_dir, 'GPNP_divided.shp')

# Create VRTs ----
# AGB
agb_dir <- '/Volumes/ejs_storage/data/raw_data/biomass/global_30m_year2000_v5'
interp_tiles <- list.files(agb_dir, '*\\.tif', full.names = TRUE)
agb_vrt <- here::here(agb_dir, 'alltiles.vrt')
gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, agb_vrt, overwrite=T) # dryrun=TRUE

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

# Prep df ----
params <-
  list(
    filter = c(name = 'Gunung Palung National Park'),
    polys = fp_polys,
    site_var = 'name',
    subdiv_var='layer',
    agb30_fp = agb_vrt, 
    tc2000_fp = tc2000_vrt,
    lossyear_fp = hansen_vrt,
    # agb_ras = interp_vrt,
    # years = c(2003, 2020),
    single_site = FALSE
  )

overwrite <- FALSE
loss_csv <- here::here(out_dir, 'loss_30m_Hansen_v1.10.csv')

# Get zonal sums from 30m data----
if( !file.exists(loss_csv) | overwrite ){
  loss_df <- extract_zonal_sums_30m(params, out_csv=loss_csv)
}
loss_df <- readr::read_csv(loss_csv)

## Plot ---- 
loss_df <- loss_df %>% 
  mutate(div_name = str_to_title(div_name) %>% 
           factor(levels=c('West','East')))

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
ggsave(fp, width=7, height=3)

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
ggsave(fp, plot=p, width=7, height=3)

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

# Test alternative piecewise approach...
# dft <- loss_df_inv
# x <- 'East'
# df_zone <- loss_df_inv %>% filter(div_name == x)
# 
# # Detect breakpoint with lowest MSE
# bp_mse <- df_zone %>% 
#   filter(Year>=2002, Year<=2021) %>% 
#   pull(Year) %>% 
#   purrr::map_dfr(function(b){
#     piecewise1 <- lm(loss_mgc ~ Year*(Year < b) + Year*(Year>=b), data=df_zone)
#     as_tibble(list(b=b, mse=summary(piecewise1)[[6]]))
#   }) %>% 
#   slice_min(mse)
# 
# # Detect breakpoint with lowest MSE
# bp_mse <- df_zone %>% 
#   filter(Year>=2001, Year<=2022-5) %>% 
#   pull(Year) %>% 
#   purrr::map_dfr(function(b1){
#     b2 <- b1+4
#     piecewise1 <- lm(loss_mgc ~ Year*(Year < b1) + 
#                        Year*(Year>=b1 & Year<b2) + 
#                        Year*(Year>=b2), 
#                      data=df_zone)
#     as_tibble(list(b1=b1, b2=b2, mse=summary(piecewise1)[[6]]))
#   }) %>% 
#   slice_min(mse)
# bp_mse
# 
# # Plot
# df_zone %>% 
#   mutate(grp = cut(Year, breaks=c(2000, bp_mse$b1, bp_mse$b2, 2022))) %>% 
#   ggplot(aes(x=Year, y=loss_mgc)) +
#   geom_point() +
#   geom_line() +
#   geom_smooth(aes(group = grp), method='lm', se=F)

# Save as CSV
pw_csv <- here::here(out_dir, 'piecewise_loss_mgc.csv')
df_pw %>% 
  # dplyr::select(Year, loss_mgc, pw_fit) %>% 
  # mutate(Year = Year %>% str_sub(1,4)) %>% 
  readr::write_csv(pw_csv)

df_pw <- read_csv(pw_csv)

## Plot ----
val_var <- y_var
y_name <- str_c('Loss in carbon (tC)')# from previous year
ymax <- max(df_pw[[val_var]])

p <- df_pw %>% 
  mutate(div_name = str_to_title(div_name) %>% 
           factor(levels=c('West','East'))) %>% 
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
ggsave(here::here(out_dir, 'piecewise_loss_mgc.png'), 
       plot = p,
       width = 4.5,
       height = 2.9,
       dpi = 150)

fp <- here::here(out_dir, 'piecewise_loss_mgc.png')
ggsave(fp, plot=p, width=7, height=3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot 30 m and 500 m data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

agb_id <- 'agb500m_y03_20'
df_csv <- here::here(out_dir, str_c(agb_id, '.csv'))
df <- readr::read_csv(df_csv)

## Prep df ----
# Standardize column names and calculate density
site_df <- df %>% 
  rename(site = params$site_var, 
         div_name = params$subdiv_var) %>%
  mutate(Dens_tCha = carbon_stock_tC / area_ha, 
         Stock_MtC = carbon_stock_tC * 1e-6, 
         Area_Mha = area_ha * 1e-6
  )

df <- site_df %>% 
  mutate(div_name = str_to_title(div_name) %>% 
           factor(levels=c('West', 'East'))) %>% 
  full_join(df_pw, by=c(stock_year='Year', div_name='div_name', site='site'))

# df <- df_pw %>% 
#   mutate(div_name = str_to_title(div_name) %>% 
#            factor(levels=c('East', 'West'))) %>% 
#   full_join(site_df, by=c(stock_year='Year', div_name='div_name', site='site'))


# labels
cat_labels <- c(loss_tC='Loss from\n500-m carbon', 
                net_change_tC='Net change from\n500-m carbon', 
                loss_mgc='Loss from\n30-m tree cover')

# Plot lines 
df %>% 
  # mutate(loss_mgc = loss_mgc * -1) %>% 
  pivot_longer(any_of(c('annual_loss_tC', 'annual_net_change_tC', 'loss_mgc')), 
               names_to='var', values_to='tC', names_prefix='annual_') %>% 
  ggplot(aes(x=stock_year, y=tC, color=var, lty=var)) +
  geom_hline(aes(yintercept=0), color='darkgray') +
  geom_line() +
  scale_color_manual(values=c(loss_tC='darkblue', 
                              net_change_tC='darkblue', 
                              loss_mgc='#F5A011'),
                     labels=cat_labels,
                     name='') + 
  scale_linetype_manual(values=c(loss_tC=1, 
                                 net_change_tC=2, 
                                 loss_mgc=1),
                        labels=cat_labels,
                        name='') + 
  scale_x_continuous(labels=label_yearintervals, name='') + 
  scale_y_continuous(labels=scales::label_number(scale_cut=scales::cut_short_scale()), name='Carbon stock lost (tC)') + 
  facet_wrap('div_name', scales='fixed') +
  theme_bw()

fp <- here::here(out_dir, paste0('plots_carbon_lost_30m_v_500m.png'))
ggsave(fp, width=7, height=3)

# labels
cat_labels <- c(net_change_tC='Net change from\n500-m carbon', 
                loss_tC='Loss from\n500-m carbon', 
                loss_mgc='Loss from\n30-m tree cover', 
                pw_fit='Piecewise trend\n(30-m)')
cat_labels %>% factor(ordered=T)

# Plot lines 
df %>% 
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
  facet_wrap('div_name', scales='fixed') +
  theme_bw() +
  theme(legend.key.size = unit(2, 'lines'))

fp <- here::here(out_dir, paste0('plots_carbon_lost_30m_v_500m_pw.png'))
ggsave(fp, width=7, height=4)
