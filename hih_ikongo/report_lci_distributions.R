#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * View Carbon pool values distributions for input polygons - DRC
# Requires:
#     * Biomass GeoTIFs 
#     * Polygons
# Author:
#     * esturdivant@woodwellclimate.org, 2023-03
# History:
#     * Copied from cree_nation folder
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dplyr)
library(tibble)
library(ggplot2)
library(patchwork)
library(stringr)
library(tidyr)
library(sf)
library(stars)
library(ggridges)
library(flextable)
library(officer)
library(janitor)

home_dir <- here::here('hih_ikongo')
out_dir <- here::here(home_dir, 'outputs')
dir.create(out_dir, recursive=TRUE)

site_poly_dir <- '~/data/hih_sites/Madagascar_Ikongo'
fp_polys <- here::here(site_poly_dir, 'Ikongo_communes_BNRGC_2018.shp')

# poly_dir <- '/Volumes/ejs_storage/data/raw_data/world_context/gadm'

working_dir <- here::here(home_dir, 'working_data')
dir.create(working_dir, recursive=TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get parameters ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
params <- list(
  polys = fp_polys,
  site_var = 'ADM3_EN'
)

agb_tifs <- list(
  list(
    rast_fp = '~/data/Walker_etal_2022/Base_Cur_AGB_BGB_MgCha_500m.tif',
    level = 'AGB_BGB_mgCha',
    value_lims = c(0, 100)
  ),
  list(
    rast_fp = '~/data/Walker_etal_2022/Base_Cur_SOC_MgCha_500m.tif',
    level = 'SOC_mgCha',
    value_lims = c(0, 400)
  ),
  list(
    rast_fp = '~/data/Walker_etal_2022/Base_Cur_AGB_BGB_SOC_MgCha_500m.tif',
    level = 'AGB_BGB_SOC_mgCha',
    value_lims = c(0, 400)
  )
)

lci_tifs <- list(
  c(list(
    rast_fp = '~/repos/carbon-quality-index/projects/v20230824/comp_bq.tif',
    level = 'BQ'
  ), params),
  c(list(
    rast_fp = '~/repos/carbon-quality-index/projects/v20230824/comp_bp.tif',
    level = 'BP'
  ), params),
  c(list(
    rast_fp = '~/repos/carbon-quality-index/projects/v20230824/comp_add.tif',
    level = 'Add'
  ), params),
  c(list(
    rast_fp = '~/repos/carbon-quality-index/projects/v20230824/comp_perm.tif',
    level = 'Perm'
  ), params),
  c(list(
    rast_fp = '~/repos/carbon-quality-index/projects/v20230824/final_lci.tif',
    level = 'Final'
  ), params),
  c(list(
    rast_fp = '~/repos/carbon-quality-index/projects/v20230824/subcomp_bq_cc.tif',
    level = 'BQ_CurCarbon'
  ), params),
  c(list(
    rast_fp = '~/repos/carbon-quality-index/projects/v20230824/subcomp_add_hist.tif',
    level = 'Add_HistLoss'
  ), params)
)

# Get values for one raster layer ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
extract_values_lci <- function(params) {
  # Read stars (proxy)
  rr <- stars::read_stars(params$rast_fp)
  
  # Load and reproject polygons
  # pols <- st_read(fp_poly, layer='ADM_ADM_1') # Load from geopackage
  pols <- st_read(params$polys) # Load from shapefile
  site_var <- params$site_var
  
  # Reproject
  pols <- pols %>% st_transform(st_crs(rr))
  
  # Get pixel values for each polygon ----
  df <- seq_len(nrow(pols)) %>% 
    purrr::map_dfr(
      function(i) {
        pol <- pols[i,]
        
        # Extract values
        ar <- rr[pol] %>% 
          stars::st_as_stars() %>% 
          pull() %>% 
          as.vector() %>% 
          na.omit() %>% 
          as_tibble()  %>%  
          mutate(name = pol[[site_var]])
      }
    )
  
  # Add variable with LCI level name
  df |> mutate(level = params$level)
}

# Summary stats ----
create_summary_table <- function(df_p){
  pix.ha <- 463.3127 * 463.3127 * 1e-4
  stats <- df_p %>% 
    mutate(value = value)  %>% 
    group_by(comp_name, name) %>% 
    summarize(Min = min(value, na.rm=TRUE),
              Q1 = quantile(value, 0.25, na.rm=TRUE),
              Median = median(value, na.rm=TRUE),
              Mean = mean(value, na.rm=TRUE),
              Q3 = quantile(value, 0.75, na.rm=TRUE),
              Max = max(value, na.rm=TRUE),
              N = n(),
              Area_ha = N * pix.ha)

  return(stats)
}

print_table_by_pool <- function(df, pool_name) {
  df <- df %>% 
    filter(comp_name == pool_name) %>% 
    ungroup()
  
  df %>% 
    bind_rows(tibble(name = 'Ikongo District', 
                     Min = min(df$Min, na.rm=TRUE),
                     Q1 = quantile(df$Q1, 0.25, na.rm=TRUE),
                     Median = median(df$Median, na.rm=TRUE),
                     Mean = mean(df$Mean, na.rm=TRUE),
                     Q3 = quantile(df$Q3, 0.75, na.rm=TRUE),
                     Max = max(df$Max, na.rm=TRUE),
                     N = sum(df$N, na.rm=TRUE),
                     Area_ha = sum(df$Area_ha, na.rm=TRUE),
    )) %>% 
    select(c(name, Mean, Area_ha)) %>% 
    print_flextable()
}

print_flextable <- function(df_out) {
  df_out %>%
    flextable() %>% 
    mk_par(j = 'name', part = 'header', 
           value = as_paragraph('')) %>% 
    mk_par(j = 'Mean', part = 'header', 
           value = as_paragraph('Mean')) %>% 
    mk_par(j = 'Area_ha', part = 'header', 
           value = as_paragraph('Area (ha)')) %>% 
    align(j = 'Mean', align = 'right', part = 'body') %>% 
    align(j = 'Area_ha', align = 'right', part = 'body') %>% 
    font(fontname = 'Helvetica Neue', part = 'all') %>%
    fontsize(size = 9, part = 'all') %>%
    bold(part = 'header') %>% 
    align(part = 'header', align = 'center') %>% 
    valign(part = 'header', valign = 'bottom') %>% 
    align(j = 'name', align = 'left', part = 'header') %>% 
    set_formatter_type(fmt_double = "%.0f") %>% 
    colformat_double(digits = 0) %>% 
    bold(j = 'name') %>% 
    bg(part = 'body', i = seq(1, nrow(df_out), 2), bg = "#EFEFEF") %>% 
    hline_top(border = fp_border(width = 1), part = 'all') %>% 
    hline_bottom(border = fp_border(width = 1), part = 'all') %>% 
    hline(i = nrow(df_out)-1, border = fp_border(width = 1)) %>%
    width(width = 1.1, unit = 'in') %>% 
    width(j = 'name', width = 2, unit = 'in')
}

# Extract values ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
out_rds <- here::here(working_dir, 'extracted_vals_LCI_v20230824.rds')

df_p <- lci_tifs %>% 
  purrr::map_dfr(extract_values_lci) %>% 
  mutate(comp_name = factor(level, levels = purrr::map_chr(lci_tifs, ~.x$level))) 
df_p %>% saveRDS(out_rds)

# Mean component scores ----
df_p <- readRDS(out_rds)

comp_codes <- c('Final', 'BQ', 'BP', 'Add', 'Perm', 'BQ_CurCarbon', 'Add_HistLoss')
comp_levels <- c('LCI', 'Biophysical\nQuality', 'Biophysical\nPotential',
                 'Additionality\nPotential', 'Risk to\nPermanence', 
                 'Biophysical Potential:\nCarbon Storage', 
                 'Additionality Potential:\nHistorical Forest Loss')
lu <- tibble(comp_name = comp_codes, component = comp_levels) %>% 
  mutate(component = factor(component, comp_levels))

stats <- create_summary_table(df_p) %>% 
  ungroup() %>% 
  left_join(lu)

stats %>% 
  filter(str_detect(comp_name, 'Add_|BQ_', negate=TRUE)) %>% 
  ggplot(aes(y=name, x=Mean, color=Mean)) +
  geom_point(stat='identity', show.legend = FALSE) +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(limits=c(25, 100), expand = c(0,0)) +
  # scale_color_viridis_c() +
  xlab(NULL) +
  ylab(NULL) +
  facet_grid(cols=vars(component)) +
  theme_bw()

ggsave(here::here(out_dir, paste0('component_scores_facets.png')), 
       width=8, height=3)

stats %>% 
  filter(str_detect(comp_name, 'Add_|BQ_')) %>% 
  ggplot(aes(y=name, x=Mean, color=Mean)) +
  geom_point(stat='identity', show.legend = FALSE) +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(limits=c(25, 100), expand = c(0,1.5)) +
  # scale_color_viridis_c() +
  xlab(NULL) +
  ylab(NULL) +
  facet_grid(cols=vars(component)) +
  theme_bw() +
  theme(axis.text.x = )

ggsave(here::here(out_dir, paste0('subcomp_scores_2facets.png')), 
       width=4, height=3)


# Print table ----
df_out <- stats %>% 
  filter(str_detect(comp_name, 'Add_|BQ_', negate=TRUE)) %>% 
  pivot_wider(id_cols='name', values_from='Mean', names_from='comp_name')

ft <- df_out %>% 
  # arrange(desc(Final)) %>% 
  flextable() %>% 
  mk_par(j = 'name', part = 'header', 
         value = as_paragraph('')) %>% 
  mk_par(j = 'BQ', part = 'header',
         value = as_paragraph('Biophysical Quality')) %>%
  mk_par(j = 'BP', part = 'header',
         value = as_paragraph('Biophysical Potential')) %>%
  mk_par(j = 'Add', part = 'header',
         value = as_paragraph('Additionality Potential')) %>%
  mk_par(j = 'Perm', part = 'header',
         value = as_paragraph('Risk to Permanence')) %>%
  mk_par(j = 'Final', part = 'header',
         value = as_paragraph('LCI')) %>%
  font(fontname = 'Helvetica Neue', part = 'all') %>%
  fontsize(size = 9, part = 'all') %>%
  bold(part = 'header') %>% 
  align(part = 'header', align = 'center') %>% 
  valign(part = 'header', valign = 'bottom') %>% 
  align(j = 'name', align = 'left', part = 'header') %>% 
  set_formatter_type(fmt_double = "%.0f") %>% 
  colformat_double(digits = 0) %>% 
  bold(j = 'name') %>% 
  bg(part = 'body', i = seq(1, nrow(df_out), 2), bg = "#EFEFEF") %>% 
  hline_top(border = fp_border(width = 1), part = 'all') %>% 
  hline_bottom(border = fp_border(width = 1), part = 'all') %>% 
  width(width = 1.1, unit = 'in') %>% 
  width(j = 'name', width = 2, unit = 'in')

ft
png_fp <- here::here(out_dir, paste0('table_component_scores.png'))
save_as_image(ft, png_fp)


# Print table ----
df_out <- stats %>% 
  filter(str_detect(comp_name, 'Add_|BQ_')) %>% 
  pivot_wider(id_cols='name', values_from='Mean', names_from='comp_name')

ft <- df_out %>% 
  # arrange(desc(Final)) %>% 
  flextable() %>% 
  mk_par(j = 'name', part = 'header', 
         value = as_paragraph('')) %>% 
  mk_par(j = 'Add_HistLoss', part = 'header',
         value = as_paragraph('Historical Forest Loss')) %>%
  mk_par(j = 'BQ_CurCarbon', part = 'header',
         value = as_paragraph('Carbon Storage')) %>%
  font(fontname = 'Helvetica Neue', part = 'all') %>%
  fontsize(size = 9, part = 'all') %>%
  bold(part = 'header') %>% 
  align(part = 'header', align = 'center') %>% 
  valign(part = 'header', valign = 'bottom') %>% 
  align(j = 'name', align = 'left', part = 'header') %>% 
  set_formatter_type(fmt_double = "%.0f") %>% 
  colformat_double(digits = 0) %>% 
  bold(j = 'name') %>% 
  bg(part = 'body', i = seq(1, nrow(df_out), 2), bg = "#EFEFEF") %>% 
  hline_top(border = fp_border(width = 1), part = 'all') %>% 
  hline_bottom(border = fp_border(width = 1), part = 'all') %>% 
  width(width = 1, unit = 'in') %>% 
  width(j = 'name', width = 2, unit = 'in')

ft
png_fp <- here::here(out_dir, paste0('table_subcomponent_scores.png'))
save_as_image(ft, png_fp)

df_out <- stats %>% 
  pivot_wider(id_cols='name', values_from='Mean', names_from='comp_name')

df_out %>% 
  mutate(across(where(is.numeric), ~ round(.x))) %>% 
  readr::write_csv(here::here(out_dir, 'Ikongo_LCI_scores.csv'))

# Map ----
## Functions----
prep_polys <- function(adm1_fp, crs=st_crs('epsg:4326')) {
  
  if(str_detect(adm1_fp, 'gpkg$')) {
    adm1 <- st_read(adm1_fp, layer='ADM_ADM_1') %>% 
      st_transform(crs)
  } else {
    adm1 <- st_read(adm1_fp) %>% 
      st_transform(crs)
  }
  
  # Get bounding box as st_bbox, sfc, and wkt
  bb <- adm1 %>% st_buffer(20000) %>% st_bbox()
  bb_wkt <- bb %>% 
    st_as_sfc() %>% 
    st_geometry() %>% 
    st_transform(st_crs('epsg:4326')) %>% 
    st_as_text()
  
  adm0_shp <- here::here(poly_dir, 'gadm36_0.shp')
  adm0 <- st_read(adm0_shp, wkt_filter=bb_wkt) %>% 
    st_transform(st_crs(rr)) %>% 
    st_simplify(dTolerance=1000)
  
  return(list(adm1=adm1, adm0=adm0, bb=bb))
}

get_df <- function(rr, clip_poly, downsample=8) {
  # Extract values
  ar <- rr[clip_poly] |> 
    stars::st_as_stars(downsample = downsample) %>%
    as_tibble() %>% 
    rename(mgcha = 3) %>% 
    filter(!is.na(mgcha))
}

plot_map <- function(ar, adm1, adm0, bb, subject='carbon', 
                     value_lims=c(0,250), legend_title=NULL, 
                     palette='viridis', 
                     GID_0 = 'AUS'){
  
  roi <- adm0 %>% filter(GID_0 == GID_0)
  
  invmask <- st_as_sfc(bb) %>% 
    st_difference(roi)
  
  p <- ar %>%
    ggplot() +
    geom_raster(aes(x,y, fill = mgcha)) +
    
    geom_sf(data = adm1 %>% select(1), fill = NA, color = "grey40", size = 0.7) +
    geom_sf(data = adm1 %>% select(1), fill = NA, color = "white", size = 0.2) +
    geom_sf(data = adm0, fill = NA, color = "white") +
    # geom_sf(data = adm0 %>% filter(GID_0 != 'COD'), fill = "white", alpha = 0.6, color = NA) +
    geom_sf(data = invmask, fill = "grey40", alpha = 0.6, color = NA) +
    geom_sf(data = roi, fill = NA, color = "grey40", size = 0.7) +
    geom_sf(data = roi, fill = NA, color = "white")+
    coord_sf(xlim = c(bb$xmin, bb$xmax),
             ylim = c(bb$ymin, bb$ymax),
             expand = F,
             crs= sf::st_crs(3112)) +
    ggthemes::theme_map() +
    theme(
      axis.title = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.margin = margin(0,0,0,10),
      legend.text = element_text(size=rel(1.2)),
      legend.title = element_text(size=rel(1.2), margin=margin(0,10,0,0)),
      panel.background = element_rect(fill='#6A90C3', color=NA)
    ) +
    guides(fill = guide_colorbar(barheight = 0.8, barwidth = 5)) +
    
    scale_fill_viridis_c(option=palette,
                         direction=1,
                         name = legend_title,
                         limits = value_lims,
                         n.breaks = 2,
                         oob = scales::squish) 
    # colorspace::scale_fill_continuous_sequential(palette,
    #                                              na.value = "transparent",
    #                                              name = legend_title,
    #                                              rev = F,
    #                                              limits = value_lims,
    #                                              n.breaks = 2,
    #                                              oob = scales::squish) 
  
}

create_map <- function(params, polys=NULL, subject='carbon', downsample=5, palette='viridis'){
  
  # Load raster
  rr <- stars::read_stars(params$rast_fp)
  
  # Conditionally set limits for color scale
  if(subject=='carbon'){
    value_lims <- params$value_lims
    legend_title = "Carbon\n(tC/ha)"
  } else if(subject=='lci'){
    rr <- rr / 100
    value_lims <- c(0,100)
    legend_title = "Index\nvalue"
  } else {
    cat('subject argument "', subject, '" not recognized. Accepts values of "carbon" or "lci".')
    }
  
  # Load polygons if not already in environment
  if(is.null(polys)) polys <- prep_polys(fp_polys, st_crs(rr))
  
  # Convert raster to tibble for plotting
  clip_poly <- st_as_sfc(polys$bb)
  # clip_poly <- polys$adm0 %>% filter(GID_0 == 'COD')
  ar <- get_df(rr, clip_poly, downsample=downsample)
  
  # Plot
  p <- plot_map(ar, adm1=polys$adm1, adm0=polys$adm0, 
                bb=polys$bb, subject=subject, 
                value_lims, legend_title=legend_title,
                palette=palette)
  
  # Save
  ggsave(here::here(out_dir, paste0('map_', params$level, '.png')), 
         plot=p, width=8, height=8.2)
}

## Carbon maps ----
rr <- stars::read_stars(lci_tifs[[1]]$rast_fp)
polys <- prep_polys(fp_polys, st_crs(rr))

# Run for all carbon tifs
agb_tifs[3] %>% purrr::walk(create_map, 
                            polys=polys, 
                            subject='carbon', 
                            downsample=1, 
                            palette='viridis')

params <- agb_tifs[[3]]
# Plot
# ar <- stars::read_stars(agb_tifs[[3]]$rast_fp)
# 
# p <- plot_map(ar, polys$adm1, polys$adm0, bb=polys$bb, subject=subject, 
#               value_lims, legend_title=legend_title,
#               palette=palette)

## LCI maps ----
polys <- st_read(fp_polys)

# Run for all carbon tifs
lci_tifs[1] %>% purrr::walk(create_map, 
                            polys=polys, 
                            subject='lci', 
                            downsample=1, 
                            palette='viridis')

# Plot density ridges ----
# Density ridge plot
plot_densridg <- function(df, y='name') {
  df |> 
    ggplot(aes(x=value, y = .data[[y]], fill = stat(x))) +
    geom_density_ridges_gradient() +
    scale_fill_viridis_c(name = "Value", option = "C", guide=NULL) +
    scale_y_discrete(expand = expansion(mult = c(0, .1))) + 
    theme_ridges() +
    theme(axis.title = element_blank(), 
          axis.text = element_text(size = 10),
          panel.spacing=unit(1,"lines"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(color='white', fill='white', size=2))
}

## Plot composite index ----
df_p |> 
  filter(str_detect(comp_name, 'Final', negate=T)) |>
  plot_densridg(y='name')

df_p |> 
  filter(str_detect(comp_name, 'BQ')) |> 
  plot_densridg(y='name') +
  ggtitle('Biophysical Quality')

df_p |> 
  filter(str_detect(comp_name, 'BP')) |> 
  plot_densridg(y='name') +
  ggtitle('Biophysical Potential')

df_p |> 
  plot_densridg(y='comp_name') +
  ggtitle('Scores for Ikongo District')
