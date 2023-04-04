#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * View Carbon pool values distributions for input polygons - Cree Nation
# Requires:
#     * Biomass GeoTIFs 
#     * Polygons
# Author:
#     * esturdivant@woodwellclimate.org, 2023-03
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

poly_dir <- '~/data/misc/CreeNation/Eeyou_Istchee'
fp_polys <- here::here(poly_dir, 'Eeyou_Istchee.shp')
fp_polys <- here::here(poly_dir, 'EI_Community_extent_2015.shp')

# Prep parameters ----
df <- sf::st_read(fp_polys)
df <- df |> 
  mutate(name = 'Eeyou Istchee')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get parameters ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
params <- list(
  polys = fp_polys,
  site_var = 'name'
)
params <- list(
  polys = fp_polys,
  site_var = 'COMUNITY'
)

tifs_params <- list(
  list(
    rast_fp = '~/data/Walker_etal_2022/Base_Cur_AGB_MgCha_500m.tif',
    level = 'AGB_mgCha'
  ),
  list(
    rast_fp = '~/data/Walker_etal_2022/Base_Cur_AGB_BGB_MgCha_500m.tif',
    level = 'AGB_BGB_mgCha'
  ),
  list(
    rast_fp = '~/data/Walker_etal_2022/Base_Cur_SOC_MgCha_500m.tif',
    level = 'SOC_mgCha'
  ),
  list(
    rast_fp = '~/data/Walker_etal_2022/Base_Cur_AGB_BGB_SOC_MgCha_500m.tif',
    level = 'AGB_BGB_SOC_mgCha'
  )
)

# Get values for one raster layer ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fp_poly <- params$polys
site_var <- params$site_var

extract_values <- function(params) {
  # Read stars (proxy)
  rr <- stars::read_stars(params$rast_fp)
  
  # Pixel area in hectares to convert density to stock
  dims <- st_dimensions(rr)
  pix_ha <- abs(dims$x$delta) * abs(dims$y$delta) * 1e-4
  
  # Load and reproject polygons
  pols <- st_read(fp_poly) |> st_transform(st_crs(rr))
  
  # Get pixel values for each polygon ----
  df <- seq_len(nrow(pols)) |> 
    purrr::map_dfr(
      function(i) {
        pol <- pols[i,]
        
        # Extract values
        ar <- rr[pol] |> 
          stars::st_as_stars() |> 
          pull() |> 
          as.vector() |> 
          na.omit() |> 
          as_tibble() |> 
          mutate(name = pol[[site_var]],
                 value = value * pix_ha)
      }
    )
  
  # Add variable with LCI level name
  df |> mutate(level = params$level)
}

print_flextable <- function(df_out) {
  df_out %>%
    flextable() %>% 
    mk_par(j = 'name', part = 'header', 
           value = as_paragraph('')) %>% 
    mk_par(j = 'Dens_tCha', part = 'header', 
           value = as_paragraph('Carbon density (tC/ha)')) %>% 
    mk_par(j = 'Stock_MtC', part = 'header', 
           value = as_paragraph('Carbon stock (MtC)')) %>% 
    align(j = 'Dens_tCha', align = 'right', part = 'body') %>% 
    align(j = 'Stock_MtC', align = 'right', part = 'body') %>% 
    font(fontname = 'Helvetica Neue', part = 'all') %>%
    fontsize(size = 9, part = 'all') %>%
    bold(part = 'header') %>% 
    align(part = 'header', align = 'center') %>% 
    valign(part = 'header', valign = 'bottom') %>% 
    align(j = 'name', align = 'left', part = 'header') %>% 
    set_formatter_type(fmt_double = "%.0f") %>% 
    bold(j = 'name') %>% 
    bg(part = 'body', i = seq(1, nrow(df_out), 2), bg = "#EFEFEF") %>% 
    hline_top(border = fp_border(width = 1), part = 'all') %>% 
    hline_bottom(border = fp_border(width = 1), part = 'all') %>% 
    hline(i = nrow(df_out)-1, border = fp_border(width = 1)) %>%
    width(width = 1.1, unit = 'in') %>% 
    # width(j = 'Stock_MtC', width = 0.7, unit = 'in') %>% 
    width(j = 'name', width = 1.1, unit = 'in')
}

print_table_by_pool <- function(df, pool_name) {
  df <- df %>% 
    filter(comp_name == pool_name) %>% 
    ungroup() %>% 
    select(c(name, Dens_tCha, Stock_MtC))
  
  df %>% 
    bind_rows(tibble(name = 'Total', Stock_MtC = sum(df$Stock_MtC, na.rm = TRUE))) %>% 
    print_flextable()
}

# Extract values ----
df_p <- tifs_params |> purrr::map_dfr(extract_values) |> 
  mutate(comp_name = factor(level, levels = purrr::map_chr(tifs_params, ~.x$level))) 

# Summary stats ----
pix.ha <- 463.3127 * 463.3127 * 1e-4
stats <- df_p |> 
  # mutate(name = forcats::fct_rev(name)) |> 
  group_by(comp_name, name) |> 
  summarize(Min = min(value, na.rm=TRUE),
            Q1 = quantile(value, 0.25, na.rm=TRUE),
            Median = median(value, na.rm=TRUE),
            Mean = mean(value, na.rm=TRUE),
            Q3 = quantile(value, 0.75, na.rm=TRUE),
            Max = max(value, na.rm=TRUE),
            N = n(),
            Stock_tC = sum(value, na.rm=TRUE), 
            Area_ha = N * pix.ha, 
            Dens_tCha = Stock_tC / Area_ha, 
            Stock_MtC = Stock_tC * 1e-6, 
            Area_Mha = Area_ha * 1e-6)

pool_name <- 'AGB_BGB_mgCha'
png_fp <- here::here('cree_nation', 'outputs', paste0('table_', pool_name, '.png'))
ft <- stats %>% print_table_by_pool(pool_name)
save_as_image(ft, png_fp)

pool_name <- 'SOC_mgCha'
png_fp <- here::here('cree_nation', 'outputs', paste0('table_', pool_name, '.png'))
ft <- stats %>% print_table_by_pool(pool_name)
save_as_image(ft, png_fp)

pool_name <- 'AGB_BGB_SOC_mgCha'
png_fp <- here::here('cree_nation', 'outputs', paste0('table_', pool_name, '.png'))
ft <- stats %>% print_table_by_pool(pool_name)
save_as_image(ft, png_fp)

# Plot density ridges ----
# Density ridge plot
plot_densridg <- function(df) {
  df |> 
    ggplot(aes(x=value, y = comp_name, fill = stat(x))) +
    geom_density_ridges_gradient() +
    scale_fill_viridis_c(name = "Value", option = "C", 
                         guide=NULL
                         ) +
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
  filter(str_detect(comp_name, 'SOC', negate=T)) |> 
  plot_densridg()

df_p |> 
  filter(str_detect(comp_name, 'SOC')) |> 
  plot_densridg()