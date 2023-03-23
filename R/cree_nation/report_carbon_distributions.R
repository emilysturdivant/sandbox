#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * View LCI values distributions for input polygons
# Requires:
#     * LCI GeoTIFs 
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

poly_dir <- '~/data/misc/CreeNation/Eeyou_Istchee'
fp_polys <- here::here(poly_dir, 'Eeyou_Istchee.shp')

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

# Extract values ----
df_p <- tifs_params |> purrr::map_dfr(extract_values) |> 
  mutate(comp_name = factor(level, levels = purrr::map_chr(tifs_params, ~.x$level))) 

# Summary stats ----
pix.ha <- 463.3127 * 463.3127 * 1e-4
stats <- df_p |> 
  # mutate(name = forcats::fct_rev(name)) |> 
  group_by(comp_name) |> 
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