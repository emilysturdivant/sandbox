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

home_dir <- here::here('drc_202305')

poly_dir <- '/Volumes/ejs_storage/data/raw_data/world_context/gadm'
fp_polys <- here::here(poly_dir, 'gadm41_COD.gpkg')

working_dir <- here::here(home_dir, 'working_data')
dir.create(working_dir, recursive=TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get parameters ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
params <- list(
  polys = fp_polys,
  site_var = 'name'
)
params <- list(
  polys = fp_polys,
  site_var = 'NAME_1'
)

tifs_params <- list(
  # list(
  #   rast_fp = '~/data/Walker_etal_2022/Base_Cur_AGB_MgCha_500m.tif',
  #   level = 'AGB_mgCha'
  # ),
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
  pols <- st_read(fp_poly, layer='ADM_ADM_1') |> st_transform(st_crs(rr))
  
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
df_p <- tifs_params %>% 
  purrr::map_dfr(extract_values) %>% 
  mutate(comp_name = factor(level, levels = purrr::map_chr(tifs_params, ~.x$level))) 

df_p %>% saveRDS(here::here(working_dir, 'extracted_vals.rds'))
df_p <- readRDS(here::here(working_dir, 'extracted_vals.rds'))

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

fn <- scales::label_number(scale_cut = scales::cut_si(""))
totals <- stats %>% 
  summarize(Area_ha = sum(Area_ha), 
            Stock_tC = sum(Stock_tC), 
            cdens_tCha = Stock_tC / Area_ha) %>% 
  mutate(area_str = fn(Area_ha), 
         stock_str = fn(Stock_tC), 
         dens_str = fn(cdens_tCha), 
         across(where(is.character), ~ 
                  str_replace_all(.x, 'M', 'million') %>% 
                  str_replace_all('G', 'billion')))
totals <- totals %>% 
  mutate(text = str_glue("The region stores {stock_str} metric tons (tC) of \\
  woody biomass carbon aboveground (leaves, branches, stems) and \\
  belowground (roots) with an average density of {dens_str} tons \\
  of carbon per hectare (tC/ha)*."), 
         footer = str_glue("*Based on a non-water land area of {area_str} hectares."))

pool_name <- 'AGB_BGB_mgCha'
png_fp <- here::here('drc_202305', 'outputs', paste0('table_', pool_name, '.png'))
ft <- stats %>% print_table_by_pool(pool_name)
save_as_image(ft, png_fp)
totals %>% filter(comp_name == pool_name) %>% pull(text)
totals %>% filter(comp_name == pool_name) %>% pull(footer)


# data_long <- stats %>%
#   mutate(
#     group = cut(
#       seq_len(n()), 3, labels = paste("Set", 1:3))
#   ) %>%
#   group_by(group) %>%
#   mutate(row = seq_len(n()))
# data_long %>% select(row)
# 
# colfun <- function(z) {
#   cols <- rep("transparent", length(z))
#   cols[z %in% "C"] <- "#FF9999"
#   cols[z %in% "D"] <- "#FF0000"
#   cols
# }
# 
# 
# tab <- tabulator(
#   x = data_long,
#   rows = "row",
#   columns = "group",
#   `Col A` = as_paragraph(Col.A),
#   `Col B` = as_paragraph(Col.B),
#   `Col C` = as_paragraph(Col.C),
# )
# 
# visibles_a <- tabulator_colnames(tab, columns = "Col A", type = "columns")
# visibles_b <- tabulator_colnames(tab, columns = "Col B", type = "columns")
# visibles_c <- tabulator_colnames(tab, columns = "Col C", type = "columns")
# 
# hidden_a <- tabulator_colnames(tab, columns = "Col.A", type = "hidden")
# hidden_b <- tabulator_colnames(tab, columns = "Col.B", type = "hidden")
# hidden_c <- tabulator_colnames(tab, columns = "Col.C", type = "hidden")
# 
# ft <- as_flextable(tab)
# ft <- bg(
#   x = ft,
#   j = visibles_c,
#   bg = colfun,
#   source = hidden_c,
#   part = "body"
# ) %>%
#   bg(
#     j = visibles_b,
#     bg = colfun,
#     source = hidden_c,
#     part = "body"
#   ) %>%
#   bg(
#     j = visibles_a,
#     bg = colfun,
#     source = hidden_c,
#     part = "body"
#   )


pool_name <- 'SOC_mgCha'
png_fp <- here::here('drc_202305', 'outputs', paste0('table_', pool_name, '.png'))
ft <- stats %>% print_table_by_pool(pool_name)
save_as_image(ft, png_fp)
totals %>% filter(comp_name == pool_name) %>% pull(text)
totals %>% filter(comp_name == pool_name) %>% pull(footer)

pool_name <- 'AGB_BGB_SOC_mgCha'
png_fp <- here::here('drc_202305', 'outputs', paste0('table_', pool_name, '.png'))
ft <- stats %>% print_table_by_pool(pool_name)
save_as_image(ft, png_fp)
totals %>% filter(comp_name == pool_name) %>% pull(text)
totals %>% filter(comp_name == pool_name) %>% pull(footer)

# Map ----

## Prep polygons ----
adm1 <- st_read(fp_poly, layer='ADM_ADM_1') %>% 
  st_transform(st_crs(rr))

# Get bounding box as st_bbox, sfc, and wkt
bb <- adm1 %>% st_buffer(5000) %>% st_bbox()
bb_sf <- bb %>% st_as_sfc()
bb_wkt = bb_sf %>% 
  st_geometry() %>% 
  st_transform(st_crs('epsg:4326')) %>% 
  st_as_text()

adm0_shp <- here::here(poly_dir, 'gadm36_0.shp')
adm0 <- st_read(adm0_shp, wkt_filter=bb_wkt) %>% 
  st_transform(st_crs(rr)) %>% 
  st_simplify(dTolerance=1000)

## Raster ----
params <- tifs_params[[1]]
value_lims <- c(0, 250)

params <- tifs_params[[2]]
value_lims <- c(0,400)

params <- tifs_params[[3]]
value_lims <- c(0,550)

# Read stars (proxy)
rr <- stars::read_stars(params$rast_fp)

# Extract values
ar <- rr[bb_sf] |> 
  stars::st_as_stars(downsample = 8) %>%
  as_tibble() %>% 
  rename(mgcha = 3) %>% 
  filter(!is.na(mgcha))

## Map ----
p <-
  ar %>%
  ggplot() +
  geom_raster(aes(x,y, fill = mgcha)) +
  
  geom_sf(data = adm1 %>% select(1), fill = NA, color = "grey40", size = 0.5) +
  geom_sf(data = adm1 %>% select(1), fill = NA, color = "white", size = 0.2) +
  geom_sf(data = adm0 %>% filter(GID_0 != 'COD'), fill = "white", alpha = 0.4, color = NA) +
  # geom_sf(data = adm0, fill = NA, color = "grey40", size = 0.4) +
  geom_sf(data = adm0, fill = NA, color = "white") +
  geom_sf(data = adm0 %>% filter(GID_0 == 'COD'), fill = NA, color = "grey40", size = 0.7) +
  geom_sf(data = adm0 %>% filter(GID_0 == 'COD'), fill = NA, color = "white") +
  
  colorspace::scale_fill_continuous_sequential("viridis",
                                               na.value = "transparent",
                                               name = "Carbon\n(tC/ha)",
                                               rev = F,
                                               limits = value_lims,
                                               oob = scales::squish
  ) +
  coord_sf(xlim = c(bb$xmin, bb$xmax),
           ylim = c(bb$ymin, bb$ymax),
           expand = F) +
  ggthemes::theme_map() +
  theme(
    axis.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
    legend.margin = margin(0,0,0,0),
        panel.background = element_rect(fill='#6A90C3', color=NA)
        ) +
  
  guides(fill = guide_colorbar(barheight = 0.8, barwidth = 10))
p

ggsave(here::here(home_dir, 'outputs', paste0('map_', params$level, '.png')), 
       plot=p, width=8, height=8.2)

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