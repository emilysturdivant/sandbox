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
out_dir <- here::here('drc_202305', 'outputs')

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

agb_tifs <- list(
  list(
    rast_fp = '~/data/Walker_etal_2022/Base_Cur_AGB_BGB_MgCha_500m.tif',
    level = 'AGB_BGB_mgCha',
    value_lims = c(0, 250)
  ),
  list(
    rast_fp = '~/data/Walker_etal_2022/Base_Cur_SOC_MgCha_500m.tif',
    level = 'SOC_mgCha',
    value_lims = c(0, 400)
  ),
  list(
    rast_fp = '~/data/Walker_etal_2022/Base_Cur_AGB_BGB_SOC_MgCha_500m.tif',
    level = 'AGB_BGB_SOC_mgCha',
    value_lims = c(0, 550)
  )
)

lci_tifs <- list(
  list(
    rast_fp = '~/repos/carbon-quality-index/data/aggregated/allranked/comp_bq.tif',
    level = 'BQ'
  ),
  list(
    rast_fp = '~/repos/carbon-quality-index/data/aggregated/allranked/comp_bp.tif',
    level = 'BP'
  ),
  list(
    rast_fp = '~/repos/carbon-quality-index/data/aggregated/allranked/comp_add.tif',
    level = 'Add'
  ),
  list(
    rast_fp = '~/repos/carbon-quality-index/data/aggregated/allranked/comp_perm.tif',
    level = 'Perm'
  ),
  list(
    rast_fp = '~/repos/carbon-quality-index/data/aggregated/allranked/final.tif',
    level = 'Final'
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
df_p <- agb_tifs %>% 
  purrr::map_dfr(extract_values) %>% 
  mutate(comp_name = factor(level, levels = purrr::map_chr(agb_tifs, ~.x$level))) 

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

pool_name <- 'AGB_BGB_mgCha'
png_fp <- here::here(out_dir, paste0('table_', pool_name, '.png'))
ft <- stats %>% print_table_by_pool(pool_name)
save_as_image(ft, png_fp)

total_woody <- totals %>% 
  filter(comp_name == pool_name) %>% 
  mutate(text = str_glue("The region stores {stock_str} metric tons (tC) of \\
              woody biomass carbon aboveground (leaves, branches, stems) and \\
              belowground (roots) with an average density of {dens_str} tons \\
              of carbon per hectare (tC/ha)*."), 
         footer = str_glue("*Based on a non-water land area of {area_str} hectares.")
         )
total_woody %>% pull(text)
totals %>% pull(footer)


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
png_fp <- here::here(out_dir, paste0('table_', pool_name, '.png'))
ft <- stats %>% print_table_by_pool(pool_name)
save_as_image(ft, png_fp)
totals %>% filter(comp_name == pool_name) %>% pull(text)
totals %>% filter(comp_name == pool_name) %>% pull(footer)

pool_name <- 'AGB_BGB_SOC_mgCha'
png_fp <- here::here(out_dir, paste0('table_', pool_name, '.png'))
ft <- stats %>% print_table_by_pool(pool_name)
save_as_image(ft, png_fp)
totals %>% filter(comp_name == pool_name) %>% pull(text)
totals %>% filter(comp_name == pool_name) %>% pull(footer)

# Map ----
## Functions----
prep_polys <- function(adm1_fp, crs=st_crs('epsg:4326')) {
  adm1 <- st_read(adm1_fp, layer='ADM_ADM_1') %>% 
    st_transform(crs)
  
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
                     value_lims=c(0,250), legend_title=NULL, palette='viridis'){
  
  roi <- adm0 %>% filter(GID_0 == 'COD')
  
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
             expand = F) +
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
  p <- plot_map(ar, polys$adm1, polys$adm0, bb=polys$bb, subject=subject, 
                value_lims, legend_title=legend_title,
                palette=palette)
  
  # Save
  ggsave(here::here(out_dir, paste0('map_', params$level, '.png')), 
         plot=p, width=8, height=8.2)
}

## Carbon maps ----
rr <- stars::read_stars(agb_tifs[[1]]$rast_fp)
polys <- prep_polys(fp_polys, st_crs(rr))

# Run for all carbon tifs
agb_tifs %>% purrr::walk(create_map, 
                            polys=polys, 
                            subject='carbon', 
                            downsample=1, 
                            palette='viridis')

## LCI maps ----
# Run for all carbon tifs
lci_tifs[1] %>% purrr::walk(create_map, 
                            polys=polys, 
                            subject='lci', 
                            downsample=1, 
                            palette='viridis')

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