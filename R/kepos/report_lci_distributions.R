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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get parameters for each site ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
poly_dir <- '~/data/misc/Kepos_examplecarbonoffsetproject'
fp_polys <- here::here(poly_dir, 'envira_polys.geojson')

# Pre-process Envira polygons from Kepos ----
polys <- list.files(poly_dir, '.geojson') |> 
  purrr::map_dfr(function(x) {
    df <- sf::st_read(here::here(poly_dir, x)) |> 
      sf::st_make_valid()
    cols <- c(name = NA_character_, ADM2_PT = NA_character_, ADM1_PT = NA_character_)
    df <- add_column(df, !!!cols[setdiff(names(cols), names(df))])
    df |> mutate(filename = x, 
             name = coalesce(name, ADM2_PT, ADM1_PT))
    })
polys |> sf::st_write(fp_polys)


params <- list(
  polys = fp_polys,
  site_var = 'name'
)

lci_tifs <- list(
  list(
    rast_fp = '~/repos/carbon-quality-index/data/aggregated/allranked/final.tif',
    level = 'Composite Index'
  ),
  list(
    rast_fp = '~/repos/carbon-quality-index/data/aggregated/allranked/comp_bq.tif',
    level = 'Biophysical Quality'
  ),
  list(
    rast_fp = '~/repos/carbon-quality-index/data/aggregated/allranked/comp_bp.tif',
    level = 'Biophysical Potential'
  ),
  list(
    rast_fp = '~/repos/carbon-quality-index/data/aggregated/allranked/comp_add.tif',
    level = 'Additionality Potential'
  ),
  list(
    rast_fp = '~/repos/carbon-quality-index/data/aggregated/allranked/comp_perm.tif',
    level = 'Risk to Permanence'
  )
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All sites in one DF ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get values for one raster layer ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fp_poly <- params$polys
site_var <- params$site_var

extract_values <- function(params) {
  # Read stars (proxy)
  rr <- stars::read_stars(params$rast_fp)
  
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
          mutate(name = pol[[site_var]])
      }
    )
  
  # Add variable with LCI level name
  df |> mutate(level = params$level)
}

# Density ridge plot
plot_densridg <- function(df) {
  df |> 
    ggplot(aes(x=value, y = name, fill = stat(x))) +
    geom_density_ridges_gradient(
      bandwidth = 1, 
      rel_min_height = 0.0005, 
      show.legend = F,
      panel_scaling = T) +
    scale_fill_viridis_c(name = "Value", option = "C", limits = c(0,100)) +
    scale_x_continuous(limits = c(0,100)) + 
    scale_y_discrete(expand = expansion(mult = c(0, .1))) + 
    theme_ridges() +
    theme(axis.title = element_blank(), 
          axis.text = element_text(size = 10),
          panel.spacing=unit(1,"lines"),
          strip.text = element_text(face = "bold"),
          strip.background = element_rect(color='white', fill='white', size=2)) +
    facet_wrap('comp_name', ncol=2)
}

df <- lci_tifs |> purrr::map_dfr(extract_values)
    
# Prep output DF for plotting
df_p <- df |> 
  mutate(value = value * 0.01, 
         name = str_replace_all(name, 'Leakage area', 'LA'),
         name = factor(name, levels = c("LA 4", "LA 3",
                                        "LA 2", "LA 1", 
                                        "Project area", "Feijó", "Acre")), 
         comp_name = factor(level, levels = purrr::map_chr(lci_tifs, ~.x$level)),
         level = case_when(comp_name == 'Composite Index' ~ 'final', 
                           TRUE ~ 'component'),
         ) 

# Summary stats ----
stats <- df_p |> 
  mutate(name = forcats::fct_rev(name)) |> 
  group_by(name, comp_name) |> 
  summarize(Min = min(value),
            Q1 = quantile(value, 0.25),
            Median = median(value),
            Mean = mean(value),
            Q3 = quantile(value, 0.75),
            Max = max(value))

stats |> filter(str_detect(comp_name, 'Composite')) |> 
  readr::write_csv(here::here('outputs', 'LCI_EnviraAmazonia', 'stats', 'lci_final.csv'))
stats |> filter(str_detect(comp_name, 'Quality')) |> 
  select(!comp_name) |> 
  readr::write_csv(here::here('outputs', 'LCI_EnviraAmazonia', 'stats', 'lci_comp_bq.csv'))
stats |> filter(str_detect(comp_name, 'Biophysical Potential')) |> 
  select(!comp_name) |> 
  readr::write_csv(here::here('outputs', 'LCI_EnviraAmazonia', 'stats', 'lci_comp_bp.csv'))
stats |> filter(str_detect(comp_name, 'Additionality')) |> 
  select(!comp_name) |> 
  readr::write_csv(here::here('outputs', 'LCI_EnviraAmazonia', 'stats', 'lci_comp_add.csv'))
stats |> filter(str_detect(comp_name, 'Permanence')) |> 
  select(!comp_name) |> 
  readr::write_csv(here::here('outputs', 'LCI_EnviraAmazonia', 'stats', 'lci_comp_perm.csv'))

# Plot density ridges ----
## Plot composite index ----
df_p |> 
  filter(level == 'final') |> 
  plot_densridg()
ggsave(here::here('outputs', 'LCI_EnviraAmazonia', 'lci_final_densridg.png'),
       width = 3, height = 2.25)

## Plot components all together -----
df_p |> 
  filter(level == 'component') |> 
  plot_densridg()
ggsave(here::here('outputs', 'LCI_EnviraAmazonia', 'lci_comps_densridg_panscalF.png'),
       width = 6, height = 4.5)

p + facet_wrap('comp_name', ncol=1)
ggsave(here::here('outputs', 'LCI_EnviraAmazonia', 'lci_comps_densridg_panscalF_1col.png'),
       width = 348, height = 629, units='px')

## Plot components separately -----

df_p |> filter(str_detect(comp_name, 'Composite')) |> 
  plot_densridg()
ggsave(here::here('outputs', 'LCI_EnviraAmazonia', 'lci_final.png'),
       width = 3.3, height = 2.8, units='in')

df_p |> filter(str_detect(comp_name, 'Biophysical Quality')) |> 
  plot_densridg()
ggsave(here::here('outputs', 'LCI_EnviraAmazonia', 'lci_comp_bq.png'),
       width = 5.2, height = 3.3, units='in')
ggsave(here::here('outputs', 'LCI_EnviraAmazonia', 'lci_comp_bq.png'),
       width = 3.3, height = 2.8, units='in')

df_p |> filter(str_detect(comp_name, 'Biophysical Potential')) |> 
  plot_densridg()
ggsave(here::here('outputs', 'LCI_EnviraAmazonia', 'lci_comp_bp.png'),
       width = 3.3, height = 2.8, units='in')

df_p |> filter(str_detect(comp_name, 'Additionality Potential')) |> 
  plot_densridg()
ggsave(here::here('outputs', 'LCI_EnviraAmazonia', 'lci_comp_add.png'),
       width = 3.3, height = 2.8, units='in')

df_p |> filter(str_detect(comp_name, 'Permanence')) |> 
  plot_densridg()
ggsave(here::here('outputs', 'LCI_EnviraAmazonia', 'lci_comp_perm.png'),
       width = 3.3, height = 2.8, units='in')

# Facetted histograms ----
df_p |> 
  mutate(name = forcats::fct_rev(name)) |> 
  ggplot(aes(x=value, y = ..density..)) +
  geom_histogram() +
  facet_wrap('name') + 
  theme_minimal() +
  # theme(axis.title = element_blank()) +
  ggtitle("Composite Index")

ggsave(here::here('outputs', 'LCI_EnviraAmazonia', 'lci_histdens.png'),
       width = 5, height = 4)

# Histograms
df_filt <- df_p |> 
  filter(level == 'final') |> 
  filter(name != 'Acre' & name != 'Feijó')  |> 
  mutate(name = forcats::fct_rev(name))

# Annotations
vlines <- df_filt |> 
  group_by(name, comp_name) |> 
  summarize(Min = min(value),
            Q1 = quantile(value, 0.25),
            Median = median(value),
            Mean = mean(value),
            Q3 = quantile(value, 0.75),
            Max = max(value)) |> 
  pivot_longer(cols = Min:Max, names_to='label') %>% 
  mutate(lty = ifelse(label %in% c('Mean', 'Median'), 'solid', 'dashed'))

g <- df_filt |> 
  ggplot(aes(x=value, fill=stat(x))) +
  geom_histogram(show.legend = F) + 
  scale_fill_viridis_c(name = "Value", option = "C", limits = c(0,100)) +
  scale_x_continuous(limits = c(0,100), position = "top") + 
  scale_y_continuous(expand = expansion(mult=c(0.2, 0))) + 
  theme_minimal() +
  theme(axis.title = element_blank(), 
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(color=NA, fill=NA, size=2)) +
  facet_wrap('name', ncol=2, scales='free_y', strip.position="bottom")

g +
  # 2nd layer: Vertical lines for stat markers
  geom_linerange(data = vlines, 
               aes(x = value, linetype = lty), 
               inherit.aes = FALSE, 
               ymin = -50, 
               ymax = 0) +
  scale_linetype_identity() + 
  
  # 3rd layer: Annotations for stats lines
  geom_text(data = vlines, 
            aes(x = value, label = label, y = -50), 
            position = position_dodge(width=.9),
            inherit.aes = FALSE, 
            hjust = 0,
            angle = -45, 
            size = rel(2.5))
  