#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Calculate annual biomass loss for input polygons
# Requires:
#     * GEE account with access to global_AGB_2000_30m_Mgha_V4
#     * site polygon
#     * 02_report_from_Hansen_data.R for functions following the GEE section
# Author:
#     * esturdivant@woodwellclimate.org, 2021-10-10
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load libraries 
library(flextable)
library(officer)
library(sf)
library(patchwork)
library(segmented)
library(tidyverse)
library(tmap)

# Run GEE process
source('R/carbon_reports/01_calculate_loss_with_rgee.R')

# Load functions 
source('R/carbon_reports/02_functions.R')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load new sf for all sites ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data
sf_in <- st_read(polys_fp)
df_sf <- st_read(out_fp) %>% 
  bind_cols(name = sf_in$name)  |> 
  mutate(name = factor(name, levels = c("Leakage area 4", "Leakage area 3",
                                        "Leakage area 2", "Leakage area 1", 
                                        "Project area", "Feijó", "Acre")), 
         name = forcats::fct_rev(name))

# Get site and division names 
(site <- str_split(basename(out_fp), '[_\\W]', simplify = TRUE)[,1])
site_code <- site

# Get values by county ----
df_sf_sums <- df_sf %>% 
  mutate(c_2000_MtC = carbon_2000_mgc / 1e6, 
         c_loss_MtC = c_loss_mgc / 1e6,
         c_loss_pct = c_loss_MtC / c_2000_MtC * 100, 
         c_dens_2000_tCha = carbon_2000_mgc / forest_2000_ha,
         c_dens_loss_tCha = c_loss_mgc / forest_2000_ha,
         c_dens_loss_pct = c_dens_loss_tCha / c_dens_2000_tCha * 100,
         forest_loss_pct = forest_loss_ha / forest_2000_ha * 100
         ) %>%
  dplyr::select(any_of(site_div_var), 
         area_ha,
         forest_2000_ha, 
         forest_loss_ha,
         forest_loss_pct,
         c_2000_MtC,
         c_loss_MtC, 
         c_loss_pct,
         c_dens_2000_tCha, 
         c_dens_loss_tCha, 
         c_dens_loss_pct)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create output table of 20-year loss ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if( nrow(df_sf_sums) > 1 ) {
  # Totals row
  df_total <- df_sf_sums %>% 
    st_drop_geometry() %>% 
    summarize(
      across(any_of(ends_with('_ha')), ~ mean(.x, na.rm = TRUE)),
      across(any_of(ends_with('_MtC')), ~ mean(.x, na.rm = TRUE)),
      across(any_of(ends_with('_pct')), ~ mean(.x, na.rm = TRUE)),
      across(any_of(ends_with('_tCha')), ~ mean(.x, na.rm = TRUE))) %>% 
    mutate({{site_div_var}} := 'Mean')
  
  # Combine and change column names
  df_out <- df_sf_sums %>% 
    st_drop_geometry() %>% 
    bind_rows(df_total)
  
} else {
  
  df_out <- df_sf_sums %>% 
    st_drop_geometry()
}

# Format table ----
ft <- df_out %>% 
  mutate(
    across(any_of(ends_with('_ha')), ~ round(.x)),
    across(any_of(ends_with('_MtC')), ~ round(.x, 1)),
    across(any_of(ends_with('_pct')), ~ round(.x)),
    across(any_of(ends_with('_tCha')), ~ round(.x, 1)),
    forest_loss_ha = str_c(format(forest_loss_ha, big.mark = ","), 
                           str_glue('  ({forest_loss_pct}%)')),
    c_loss_MtC = str_glue('{c_loss_MtC}  ({c_loss_pct}%)'),
    c_dens_loss_tCha = str_c(c_dens_loss_tCha, str_glue('  ({c_dens_loss_pct}%)'))
  ) %>%
  dplyr::select(-area_ha, -forest_loss_pct, -c_loss_pct, -c_dens_loss_pct) %>% 
  flextable() %>% 
  mk_par(j = site_div_var, part = 'header', 
          value = as_paragraph('')) %>% 
  mk_par(j = 'forest_2000_ha', part = 'header', 
          value = as_paragraph('Forest area in 2000 (ha)')) %>% 
  mk_par(j = 'forest_loss_ha', part = 'header', 
          value = as_paragraph('Forest\narea loss\n(ha)')) %>% 
  mk_par(j = 'c_2000_MtC', part = 'header', 
          value = as_paragraph('Carbon stock in 2000 (MtC)')) %>% 
  mk_par(j = 'c_loss_MtC', part = 'header', 
          value = as_paragraph('Carbon\nstock loss\n(MtC)')) %>% 
  mk_par(j = 'c_dens_2000_tCha', part = 'header', 
          value = as_paragraph('Carbon density in 2000 (tC/ha)')) %>% 
  mk_par(j = 'c_dens_loss_tCha', part = 'header', 
          value = as_paragraph('Carbon density loss (tC/ha)')) %>% 
  align(j = 'forest_loss_ha', align = 'right', part = 'body') %>% 
  align(j = 'c_dens_loss_tCha', align = 'right', part = 'body') %>% 
  align(j = 'c_loss_MtC', align = 'right', part = 'body') %>% 
  font(fontname = 'Times New Roman', part = 'all') %>%
  fontsize(size = 10, part = 'all') %>%
  bold(part = 'header') %>% 
  align(part = 'header', align = 'center') %>% 
  valign(part = 'header', valign = 'bottom') %>% 
  align(j = site_div_var, align = 'left', part = 'header') %>% 
  bold(j = site_div_var) %>% 
  bg(part = 'body', i = seq(1, nrow(df_out), 2), bg = "#EFEFEF") %>% 
  hline_top(border = fp_border(width = 1), part = 'all') %>% 
  hline_bottom(border = fp_border(width = 1), part = 'all') %>% 
  hline(i = nrow(df_out)-1, border = fp_border(width = 1)) %>% 
  width(width = 0.9, unit = 'in') %>% 
  width(j = 'c_loss_MtC', width = 1, unit = 'in') %>% 
  width(j = 'c_dens_loss_tCha', width = 1, unit = 'in') %>% 
  width(j = 'forest_loss_ha', width = 1.1, unit = 'in') %>%
  width(j = 'forest_2000_ha', width = 0.7, unit = 'in') %>% 
  width(j = site_div_var, width = 0.85, unit = 'in')
ft

# Save in Word doc to copy to report
sums_doc <- here::here('outputs', site, 'loss_table_pcts.docx')
dir.create(dirname(sums_doc), showWarnings = FALSE)
save_as_docx(ft, path = sums_doc)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create piecewise regression plots ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tidy 
df <- df_sf %>% st_drop_geometry()
df_site <- tidy_forest_loss_df(df)

(div_names <- df_sf_sums[[site_div_var]])

# convert units to million metric tons of carbon (MtC) if it makes sense 
if (round(max(df_site$carbon_loss_MgC)/1e6) > 1) {
  df_site_t <- df_site %>% 
    mutate(c_loss_flex = carbon_loss_MgC / 1e6)
  
  c_units = 'MtC'
} else {
  df_site_t <- df_site %>% 
    mutate(c_loss_flex = carbon_loss_MgC)
  c_units = 'tC'
}

# Prep output DF for plotting
df_p <- df_site_t |> 
  mutate(carbon_loss_pct = carbon_loss_MgC / carbon_2000_mgc)
brks <- seq(2001, 2021, 2)
labels <- brks %>% str_sub(3,4) %>% str_c("'", .)

df_p |> 
  filter(name != 'Feijó' & name != 'Acre') %>% 
  ggplot(aes(x = year, y = c_loss_flex)) +
  geom_point(size = .3, color = 'grey30') +
  geom_line(color = 'grey30', size = .5) + 
  scale_x_continuous(name = "Year",
                     breaks = brks,
                     expand = c(0.01, 0.01),
                     labels = labels) +
  scale_y_continuous(name = str_c('Loss in aboveground carbon (', c_units, ')'),
                     labels = scales::comma
  ) +
  theme_minimal() +
  theme(
    # text = element_text(family = 'Times'),
    strip.text = element_text(face = 'bold'),
    axis.text = element_text(family = 'Helvetica'),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid.minor = element_blank())  +
  facet_wrap('name', 
             # scales='free_y', 
             ncol=1)

ggsave(file.path('outputs', site, str_c('carbon_loss_2001_2021_fixed.png')), 
       width = 4, height = 6.3, units='in')

df_p |> 
  filter(name != 'Feijó' & name != 'Acre') %>% 
  ggplot(aes(x = year, y = carbon_loss_pct)) +
  geom_point(size = .3, color = 'grey30') +
  geom_line(color = 'grey30', size = .5) + 
  scale_x_continuous(name = "Year",
                     breaks = brks,
                     expand = c(0.01, 0.01),
                     labels = labels) +
  scale_y_continuous(name = str_c('Percent loss in aboveground carbon (%)'),
                     labels = scales::percent
  ) +
  theme_minimal() +
  theme(
    # text = element_text(family = 'Times'),
    strip.text = element_text(face = 'bold'),
    axis.text = element_text(family = 'Helvetica'),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid.minor = element_blank())  +
  facet_wrap('name', 
             # scales='free_y', 
             ncol=1)

ggsave(file.path('outputs', site, str_c('carbon_loss_pct_2001_2021_fixed.png')), 
       width = 4, height = 6.3, units='in')

df_p |> 
  filter(name == 'Feijó' | name == 'Acre') %>% 
  # ggplot(aes(x = year, y = c_loss_flex)) +
  ggplot(aes(x = year, y = carbon_loss_pct)) +
  geom_point(size = .3, color = 'grey30') +
  geom_line(color = 'grey30', size = .5) + 
  scale_x_continuous(name = "Year",
                     breaks = brks,
                     expand = c(0.01, 0.01),
                     labels = labels) +
  # scale_y_continuous(name = str_c('Loss in aboveground carbon (', c_units, ')'),
  #                    labels = scales::comma
  scale_y_continuous(name = str_c('Percent loss in aboveground carbon (%)'),
                     labels = scales::percent
  ) +
  theme_minimal() +
  theme(
    # text = element_text(family = 'Times'),
    strip.text = element_text(face = 'bold'),
    axis.text = element_text(family = 'Helvetica'),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.grid.minor = element_blank())  +
  facet_wrap('name', 
             # scales='free_y', 
             ncol=1)

# ggsave(file.path('outputs', site, str_c('carbon_loss_2001_2021_muni_state_fixed.png')), 
#        width = 4, height = 4, units='in')
ggsave(file.path('outputs', site, str_c('carbon_loss_pct_01_21_muni_state_fixed.png')), 
       width = 4, height = 4, units='in')

# Plot piecewise
plots <- list()
for (i in 1:length(div_names)) {
  div_name <-  div_names[[i]]
  print('')
  print(paste0(i, ': ', div_name))
  
  df_zone <- filter(df_site_t, .data[[site_div_var]] == div_name)
  
  try(rm(pw_fit))
  pw_fit <- get_piecewise_line(df_zone)
  p <- plot_pw_fit(df_zone, div_name, pw_fit, brks, labels)
  try(rm(pw_fit))
  plots[[i]] <- p
  
}

params <- get_params(length(div_names))
y_max <- max(df_site_t$c_loss_flex)
y_maxr <- signif(y_max, 2) 
y_min <- round(y_maxr - y_max) * -2
formatted_plots <- layout_plots(plots, params, y_lim = c(y_min, y_maxr))
formatted_plots <- layout_plots(plots, params, y_lim = NULL)

ggsave(file.path('outputs', site, str_c('carbon_loss_2001_2020_pw.png')), 
       plot = formatted_plots,
       width = params$png_width,
       height = params$png_height)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Total piecewise regression plot ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_tot <- df_site %>% 
  group_by(year) %>% 
  select(any_of(c('carbon_loss_MgC', 'forest_loss_ha'))) %>% 
  summarize(across(where(is.double), ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup()

if (round(max(df_tot$carbon_loss_MgC)/1e6) > 1) {
  df_tot_t <- df_site %>% 
    mutate(c_loss_flex = carbon_loss_MgC / 1e6)
}

try(rm(pw_fit))
df_zone <- df_tot_t
pw_fit <- get_piecewise_line(df_tot_t)
p <- plot_pw_fit(df_tot_t, site, pw_fit)
try(rm(pw_fit))

# plots <- create_pw_plot_list(div_names, df_tot_t)
params <- get_params(1)
formatted_plots <- layout_plots(p, params, fix_y = FALSE)

ggsave(file.path('outputs', site, str_c(site_code, '_total_2001_2020_pw.png')), 
       plot = formatted_plots,
       width = params$png_width,
       height = params$png_height)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Map, choropleth by carbon loss ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_sf_sums %>% 
  st_write(here::here('outputs', site, 'counties_carbon_loss.gpkg'),
           append = FALSE)

tmap_mode('plot')
p1 <- tm_shape(df_sf_sums) + 
  tm_polygons(col = 'c_loss_MtC', palette = 'Reds', title = 'Carbon stock loss (MtC)')
p2 <- tm_shape(df_sf_sums) + 
  tm_polygons(col = 'c_loss_pct', palette = 'Reds', title = 'Carbon stock loss (%)')
