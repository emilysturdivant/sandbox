#!/usr/bin/env Rscript
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Script:
#   plot_extracted_metrics.R
#
# Description:
#   Create graphs from geoJSON files exported from GEE script HIH/Test5b_forestIPLCs_buffer
# 
# Author:
#   Emily Sturdivant - Jan 2024
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(jsonlite)

# Create graphs from GEE GeoJSONs ----
# ~~~~~~~~~~~~~~~~

## area, carbon, and population from Test5b_forestIPLCs_buffer ----
# Load results
results <- jsonlite::read_json(here::here('hih/scaling/tropicalIPLC_forests_and_buffer_sums.geojson'))
out <- results$features[[1]]$properties
tbl <- out %>% purrr::map_dfr(as_tibble)

# Format DF
df2 <- tbl %>% 
  rename(continent='CONTINENT') %>% 
  mutate(area_b = area_fb - area_f, 
         pop_b = pop_fb - pop_f,  
         carbon_b = carbon_fb - carbon_f) %>% 
  mutate(across(matches('area|pop'), ~ .x / 1e6)) %>% 
  pivot_longer(area_b:pop_fb, values_to='value', names_sep = '_', names_to=c('unit', 'zone')) %>% 
  mutate(unit = case_when(unit == 'area' ~ 'Area (million ha)',
                          unit == 'pop' ~ 'Population (million people)',
                          unit == 'carbon' ~ 'Carbon (PgC)', 
                          TRUE ~ unit), 
         zone = case_when(zone == 'b' ~ '2 km forest buffer', 
                          zone == 'f' ~ 'IPLC tropical forests', 
                          zone == 'fb' ~ 'forestplus', 
                          TRUE ~ zone), 
         # zone = factor(zone, levels=c('buffer', 'forest'), ordered=T)
         continent = factor(continent, levels=c('Americas', 'Africa', 'Australasia'), ordered=T)
         ) %>% 
  arrange(desc(zone))

# Plot
df2 %>% 
  filter(zone!='forestplus') %>% 
  ggplot(aes(x=continent, y=value, group=continent, fill=zone)) + 
  scale_y_continuous(labels=scales::comma, name=NULL) +
  scale_x_discrete(name=NULL) +
  scale_fill_discrete(name=NULL) +
  geom_col() +
  facet_wrap(vars(unit), nrow=3, scales='free_y') +
  cowplot::theme_cowplot()

# Save plot
ggsave(here::here('hih/outputs/scaling', 'forestbuffer_continent.png'),
       width = 6, height = 8)

# Save CSV
df2 %>% 
  write_csv(here::here('hih/scaling/tropicalIPLC_forests_and_buffer_sums.csv'))

# Look at values
df2 %>% filter(str_detect(unit, 'Population')) %>% 
  group_by(zone) %>% 
  summarize(sum(value))

## Additionality component scores from Test6_risk_threshold ----
results <- jsonlite::read_json(here::here('hih/scaling/additionality_summarized (2).geojson'))
tbl <- results$features[[1]]$properties %>% purrr::map_dfr(as_tibble)

df2 <- tbl %>% 
  # rename(continent='CONTINENT') %>% 
  mutate(area_gt90_buffer = area_gt90_forestbuff - area_gt90_forIPLC) %>% 
  mutate(area_gt70_buffer = area_gt70_forestbuff - area_gt70_forIPLC) %>% 
  # select(!starts_with('mean')) %>% 
  mutate(across(starts_with('area'), ~ .x / 1e6)) %>% 
  pivot_longer(starts_with('area'), values_to='value', names_sep = '_', names_to=c('unit', 'threshold', 'zone')) %>% 
  mutate(unit = case_when(unit == 'area' ~ 'Area (million ha)',
                          TRUE ~ unit), 
         zone = case_when(zone == 'biome' ~ 'Biome', 
                          zone == 'forest' ~ 'Tropical forests',  
                          zone == 'IPLC' ~ 'IPLC lands', 
                          zone == 'forIPLC' ~ 'IPLC tropical forests', 
                          zone == 'buffer' ~ '2 km forest buffer',
                          zone == 'forestbuff' ~ 'In or <2 km from IPLC tropical forests',
                          TRUE ~ zone), 
         threshold = case_when(threshold == 'gt70' ~ '70th percentile', 
                               threshold == 'gt90' ~ '90th percentile', 
                          TRUE ~ threshold), 
         # zone = factor(zone, levels=c('buffer', 'forest'), ordered=T)
         continent = factor(continent, levels=c('Americas', 'Africa', 'Australasia'), ordered=T)
  ) %>% 
  arrange(desc(zone))

df2 %>% 
  filter(zone %in% c('2 km forest buffer', 'IPLC tropical forests')) %>% 
  ggplot(aes(x=continent, y=value, group=continent, fill=zone)) + 
  scale_y_continuous(labels=scales::comma, name='Area (million ha)') +
  scale_x_discrete(name=NULL) +
  scale_fill_discrete(name=NULL) +
  geom_col() +
  facet_wrap(vars(threshold)) +
  cowplot::theme_cowplot()

ggsave(here::here('hih/scaling/outputs', 'addit_gt70gt90_forest_buffer.png'),
       width = 8, height = 4)

# Save
df2 %>% 
  write_csv(here::here('hih/scaling/addit_gt70gt90_forest_buffer.csv'))

df2 %>% 
  group_by(zone, threshold) %>% 
  summarize(area_Mha = sum(value)) %>% 
  filter(zone %in% c('In or <2 km from IPLC tropical forests', 'IPLC tropical forests')) %>% 
  pivot_wider(names_from=threshold, values_from=area_Mha) %>% 
  write_csv(here::here('hih/scaling/addit_table_gt70gt90_forest_buffer.csv'))
