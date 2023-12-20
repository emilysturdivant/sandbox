# Work with HIH site polygons


library(tidyverse)
library(sf)
library(stringr)
library(units)
library(tmap)
tmap_mode('view')

sites <- st_read('~/data/hih_sites/HIH_RX_sites_feb2023.geojson')

# Filter out sites that aren't currently included on Rainforest Exchange
sites %>% 
  filter( !(rx_id %in% c(22,21,20,18,17,10,9,8,7)))

# Merge each site into one polygon
sites_diss <- sites %>% 
  filter(rx_id %in% c(22,21,20,18,17,10,9,8,7)) %>% 
  group_by(rx_id, site_code, site_id, country, site_name) %>% 
  summarize() %>% 
  ungroup() %>% 
  st_zm(drop=T) %>% 
  mutate(area_ha = st_area(geometry) %>% set_units(ha) %>% drop_units())

# Look
qtm(sites_diss, fill_alpha=0.5)

# Save as individual shapefiles
for( i in 1:nrow(sites_diss) ) {
  s <- sites_diss %>% slice(i)
  fp <- here::here('~/data/hih_sites/HIH_RX_sites_feb2023_shps', 
                   str_c(s$site_code, '.shp'))
  print(fp)
  s %>% st_write(fp)
}

# Areas
# Get total area and range of areas
sites_diss %>% 
  arrange(area_ha)

sites_diss %>% 
  st_drop_geometry() %>% 
  summarize(min = min(area_ha),
            max = max(area_ha), 
            total = sum(area_ha))

# Bar chart
sites_diss %>% 
  ggplot(aes(area_ha)) +
  geom_histogram(col='white') + #binwidth=15000, ) +
  scale_x_continuous(name='Area (ha)', labels=scales::comma, 
                     expand=expansion(mult=c(0.01, 0.05))) +
  scale_y_continuous(name='Count', breaks=seq(0,2), minor_breaks=NULL, 
                     expand=expansion(mult=c(0.01, 0.05))) +
  theme_bw()

# Bar chart
sites_diss %>% 
  ggplot(aes(y=area_ha, x=fct_reorder(site_name, area_ha))) +
  geom_col(orientation='x') +
  scale_y_continuous(name='Area (ha)', labels=scales::comma,
                     expand=expansion(mult=c(0.01, 0.05))) +
  scale_x_discrete(name=NULL) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

