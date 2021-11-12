
library(stars)
library(terra)
library(colorspace)
library(tidyverse)
library(spData)

# Initialize variables 
# Woodwell Carbon and Tropics palettes
pal_arctic <- c('#d4e5e6', '#afd2d4', '#8ac1c4', '#62b2b5', '#24a4a6', '#0d7072', '#15c9ca')
pal_risk <- c('#f6e9cd', '#f0daa4', '#ebcb7d', '#e5be52', '#dfb125', '#937613', '#f6cf1c')
pal_carbon <- c('#d8d4cd', '#bab4a7', '#9d9786', '#847f6b', '#6b6a52')
pal_tropics <- c('#d5ead3', '#b1d9ad', '#8bca8b', '#62bd67', '#1ab14b', '#037a31')
pal_tropics_accents <- c(pal_tropics[6], '#0ecd5d')

# FQ index
pal_forestbio <- c(pal_carbon[1], pal_arctic)
pal_humz <- c(pal_carbon[c(2,1)], pal_risk[1:6])
pal_forestbio <- c(pal_carbon[c(1,2,3)], pal_arctic[c(4, 6)])
pal_humz <- c(pal_carbon[c(1,2,3)], pal_risk[c(4, 6)])

# Combine
priorities_idx_pal <- c(pal_carbon, pal_tropics_accents)
pal_carbontropics <- c(rev(pal_carbon), pal_tropics)
priorities_11 <- c(pal_carbon, pal_tropics[6], '#0ecd5d')
priorities_5 <- c(pal_tropics[1:5], pal_tropics[6], '#0ecd5d')
priorities_4 <- c(pal_tropics[c(1, 4)], pal_tropics[6], '#0ecd5d')
priorities_3 <- c(pal_tropics[1], pal_tropics[6], '#0ecd5d')

# Dark green highest
priorities_11 <- c(pal_carbon[c(1,2,3)], pal_tropics[c(4, 6)])
priorities_11 <- c('#f1ede5', '#f1ede5', pal_carbon[c(1, 1)], 
                   pal_tropics[c(2, 3, 4, 5, 6)], '#005520')
priorities_3 <- c(pal_tropics[c(1, 4, 6)])
priorities_4 <- c(pal_tropics[c(1, 2, 4, 6)])
priorities_4 <- c(pal_tropics[c(1, 4, 6)], '#0ecd5d')


priorities_11 <- c('#f1ede5', '#f1ede5', pal_tropics[c(1, 2, 3, 4, 5, 6)], '#005520')

demoplot(rev(pal_humz), type = 'heatmap')
demoplot(rev(priorities_11), type = 'heatmap')

data_dir <- '/Users/emilysturdivant/data'
idx_tif <- list.files('/Volumes/GoogleDrive/My Drive/Earth Engine Exports', 
                      'HIH_PlanetaryHealthIndex_v4b_10km_percentiles',
                      full.names = TRUE)[[1]]

# Load biomes
biomes_shp <- here::here(data_dir, 'raw_data', 'Ecoregions2017', 'Ecoregions2017.shp')
biomes <- st_read(biomes_shp)
biomes_dhf <- biomes %>% filter(BIOME_NUM == 1) %>% 
  st_union()
  
# Load boundaries
borders_SA <- world %>% filter(continent == 'South America' |
                                 subregion == 'Central America')
bb_SA <- st_bbox(borders_SA)
bb_SA <- c(xmin = -100, ymin = -31, xmax = -35, ymax = 25)
countries_msf_shp <- file.path(data_dir, 'gadm', 'gadm0_tropics_simp01big9.shp')
borders <- st_read(countries_msf_shp)
# borders_SA <- borders %>% filter(str_detect(NAME_0, regex('brazil|peru|colombia|venezuela|')))

# Plot with stars ----
# Load as stars
idx_stars <- read_stars(idx_tif)

# Plot richness
breaks <- seq(0, 100, length.out=11)
palette <- priorities_11

bb <- bb_SA
ggplot() +
  geom_sf(data = borders_SA,
          fill = "white",
          size = 0.2,
          color = "#969696"#alpha("#969696", 0.2)
  ) +
  geom_stars(data = idx_stars) +
  geom_sf(data = borders_SA,
          fill = "transparent",
          size = 0.2,
          color = "#969696"#alpha("#969696", 0.2)
  ) +
  scale_fill_gradientn(str_glue("Percentile"), 
                       na.value = "transparent", 
                       colors = palette, 
                       breaks = breaks,
                       labels = breaks,
                       limits = as.vector(c(0, 100))) + 
  coord_sf(xlim = bb[c('xmin', 'xmax')], ylim = bb[c('ymin', 'ymax')]) +
  # coord_sf(xlim = c(-90, 165), ylim = c(-34, 33)) +
  theme_minimal() +
  theme(legend.title.align = 0,
        legend.justification = c(1,1),
        plot.background = element_rect(fill = pal_arctic[1]),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

# Save
ggsave(out_fp, width=9, height=5.7, dpi=120)


# Convert raster to dataframe
idx_ras <- rast(idx_tif)
idx_df <- idx_ras %>% as.data.frame(xy = T)
idx_df <- idx_df %>% filter(constant != 0)
idx_df %>% count(constant) %>% arrange(desc = TRUE) %>% head()

# Plot
agb_map <- ggplot(idx_df) +
  geom_raster(aes(x = x, y = y, fill = constant)) +
  # geom_tile(aes(x = x, y = y, fill = constant)) +
  geom_sf(data = borders, fill = NA, color = "gray39", size = 0.3) +
  scale_fill_gradientn(expression(paste("Aboveground\nbiomass (Mg ha"^"-1", ")")), 
                       na.value = "transparent", 
                       colors = agb1_palette,
                       limits = c(0, 120),
                       oob = scales::squish
  ) +
  ggthemes::theme_map() +
  ggtitle(lyr_name) +
  theme(legend.position=c(0.23, 0.9),
        legend.title.align=0,
        legend.justification = c(1,1),
        legend.box.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(vjust = -8, hjust = 0.048)) 