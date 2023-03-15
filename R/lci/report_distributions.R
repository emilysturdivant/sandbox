#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * View LCI values distributions for input polygons
# Requires:
#     * LCI GeoTIFs 
#     * Polygons
# Author:
#     * esturdivant@woodwellclimate.org, 2023-03
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(sf)
library(tmap)

acre_json <- '/Users/esturdivant/data/misc/Kepos_examplecarbonoffsetproject/acre.geojson'
acre <- st_read(acre_json) |> st_make_valid()

tmap_mode('view')
qtm(acre)

# Clip to polygon ----






# Convert raster to dataframe ----
diff_df <- as.data.frame(diff_r, xy=TRUE) %>% 
  rename(diff = 3)

# Plot differences
diff_label <- "Change in ACD"
diff_palette <- c('#d01c8b', '#f1b6da', '#f7f7f7', '#b8e186', '#4dac26')
diff_leg_labels <- c('<-50', '-25', '0', '25', 
                     expression(paste(">50 Mg C ha"^"-1")))

# Histogram of differences
p_hist <- ggplot(diff_df, aes(x=diff)) + 
  geom_histogram(aes(fill = after_stat(x)), binwidth = 5,
                 show.legend = TRUE)+
  scale_fill_gradientn(colors = diff_palette, 
                       name = diff_label,
                       labels = diff_leg_labels,
                       limits = c(-50, 50),
                       oob = scales::squish
  ) + 
  scale_x_continuous(name = expression(paste("Change in ACD (Mg C ha"^"-1",")"))) +
  scale_y_continuous(name = 'Frequency (thousands of pixels)',
                     breaks = seq(0, 750, 250)*1000,
                     labels = seq(0, 750, 250)) +
  coord_cartesian(xlim=c(-100, 100)) +
  ggtitle("Distribution of change in aboveground carbon density 2005-2013") +
  theme(axis.ticks.y = element_blank(),
        legend.text.align = 0,
        panel.grid = element_line(color = 'lightgray'),
        panel.background = element_rect(fill = 'darkgray'))

# View plots
p_hist