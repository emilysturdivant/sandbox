params2 <- list( 
  location = 'GPNP',
  shp = '/mnt/data3/esturdivant/data/shp/hih_sites/GPNP_dissolved.shp',
  years = c(2003, 2018),
  units = 'sqkm',
  rendered_by_shiny = FALSE,
  session = NA
)

params2 <- list( 
  location = 'BBBR',
  shp = '/mnt/data3/esturdivant/data/shp/hih_sites/BBBR_dissolved_v3.shp',
  years = c(2003, 2018),
  units = 'sqkm',
  rendered_by_shiny = FALSE,
  session = NA
)

params2 <- list(
  location = 'Manombo',
  shp = '~/data/shp/hih_sites/ManomboNP_all.shp',
  years = c(2003, 2018),
  units = 'sqkm',
  rendered_by_shiny = FALSE,
  session = NA
)

library(raster)
library(dplyr)
library(ggplot2)
library(reshape2)
library(flextable)
library(officer)
library(rasterVis)
library(gridExtra)
library(tidyverse)
library(ggpattern)
library(patchwork)
library(lubridate)

out_csv <- here::here('data/cloudops_exports', 
                      stringr::str_c('plot_carbon_timeseries_', params2$location, '.csv'))
df <- read_csv(out_csv)

tot.agb.tc.fy <- 13064491
tot.agb.tc.fy <- 13064491

df1 <- df %>% 
  arrange(end_year) %>% 
  group_by(category) %>% 
  mutate(cumsum = cumsum(agb_tc)) %>% 
  ungroup() %>% 
  mutate(stock_tc = tot.agb.tc.fy + cumsum)

# convert units to million metric tons of carbon (MtC) if it makes sense to
if (round(min(df1$stock_tc)/1e6) > 1) {
  df1 <- df1 %>% 
    mutate(stock_tc = stock_tc / 1e6,
           cumsum = cumsum / 1e6)
  tot.agb.tc.fy <- tot.agb.tc.fy / 1e6
  units.short <- 'MtC'
  units.long <- 'million metric tons of aboveground carbon'
} else {
  units.short <- 'tC'
  units.long <- 'metric tons of aboveground carbon'
}

df3 <- df1 %>% 
  select(category, end_year, cumsum) %>% 
  spread(key = 'category', value = 'cumsum', drop = TRUE) %>% 
  bind_rows(list(end_year = 2003, Gain = 0, Net = 0, Loss = 0)) %>% 
  arrange(end_year) %>% 
  mutate(baseline = tot.agb.tc.fy, 
         nochange = baseline + Loss,
         stock = nochange + Gain,  
         netC_from_gain = stock - nochange,
         netC_from_loss = baseline - nochange, 
         year = end_year %>% stringr::str_c('0101') %>% lubridate::as_date()
  )

df_lines <- df3 %>% 
  gather(key = 'key', value = 'value', baseline, nochange, stock)

df_area <- df3 %>% 
  gather(key = 'key', value = 'value', nochange, netC_from_gain) %>% 
  mutate(key = factor(key, levels = c("netC_from_gain", "nochange")))


# Set y-axis limits
ymin <- 950000
ymin <- 0
ymin <- 12.5
ymax <- max(df_lines$value, na.rm = TRUE)
if(is.na(ymin)) ymin <- min(df_lines$value, na.rm = TRUE)
ypad <- (ymax - ymin)*0.1

# theme_hih <- function() {
#   theme_minimal() +
#     theme(
#       panel.border = element_rect(color = 'black', fill = NULL),
#       plot.title.position = 'plot',
#       text = element_text(family = 'Helvetica'), # Times
#       axis.text = element_text(family = 'Helvetica', color = 'black'),
#       axis.title = element_text(family = 'Helvetica', color = 'black'),
#       axis.ticks = element_line(color = 'black'),
#       # axis.title.y = element_blank(),
#       panel.grid.minor = element_blank()) 
# }

p <- df_area %>% 
  ggplot(aes(x = year, y = value)) +
  geom_area(aes(fill = key))+
  scale_fill_manual(name = '',
                    # labels = c(baseline = 'Net change in C due to loss', 
                    #            netC_from_gain = 'Net change in C due to gain', 
                    #            stock ='', nochange=''),
                    values = c(nochange='seagreen4', 
                               netC_from_gain ='seagreen2', 
                              stock ='seagreen4', 
                              baseline = 'red'),
                    # limits = c('baseline', 'netC_from_gain')
                    guide=FALSE
                    ) +
  geom_line(data = df_lines, aes(color = key)) +
  scale_color_manual(name = '',
                     values = c(nochange='seagreen4', stock ='seagreen4', baseline = 'red'), 
                     guide=FALSE) +
  geom_ribbon_pattern(data = df3, 
                      aes(x = year, ymin = nochange, ymax = baseline), 
                      inherit.aes = FALSE, 
                      pattern_color = NA,
                      pattern_fill = "red",
                      pattern = "stripe",
                      # pattern_density = 0.2, 
                      # pattern_key_scale_factor = 1.3,
                      pattern_spacing = 0.025,
                      fill = NA, 
                      color = NA, 
                      alpha=0.5) + 
  scale_pattern_discrete(guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Year",
               expand = c(0, 0),
               breaks = scales::pretty_breaks(n = 3)
  ) +
  scale_y_continuous(name = paste0('Aboveground carbon (', units.short, ')'),
                     labels = scales::label_comma(accuracy = 0.1),
                     breaks = scales::breaks_extended(n = 7),
                     # limits = c(min = ymin - ypad, max = ymax + ypad),
                     expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, ymax + ypad)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(family = 'Arial'),
    axis.text.x = element_text(hjust = 1, angle = 35)
  )

p <- p +
  scale_x_date(name = "Year",
               expand = c(0, 0),
               # breaks = scales::pretty_breaks(n = 3)
               # breaks = year(as_date(c('20030101', '20180101'))),
               date_breaks = '15 years',
               labels = c("2003","2018")
  ) 


p_zoom <- p +
  scale_x_date(name = "Year",
               expand = c(0, 0),
               # date_breaks = '5 years',
               # labels = c("2003","2008","2013","2018"),
               date_breaks = '3 years',
               labels = c("2003","2006","2009","2012","2015","2018")
  ) +
  coord_cartesian(ylim = c(ymin, ymax + ypad)) +
  theme(axis.title.y = element_blank())

p
p_zoom
plot_layout()
p + p_zoom

# Save as PNG ----
out_dir <- here::here('outputs', 'carbon', params2$location)
dir.create(out_dir, recursive = TRUE)

ggsave(here::here(out_dir, str_c(params2$location, '_carbon_cumulative_from0.png')), 
       plot = p,
       width = 2, # 2
       height = 4, # 2.9
       dpi = 150)

ggsave(here::here(out_dir, str_c(params2$location, '_carbon_cumulative_zoom.png')), 
       plot = p_zoom,
       width = 6.2, # 4.5
       height = 4, # 2.9
       dpi = 150)
