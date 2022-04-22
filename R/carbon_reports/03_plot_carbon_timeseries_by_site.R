#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Plot carbon stock time series from carbon change DF for site
# Requires:
#     * R version >3
#     * DF created in CloudOps with __.R from 500 m data
#     * parameters for site
#     * total carbon stock in 2003 from __.R
# Author:
#     * esturdivant@woodwellclimate.org, 2022-04-15
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
library(grid)

params2 <- list(
  location = 'GPNP',
  shp = '/mnt/data3/esturdivant/data/shp/hih_sites/GPNP_dissolved.shp',
  years = c(2003, 2018),
  units = 'sqkm',
  rendered_by_shiny = FALSE,
  session = NA,
  carbon_y1 = 13064491,
  ymin = 12.5
)

# params2 <- list( 
#   location = 'BBBR',
#   shp = '/mnt/data3/esturdivant/data/shp/hih_sites/BBBR_dissolved_v3.shp',
#   years = c(2003, 2018),
#   units = 'sqkm',
#   rendered_by_shiny = FALSE,
#   session = NA, 
#   carbon_y1 = NA, 
#   ymin = NA
# )
# 
params2 <- list(
  location = 'Manombo',
  shp = '~/data/shp/hih_sites/ManomboNP_all.shp',
  years = c(2003, 2018),
  units = 'sqkm',
  rendered_by_shiny = FALSE,
  session = NA,
  carbon_y1 = 1169439,
  ymin = .95
)

# Load and prep data ----
out_csv <- here::here('data/cloudops_exports', 
                      str_c('plot_carbon_timeseries_', params2$location, '.csv'))
df <- read_csv(out_csv)
tot.agb.tc.fy <- params2$carbon_y1

out_dir <- here::here('outputs', 'carbon', params2$location)
dir.create(out_dir, recursive = TRUE)

# prep: get cumulative sum
df1 <- df %>% 
  arrange(end_year) %>% 
  group_by(category) %>% 
  mutate(cumsum = cumsum(agb_tc)) %>% 
  ungroup() %>% 
  mutate(stock_tc = tot.agb.tc.fy + cumsum)

# convert units to million metric tons of carbon (MtC) if it makes sense to
if (round(max(df1$stock_tc)/1e6) >= 1) {
  df1 <- df1 %>% 
    mutate(stock_tc = stock_tc / 1e6,
           cumsum = cumsum / 1e6)
  tot.agb.tc.fy <- tot.agb.tc.fy / 1e6
  units.short <- 'MtC'
  units.long <- 'million metric tons'
} else {
  units.short <- 'tC'
  units.long <- 'metric tons'
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Original: stock with cumulative loss (red hatching) and gain (light green) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# calculate values 
df2 <- df1 %>% 
  select(category, end_year, cumsum) %>% 
  spread(key = 'category', value = 'cumsum', drop = TRUE) %>% 
  bind_rows(list(end_year = 2003, Gain = 0, Net = 0, Loss = 0)) %>% 
  arrange(end_year) %>% 
  mutate(baseline = tot.agb.tc.fy, 
         nochange = baseline + Loss,
         stock = nochange + Gain,  
         netC_from_gain = stock - nochange,
         netC_from_loss = baseline - nochange, 
         year = end_year %>% str_c('0101') %>% lubridate::as_date()
  )

# Prep for ggplot
df3 <- df2 %>% 
  gather(key = 'key', value = 'value', baseline, nochange, stock, netC_from_gain)

df_lines <- df3 %>% 
  filter(key %in% c('baseline', 'nochange', 'stock'))

df_area <- df3 %>% 
  filter(key %in% c('nochange', 'netC_from_gain')) %>% 
  mutate(key = factor(key, levels = c("netC_from_gain", "nochange")))

# Set y-axis limits
ymin <- params2$ymin
ymax <- max(df_lines$value, na.rm = TRUE)
ypad <- (ymax - ymin)*0.1

# Set group colors and labels
cat_labels <- c(nochange='Persistent carbon',
                baseline = 'Change in carbon due to loss',
                netC_from_gain = 'Change in carbon due to growth',
                stock ='Stock')
cat_vals <- c(nochange='seagreen4', 
              netC_from_gain ='seagreen2', 
              stock ='seagreen4', 
              baseline = 'red')

# Plot (0 - max y-axis)
p_full <- df3 %>% 
  ggplot(aes(x = year, y = value, fill = key, color = key)) +
  geom_area(data = df_area) +
  geom_line(data = df_lines) +
  geom_ribbon_pattern(data = df2, 
                      aes(x = year, ymin = nochange, ymax = baseline), 
                      y = NA,
                      fill = NA, 
                      color = cat_vals[['baseline']],
                      pattern_color = NA,
                      pattern_fill = cat_vals[['baseline']],
                      pattern = "stripe",
                      pattern_spacing = 0.025,
                      alpha=0.5) + 
  scale_pattern_discrete(guide=guide_legend()) +
  scale_fill_manual(name = '',
                    labels = cat_labels,
                    values = cat_vals,
                    guide=guide_legend(reverse = TRUE)
  ) +
  scale_color_manual(name = '',
                     labels = cat_labels,
                     values = cat_vals,
                     guide=guide_legend(reverse = TRUE)) +
  scale_x_date(name = "Year",
               expand = c(0, 0),
               # breaks = scales::pretty_breaks(n = 3)
               date_breaks = '15 years',
               labels = c("2003","2018")
  ) +
  scale_y_continuous(name = paste0('Aboveground carbon (', units.long, ')'),
                     labels = scales::label_comma(accuracy = 0.01),
                     breaks = scales::breaks_extended(n = 7),
                     expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, ymax + ypad)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(family = 'Arial'),
    axis.text.x = element_text(hjust = 1, angle = 35),
    axis.title.x = element_blank(), 
    legend.position = 'none'
  )
 
# Plot zoomed to top of plot
p_zoom <- p_full +
  scale_x_date(name = "Year",
               expand = c(0, 0),
               date_breaks = '3 years',
               labels = c("2003","2006","2009","2012","2015","2018")
  ) +
  coord_cartesian(ylim = c(ymin, ymax + ypad)) +
  theme(legend.position = c(0.24, 0.9),
        axis.title.y = element_blank())
p_zoom

# Combine in patchwork
(p_out <- p_full + plot_spacer() + p_zoom + 
    plot_layout(ncol = 3, widths = c(1, 0.2, 4)))

# Save with zoom lines
png(here::here(out_dir, str_c(params2$location, '_carbon_patchwork_line.png')),
    width = 7.2, height = 4, units = 'in', res = 150)
p_out
grid.draw(linesGrob(x = unit(c(0.244, 0.36), "npc"), 
                    y = unit(c(0.96, 0.96), "npc"),
                    gp = gpar(lty = 2, lwd = 0.2)))
grid.draw(linesGrob(x = unit(c(0.244, 0.36), "npc"), 
                    y = unit(c(0.76, 0.12), "npc"),
                    gp = gpar(lty = 2, lwd = 1)))
dev.off()

# Save each one separately
ggsave(here::here(out_dir, str_c(params2$location, '_carbon_cumulative_from0.png')), 
       plot = p_full,
       width = 2, # 2
       height = 4, # 2.9
       dpi = 150)

ggsave(here::here(out_dir, str_c(params2$location, '_carbon_cumulative_zoom.png')), 
       plot = p_zoom,
       width = 6.2, # 4.5
       height = 4, # 2.9
       dpi = 150)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Seth's idea: stock with best and worst case scenario lines ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tot.agb.tc.fy <- params2$carbon_y1
df1 <- df %>% 
  arrange(end_year) %>% 
  group_by(category) %>% 
  mutate(cumsum = cumsum(agb_tc)) %>% 
  ungroup() %>% 
  mutate(stock_tc = tot.agb.tc.fy + cumsum)

# convert units to million metric tons of carbon (MtC) if it makes sense to
if (round(max(df1$stock_tc)/1e6) >= 1) {
  df1 <- df1 %>% 
    mutate(stock_tc = stock_tc / 1e6,
           cumsum = cumsum / 1e6)
  tot.agb.tc.fy <- tot.agb.tc.fy / 1e6
  units.short <- 'MtC'
  units.long <- 'million metric tons'
} else {
  units.short <- 'tC'
  units.long <- 'metric tons'
}

# Plot cumulative loss and gain 
df2 <- df1 %>% 
  select(category, end_year, cumsum) %>% 
  spread(key = 'category', value = 'cumsum', drop = TRUE) %>% 
  bind_rows(list(end_year = 2003, Gain = 0, Net = 0, Loss = 0)) %>% 
  arrange(end_year) %>% 
  rowwise() %>% 
  mutate(baseline = tot.agb.tc.fy, 
         net_change = Gain + Loss,
         stock = baseline + net_change,  
         stock_noloss = stock - Loss,  
         stock_nogain = stock - Gain,  
         year = end_year %>% stringr::str_c('0101') %>% lubridate::as_date()
  )

df3 <- df2 %>% 
  gather(key = 'key', value = 'value', stock, stock_noloss, stock_nogain)

df_area <- df3 %>% filter(key == 'stock')
df_lines <- df3 %>% filter(key != 'stock')

# Set y-axis limits
ymin <- params2$ymin
ymax <- max(df3$value, na.rm = TRUE)
if(is.na(ymin)) ymin <- min(df3$value, na.rm = TRUE)
ypad <- (ymax - ymin)*0.1

# Set colors and labels
stock_line ='black'
stock ='#99D594'
stock ='seagreen4'
stock_nogain = 'red'
stock_noloss = 'dodgerblue3'

cat_labels <- c(stock_noloss = 'Stock without loss', # Carbon stock assuming no loss
                stock_nogain = 'Stock without growth', # Carbon stock assuming no gain
                stock ='Stock')
cat_vals <- c(stock_nogain = stock_nogain, 
              stock = stock_line, 
              stock_noloss = stock_noloss)
cat_fill_vals <- c(stock_nogain = stock_nogain, 
              # stock ='palegreen2', 
              stock = stock, 
              stock_noloss = stock_noloss)
cat_lty <- c(stock_nogain = '11', 
                   stock = 'solid', 
                   stock_noloss = '11')

# Plot from 0 to max
p_full <- df3 %>% 
  ggplot(aes(x = year, y = value, fill = key, color = key, linetype = key)) +
  geom_area(data = df_area) +
  geom_line(data = df3, size = 1) +
  scale_fill_manual(name = '',
                    labels = cat_labels,
                    values = cat_fill_vals,
                    guide=FALSE
  ) +
  scale_color_manual(name = '',
                     labels = cat_labels,
                     values = cat_vals,
                     guide= guide_legend(reverse = TRUE)) +
  scale_linetype_manual(name = '',
                     labels = cat_labels,
                     values = cat_lty,
                     guide= guide_legend(reverse = TRUE)) +
  scale_x_date(name = "Year",
               expand = c(0, 0),
               # breaks = scales::pretty_breaks(n = 3)
               date_breaks = '15 years',
               labels = c("2003","2018")
  ) +
  scale_y_continuous(name = paste0('Aboveground carbon (', units.long, ')'),
                     # labels = scales::label_comma(accuracy = 0.01),
                     labels = scales::label_comma(accuracy = 0.1),
                     breaks = scales::breaks_extended(n = 7),
                     expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, ymax + ypad)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(family = 'Arial', color = 'black'),
    panel.border = element_rect(color = 'black'),
    axis.text.x = element_text(hjust = 1, angle = 35),
    axis.title.x = element_blank(), 
    legend.position = 'none'
  ) + 
  guides(color=guide_legend(override.aes=list(fill=NA)))

# Plot zoomed
p_zoom <- p_full +
  scale_x_date(name = "Year",
               expand = c(0, 0),
               date_breaks = '3 years',
               labels = c("2003","2006","2009","2012","2015","2018")
  ) +
  coord_cartesian(ylim = c(ymin, ymax + ypad)) +
  theme(axis.title.y = element_blank(), 
        legend.position = 'none'
        # legend.position = c(0.24, 0.9),
        # legend.key = element_rect(fill = NA)
        )
p_zoom

# Combine in patchwork
(p_out <- p_full + plot_spacer() + p_zoom + 
    plot_layout(ncol = 3, widths = c(1, 0.2, 4)))

# Save with zoom lines
out_dir <- here::here('outputs', 'carbon', params2$location)
png(here::here(out_dir, str_c(params2$location, '_carbon_v3_patchwork_line.png')),
    width = 7.2, height = 4, units = 'in', res = 150)
p_out
grid.draw(linesGrob(x = unit(c(0.244, 0.36), "npc"), 
                    y = unit(c(0.96, 0.96), "npc"),
                    gp = gpar(lty = 2, lwd = 1, col = 'gray50')))
grid.draw(linesGrob(x = unit(c(0.244, 0.36), "npc"), 
                    y = unit(c(0.76, 0.12), "npc"),
                    gp = gpar(lty = 2, lwd = 1, col = 'gray50')))
dev.off() 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Stock + annual change bars: overlap stock area with bar chart ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# convert units to million metric tons of carbon (MtC) if it makes sense to
stock_y1 <- params2$carbon_y1
if (round(stock_y1/1e6) >= 1) {
  df1 <- df %>% mutate(agb_tc = agb_tc / 1e6)
  tot.agb.tc.fy <- stock_y1 / 1e6
  units.short <- 'MtC'
  units.long <- 'million metric tons'
} else {
  df1 <- df 
  tot.agb.tc.fy <- stock_y1
  units.short <- 'tC'
  units.long <- 'metric tons'
}

df_annual <- df1 %>% 
  select(category, end_year, agb_tc) %>% 
  spread(key = 'category', value = 'agb_tc', drop = TRUE) %>% 
  arrange(end_year) %>% 
  mutate(Net = tot.agb.tc.fy + cumsum(Net),
         gain_line = Net + Gain, 
         loss_line = Net + Loss, 
         year = end_year %>% stringr::str_c('0101') %>% lubridate::as_date())

# Percents
df_pcts <- df_annual %>% 
  mutate(Loss = Loss * -1,
         Loss = Loss / tot.agb.tc.fy * 100,
         Gain = Gain / tot.agb.tc.fy * 100) %>% 
  select(year, Gain, Loss) %>% 
  pivot_longer(cols = Gain:Loss)

# Set y-axis limits
stock_ymin <- 0
stock_ymin <- params2$ymin
stock_ymax <- max(df_annual$Net, na.rm = TRUE)
stock_ypad <- (stock_ymax - stock_ymin)*0.1

# Set y-axis limits
bar_ymin <- 0
bar_ymax <- max(df_annual$Gain, df_annual$Loss*-1,  na.rm = TRUE)
bar_ypad <- (bar_ymax - bar_ymin)*0.1

# Set y-axis limits
pct_ymin <- 0
pct_ymax <- max(df_pcts$value,  na.rm = TRUE)
pct_ypad <- (pct_ymax - pct_ymin)*0.1

# Overlap
df_stock <- df_annual %>% 
  bind_rows(list(year = lubridate::as_date('20030101'), Net = tot.agb.tc.fy)) 

conv_val <- ((stock_ymax - stock_ymin) / (pct_ymax * 2.5))

colr_stock <- '#C1F8D9'
colr_stock_line <- 'seagreen4'
colr_loss <- 'firebrick3'
colr_loss_line <- 'white'
colr_gain <- '#798897'
colr_gain_line <- 'white'

df_stock_long <- df_stock %>% 
  select(Net, year) %>% 
  pivot_longer(cols = Net)

df_bind <- df_stock_long %>% 
  bind_rows(df_pcts)

p_stock_full <- df_bind %>% 
  ggplot() +
  geom_area(data = df_stock_long, aes(x = year, y = value, fill = name, color = name)) +
  scale_fill_manual(
    labels = c(Net = 'Aboveground carbon stock', 
               Gain = "Annual growth in C", 
               Loss = "Annual loss in C"),
    values = c(Net = colr_stock, Gain = colr_gain, Loss = colr_loss),
    name = '', guide=FALSE) +
  scale_color_manual(
    labels = c(Net = 'Aboveground carbon stock', 
               Gain = "Annual growth in C", 
               Loss = "Annual loss in C"),
    values = c(Net = colr_stock_line, Gain = colr_gain_line, Loss = colr_loss_line),
    name = '', guide=FALSE) +
  scale_x_date(name = "Year",
               expand = c(0, 0),
               date_breaks = '5 years',
               date_labels = '%Y'
  ) +
  scale_y_continuous(
    name = paste0('Aboveground carbon stock (', units.long, ')'),
    labels = scales::label_comma(accuracy = 0.1),
    breaks = scales::breaks_extended(n = 7),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, stock_ymax + stock_ypad)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    text = element_text(family = 'Arial'),
    axis.text.x = element_text(hjust = 1, angle = 35),
    axis.title.x = element_blank()
  ) 

p_stock <- df_bind %>% 
  ggplot() +
  geom_area(data = df_stock_long, aes(x = year, y = value, fill = name, color = name)) +
  # annotate(geom = 'text', hjust = 0, color = colr_stock_line,
  #          x = as_date('20040101'), y = tot.agb.tc.fy - conv_val*1.5,
  #          label = 'Standing stock', fontface =2, size = 3) +
  geom_bar(data = df_pcts, 
           aes(x = year - 182, y = value * conv_val + stock_ymin, 
               fill = name, color = name),
           position="dodge", stat="identity",
           width = 280, size = .2) +
  scale_fill_manual(
    labels = c(Net = 'Aboveground carbon stock', 
               Gain = "Annual growth in C", 
               Loss = "Annual loss in C"),
    values = c(Net = colr_stock, Gain = colr_gain, Loss = colr_loss),
    name = '', guide=guide_legend(reverse=TRUE)) +
  scale_color_manual(
    labels = c(Net = 'Aboveground carbon stock', 
               Gain = "Annual growth in C", 
               Loss = "Annual loss in C"),
    values = c(Net = colr_stock_line, Gain = colr_gain_line, Loss = colr_loss_line),
    name = '', guide=guide_legend(reverse=TRUE)) +
  scale_x_date(name = "Year",
               expand = c(0, 0),
               date_breaks = '3 years',
               date_labels = '%Y'
  ) +
  scale_y_continuous(
    name = paste0('Aboveground carbon stock (', units.long, ')'),
    labels = scales::label_comma(accuracy = 0.01),
    # labels = function(x) paste(format(x, digits = 2), 'mil'),
    breaks = scales::breaks_extended(n = 7),
    expand = c(0, 0),
    sec.axis = sec_axis(~ ( . - stock_ymin) / conv_val, 
                        name = 'Annual change (% of 2003 stock)', 
                        # lims = c(0, 2), 
                        breaks = seq(0, round(pct_ymax, 1), round(pct_ymax, 1) / 3),
                        labels = function(x) paste0(format(x, digits = 1), '%'))
  ) +
  coord_cartesian(ylim = c(stock_ymin, stock_ymax + stock_ypad)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    text = element_text(family = 'Arial'),
    axis.text.x = element_text(hjust = 1, angle = 35),
    # axis.title.y.right = element_text(hjust = 1),
    axis.title.y.left = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.key.height = unit(0.7, 'line'),
    legend.key.width = unit(0.45, 'line'),
    legend.direction = 'vertical', 
    legend.position = c(0.78, 0.92),
    # legend.position = c(0.24, 0.92),
    legend.box.margin = margin(0, 0, 0, 0, unit = 'pt'),
    legend.margin = margin(0, 0, 0, 0, unit = 'pt'),
    legend.background = element_rect(fill = NULL, color = NULL)
  ) 
p_stock

(p_out <- p_stock_full + plot_spacer() + p_stock +
    plot_layout(ncol = 3, widths = c(1, 0.2, 4)))

ggsave(here::here(out_dir, str_c(params2$location, '_carbon_patchwork.png')), 
       plot = p_out,
       width = 7.2, # 4.5
       height = 4, # 2.9
       dpi = 150)

# Stock only (prep in previous section) ----
colr_stock <- 'seagreen4'
colr_stock_line <- 'seagreen4'

p_stock_full <- df_bind %>% 
  ggplot() +
  geom_area(data = df_stock_long, aes(x = year, y = value, fill = name, color = name)) +
  scale_fill_manual(
    labels = c(Net = 'Aboveground carbon stock', 
               Gain = "Annual growth in C", 
               Loss = "Annual loss in C"),
    values = c(Net = colr_stock, Gain = colr_gain, Loss = colr_loss),
    name = '', guide=guide_legend(reverse=TRUE)) +
  scale_color_manual(
    labels = c(Net = 'Aboveground carbon stock', 
               Gain = "Annual growth in C", 
               Loss = "Annual loss in C"),
    values = c(Net = colr_stock_line, Gain = colr_gain_line, Loss = colr_loss_line),
    name = '', guide=guide_legend(reverse=TRUE)) +
  scale_x_date(name = "Year",
               expand = c(0, 0),
               date_breaks = '5 years',
               date_labels = '%Y'
  ) +
  scale_y_continuous(
    name = paste0('Aboveground carbon stock (', units.long, ')'),
    labels = scales::label_comma(accuracy = 0.1),
    breaks = scales::breaks_extended(n = 7),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(0, stock_ymax + stock_ypad)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    text = element_text(family = 'Arial'),
    axis.text.x = element_text(hjust = 1, angle = 35),
    axis.title.x = element_blank(),
    legend.position = 'none'
  ) 

p_stock <- p_stock_full +
  scale_x_date(name = "Year",
               expand = c(0, 0),
               date_breaks = '3 years',
               date_labels = '%Y'
  ) +
  scale_y_continuous(
    name = paste0('Aboveground carbon stock (', units.long, ')'),
    labels = scales::label_comma(accuracy = 0.01),
    breaks = scales::breaks_extended(n = 7),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(stock_ymin, stock_ymax + stock_ypad)) +
  theme(
    axis.title.y.left = element_blank(),
    legend.title = element_blank(),
    legend.key.height = unit(0.7, 'line'),
    legend.key.width = unit(0.45, 'line'),
    legend.direction = 'vertical', 
    legend.position = c(0.78, 0.92),
    # legend.position = c(0.26, 0.92),
    legend.box.margin = margin(0, 0, 0, 0, unit = 'pt'),
    legend.margin = margin(0, 0, 0, 0, unit = 'pt'),
    legend.background = element_rect(fill = NULL, color = NULL)
  ) 

(p_out <- p_stock_full + plot_spacer() + p_stock +
    plot_layout(ncol = 3, widths = c(1, 0.2, 4)))

# Save as PNG 
out_dir <- here::here('outputs', 'carbon', params2$location)
dir.create(out_dir, recursive = TRUE)
ggsave(here::here(out_dir, str_c(params2$location, '_carbon_stock_patchwork.png')), 
       plot = p_out,
       width = 7.2, # 4.5
       height = 4, # 2.9
       dpi = 150)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cumulative loss and gain with loss on top ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tot.agb.tc.fy <- params2$carbon_y1
df1 <- df %>% 
  arrange(end_year) %>% 
  group_by(category) %>% 
  mutate(cumsum = cumsum(agb_tc)) %>% 
  ungroup() %>% 
  mutate(stock_tc = tot.agb.tc.fy + cumsum)

# convert units to million metric tons of carbon (MtC) if it makes sense to
if (round(max(df1$stock_tc)/1e6) >= 1) {
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

# Plot cumulative loss and gain ----
df3 <- df1 %>% 
  select(category, end_year, cumsum) %>% 
  spread(key = 'category', value = 'cumsum', drop = TRUE) %>% 
  bind_rows(list(end_year = 2003, Gain = 0, Net = 0, Loss = 0)) %>% 
  arrange(end_year) %>% 
  rowwise() %>% 
  # Manombo
  # mutate(baseline = tot.agb.tc.fy, 
  #        net_change = Gain + Loss,
  #        stock = baseline + net_change,  
  #        # gross loss 
  #        netC_from_loss = baseline + Loss,
  #        loss_ymin = min(netC_from_loss, baseline),
  #        loss_ymax = max(netC_from_loss, baseline),
  #        # gross gain
  #        gain_ymin = min(netC_from_loss, baseline),
  #        gain_ymax = gain_ymin + Gain,
  #        year = end_year %>% stringr::str_c('0101') %>% lubridate::as_date()
  # )
  # GPNP
  mutate(baseline = tot.agb.tc.fy, 
       net_change = Gain + Loss,
       stock = baseline + net_change,  
       # gross gain
       gain_ymin = min(baseline),
       gain_ymax = gain_ymin + Gain,
       # gross loss 
       netC_from_loss = gain_ymax + Loss,
       loss_ymax = max(gain_ymax),
       loss_ymin = loss_ymax + Loss,
       year = end_year %>% stringr::str_c('0101') %>% lubridate::as_date()
  )
  
# Set y-axis limits
ymin <- 0
ymin <- params2$ymin
ymax <- max(df3$baseline, df3$loss_ymax, df3$gain_ymax, na.rm = TRUE)
if(is.na(ymin)) ymin <- min(df_lines$value, na.rm = TRUE)
ypad <- (ymax - ymin)*0.1
ypad <- (ymax - ymin)*0.3

# 
colr_nochange_area = '#9bdeac'
colr_gain_pattern = "#3eb38f"
colr_loss_pattern = '#aa2143'

colr_nochange_line = colr_loss_pattern
colr_netC_from_gain_area = colr_nochange_area
colr_stock_area = colr_nochange_area
colr_baseline_area = colr_nochange_line
colr_stock_line = colr_nochange_area
colr_baseline_line = colr_baseline_area

pttrn_gain = "stripe"
pttrn_loss = "circle"
colr_loss_pattern = colr_baseline_area

p_zoom <- df3 %>% 
  ggplot() +
  geom_area(aes(x = year, y = stock), fill = colr_stock_area) +
  # gain
  # geom_ribbon_pattern(aes(x = year, ymin = gain_ymin, ymax = gain_ymax), 
  #                     inherit.aes = FALSE, 
  #                     pattern = pttrn_gain, 
  #                     pattern_fill = colr_gain_pattern,
  #                     color = colr_gain_pattern, 
  #                     pattern_color = NA,
  #                     pattern_spacing = 0.02,
  #                     pattern_density = 0.2,
  #                     pattern_angle = 65,
  #                     fill = NA, 
  #                     # alpha = 1, 
  #                     outline.type = 'both') + 
  # loss
  geom_ribbon_pattern(aes(x = year, ymin = loss_ymin, ymax = loss_ymax), 
                      inherit.aes = FALSE, 
                      pattern = pttrn_loss, 
                      pattern_fill = colr_loss_pattern,
                      color = colr_loss_pattern, 
                      fill = NA, 
                      pattern_color = NA,
                      pattern_spacing = 0.01,
                      pattern_density = 0.6,
                      pattern_angle = 0,
                      # alpha=0.5, 
                      outline.type = 'both') + 
  scale_pattern_discrete(guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Year",
               expand = c(0, 0),
               date_breaks = '3 years',
               labels = c("2003","2006","2009","2012","2015","2018")
  ) +
  scale_y_continuous(name = paste0('Aboveground carbon (', units.short, ')'),
                     # labels = scales::label_comma(accuracy = 0.01),
                     labels = scales::label_comma(accuracy = 0.1),
                     breaks = scales::breaks_extended(n = 7),
                     # limits = c(min = ymin - ypad, max = ymax + ypad),
                     expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(ymin, ymax + ypad)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(family = 'Arial'),
    axis.text.x = element_text(hjust = 1, angle = 35)
  )
p_zoom

# Original with pattern for gain and loss ----
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
ymin <- 0
ymin <- params2$ymin
ymax <- max(df_lines$value, na.rm = TRUE)
if(is.na(ymin)) ymin <- min(df_lines$value, na.rm = TRUE)
ypad <- (ymax - ymin)*0.1
ypad <- (ymax - ymin)*0.3

# Ugly 1
colr_nochange_area = 'seagreen4'
colr_netC_from_gain_area = 'seagreen4'
colr_stock_area ='seagreen4'
colr_baseline_area = 'red'

colr_nochange_line ='red'
colr_stock_line ='seagreen4'
colr_baseline_line = 'red'

pttrn_gain = "stripe"
colr_gain_pattern = "seagreen1"
pttrn_loss = "circle"
colr_loss_pattern = "red"

# 
colr_nochange_area = '#9bdeac'
colr_gain_pattern = "#3eb38f"
colr_loss_pattern = '#aa2143'

colr_nochange_line = colr_loss_pattern
colr_netC_from_gain_area = colr_nochange_area
colr_stock_area = colr_nochange_area
colr_baseline_area = colr_nochange_line
colr_stock_line = colr_nochange_area
colr_baseline_line = colr_baseline_area

pttrn_gain = "stripe"
pttrn_loss = "circle"
colr_loss_pattern = colr_baseline_area

p_zoom <- df_area %>% 
  ggplot(aes(x = year, y = value)) +
  geom_area(aes(fill = key))+
  scale_fill_manual(name = '',
                    values = c(nochange = colr_nochange_area, 
                               netC_from_gain = colr_netC_from_gain_area, 
                              stock = colr_stock_area, 
                              baseline = colr_baseline_area),
                    guide=FALSE
                    ) +
  geom_ribbon_pattern(data = df3, 
                      aes(x = year, ymin = nochange, ymax = stock), 
                      inherit.aes = FALSE, 
                      pattern = pttrn_gain, 
                      pattern_fill = colr_gain_pattern,
                      color = colr_nochange_area, 
                      # pattern_color = 'grey30',
                      pattern_color = NA,
                      pattern_spacing = 0.02,
                      pattern_density = 0.2,
                      pattern_angle = 65,
                      fill = NA, 
                      alpha = 1, 
                      outline.type = 'both') + 
  geom_ribbon_pattern(data = df3, 
                      aes(x = year, ymin = nochange, ymax = baseline), 
                      inherit.aes = FALSE, 
                      pattern = pttrn_loss, 
                      pattern_fill = colr_loss_pattern,
                      color = colr_loss_pattern, 
                      fill = NA, 
                      pattern_color = NA,
                      pattern_spacing = 0.01,
                      pattern_density = 0.6,
                      pattern_angle = 0,
                      alpha=0.5, 
                      outline.type = 'both') + 
  # geom_line(data = df_lines, aes(color = key), size = 1) +
  # scale_color_manual(name = '',
  #                    values = c(nochange = colr_nochange_line, 
  #                               stock = colr_stock_line, 
  #                               baseline = colr_baseline_line), 
  #                    guide=FALSE) +
  scale_pattern_discrete(guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Year",
               expand = c(0, 0),
               date_breaks = '3 years',
               labels = c("2003","2006","2009","2012","2015","2018")
  ) +
  scale_y_continuous(name = paste0('Aboveground carbon (', units.short, ')'),
                     # labels = scales::label_comma(accuracy = 0.01),
                     labels = scales::label_comma(accuracy = 0.1),
                     breaks = scales::breaks_extended(n = 7),
                     # limits = c(min = ymin - ypad, max = ymax + ypad),
                     expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(ymin, ymax + ypad)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(family = 'Arial'),
    axis.text.x = element_text(hjust = 1, angle = 35)
  )
p_zoom

p <- p_zoom +
  coord_cartesian(ylim = c(0, ymax + ypad)) +
  scale_x_date(name = "Year",
               expand = c(0, 0),
               # breaks = scales::pretty_breaks(n = 3)
               # breaks = year(as_date(c('20030101', '20180101'))),
               date_breaks = '15 years',
               labels = c("2003","2018")
  ) 
p

# Save as PNG
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



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Lines: Stock with annual changes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- df %>% arrange(end_year) 

# convert units to million metric tons of carbon (MtC) if it makes sense to
stock_y1 <- params2$carbon_y1
if (round(stock_y1/1e6) >= 1) {
  df1 <- df %>% mutate(agb_tc = agb_tc / 1e6)
  tot.agb.tc.fy <- stock_y1 / 1e6
  units.short <- 'MtC'
  units.long <- 'million metric tons of aboveground carbon'
} else {
  df1 <- df 
  tot.agb.tc.fy <- stock_y1
  units.short <- 'tC'
  units.long <- 'metric tons of aboveground carbon'
}

# Spread
df3 <- df1 %>% 
  select(category, end_year, agb_tc) %>% 
  spread(key = 'category', value = 'agb_tc', drop = TRUE) %>% 
  mutate(
    cum_net = cumsum(Net),
    stock = tot.agb.tc.fy + cum_net, # area & line
    nochange = stock - Net,  # area & line
    C_gain = stock - Gain,
    C_loss = nochange - Loss, 
    stock_diff = stock - nochange,  # area
    gain_diff = C_gain - nochange,
    loss_diff = C_loss - nochange, 
    year = end_year %>% stringr::str_c('0101') %>% lubridate::as_date()
  )

df_lines <- df3 %>% 
  gather(key = 'key', value = 'value', nochange, stock, C_gain, C_loss)

ggplot() +
  geom_line(data = df_lines, aes(x = year, y = value, color = key)) +
  scale_color_manual(name = '',
                     values = c(nochange ='seagreen4', stock ='seagreen4', 
                                C_gain = 'seagreen2', C_loss = 'red'), 
                     guide=FALSE) +
  coord_cartesian(ylim = c(ymin, ymax + ypad)) +
  theme_bw()


# Two panels: stock and annual change bars ----
# Plot standing stock 
p_stock <- df_annual %>% 
  bind_rows(list(year = lubridate::as_date('20030101'), Net = tot.agb.tc.fy)) %>% 
  ggplot(aes(x = year)) +
  geom_area(aes(y = Net), fill = colr_stock_area) +
  scale_x_date(name = "",
               expand = c(0, 0),
               date_breaks = '3 years',
               # labels = c("2003","2006","2009","2012","2015","2018")
               date_labels = '%Y'
  ) +
  scale_y_continuous(name = paste0('Aboveground carbon (', units.short, ')'),
                     labels = scales::label_comma(accuracy = 0.1),
                     breaks = scales::breaks_extended(n = 7),
                     expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(stock_ymin, stock_ymax + stock_ypad)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(family = 'Arial'),
    axis.text.x = element_text(hjust = 1, angle = 35)
  )+
  ggtitle('Standing stock')
p_stock

# Bar chart of gain and loss
p_bar <- df_annual %>% 
  mutate(Loss = Loss * -1) %>% 
  select(year, Gain, Loss) %>% 
  pivot_longer(cols = Gain:Loss) %>% 
  ggplot(aes(x = year, y = value, fill = name)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(name = '',
                    values = c(Gain = colr_stock_area,
                               Loss = colr_loss_pattern),
                    guide=FALSE
  ) +
  scale_x_date(name = "Year",
               expand = c(0, 0),
               date_breaks = '2 years',
               date_labels = '%Y'
  ) +
  scale_y_continuous(name = paste0('Aboveground carbon (', units.short, ')'),
                     # labels = scales::label_comma(accuracy = 0.01),
                     labels = scales::label_comma(accuracy = 0.1),
                     breaks = scales::breaks_extended(n = 7),
                     # limits = c(min = bar_ymin - bar_ypad, max = bar_ymax + bar_ypad),
                     expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(bar_ymin, bar_ymax + bar_ypad)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(family = 'Arial'),
    axis.text.x = element_text(hjust = 1, angle = 35)
  ) +
  ggtitle('Annual gains and losses')
p_bar

# Bar plot of percent gain and loss
p_bar <- df_pcts %>% 
  ggplot(aes(x = year, y = value, fill = name)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(name = '',
                    values = c(Gain = colr_stock_area,
                               Loss = colr_loss_pattern),
                    guide=FALSE
  ) +
  scale_x_date(name = "Year",
               expand = c(0, 0),
               date_breaks = '2 years',
               date_labels = '%Y'
  ) +
  scale_y_continuous(name = paste0('Percent of standing stock (%)'),
                     # labels = scales::label_comma(accuracy = 0.01),
                     labels = scales::label_comma(accuracy = 0.1),
                     breaks = scales::breaks_extended(n = 7),
                     # limits = c(min = pct_ymin - pct_ypad, max = pct_ymax + pct_ypad),
                     expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(pct_ymin, pct_ymax + pct_ypad)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(family = 'Arial'),
    axis.text.x = element_text(hjust = 1, angle = 35)
  ) +
  ggtitle('Annual gains and losses') 
p_bar

p_stock / p_bar + plot_layout(nrow = 2, heights = c(2,1))

