# Plot survey results for PI meeting, 8/20/2024

library(tidyverse)
library(khroma)

xls <- here::here('data', 'misc', 'Survey_summary.xlsx')
dat <- readxl::read_excel(xls, col_names=TRUE)
y_order <- dat$label
cats <- dat %>% distinct(Category)

# All bars gray
dat %>% 
  mutate(label = factor(label, ordered=TRUE, levels=rev(y_order)),
         Category = factor(Category, ordered=TRUE, levels=names(colors_lab2col))) %>% 
  ggplot(aes(y=label, x=Votes)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = expansion(c(0, 0.05)), 
                     minor_breaks = NULL,
                     breaks = seq(0, 15, 3)) +
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(panel.grid.major.y=element_blank(),
        axis.ticks=element_blank())

ggsave(here::here('outputs', 'misc', 'Survey_summary.png'),
       width=4, height=3, dpi=400)

# Bars colored by category
muted <- color("muted")
pal <- muted(7)[3:6]
colors_lab2col <- pal %>% setNames(rev(seq(1,4)))
dat %>% 
  mutate(label = factor(label, ordered=TRUE, levels=rev(y_order)),
         Category = factor(Category, ordered=TRUE, levels=names(colors_lab2col))) %>% 
  ggplot(aes(y=label, x=Votes, fill=Category)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = expansion(c(0, 0.05)), 
                     minor_breaks = NULL,
                     breaks = seq(0, 15, 3)) +
  scale_fill_manual(values=colors_lab2col) +
  labs(x=NULL, y=NULL) +
  theme_bw() +
  theme(panel.grid.major.y=element_blank(),
        axis.ticks=element_blank())

ggsave(here::here('outputs', 'misc', 'Survey_summary_groups.png'),
       width=4, height=3, dpi=400)
