rm(list=ls())

library(tidyverse)
library(cowplot)

'%notin%' = Negate("%in%")

dat = readRDS("data/data_public.rds")

#Virus Color Assignments
virus_colors <- c(
  "Flu A/H3N2" = "deeppink",
  "Flu A/H1N1pdm09" = "red",
  "Flu B" = "lightpink2",
  "RSV" = "goldenrod1",
  "HMPV" = "steelblue",
  "RV/EV" = "olivedrab",
  "SARS-CoV-2" = "orchid3"
)


legend_plot <- ggplot() +
  geom_line(aes(1, 1, color = "Flu A/H3N2"), linewidth = 1) +
  geom_line(aes(1, 1, color = "Flu A/H1N1pdm09"), linewidth = 1) +
  geom_line(aes(1, 1, color = "Flu B"), linewidth = 1) +
  geom_line(aes(1, 1, color = "RSV"), linewidth = 1) +
  geom_line(aes(1, 1, color = "HMPV"), linewidth = 1) +
  geom_line(aes(1, 1, color = "RV/EV"), linewidth = 1) +
  geom_line(aes(1, 1, color = "SARS-CoV-2"), linewidth = 1) +
  geom_line(aes(1, 1, linetype = "January 1"), linewidth = 0.8) +
  geom_line(aes(1, 1, linetype = "July 1"), linewidth = 0.8) +
  scale_color_manual(values = virus_colors, name = NULL) +
  scale_linetype_manual(
    values = c("January 1" = "dashed", "July 1" = "dotted"),
    name = NULL
  ) +
  guides(
    color = guide_legend(nrow = 2, order = 2),
    linetype = guide_legend(nrow=2, order = 1)
  ) +
  theme_void() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm")
  )

shared_legend <- get_legend(legend_plot)

#Panel A
fig1A <- ggplot(dat %>% filter(date >= "2016-07-01")) +
  theme_bw() +
  geom_line(aes(date, fluAh3n2_proxy, color = "Flu A/H3N2"), linewidth = 1) +
  geom_line(aes(date, fluAh1n1pdm09_proxy, color = "Flu A/H1N1pdm09"), linewidth = 1) +
  geom_line(aes(date, fluB_proxy, color = "Flu B"), linewidth = 1) +
  geom_vline(xintercept = as.Date(paste0(2017:2025, "-01-01")),
             linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = as.Date(paste0(2017:2025, "-07-01")),
             linetype = "dotted") +
  scale_color_manual(values = virus_colors, name = NULL) +
  labs(x = NULL, y = NULL) +
  scale_x_date(position = "top")+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = margin(2, 5, 2, 5),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank()
        )
fig1A
#Panel B
fig1B.1 <- ggplot(dat %>% filter(date >= "2016-07-01")) +
  theme_bw() +
  geom_line(aes(date, rsv_proxy_pediatric, color = "RSV"), linewidth = 1) +
#  geom_line(aes(date, hmpv_proxy, color = "HMPV"), linewidth = 1) +
#  geom_line(aes(date, rhino_proxy, color = "RV/EV"), linewidth = 1) +
  geom_vline(xintercept = as.Date(paste0(2017:2025, "-01-01")),
             linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = as.Date(paste0(2017:2025, "-07-01")),
             linetype = "dotted") +
  scale_color_manual(values = virus_colors, name = NULL) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = margin(2, 5, 2, 5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
fig1B.1

fig1B.2 <- ggplot(dat %>% filter(date >= "2016-07-01")) +
  theme_bw() +
  #geom_line(aes(date, rsv_proxy_pediatric, color = "RSV"), linewidth = 1) +
  geom_line(aes(date, hmpv_proxy, color = "HMPV"), linewidth = 1) +
  #geom_line(aes(date, rhino_proxy, color = "RV/EV"), linewidth = 1) +
  geom_vline(xintercept = as.Date(paste0(2017:2025, "-01-01")),
             linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = as.Date(paste0(2017:2025, "-07-01")),
             linetype = "dotted") +
  scale_color_manual(values = virus_colors, name = NULL) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = margin(2, 5, 2, 5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
fig1B.2

fig1B.3 <- ggplot(dat %>% filter(date >= "2016-07-01")) +
  theme_bw() +
 # geom_line(aes(date, rsv_proxy_pediatric, color = "RSV"), linewidth = 1) +
 # geom_line(aes(date, hmpv_proxy, color = "HMPV"), linewidth = 1) +
  geom_line(aes(date, rhino_proxy, color = "RV/EV"), linewidth = 1) +
  geom_vline(xintercept = as.Date(paste0(2017:2025, "-01-01")),
             linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = as.Date(paste0(2017:2025, "-07-01")),
             linetype = "dotted") +
  scale_color_manual(values = virus_colors, name = NULL) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = margin(2, 5, 2, 5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
fig1B.3


#Panel C
fig1C <- ggplot(dat %>% filter(date >= "2016-07-01")) +
  theme_bw() +
  geom_rect(
    aes(xmin = as.Date("2016-07-01"),
        xmax = as.Date("2022-07-01"),
        ymin = -Inf, ymax = Inf),
    fill = "lightgrey"
  ) +
  geom_line(aes(date, covid_proxy_alternative_extended, color = "SARS-CoV-2"), linewidth = 1) +
  geom_vline(xintercept = as.Date(paste0(2017:2025, "-01-01")),
             linetype = "dashed", linewidth = 0.8) +
  geom_vline(xintercept = as.Date(paste0(2017:2025, "-07-01")),
             linetype = "dotted") +
  scale_color_manual(values = virus_colors, name = NULL) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = margin(2, 5, 2, 5),
        axis.text.x = element_blank(),
        #xis.ticks.x = element_blank()
        )

#Combine All
fig_panels <- plot_grid(fig1A, fig1B.1,fig1B.2,fig1B.3, fig1C, 
                        nrow = 5, align = "v",rel_heights=c(1.23,1,1,1,1))

y_label <- ggdraw() +
  draw_label(
    "Virus Activity Proxy",
    angle = 90,
    fontface = "plain"
  )


fig1_part1 <- plot_grid(
   y_label,
   fig_panels,
  ncol = 2,
  rel_widths = c(0.05, 1),
  rel_heights = c(1, 0.15)
)

fig1_complete <- plot_grid(
  fig_panels,
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 0.15)
)
fig1_complete

ggsave(plot=fig1_complete,"figures/figure1.png",height=5,width=10,units='in')
ggsave(plot=fig1_complete,"figures/TIFF/figure1.tiff",height=5,width=10,units='in')
