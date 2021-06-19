## Figure 5. Distance-dependency in simulated flocks with and without predator avoidance. 
source('count_switch_esc_dir.R')
library(dplyr)

load('../Data/simulated/sim_datas_no_avoid.RData')
noesc_data <- lapply(sim_datas, function(x){if (x$config$Pigeon$N < 15) return(x$self_main) })
noesc_data[sapply(noesc_data, is.null)] <- NULL

load('../Data/simulated/sim_datas_chase.RData')
esc_data <- lapply(sim_datas, function(x){if (x$config$Pigeon$N < 15) return(x$self_main)})
esc_data[sapply(esc_data, is.null)] <- NULL

# PLOTS

## B. Switch of escape direction
esc_switch <- switch_esc_dir(esc_data)

esc_switch <- data.table::rbindlist(esc_switch)

esc_switch <- esc_switch %>% group_by(dist_group, avoid_switch) %>%
  summarise(  perc_sd = sd(perc, na.rm = TRUE),
              perc_mean = mean(perc, na.rm = TRUE)
  )


switch_esc <- ggplot2::ggplot(esc_switch[esc_switch$avoid_switch == 1,], ggplot2::aes(x = dist_group, y = as.numeric(perc_mean))) +
  ggplot2::geom_col(fill = "#0A6649", color = 'black',alpha= 0.75, size = 1.2) +
  ggplot2::theme_bw() +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=abs(perc_mean), ymax=abs(perc_mean)+abs(perc_sd)), width=.2, size=1.2,
                         position= ggplot2::position_dodge(.9)) +
  ggplot2::labs(x = '', y = 'Escape-direction change \n frequency (% of timesteps)', title = '') +
  ggplot2::ylim(c(-1, 13)) +
  ggplot2::theme(legend.position = 'none',
                 axis.text = ggplot2::element_blank(),
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 axis.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5))

noesc_switch <- switch_esc_dir(noesc_data)
noesc_switch <- data.table::rbindlist(noesc_switch)

noesc_switch <- noesc_switch %>% group_by(dist_group, avoid_switch) %>%
  summarise(  perc_sd = sd(perc, na.rm = TRUE),
              perc_mean = mean(perc, na.rm = TRUE)
  )
switch_noesc <- ggplot2::ggplot(noesc_switch[noesc_switch$avoid_switch == 1,], ggplot2::aes(x = dist_group, y = as.numeric(perc_mean))) +
  ggplot2::geom_col(fill = "#ECDDC3" , color = 'black',alpha= 0.75, size = 1.2) +
  ggplot2::theme_bw() +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=abs(perc_mean), ymax=abs(perc_mean)+abs(perc_sd)), width=.2, size=1.2,
                         position= ggplot2::position_dodge(.9)) +
  
  ggplot2::labs(x = '', y = 'Frequency of escape-direction\n  change (% of time)', title = '') +
  ggplot2::ylim(c(-1, 13)) +
  ggplot2::theme(legend.position = 'none',
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 22, face = 'bold'),
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 24, face = 'bold'),
                 plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5))


## A. Angle between predator and prey

one_esc <- data.table::rbindlist(esc_data)
one_esc <- one_esc[!(is.na(one_esc$dist_group))& one_esc$dist_group != '>60',]
one_noesc <- data.table::rbindlist(noesc_data)
one_noesc <- one_noesc[!(is.na(one_noesc$dist_group))& one_noesc$dist_group != '>60',]

angl_esc <- ggplot2::ggplot(one_esc, ggplot2::aes(x = dist_group , y = abs(pracma::rad2deg(radAwayPred)))) +
  ggplot2::geom_boxplot(fill = '#0A6649', color = 'black', alpha= 0.75,size = 1.2 , outlier.colour = NA) +
  ggplot2::theme_bw()   +
  ggplot2::ylim(c(0,100))+
  ggplot2::labs(x = '', y = 'Angle between predator & \n prey  headings (degrees)', title = '') +
  ggplot2::theme(legend.position = 'none',
                 axis.text = ggplot2::element_blank(),
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 axis.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 24, face = "bold", hjust = 0.5))

angl_noesc <- ggplot2::ggplot(one_noesc, ggplot2::aes(x = dist_group , y = abs(pracma::rad2deg(radAwayPred)))) +
  ggplot2::geom_boxplot(fill = "#ECDDC3", color = 'black', alpha= 0.75,size = 1.2, outlier.colour = NA) +
  ggplot2::theme_bw()   +
  ggplot2::ylim(c(0,100))+
  ggplot2::labs(x = '', y = 'Angle between predator & \n prey  headings (degrees)', title = '') +
  ggplot2::theme(legend.position = 'none',
                 axis.text.x = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_text(size = 22, face = 'bold'),
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_text(size = 24, face = 'bold'),
                 plot.title = ggplot2::element_text(size = 24, face = "bold", hjust = 0.5))

library(dplyr)

## C. Escape direction concesus 

esc_dir <- one_esc %>% group_by(dist_group, time) %>%
  summarise(esc_mean = abs(mean(sign(radAwayPred), na.rm = TRUE)))

esc_dir <- esc_dir %>% group_by(dist_group) %>%
  summarise(esc_sd = sd(esc_mean, na.rm = TRUE),
            esc_mean = median(esc_mean, na.rm = TRUE))

escdir_esc <- ggplot2::ggplot(esc_dir, ggplot2::aes(x = dist_group , y = esc_mean)) +
  ggplot2::geom_col(fill = '#0A6649', color = 'black', alpha= 0.75,size = 1.2) +
  ggplot2::theme_bw()   +
  ggplot2::geom_errorbar(ggplot2::aes(ymin= esc_mean, ymax = esc_mean+ esc_sd), width=.2,size=1.2,
                         position= ggplot2::position_dodge(.9)) +
  ggplot2::ylim(c(0,1.4))+
  ggplot2::labs(x = 'Distance to predator (m)', y = 'Escape direction \n consensus ', title = '') +
  ggplot2::theme(legend.position = 'none',
                 axis.text.y = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_text(size = 22, face = 'bold'),  
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 axis.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5))


noesc_dir <- one_noesc %>% group_by(dist_group, time) %>%
  summarise(esc_mean = abs(mean(sign(radAwayPred), na.rm = TRUE)))

noesc_dir <- noesc_dir %>% group_by(dist_group) %>%
  summarise(esc_sd = sd(abs(esc_mean), na.rm = TRUE),
            esc_mean = mean(abs(esc_mean), na.rm = TRUE))

escdir_noesc <- ggplot2::ggplot(noesc_dir, ggplot2::aes(x = dist_group , y = abs(esc_mean))) +
  ggplot2::geom_col(fill = "#ECDDC3", color = 'black', alpha= 0.75, size = 1.2) +
  ggplot2::theme_bw()   +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=abs(esc_mean), ymax=abs(esc_mean)+abs(esc_sd)), width=.2, size=1.2,
                         position= ggplot2::position_dodge(.9)) +
  ggplot2::ylim(c(0,1.1))+
  ggplot2::labs(x = 'Distance to predator (m)', y = 'Escape direction \n consensus ', title = '') +
  ggplot2::theme(legend.position = 'none',
                 axis.text = ggplot2::element_text(size = 22, face = 'bold'),           
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 axis.title.x = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(size = 24, face = 'bold'),           
                 plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5))

angl_noesc <- angl_noesc + 
  ggplot2::labs(title = 'Simulated data without predator-avoidance')

angl_esc <- angl_esc + ggplot2::labs(title = 'Simulated data with predator-avoidance')

comboplot <- cowplot::plot_grid( angl_noesc, angl_esc, switch_noesc, switch_esc,  escdir_noesc, escdir_esc, 
                                 nrow= 3, ncol = 2)

x.grob <- grid::textGrob("Distance to predator (m)", 
                         gp=grid::gpar(fontface = "bold",fontfamily = "Palatino Linotype", fontsize=26))

comboplot <- comboplot + cowplot::draw_plot_label(
  c('A1', 'A2', 'B1', 'B2', 'C1', 'C2'),
  x = c(0.44, 0.94, 0.44,0.94, 0.44,0.94 ),
  y = c(0.965, 0.965, 0.642 ,0.642, 0.31 ,0.31),
  size = 22,
  fontface = "bold",
  family = "Palatino Linotype",
)

fig_to_save <- gridExtra::grid.arrange(gridExtra::arrangeGrob(comboplot,  bottom = x.grob))


ragg::agg_png('../Results/fig5.png', width = 1200, height = 1100, units = 'px')
cowplot::plot_grid(fig_to_save)
invisible(dev.off())
