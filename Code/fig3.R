## Figure 3. Comparison of real and simulated flocks of pigeons.

## Model performance

# Load simulated data
transf_data_dir <- '../Data/simulated/'

sim_type <- 'chase'
load(paste0(transf_data_dir, 'sim_datas_', sim_type, '.RData')) # name: sim_datas

# Load empirical data

data_dir <- '../Data/empirical/transformed/'
self_c <- read.csv(paste0(data_dir, 'all_self_c.csv'))
ctrl_data_self <- split(self_c, self_c$column_label)
self_c <- read.csv(paste0(data_dir, 'all_neighb_c.csv'))
ctrl_data_neighb <- split(self_c, self_c$column_label)
self_c <- read.csv(paste0(data_dir, 'all_spatial_struct_c.csv'))
ctrl_data_sp_struct <- split(self_c, self_c$column_label)


# Plot Comparisons

EMP <- 2 
SIM <- 56 
simd <- sim_datas[[SIM]]
emp_self <- ctrl_data_self[[EMP]]
Zissou1_cols <- c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00")

sp_sim <- ggplot2::ggplot(simd$self_main) +
  ggplot2::geom_density(ggplot2::aes(speed), fill = Zissou1_cols[1], color = 'black', alpha = 1, size = 1.2) +
  ggplot2::geom_vline(xintercept = mean(simd$self_main$speed, na.rm = TRUE), linetype="dotted",
                      color = "black", size=1.5)+
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Speed (m/s)", y = "Frequency") +
  ggplot2::xlim(c(12.5, 25))+
  ggplot2::theme(legend.title = ggplot2::element_text(size = 22),
                 legend.position = 'top',
                 legend.direction = 'vertical',
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 legend.text = ggplot2::element_text(size = 22),
                 axis.title.y = ggplot2::element_blank(),
                 axis.text = ggplot2::element_text(size = 22, face = 'bold'),
                 axis.title.x = ggplot2::element_text(size = 22, face = 'bold'),
                 plot.title = ggplot2::element_text(size = 24, face = "bold"))

sp_sim  


sp_emp_df <- emp_self[complete.cases(emp_self$speed) & emp_self$speed > quantile(emp_self$speed, 0.1, na.rm = TRUE),]
sp_emp <- ggplot2::ggplot(sp_emp_df) +
  ggplot2::geom_density(ggplot2::aes(speed), fill = Zissou1_cols[4], color = 'black', alpha = 0.9, size = 1.2) +
  ggplot2::geom_vline(xintercept = mean(sp_emp_df$speed, na.rm = TRUE), linetype="dotted",
                      color = "black", size=1.5)+
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Speed (m/s)", y = "Frequency") +
  ggplot2::xlim(c(12.5, 25))+
  ggplot2::theme(legend.title = ggplot2::element_text(size = 22),
                 legend.position = 'top',
                 legend.direction = 'vertical',
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 legend.text = ggplot2::element_text(size = 22),
                 axis.text.y = ggplot2::element_text(size = 22, face = 'bold'),
                 axis.text.x = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 24, face = "bold"))

sp_emp
speeds <- cowplot::plot_grid(sp_emp, sp_sim, ncol = 1)

## ----------------------
## NND

EMP <- 11 
SIM <- 38 
simd <- sim_datas[[SIM]]
emp_neighb <- ctrl_data_neighb[[EMP]]
nnd_sim <- ggplot2::ggplot(simd$neighb) +
  ggplot2::geom_density(ggplot2::aes(dist2n1), fill = Zissou1_cols[1], color = 'black', alpha = 1, size = 1.2) +
  ggplot2::geom_vline(xintercept = mean(simd$neighb$dist2n1, na.rm = TRUE), linetype="dotted",
                      color = "black", size=1.5)+
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Nearest neighbor distance (m)", y = "Frequency") +
  ggplot2::xlim(c(0, 6))+
  ggplot2::theme(legend.title = ggplot2::element_text(size = 22),
                 legend.position = 'top',
                 legend.direction = 'vertical',
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 legend.text = ggplot2::element_text(size = 22),
                 axis.text = ggplot2::element_text(size = 22, face = 'bold'),
                 axis.title.x = ggplot2::element_text(size = 22, face = 'bold'),
                 axis.title.y = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 24, face = "bold"))

nnd_sim

nnd_emp_df <- emp_neighb[complete.cases(emp_neighb$dist2n1),]
nnd_emp <- ggplot2::ggplot(nnd_emp_df) +
  ggplot2::geom_density(ggplot2::aes(dist2n1), fill = Zissou1_cols[4], color = 'black', alpha = 0.9, size = 1.2) +
  ggplot2::geom_vline(xintercept = mean(nnd_emp_df$dist2n1, na.rm = TRUE), linetype="dotted",
                      color = "black", size=1.5)+
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Nearest neighbor distance (m)", y = "Frequency") +
  ggplot2::xlim(c(0, 6))+
  ggplot2::theme(legend.title = ggplot2::element_text(size = 22),
                 legend.position = 'top',
                 legend.direction = 'vertical',
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 legend.text = ggplot2::element_text(size = 22),
                 axis.text.y = ggplot2::element_text(size = 22, face = 'bold'),
                 axis.text.x = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 24, face = "bold"))

nnds <- cowplot::plot_grid(nnd_emp, nnd_sim, ncol = 1)
nnds

## ------------------------------------------
## Relative pos
EMP <- 13 
SIM <- 15 
simd <- sim_datas[[SIM]]
emp_struct <- ctrl_data_sp_struct[[EMP]]
emp_nn <- emp_struct[emp_struct$neighb == 1, ]
rel_emp <- ggplot2::ggplot(emp_nn,
                           ggplot2::aes(x = x, y = y)) +
  ggplot2::stat_density_2d(ggplot2::aes(fill = ggplot2::after_stat(nlevel)),
                           geom = "polygon", bins = 10, color = 'black', alpha = 0.5) +
  ggplot2::geom_point(ggplot2::aes(x = 0, y = 0),
                      color = 'black', 
                      shape = 17, 
                      size = 3) +
  ggplot2::coord_equal() +
  ggplot2::lims(x = c(-2, 2),
                y = c(-2, 2)) +
  ggplot2::scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1",
                                                                   25, 
                                                                   type = "continuous"))+
  ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = 12)) +
  
  ggplot2::labs(x = "Relative position of\nnearest neighbor - X",
                y = "Relative position of\nnearest neighbor - Y",
                fill = "Density:") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 22),
                 legend.position = "right",
                 legend.direction = 'horizontal',
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 legend.text = ggplot2::element_text(size = 18),
                 axis.text.y = ggplot2::element_text(size = 22, face = 'bold'),
                 axis.text.x = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 24, face = "bold"))



legen <- cowplot::get_legend(rel_emp)
rel_emp <- rel_emp + ggplot2::theme(legend.position="none")

simd$sp_struct <- FlockAnalyzeR::neighb_relative_positions(simd$neighb)
sim_nn <- simd$sp_struct[simd$sp_struct$neighb == 1, ]
rel_sim <- ggplot2::ggplot(sim_nn,
                           ggplot2::aes(x = x, y = y)) +
  ggplot2::stat_density_2d(ggplot2::aes(fill = ggplot2::after_stat(nlevel)),
                           geom = "polygon", bins = 10, color = 'black', alpha = 0.5) +
  ggplot2::geom_point(ggplot2::aes(x = 0, y = 0), 
                      colour = 'black', 
                      shape = 17, 
                      size = 3) +
  ggplot2::coord_equal() +
  ggplot2::lims(x = c(-2, 2),
                y = c(-2, 2)) +
  ggplot2::scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1",
                                                                   25, 
                                                                   type = "continuous"))+
  ggplot2::labs(x = "Relative position of\nnearest neighbor - X",
                y = "Relative position of\nnearest neighbor - Y",
                color = 'Density') +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 22),
                 legend.position = "none",
                 legend.direction = 'horizontal',
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 legend.text = ggplot2::element_text(size = 22),
                 axis.text = ggplot2::element_text(size = 22, face = 'bold'),
                 axis.title.x = ggplot2::element_text(size = 22, face = 'bold'),
                 axis.title.y = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 24, face = "bold"))

rels <- cowplot::plot_grid(rel_emp, legen, rel_sim, ncol = 1, rel_heights = c(1.1,0.1,1.25))
legs <- cowplot::plot_grid(legen, ncol = 1)
## -------------------------------------------------
## MULTIPLOT

speed_nnd <- cowplot::plot_grid(speeds, nnds, ncol = 2,  rel_widths = c(1,1))

y.grob <- grid::textGrob("Frequency", 
                         gp= grid::gpar(fontface="bold", fontfamily = "Palatino Linotype", fontsize=24), rot=90)

speed_nnd <- gridExtra::grid.arrange(gridExtra::arrangeGrob(speed_nnd,  left = y.grob))

y.grob <- grid::textGrob("Relative position of nearest neighbor - Y", 
                         gp= grid::gpar(fontface="bold", fontfamily = "Palatino Linotype", fontsize=24), rot=90)
rels <- gridExtra::grid.arrange(gridExtra::arrangeGrob(rels,  left = y.grob))

comboplot <- cowplot::plot_grid(speed_nnd, rels, ncol = 2,  rel_widths = c(2,1.3))

comboplot_lab <- comboplot + cowplot::draw_plot_label(
  c('A1', 'A2', 'B1', 'B2', 'C1', 'C2'),
  x = c(0.26, 0.26, 0.56, 0.56, 0.949, 0.949),
  y = c(0.995, 0.495, 0.995 ,0.495, 0.995,0.50),
  size = 22,
  fontface = "bold",
  family = "Palatino Linotype",
)

fig_to_save <- gridExtra::grid.arrange(gridExtra::arrangeGrob(comboplot_lab,  left = y.grob))

# ragg::agg_png('../Results/fig3_ragg.png', width = 1300, height = 1000, units = 'px')
# cowplot::plot_grid(comboplot_lab)
# invisible(dev.off())

ggplot2::ggsave(
  filename = '../Results/fig3.png',
  plot = comboplot_lab,
  width = 15,
  height = 10,
  dpi = 500
)








