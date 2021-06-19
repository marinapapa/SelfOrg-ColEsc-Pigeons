## Figure 7. Effect of coordination forces on `in-conflict' flock-members within 30 meters to the predator in HoPE. 

source('plots_functions.R')
source('load_data.R')

one_sim <- import_base_data('../Data/simulated_raw/HoPE_track_eg/', 
                                           data_type = 'sim',
                                           types = c('self', 'forces'))

df <- one_sim$self
df$ali_angl <- one_sim$forces$ali_angl
df$coh_angl <- one_sim$forces$coh_angl
df$sep_angl <- one_sim$forces$sep_angl

df <- df[df$dist2pred < 60,]
distance_groups <- c('<10', '10-20', '20-30', '30-40', '40-50', '50-60');
dg_breaks <- c(seq(0, 60, 10))
df$dist_group <- cut(df$dist2pred, breaks = dg_breaks, labels = distance_groups)

partself <-df[df$time > 235 & df$time < 270,]

pred_pos <- partself[partself$id == 0, ]
pred_x <- rep(NA, nrow(pred_pos))
pred_y <- rep(NA, nrow(pred_pos))
for (i in 1:nrow(pred_pos))
{
  pred_x[i] <- pred_pos$posx[i] + (pred_pos$dist2pred[i] * pred_pos$dirX2pred[i])
  pred_y[i] <- pred_pos$posy[i] + (pred_pos$dist2pred[i] * pred_pos$dirY2pred[i]) 
}

pred <- data.frame(pred_x, pred_y, time = pred_pos$time)
tplot <- merge(pred, partself)

### --------------------------------
## TOTAL FORCE
allforce <- tplot[, c('ali_angl', 'coh_angl', 'sep_angl', 'dist2pred', 'conflict')]
allforce <- allforce[allforce$conflict == 1,]
allforce <- reshape2::melt(allforce, id.vars = 'dist2pred')
colnames(allforce) <- c('dist2pred', 'force', 'angle');
allforce$force <- as.character(allforce$force)
allforce$force_type <-allforce$force 

allforce[allforce$force == 'ali_angl', 'force'] <- 4
allforce[allforce$force == 'coh_angl', 'force'] <- 1
allforce[allforce$force == 'sep_angl', 'force'] <- 2

allforce$force <- as.numeric(allforce$force)
distance_groups <- c('<10', '10-20', '20-30', '30-40', '40-50', '50-60');
dg_breaks <- c(seq(0, 60, 10))
allforce$dist_group <- cut(allforce$dist2pred, breaks = dg_breaks, labels = distance_groups)
tdf <- allforce[, c('force', 'angle')]
for (i in 1:nrow(tdf))
{
  tdf[i,] <- pointRelPos(dist = tdf$force[i], angl = tdf$angle[i])
}
force_labels <- c('1' = 'Attraction', '2' =  'Avoidance', '4' = 'Alignment')

tdf$dist_group <- allforce[, 'dist_group']
tdf$force_type <- allforce[, 'force_type']
tdf <- tdf[tdf$force_type != 'sep_angl',]
tdf <- tdf[!is.na(tdf$dist_group),]

tdf <- tdf[tdf$dist_group %in% c("<10", "10-20", "20-30"),]
Rushmore <-  c("#efd4a6", "#EABE94", "#0B775E", "#35274A" ,"#F2300F")

fcp <- ggplot2::ggplot(tdf, ggplot2::aes(x = force, y = angle)) +
  ggplot2::stat_density_2d(ggplot2::aes(fill = ggplot2::after_stat(ndensity)), geom = "raster", alpha = 0.9, contour = FALSE) +
  ggplot2::facet_wrap( ~ dist_group , labeller = ggplot2::labeller(force_type = force_labels)) +
  ggplot2::guides(colour = FALSE)+
  ggplot2::geom_vline(xintercept = 0, linetype = 2, alpha = 0.9) + 
  ggplot2::guides(fill = ggplot2::guide_colourbar(barwidth = 15))+
  ggplot2::coord_equal() +
  ggplot2::ylim(c(-2, max(tdf$angle, na.rm = TRUE) + 1.8))+
  ggplot2::xlim(c(-2,2))+
  ggplot2::geom_point(ggplot2::aes(x = 0, y = 0), colour = 'black', size = 5, shape = 17)+
  ggplot2::scale_fill_gradientn(colours = Rushmore)+
  ggplot2::scale_color_manual(breaks = c('1', '4'),
                              labels = c('Attraction', 'Alignment'),
                              values = c('black', 'black'))+
  ggplot2::theme_bw()+
  ggplot2::geom_segment(ggplot2::aes(0, 0, xend = 0, yend= 2),
                        lineend = 'butt', linejoin = 'round',  size = 1, arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm"))) + 
  ggplot2::labs(fill = 'Force\ndensity') +
  ggplot2::labs(x = "X", y = 'Y', title = '')+
    ggplot2::theme(
      text = ggplot2::element_text(family = 'Palatino Linotype'),
      axis.title = ggplot2::element_text(size = 18, face = "bold"),
      legend.position="top",
      legend.justification = 'center',
      legend.background = ggplot2::element_rect(colour = 'black', 
                                                fill = 'white'),
      legend.direction = 'horizontal',
      legend.text = ggplot2::element_text(size = 16),
      legend.margin = ggplot2::margin(6, 20, 6, 15),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 18),
      legend.title = ggplot2::element_text(size = 20, face = 'bold'),
      plot.title = ggplot2::element_text(size = 22),
      strip.background = ggplot2::element_rect(colour = "black", fill = "white"),
      strip.text = ggplot2::element_text(size = 18, face = "bold")
    ) 
  

tosave <- 
cowplot::ggdraw() +
  cowplot::draw_plot(fcp) +
  cowplot::draw_image("../Images/falcon.png",  x = -0.35, y = -0.33, scale = .1) +
  cowplot::draw_image("../Images/falcon.png",  x = -0.07, y = -0.33, scale = .1) +
  cowplot::draw_image("../Images/falcon.png",  x = 0.37, y = -0.33, scale = .1)

# ragg::agg_png('../Results/fig7.png', width = 1000, height = 800, units = 'px')
# cowplot::plot_grid(tosave)
# invisible(dev.off())


ggplot2::ggsave(
  plot = tosave, 
  filename = paste0('../Results/fig7.R'),
  width = 14,
  height = 11,
  dpi = 500
)

