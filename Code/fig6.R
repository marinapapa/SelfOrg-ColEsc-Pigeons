## Figure 6. Progression of a collective escape in HoPE. 

source('subplots_functions.R')
source('load_data.R')
library(ggtext)
# extrafont::loadfonts(device = "win")

eg_sim <- import_base_data('../Data/simulated_raw/HoPE_track_eg/', 
                                           data_type = 'sim',
                                           types = c('self', 'forces'))

df <- eg_sim$self
df$ali_angl <- eg_sim$forces$ali_angl
df$coh_angl <- eg_sim$forces$coh_angl
df$sep_angl <- eg_sim$forces$sep_angl

df <- df[df$dist2pred < 60,]
distance_groups <- c('<10', '10-20', '20-30', '30-40', '40-50', '50-60');
dg_breaks <- c(seq(0, 60, 10))
df$dist_group <- cut(df$dist2pred, breaks = dg_breaks, labels = distance_groups)
df <- df[!(is.na(df$dist_group)),]


one_attack <- df[df$time > 235 & df$time < 270,]

pred_pos <- one_attack[one_attack$id == 0, ]
pred_x <- rep(NA, nrow(pred_pos))
pred_y <- rep(NA, nrow(pred_pos))
for (i in 1:nrow(pred_pos))
{
  pred_x[i] <- pred_pos$posx[i] + (pred_pos$dist2pred[i] * pred_pos$dirX2pred[i])
  pred_y[i] <- pred_pos$posy[i] + (pred_pos$dist2pred[i] * pred_pos$dirY2pred[i]) 
}

pred <- data.frame(pred_x, pred_y, time = pred_pos$time)
tplot <- merge(pred, one_attack)

# select representative timepoints
ss1 <- tplot[tplot$time == 241,]
ss2 <- tplot[tplot$time == 246,]
ss3 <- tplot[tplot$time == 248,]
ss4 <- tplot[tplot$time == 249.4,]
ss5 <- tplot[tplot$time == 250.4,]
ss6 <- tplot[tplot$time == 250.8,]
ss7 <- tplot[tplot$time == 251.6,]


## ---------------------------------------------------------
## MAIN TRACK PLOT

main_track <- ggplot2::ggplot(tplot[(dplyr::last(which(tplot$time < 240))+1):which(tplot$time > 253)[1],]) +
  ggplot2::geom_point(ggplot2::aes(posy, posx, color= dist2pred), size = 3)+
  ggplot2::scale_color_gradientn(colours = rev(wesanderson::wes_palette("Zissou1", 
                                                                        25, 
                                                                        type = "continuous")))+
  ggplot2::geom_segment(ggplot2::aes(x = 360, xend = 390, y = 175,  yend = 175, linetype = 'solid'),inherit.aes = FALSE) + 
  ggplot2::geom_segment(ggplot2::aes(x = 360, xend = 360, y = 173,  yend = 177, linetype = 'solid'),inherit.aes = FALSE) + 
  ggplot2::geom_segment(ggplot2::aes(x = 390, xend = 390, y = 173,  yend = 177, linetype = 'solid'),inherit.aes = FALSE) + 
  ggplot2::scale_linetype_identity() + 
  ggplot2::geom_text(ggplot2::aes(x = 375, y = 175),  label = '30 m', vjust = -0.5, size = 6, family = 'Palatino Linotype') +
  ggplot2::theme_bw() +
  ggplot2::labs(x= 'X', 
                y = 'Y', 
                color = 'Predator-prey\ndistance (m):', 
                title = 'A. Flock\'s track during an attack') +
    ggplot2::coord_equal() +
  ggplot2::scale_y_reverse() +
  
  ggplot2::geom_point(ggplot2::aes( y = ss1$posx[9], x = ss1$posy[9]), shape = 5, size = 8) +
  ggplot2::geom_point(ggplot2::aes( y = ss1$pred_x[9], x = ss1$pred_y[9]), shape = 18, size = 11) +
  ggplot2::annotate(geom = 'text', y = ss1$pred_x[1], x = ss1$pred_y[1], label = "1", size = 6, color = 'white', fontface =2, family = 'Palatino Linotype') +
  ggplot2::annotate(geom = 'text', y = ss1$posx[9], x = ss1$posy[9], label = "1", size = 6, color = 'black', fontface =2, family = 'Palatino Linotype') +
  
  ggplot2::geom_point(ggplot2::aes( y = ss2$posx[1], x = ss2$posy[1]), shape = 5, size = 8) +
  ggplot2::geom_point(ggplot2::aes( y = ss2$pred_x[1], x = ss2$pred_y[1]), shape = 18, size = 11) +
  ggplot2::annotate(geom = 'text', y = ss2$pred_x[1], x = ss2$pred_y[1], label = "2", size = 6, color = 'white', fontface = 2, family = 'Palatino Linotype') +
  ggplot2::annotate(geom = 'text', y = ss2$posx[1], x = ss2$posy[1], label = "2", size = 6, color = 'black', fontface = 2, family = 'Palatino Linotype') +
  
  ggplot2::geom_point(ggplot2::aes( y = ss3$posx[1], x = ss3$posy[1]), shape = 5, size = 8) +
  ggplot2::geom_point(ggplot2::aes( y = ss3$pred_x[1], x = ss3$pred_y[1]), shape = 18, size = 11) +
  ggplot2::annotate(geom = 'text', y = ss3$pred_x[1], x = ss3$pred_y[1], label = "3", size = 6, color = 'white', fontface =2, family = 'Palatino Linotype') +
  ggplot2::annotate(geom = 'text', y = ss3$posx[1], x = ss3$posy[1], label = "3", size = 6, color = 'black', fontface =2, family = 'Palatino Linotype') +
  
  ggplot2::geom_point(ggplot2::aes( y = ss4$posx[1], x = ss4$posy[1]), shape = 5, size = 8) +
  ggplot2::geom_point(ggplot2::aes( y = ss4$pred_x[1], x = ss4$pred_y[1]), shape = 18, size = 11) +
  ggplot2::annotate(geom = 'text', y = ss4$pred_x[1], x = ss4$pred_y[1], label = "4", size =6, color = 'white', fontface =2, family = 'Palatino Linotype') +
  ggplot2::annotate(geom = 'text', y = ss4$posx[1], x = ss4$posy[1], label = "4", size = 6, color = 'black', fontface =2, family = 'Palatino Linotype') +
  
  ggplot2::geom_point(ggplot2::aes( y = ss5$posx[1], x = ss5$posy[1]), shape = 5, size = 8) +
  ggplot2::geom_point(ggplot2::aes( y = ss5$pred_x[1], x = ss5$pred_y[1]), shape = 18, size = 11) +
  ggplot2::annotate(geom = 'text', y = ss5$pred_x[1], x = ss5$pred_y[1], label = "5", size = 6, color = 'white', fontface =2, family = 'Palatino Linotype') +
  ggplot2::annotate(geom = 'text',y =  ss5$posx[1], x = ss5$posy[1], label = "5", size = 6, color = 'black', fontface =2, family = 'Palatino Linotype') +
  
  ggplot2::geom_point(ggplot2::aes( y = ss6$posx[1], x = ss6$posy[1]), shape = 5, size = 8) +
  ggplot2::geom_point(ggplot2::aes( y = ss6$pred_x[1], x = ss6$pred_y[1]), shape = 18, size = 11) +
  ggplot2::annotate(geom = 'text', y = ss6$pred_x[1], x = ss6$pred_y[1], label = "6", size = 6, color = 'white', fontface =2, family = 'Palatino Linotype') +
  ggplot2::annotate(geom = 'text', y = ss6$posx[1], x = ss6$posy[1], label = "6", size = 6, color = 'black', fontface =2, family = 'Palatino Linotype') +
  
  ggplot2::geom_point(ggplot2::aes( y = ss7$posx[1], x = ss7$posy[1]), shape = 5, size = 8) +
  ggplot2::geom_point(ggplot2::aes( y = ss7$pred_x[1], x = ss7$pred_y[1]), shape = 18, size = 11) +
  ggplot2::annotate(geom = 'text', y = ss7$pred_x[1], x = ss7$pred_y[1], label = "7", size = 6, color = 'white', fontface = 2, family = 'Palatino Linotype') +
  ggplot2::annotate(geom = 'text', y = ss7$posx[1], x = ss7$posy[1], label = "7", size = 6, color = 'black', fontface = 2, family = 'Palatino Linotype')+
  
  ggplot2::theme(
    text = ggplot2::element_text(family = 'Palatino Linotype'),
    axis.title = ggplot2::element_text(size = 18, 
                                       face = "bold"),
    legend.position = c(0.22,0.05),
    legend.background = ggplot2::element_rect(colour = 'black', 
                                              fill = 'white'),
    legend.direction = 'horizontal',
    legend.text = ggplot2::element_text(size = 16),
    axis.text = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(size = 20, 
                                         face = 'bold'),
    plot.title = ggplot2::element_text(size = 22)
  ) 

ss1f <- ss1
ss1f$type <- c(rep('c', nrow(ss1)/2), rep('p', nrow(ss1)/2))
fake_legend <-  cowplot::get_legend(ggplot2::ggplot(ss1f) +
                                      ggplot2::geom_point(ggplot2::aes(posx, posy, shape = type)) +
                                      ggplot2::scale_shape_manual(breaks = c('c', 'p'), values = c(5, 18), labels = c( 'Flock', 'Predator') )+
                                      ggplot2::labs(shape = 'Position of:') +
                                      ggplot2::theme_bw() +
                                      ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size=5))) +
                                      ggplot2::theme(
                                        text = ggplot2::element_text(family = 'Palatino Linotype'),
                                        axis.title = ggplot2::element_text(size = 18),
                                        legend.position = 'top', #c(0.75,0.1),
                                        legend.background = ggplot2::element_rect(colour = 'black', 
                                                                                  fill = 'white'),
                                        legend.text = ggplot2::element_text(size = 18),
                                        legend.title = ggplot2::element_text(size = 20, face = 'bold'),
                                        legend.direction = 'vertical',
                                        
                                      ))

mainwithleg <- cowplot::ggdraw(main_track) + cowplot::draw_plot(fake_legend, x = -0.32, y = -0.35)


## ---------------------------------------------------------
## SIDE PLOTS

p1 <- minip(ss1, scale_dir = 3) + 
  ggplot2::xlim(c(385,405)) + ggplot2::scale_y_reverse(limits = c(360,345)) + ggplot2::coord_equal() +
  ggplot2::annotate(geom = 'richtext', y = 347, x = 404, label = "1", size = 5, color = 'black', fontface =2,  fill = NA, label.color = NA, family = 'Palatino Linotype' ) +
  ggplot2::geom_point(ggplot2::aes(y = 347, x = 404), shape = 5, size = 8, color = 'black', inherit.aes = FALSE) +
  ggplot2::annotate(geom = 'richtext', y = 359, x = 387, label = "t = 0s", size = 5, color = 'black', fontface =2, family = 'Palatino Linotype')

p2 <- minip(ss2, scale_dir = 3) + ggplot2::theme(legend.position = 'none', axis.text = ggplot2::element_blank(),axis.title = ggplot2::element_blank()) +
  ggplot2::xlim(c(370,390)) + ggplot2::scale_y_reverse(limits = c(270, 255))+ ggplot2::coord_equal()+
  ggplot2::annotate(geom = 'richtext', y = 257, x = 389, label = "2", size = 5, color = 'black', fontface =2, fill = NA, label.color = NA, family = 'Palatino Linotype') +
  ggplot2::geom_point(ggplot2::aes(y = 257, x = 389), shape = 5, size = 8, color = 'black', inherit.aes = FALSE) +
  ggplot2::annotate(geom = 'richtext', y = 269, x = 372, label = "t = 5s", size = 5, color = 'black', fontface =2, family = 'Palatino Linotype')

p3 <- minip(ss3, scale_dir = 3) + ggplot2::theme(legend.position = 'none', axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank()) + 
  ggplot2::xlim(c(340,360)) +  ggplot2::scale_y_reverse(limits = c(245, 230))+ ggplot2::coord_equal()+
  ggplot2::annotate(geom = 'richtext', y = 232, x = 359, label = "3", size = 5, color = 'black', fontface =2, fill = NA, label.color = NA, family = 'Palatino Linotype') +
  ggplot2::geom_point(ggplot2::aes(y = 232, x = 359), shape = 5, size = 8, color = 'black', inherit.aes = FALSE) + 
  ggplot2::annotate(geom = 'richtext', y = 244, x = 342, label = "t = 7s", size = 5, color = 'black', fontface =2, family = 'Palatino Linotype')


p4 <- minip(ss4, scale_dir = 3) + ggplot2::theme(legend.position = 'none', axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank()) +
  ggplot2::xlim(c(318, 338)) +  ggplot2::scale_y_reverse(limits = c(230, 215))+ ggplot2::coord_equal()+
  ggplot2::annotate(geom = 'richtext', y = 217, x = 337, label = "4", size = 5, color = 'black', fontface =2, fill = NA, label.color = NA, family = 'Palatino Linotype') +
  ggplot2::geom_point(ggplot2::aes(y = 217, x = 337), shape = 5, size = 8, color = 'black', inherit.aes = FALSE) +
  ggplot2::annotate(geom = 'richtext', y = 229, x = 320, label = "t = 8.4s", size = 5, color = 'black', fontface =2, family = 'Palatino Linotype')


p5 <- minip(ss5, scale_dir = 3)  + ggplot2::theme(
  legend.position = 'none', 
  axis.title = ggplot2::element_blank(), 
  axis.text = ggplot2::element_blank()) +
  ggplot2::xlim(c(308, 328)) +  ggplot2::scale_y_reverse(limits = c(215, 200))+ ggplot2::coord_equal() +
  ggplot2::annotate(geom = 'richtext', y = 202, x = 327, label = "5", size = 5, color = 'black', fontface =2, fill = NA, label.color = NA, family = 'Palatino Linotype') +
  ggplot2::geom_point(ggplot2::aes(y = 202, x = 327), shape = 5, size = 8, color = 'black', inherit.aes = FALSE)+
  ggplot2::geom_segment(ggplot2::aes(x = 322.5, xend = 327.5, y = 210,  yend = 210), linetype = 'solid', inherit.aes = FALSE) + 
  ggplot2::geom_segment(ggplot2::aes(x = 322.5, xend = 322.5, y = 209.5,  yend = 210.5),inherit.aes = FALSE, linetype = 'solid') + 
  ggplot2::geom_segment(ggplot2::aes(x = 327.5, xend = 327.5, y = 209.5,  yend = 210.5),inherit.aes = FALSE, linetype = 'solid') + 
  ggplot2::geom_text(ggplot2::aes(x = 325, y = 210, label = '5 m'), vjust = -0.5, size = 6) +
  ggplot2::annotate(geom = 'richtext', y = 214, x = 310, label = "t = 9.4s", size = 5, color = 'black', fontface =2, family = 'Palatino Linotype')


p6 <- minip(ss6, scale_dir = 3, ptitle = 'B. Pigeon-agents at t')  + ggplot2::theme(legend.position = 'none', axis.title  = ggplot2::element_blank(), axis.text = ggplot2::element_blank()) +
  ggplot2::xlim(c(303, 323))  +  ggplot2::scale_y_reverse(limits = c(210, 195))+ ggplot2::coord_equal() +
  ggplot2::annotate(geom = 'richtext', y = 197, x = 322, label = "6", size = 5, color = 'black', fontface =2, fill = NA, label.color = NA, family = 'Palatino Linotype') +
  ggplot2::geom_point(ggplot2::aes(y = 197, x = 322), shape = 5, size = 8, color = 'black', inherit.aes = FALSE) +
  ggplot2::annotate(geom = 'richtext', y = 209, x = 305, label = "t = 9.8s", size = 5, color = 'black', fontface =2, family = 'Palatino Linotype')


leg_plot <- minip(rbind(ss1, ss2, ss3, ss4, ss5, ss6), 2)

leges <- cowplot::get_legend(leg_plot)
p1 <- p1 + ggplot2::theme(legend.position = 'none', axis.text = ggplot2::element_blank(), axis.title.y  = ggplot2::element_blank())

minitracks <- cowplot::plot_grid(p6,  p5, p4 , p3, p2, p1, ncol = 1, rel_heights = c(1,1,1,1,1,1.12))


## ---------------------------------------------------------------
## FORCES

all_dists <- list(ss1, ss2, ss3, ss4, ss5, ss6)
all_dists_confl <- list(ss1[ss1$conflict == 1,], ss2[ss2$conflict == 1,], 
                        ss3[ss3$conflict == 1,], ss4[ss4$conflict == 1,],
                        ss5[ss5$conflict == 1,], ss6[ss6$conflict == 1,])
avdirs <- lapply(all_dists, function(x) return(c(mean(x$dirx), mean(x$diry))))
rotangl <- lapply(avdirs, function(x) rad_between(x, c(0,1)))

fps <- list()
k <- 1
for (i in all_dists)
{
  F1 <- i[, c('ali_angl', 'coh_angl', 'sep_angl', 'dist2pred')]
  F1 <- reshape2::melt(F1, id.vars = 'dist2pred')
  colnames(F1) <- c('dist2pred', 'force', 'angle');
  F1$force <- as.character(F1$force)
  F1[F1$force == 'ali_angl', 'force'] <- 4 # relative parameterization of weights in  HoPE
  F1[F1$force == 'coh_angl', 'force'] <- 1
  F1[F1$force == 'sep_angl', 'force'] <- 2
  
  F1$force <- as.numeric(F1$force)
  fps[[k]] <- plot_forces_dist2pred(F1, 
                                    av_dir = avdirs[[k]],
                                    rot_angl = rotangl[[k]], 
                                    ptitle = 'C. Coordination forces') +
    ggplot2::theme(legend.position = 'none')
  k <- k + 1
}

leg <- plot_forces_dist2pred(F1, av_dir = avdirs[[k-1]],rot_angl = rotangl[[k-1]], ptitle='') 
leg <- cowplot::get_legend(leg)
force_plots <- cowplot::plot_grid(
  
  fps[[6]]+ ggplot2::xlim(c(-3.5,2)) + ggplot2::ylim(c(-1.5,4.2)) +
    ggplot2::theme(axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank())+
    ggplot2::annotate(geom = 'richtext', y = -1.3, x = 1.8, label = "6", size = 5, color = 'black', fontface =2,family = 'Palatino Linotype', fill = NA, label.color = NA) +
    ggplot2::geom_point(ggplot2::aes(y = -1.3, x = 1.8), shape = 5, size = 8, color = 'black', inherit.aes = FALSE),
 
  fps[[5]]+ ggplot2::xlim(c(-3.7,1.8)) + ggplot2::ylim(c(-1.5,4.2)) + 
    ggplot2::theme(plot.title = ggplot2::element_blank(), axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank())+
    ggplot2::annotate(geom = 'richtext', y = -1.3, x = 1.6, label = "5", size = 5, color = 'black', fontface =2, family = 'Palatino Linotype', fill = NA, label.color = NA) +
    ggplot2::geom_point(ggplot2::aes(y = -1.3, x = 1.6), shape = 5, size = 8, color = 'black', inherit.aes = FALSE),
  
  fps[[3]]+  ggplot2::xlim(c(-4.1,1.4)) + ggplot2::ylim(c(-1.5,4.2)) + 
    ggplot2::theme(plot.title = ggplot2::element_blank(), axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank())+
    ggplot2::annotate(geom = 'richtext', y = -1.3, x = 1.2, label = "3", size = 5, color = 'black', fontface =2, fill = NA, family = 'Palatino Linotype', label.color = NA) +
    ggplot2::geom_point(ggplot2::aes(y = -1.3, x = 1.2), shape = 5, size = 8, color = 'black', inherit.aes = FALSE)+
    ggplot2::annotate(geom = 'richtext', y = 2, x = 0.3, label = "attraction", size = 7, family = 'Palatino Linotype', color = 'black', fill = NA, label.color = NA) +
    ggplot2::annotate(geom = 'richtext', y = 4, x = -2.5, label = "alignment", size = 7, family = 'Palatino Linotype', color = 'black', fill = NA, label.color = NA)+
    ggplot2::annotate(geom = 'richtext', y = 0.3, x = -2.6, label =  "flock's", size = 7, family = 'Palatino Linotype', color = 'black', fill = NA, label.color = NA) +
    ggplot2::annotate(geom = 'richtext', y = -0.1, x = -2.6, label =  "heading", size = 7, family = 'Palatino Linotype', color = 'black', fill = NA, label.color = NA) +
    ggplot2::geom_curve(data=data.frame(y = 0.4),
                        ggplot2::aes(x = -2.6, y = 0.5, xend = -2, yend = 1.1),
                        arrow= ggplot2::arrow(length=ggplot2::unit(2.2, "mm")), lwd=1, curvature = -.2,
                        color="black"),
  
  fps[[2]]+ ggplot2::xlim(c(-4.1,1.4)) + ggplot2::ylim(c(-1.5,4.2)) +
    ggplot2::theme(plot.title = ggplot2::element_blank(), axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank())+
    ggplot2::annotate(geom = 'richtext', y = -1.3, x = 1.2, label = "2", size = 5, color = 'black', fontface =2,family = 'Palatino Linotype', fill = NA, label.color = NA) +
    ggplot2::geom_point(ggplot2::aes(y = -1.3, x = 1.2), shape = 5, size = 8, color = 'black', inherit.aes = FALSE) ,
  
  fps[[1]]+ ggplot2::xlim(c(-2.1,3.4)) + ggplot2::ylim(c(-1.5,4.2)) + 
    ggplot2::theme(plot.title = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(), axis.text = ggplot2::element_blank()) +
    ggplot2::annotate(geom = 'richtext', y = -1.3, x = 3.2, label = "1", size = 5, color = 'black', fontface =2,family = 'Palatino Linotype', fill = NA, label.color = NA) +
    ggplot2::geom_point(ggplot2::aes(y = -1.3, x = 3.2), shape = 5, size = 8, color = 'black', inherit.aes = FALSE) ,
  
   ncol = 1, rel_heights = c(1.06,1,1,1,1.1))


## ---------------------------------------------------------------
## TOTAL COMBO PLOT

main_with_legs <- cowplot::plot_grid( mainwithleg, leges, leg, rel_heights = c( 2, 0.2,  0.15), ncol = 1)

totplot <- cowplot::plot_grid(main_with_legs, minitracks, force_plots, rel_widths = c(1.7, 0.9, 0.7), ncol = 3)

ggplot2::ggsave(
  plot = totplot, 
  filename = paste0('../Results/fig6.png'),
  width = 16.5,
  height = 19,
  dpi = 600
)

# ragg::agg_png('../Results/fig6test.png', width = 1300, height = 1500, units = 'px')
# cowplot::plot_grid(totplot)
# invisible(dev.off())
# 

