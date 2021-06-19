## Figure 4. Turning direction frequencies of flock members.

###############################################################################
# Empirical data (Fig 4A)

Zissou1_cols <- c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00")
Rushmore <-  c("#E1BD6D", "#EABE94", "#0B775E", "#35274A" ,"#F2300F")
cowplot::set_null_device(cairo_pdf)
extrafont::loadfonts(device = 'win')

# Conflict scenarios
load('../Data/empirical/turning_direction/DatCon.rda') # dat.con

dat.con$dist2pred <- as.numeric(dat.con$dist2pred);
distance_groups <- c('<10', '10-20', '20-30', '30-40', '40-50', '50-60', '>60');
dg_breaks <- c(seq(0, 60, 10), max(dat.con$dist2pred, na.rm = FALSE));
dat.con$dist_group <- cut(dat.con$dist2pred, breaks = dg_breaks, labels = distance_groups) ;

df_decout <- dat.con[, c('biased.ali', 'dist_group')]
colnames(df_decout) <- c('dec_out', 'dist_group')
df_decout <- df_decout[complete.cases(df_decout), ];
toplot <- plyr::count(df_decout, c('dec_out', 'dist_group'));
totcount <- plyr::count(df_decout, c('dist_group'));

for (i in 1:length(toplot$dist_group))
{
  toplot$perc[i] <- (100 * toplot$freq[i] / totcount[totcount$dist_group == toplot$dist_group[i], 2]);
}

toplot <- toplot[toplot$dist_group != '>60',]
outw <- ggplot2::ggplot(toplot, ggplot2::aes(x = toplot[, 'dist_group'])) +
  ggplot2::geom_col(ggplot2::aes(y = toplot[,'perc'], fill = toplot[, 'dec_out']), color = "black", position = ggplot2::position_dodge()) +
  ggplot2::theme_bw()   +
  ggplot2::ylim(c(0, 100)) +
  ggplot2::labs(x = 'Distance to predator (m)', y = 'Turning direction (%)', fill = 'Outwards escape', title = paste0('Empirical data')) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 22),
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 legend.position = 'top',
                 legend.direction = 'vertical',
                 legend.text = ggplot2::element_text(size = 22),
                 axis.text = ggplot2::element_text(size = 22, face = 'bold'),
                 axis.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 24, face = "bold") ) +
  ggplot2::scale_fill_manual(breaks = c("TRUE", "FALSE"),
                             labels = c("Group alignment", "Predator avoidance"),
                             values = c(Zissou1_cols[[1]], Zissou1_cols[[5]]));

# No conflict scenarios 
load('../Data/empirical/turning_direction/DatNoCon.rda') # dat.nocon
dat.nocon$dist2pred <- as.numeric(dat.nocon$dist2pred);
distance_groups <- c('<10', '10-20', '20-30', '30-40', '40-50', '50-60', '>60');
dg_breaks <- c(seq(0, 60, 10), max(dat.nocon$dist2pred, na.rm = FALSE));
dat.nocon$dist_group <- cut(dat.nocon$dist2pred, breaks = dg_breaks, labels = distance_groups) ;

df_decout <- dat.nocon[, c('biased.ali', 'dist_group')]
colnames(df_decout) <- c('dec_out', 'dist_group')
df_decout <- df_decout[complete.cases(df_decout), ];
toplot2 <- plyr::count(df_decout, c('dec_out', 'dist_group'));
totcount <- plyr::count(df_decout, c('dist_group'));

for (i in 1:length(toplot2$dist_group))
{
  toplot2$perc[i] <- (100 * toplot2$freq[i] / totcount[totcount$dist_group == toplot2$dist_group[i], 2]);
}

toplot2 <- toplot2[toplot2$dist_group != '>60',]

inw <- ggplot2::ggplot(toplot2, ggplot2::aes(x = toplot2[, 'dist_group'])) +
  ggplot2::geom_col(ggplot2::aes(y = toplot2[, 'perc'],  fill = toplot2[, 'dec_out']), color = "black", position= ggplot2::position_dodge()) +
  ggplot2::theme_bw()   +
  ggplot2::ylim(c(0, 100)) +
  ggplot2::labs(x = 'Distance to predator (m)', y = 'Turning direction (%)',fill = 'Inwards escape', title = paste0('Empirical data')) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 22),
                 legend.position = 'top',
                 legend.direction = 'vertical',
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 legend.text = ggplot2::element_text(size = 22),
                 axis.text = ggplot2::element_text(size = 22, face = 'bold'),
                 axis.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 24, face = "bold")) +
  ggplot2::scale_fill_manual(breaks = c("TRUE", "FALSE"),
                             labels = c("Group alignment / predator avoidance", "Away from group / towards predator"),
                             #values = c("orchid4", "khaki4"));
                             values = c(Rushmore[[3]], Zissou1_cols[[4]]));

gridExtra::grid.arrange(outw, inw, ncol = 2)

###############################################################################
# Simulated data (Fig 4B, 4C)
sim_types <- c(
  "chase_normal", 
  "no_avoid")

all_decouts <- lapply(seq_along(sim_types),
                      function(x) { read.csv(paste0('../Data/simulated/turn_direction/dec_out_', sim_types[x], '.csv'))}
)

norm_conf <- data_for_turn_dir(all_decouts[[1]], condition = 'conflict')
navoid_conf <- data_for_turn_dir(all_decouts[[2]], condition = 'conflict')
norm_nconf <- data_for_turn_dir(all_decouts[[1]], condition = 'noconflict')
navoid_nconf <- data_for_turn_dir(all_decouts[[2]], condition = 'noconflict')

avoid_out <- ggplot2::ggplot(norm_conf, ggplot2::aes(x = dist_group)) +
  ggplot2::geom_col(ggplot2::aes(y = perc, fill = factor(dec_out, levels = rev(levels(dec_out)))), color = "black", position = ggplot2::position_dodge()) +
  ggplot2::theme_bw()   +
  ggplot2::ylim(c(0, 100)) +
  ggplot2::labs(x = '', y = 'Turning direction (%)',  fill = 'Conflict scenarios', title = '') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 22,  face = "bold.italic"),
                 legend.position = 'top',
                 legend.justification = 'center',
                 text = ggplot2::element_text(family = "Palatino Linotype", color= 'black'),
                 legend.direction = 'vertical',
                 legend.text = ggplot2::element_text(size = 22),
                 axis.text = ggplot2::element_text(size = 22, face = 'bold'),
                 axis.title = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 24, face = "bold", hjust = 0.5)) +
  ggplot2::scale_fill_manual(breaks = c("f", "p"),
                             labels = c("Group alignment", "Predator avoidance"),
                             values = c(Zissou1_cols[[1]], Zissou1_cols[[5]]));

navoid_out <- ggplot2::ggplot(navoid_conf, ggplot2::aes(x = dist_group)) +
  ggplot2::geom_col(ggplot2::aes(y = perc, fill = factor(dec_out, levels = rev(levels(dec_out)))), color = "black", position = ggplot2::position_dodge()) +
  ggplot2::theme_bw()   +
  ggplot2::ylim(c(0, 100)) +
  ggplot2::labs(x = 'Distance to predator (m)', y = 'Turning direction (%)', title = '') +
  ggplot2::theme(legend.position = 'none',
                 axis.text = ggplot2::element_text(size = 22, face = 'bold'),
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 axis.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 24, face = "bold", hjust = 0.5)) +
  ggplot2::scale_fill_manual(breaks = c("f", "p"),
                             labels = c("Group alignment", "Predator avoidance"),
                             values = c(Zissou1_cols[[1]], Zissou1_cols[[5]]));

avoid_in <- ggplot2::ggplot(norm_nconf, ggplot2::aes(x = dist_group)) +
  ggplot2::geom_col(ggplot2::aes(y = perc,  fill =  factor(dec_out, levels = rev(levels(dec_out)))), color = "black", position= ggplot2::position_dodge()) +
  ggplot2::theme_bw()   +
  ggplot2::ylim(c(0, 100)) +
  ggplot2::labs(x = '', y = 'Turning direction (%)', fill = 'Non-conflict scenarios', title = '') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 24,  face = "bold.italic"),
                 legend.position = 'top',
                 legend.direction = 'vertical',
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 legend.text = ggplot2::element_text(size = 22),
                 axis.text = ggplot2::element_text(size = 22, face = 'bold'),
                 axis.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 24,face = "bold",hjust = 0.5)) +
  ggplot2::scale_fill_manual(breaks = c("f", "n"),
                             labels = c("Group alignment & predator avoidance", "Away from group & towards predator"),
                             values = c(Rushmore[[3]], Zissou1_cols[[4]]));

navoid_in <- ggplot2::ggplot(navoid_nconf, ggplot2::aes(x = dist_group)) +
  ggplot2::geom_col(ggplot2::aes(y = perc,  fill =  factor(dec_out, levels = rev(levels(dec_out)))), color = "black", position= ggplot2::position_dodge()) +
  ggplot2::theme_bw()   +
  ggplot2::ylim(c(0, 100)) +
  ggplot2::labs(x = 'Distance to predator (m)', y = 'Turning direction (%)', title = ' ') +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 22, face = "bold.italic"),
                 legend.position = 'none',
                 text = ggplot2::element_text(family = "Palatino Linotype"),
                 legend.direction = 'vertical',
                 legend.text = ggplot2::element_text(size = 22),
                 axis.text = ggplot2::element_text(size = 22, face = 'bold'),
                 axis.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(size = 24,face = "bold")) +
  ggplot2::scale_fill_manual(breaks = c("f", "n"),
                             labels = c("Group alignment / predator avoidance", "Away from group / towards predator"),
                             values = c(Rushmore[[3]], Zissou1_cols[[4]]));

out_leg <- cowplot::get_legend(avoid_out)
avoid_out <- avoid_out + ggplot2::theme(legend.position = 'none')
in_leg <- cowplot::get_legend(avoid_in)
avoid_in <- avoid_in + ggplot2::theme(legend.position = 'none')
legends <- cowplot::plot_grid(out_leg, in_leg, nrow = 1)

outw <- outw + 
  ggplot2::labs(title = '\n A: Empirical data ') +
  ggplot2::theme(legend.position = 'none', axis.text.x = ggplot2::element_blank(), plot.title = ggplot2::element_text(hjust = 0.5))

inw <- inw + 
  ggplot2::theme(legend.position = 'none', plot.title = ggplot2::element_blank())

avoid_out <- avoid_out +
  ggplot2::labs(title = 'B: Simulated data\nwith predator avoidance') +
  ggplot2::theme(legend.position = 'none', axis.text = ggplot2::element_blank() )

avoid_in<- avoid_in +
  ggplot2::theme(legend.position = 'none', axis.text.y = ggplot2::element_blank(), plot.title = ggplot2::element_blank())

navoid_out <- navoid_out +
  ggplot2::labs(title = 'C: Simulated data\nwithout predator avoidance') +
  ggplot2::theme(legend.position = 'none',axis.text = ggplot2::element_blank(), )

navoid_in<- navoid_in +
  ggplot2::labs(x = 'Distance to predator (m)', y = '', title = ' ') +
  ggplot2::theme(legend.position = 'none',  axis.text.y= ggplot2::element_blank(),  plot.title = ggplot2::element_blank())

emp <- cowplot::plot_grid(outw, inw, ncol = 1)
avoid_p <- cowplot::plot_grid(avoid_out, avoid_in, ncol = 1)
no_avoid_p <- cowplot::plot_grid(navoid_out, navoid_in, ncol = 1)
leges <- cowplot::plot_grid(NA, legends, NA, ncol = 3, rel_widths = c(0.1, 1, 0.1))

plots_comb <- cowplot::plot_grid(
  emp,
  avoid_p, 
  no_avoid_p,
  ncol = 3)
full_sim_emp <- cowplot::plot_grid( leges, plots_comb, rel_heights = c(0.1, 1), nrow = 2)

y.grob <- grid::textGrob("Turning direction (%)", 
                   gp= grid::gpar(fontface="bold", fontfamily = "Palatino Linotype", fontsize=26), rot=90)

x.grob <- grid::textGrob("Distance to predator (m)", 
                   gp=grid::gpar(fontface="bold",fontfamily = "Palatino Linotype", fontsize=26))


fig_to_save <- gridExtra::grid.arrange(gridExtra::arrangeGrob(full_sim_emp, left = y.grob, bottom = x.grob))

# ragg::agg_png('../Results/fig4_ragg.png', width = 1200, height = 1100, units = 'px')
# cowplot::plot_grid(fig_to_save)
# invisible(dev.off())

ggplot2::ggsave(
  plot = fig_to_save, 
  filename = paste0('../Results/fig4.png'),
  width = 17,
  height = 15,
  dpi = 600
)
