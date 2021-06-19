## ----------------------------------
## Plot-helper functions and mini plots of simulation screenshots

library(grid)
library(rlang)

rotate <- function(vec, angl)
{
  c <- cos(angl)
  s <- sin(angl)
  ret <- c(vec[1] * c - vec[2] * s, vec[1] * s + vec[2] * c)
  return(ret)
}

norm_vec <- function(x) {x / sqrt(sum(x^2))}

minip <- function(x, scale_dir = 1, ptitle = '')
{
  # legend drawing function, copied from ggplot2

  distance_groups <- c('<10', '10-20', '20-30', '30-40', '40-50');
  dg_breaks <- c(seq(0, 50, 10))
  x$dist_group <- cut(x$dist2pred, breaks = dg_breaks, labels = distance_groups)

  Zissou1_cols <- c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00")
  Rushmore <-  c("#E1BD6D", "#EABE94", "#0B775E", "#35274A" ,"#F2300F")
  x$ar_type <-  rep(NA, nrow(x))
  x[sign(x$radAwayPred) == 1, 'ar_type'] <- 'closed' 
  x[sign(x$radAwayPred) == -1, 'ar_type'] <- 'open' 
  x$ar_linejoin <-  rep(NA, nrow(x))
  x[sign(x$radAwayPred) == 1, 'ar_linejoin'] <- 'bevel' 
  x[sign(x$radAwayPred) == -1, 'ar_linejoin'] <- 'mitre' 
  x$ar_size <-  rep(NA, nrow(x))
  x[sign(x$radAwayPred) == 1, 'ar_size'] <- 0.8
  x[sign(x$radAwayPred) == -1, 'ar_size'] <- 1.2 
  x$ar_col <-  rep(NA, nrow(x))
  x[sign(x$radAwayPred) == 1, 'ar_col'] <-  Zissou1_cols[1]
  x[sign(x$radAwayPred) == -1, 'ar_col'] <- Zissou1_cols[5]
  p <- ggplot2::ggplot(x, ggplot2::aes(posy, posx)) +
    ggplot2::geom_segment(ggplot2::aes(xend = posy + scale_dir * diry, yend= posx + scale_dir * dirx,
                          color = as.factor(sign(x$radAwayPred))),
                          lineend = 'butt', linejoin = x$ar_linejoin,
                          arrow = ggplot2::arrow(type = x$ar_type, length = ggplot2::unit(0.2, "cm"))
                          ) +
     ggplot2::geom_segment(ggplot2::aes(xend = posy + scale_dir * diry, yend= posx + scale_dir * dirx,
                                        ),
                           size = 2, colour = x$ar_col,
                           lineend = 'butt', linejoin = x$ar_linejoin, 
                           arrow = ggplot2::arrow(type = x$ar_type, length = ggplot2::unit(0.2, "cm"))) + 
    ggplot2::geom_segment(ggplot2::aes(xend = posy + scale_dir * diry, yend= posx + scale_dir * dirx),
                          size = x$ar_size,
                          lineend = 'butt', linejoin = x$ar_linejoin,
                          arrow = ggplot2::arrow(type = x$ar_type, length = ggplot2::unit(0.2, "cm"))) + 
    ggplot2::geom_point(ggplot2::aes(fill = as.factor(dist_group), shape = as.factor(conflict)), size = 4) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_manual(breaks = c('<10', '10-20', '20-30','30-40'), 
                               values = c(Zissou1_cols[5], '#E69F00', Zissou1_cols[3],  Zissou1_cols[1]), 
                               labels = c('<10', '10-20', '20-30','30-40'))+   
  
    ggplot2::scale_shape_manual(breaks = c(0,1), 
                                values = c(21, 23), 
                                labels = c('Non-conflict', 'Conflict')) +
    ggplot2::scale_color_manual(breaks = c(-1,1), 
                                  #values = c('solid','dashed'), 
                                  values = c(Zissou1_cols[1], Zissou1_cols[5] ), 
                                  labels = c('Left', 'Right')) +
    ggplot2::labs(x = 'X', y = 'Y', title = ptitle,
                  color = '    Escape direction:', 
                  fill = ' B. Predator-prey distance (m):',
                  shape = '    Scenario:') +
    ggplot2::theme(
      text = ggplot2::element_text(family = 'Palatino Linotype'),
      panel.background = ggplot2::element_rect(fill = "white", #78B7C5",
                                               colour = "white", #"aliceblue",
                                               size = 0.5,
                                               linetype = "solid"),
      axis.title = ggplot2::element_text(size = 18, face = "bold"),
      plot.title = ggplot2::element_text(size = 22),
      legend.position = 'top',
      legend.direction = 'horizontal',
      legend.box = 'vertical',
      legend.box.just = 'right',
      legend.justification = 'right',
      legend.box.background = ggplot2::element_rect(colour = 'black', 
                                                    fill = 'white'),
      legend.background = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 18),
      axis.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 20,
                                           face = 'bold'),
      legend.margin = ggplot2::margin(3,5,3,5)
    ) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, 
                                                   override.aes = list(shape=21)),
                      linetype = ggplot2::guide_legend(nrow = 1),
                      color = ggplot2::guide_legend(nrow = 1, 
                                                    override.aes = list(size = 0.6)))
  return(p)
}

plot_forces_dist2pred <- function(melt_force_df, av_dir = c(0,1), rot_angl = 0, ptitle = NA, wes_pallete = "Rushmore1")
{
  melt_force_df$force_type <- as.factor(melt_force_df$force)
  distance_groups <- c('<10', '10-20', '20-30', '30-40', '40-50', '50-60');
  dg_breaks <- c(seq(0, 60, 10))
  melt_force_df$dist_group <- cut(melt_force_df$dist2pred, breaks = dg_breaks, labels = distance_groups)
  tdf <- melt_force_df[, c('force', 'angle')]
  for (i in 1:nrow(tdf))
  {
    tdf[i,] <- rotate(pointRelPos(dist = tdf$force[i], angl = tdf$angle[i]), rot_angl)
  }
  force_labels <- c('1' = 'Attraction', '2' =  'Avoidance', '4' = 'Alignment')
  
  tdf$dist_group <- melt_force_df[, 'dist_group']
  tdf$force_type <- melt_force_df[, 'force_type']
  tdf <- tdf[tdf$force_type != '2',]

  
  p <- ggplot2::ggplot(tdf, ggplot2::aes(x = angle, y = force)) +
    ggplot2::stat_density_2d(ggplot2::aes(colour = force_type,
                                          fill = ggplot2::after_stat(nlevel)),
                             geom = "polygon", 
                             alpha = 1,
                             bns = 8) +
    ggplot2::guides(colour = FALSE)+
    ggplot2::geom_abline(slope = - av_dir[1]/av_dir[2],
                         intercept = 0, 
                         linetype = 2, 
                         alpha = 0.9) + 
    ggplot2::coord_equal() +
    ggplot2::ylim(c(min(tdf$force) - 1.2, max(tdf$force) + 0.5))+
    ggplot2::xlim(c(min(tdf$angle) - 1, max(tdf$angle) + 0.7))+
    ggplot2::geom_point(ggplot2::aes(x = 0, y = 0), colour = 'black', size = 3)+
    ggplot2::scale_fill_gradientn(colours = wesanderson::wes_palette(wes_pallete, 
                                                                     25,
                                                                     type = "continuous"))+
    ggplot2::scale_color_manual(breaks = c('1', '4'),
                                labels = c('Attraction', 'Alignment'),
                                values = c('black', 'black'))+
    ggplot2::theme_bw() +
    ggplot2::labs(x = "X", y = 'Y', title = ptitle, fill = 'C. Force density:')+
    ggplot2::theme(
      text = ggplot2::element_text(family = 'Palatino Linotype'),
      legend.text = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 22),
      legend.direction = 'horizontal',
      legend.justification = 'right',
      legend.background = ggplot2::element_rect(colour = 'black', fill = 'white'),
      axis.text =  ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 18, face = "bold"),
      legend.title = ggplot2::element_text(size = 20, face = 'bold'),
      legend.position = "top",
      legend.margin = ggplot2::margin(10,13,5,6),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
      )
  return(p)
}

pointRelPos <- function(dist, angl){
  return(c(dist * sin(angl), dist * cos(angl)))
}

