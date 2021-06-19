
data_for_turn_dir <- function(df_decout, condition)
{
  df_decout$dist2pred <- as.numeric(df_decout$dist2pred);
  df_decout <- df_decout[complete.cases(df_decout) & df_decout$dist2pred < 60, ];
  
  distance_groups <- c('<10', '10-20', '20-30', '30-40', '40-50', '50-60');
  dg_breaks <- c(seq(0, 60, 10));
  df_decout$dist_group <- cut(df_decout$dist2pred, breaks = dg_breaks, labels = distance_groups) ;
  
  if (condition == 'noconflict')
  {
    df <- subset(df_decout, df_decout$conflict == 0);
    df <- turn_dir_percent(df);
    df <- df[df$dec_out != 'p',]
  }
  else if (condition == 'conflict')
  {
    df <- subset(df_decout, df_decout$conflict == 1);
    df <- turn_dir_percent(df);
    df <- df[df$dec_out != 'n',]
  }
  else { print('Pick conflict or noconflict as conditions'); return();}
  
  return(df)
}


turn_dir_percent <- function(df_decout)
{
  df_decout <- df_decout[complete.cases(df_decout), ];
  toplot <- plyr::count(df_decout, c('dec_out', 'dist_group'));
  totcount <- plyr::count(df_decout, c('dist_group'));
  
  for (i in 1:length(toplot$dist_group))
  {
    toplot$perc[i] <- (100 * toplot$freq[i] / totcount[totcount$dist_group == toplot$dist_group[i], 2]);
  }
  
  return(toplot);
}