# Helper function calculating the relative position of 
# a neighbor based on the models output

neighb_relative_positions <- function(
  df_neighb
)
{
  print('Calculating neihbors relative positions...');
  n_rel_pos <- data.frame(matrix(NA, ncol = 3));
  colnames(n_rel_pos) <- c('neighb', 'x', 'y');
  
  colNnames <- rep(NA, 2 * (length(unique(df_neighb$id)) - 1));
  k <- 1;
  for (i in 1:(length(unique(df_neighb$id))-1))
  {
    colNnames[k] <- paste0('dist2n', i);
    colNnames[k+1] <- paste0('bAngl2n', i);
    k <- k + 2;
  }
  
  stl <- length(df_neighb[,1]) - 1
  k <- 1
  m <- 1
  for (i in 1:(length(colNnames)/2))
  {
    ords <- rep(i, length(df_neighb[,1]))
    
    xys <- pointRelPos(df_neighb[, colNnames[m]], df_neighb[, colNnames[m + 1]]);
    x <- xys[seq(1,length(xys)-1,2)]
    y <- xys[seq(2,length(xys),2)]
    
    tdf <- cbind(ords, x, y)
    n_rel_pos[k:(k+ stl),]<- tdf
    k <- k + stl + 1
    m <- m +2
    
  }
  return(n_rel_pos)
}