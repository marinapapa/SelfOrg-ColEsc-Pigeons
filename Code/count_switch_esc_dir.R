
switch_esc_dir <- function(data)
{
  dfall <- lapply(data, 
                  function(x) {
                    pigs <- split(x, x$id)
                    dfall <- lapply(pigs, 
                                    function(y) {
                                      t0 <- c(NA, sign(y$radAwayPred))
                                      t1 <- c(sign(y$radAwayPred), NA)
                                      y$avoid_switch <- (t0 != t1)[-length(t0)]
                                      return(y)
                                    })
                    return(data.table::rbindlist(dfall))
                  })
  
  toplot <- lapply(dfall, 
                   function(x) {
                     x$avoid_switch <- as.numeric(x$avoid_switch)
                     df_dec <- x[!(is.na(x$avoid_switch))& x$dist_group != '>60',]
                     
                     toplot <- df_dec %>% group_by(dist_group, avoid_switch) %>%
                       summarise(perc = abs(mean(sign(radAwayPred), na.rm = TRUE)))
                     
                     toplot <- plyr::count(df_dec, c('avoid_switch', 'dist_group'));
                     totcount <- plyr::count(df_dec, c('dist_group'));
                     
                     for (i in 1:length(toplot$dist_group))
                     {
                       toplot$perc[i] <- (100 * toplot$freq[i] / totcount[totcount$dist_group == toplot$dist_group[i], 2]);
                     }
                     return(toplot)
                   })
  
  return(toplot)
}