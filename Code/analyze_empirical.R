# Summary statistics of the empirical data of pigeon flocks

data_dir <- '../Data/empirical/transformed/'

#################################################################################
# Speed, nearest neighbor distance and flock shape
self_c <- read.csv(paste0(data_dir, 'all_self_c.csv'))
self_p <- read.csv(paste0(data_dir, 'all_self_p.csv'))
flocks_c <- read.csv(paste0(data_dir, 'all_flock_c.csv'))
flocks_p <- read.csv(paste0(data_dir, 'all_flock_p.csv'))

summary(c(self_c$speed, self_p$speed))
summary(c(self_c$dist2n1, self_p$dist2n1))
sd(c(self_c$dist2n1, self_p$dist2n1), na.rm = TRUE)
summary(c(flocks_p$shape, flocks_c$shape))
sd(c(flocks_p$shape, flocks_c$shape), na.rm = TRUE)


#################################################################################
# Bearing angle to nearest neighbor

neighb_c <- read.csv(paste0(data_dir, 'all_neighb_c.csv'))
neighb_p <- read.csv(paste0(data_dir, 'all_neighb_p.csv'))

neighb_c <- neighb_c[complete.cases(neighb_c$bAngl2n1),]
neighb_p <- neighb_p[complete.cases(neighb_p$bAngl2n1),]

summary(abs(neighb_c$bAngl2n1))

# SHOW UNIFORM DISTRIBUTED
set.seed(1)
unif_distr <- runif(100, -180, 180)
ks.test(unif_distr, c(neighb_c$bAngl2n1,neighb_p$bAngl2n1))

# SHOW NO DIFFERENCE BETWEEN PREDATOR AND CONTROL
#wilcox.test(neighb_c$bAngl2n1, neighb_p$bAngl2n1, paired = FALSE, alternative = "two.sided") # cause uniform distributed


