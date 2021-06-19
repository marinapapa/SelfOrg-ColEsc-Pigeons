## Statistical analysis for turning-direction frequency under attack

sim_types <- c("chase_normal", 
                "no_avoid")

all_decouts <- lapply(seq_along(sim_types),
                      function(x) { read.csv(paste0('../Data/simulated/turn_direction/dec_out_', sim_types[x], '.csv'))}
)

# normal simulations (with predator avoidance)
normw <- all_decouts[[1]]
conf <- subset(normw, normw$conflict == 1);
percs <- FlockAnalyzeR::dec_out_percent(conf);

grps <- percs[percs$dist_group %in% c('<10', '10-20','20-30', '30-40','40-50'),]
grps <- grps[grps$dec_out == 'f', 'perc']

prop.trend.test(x = grps, n = rep(100, 5))

# control simulations (no predator avoidance)

noavoid <- all_decouts[[2]]
conf_b <- subset(noavoid, noavoid$conflict == 1);
percs <- FlockAnalyzeR::dec_out_percent(conf_b);

grps <- percs[percs$dist_group %in% c('<10', '10-20','20-30', '30-40', '40-50'),]
grps <- grps[grps$dec_out == 'f', 'perc']

prop.trend.test(x = grps, n = rep(100, 5))
