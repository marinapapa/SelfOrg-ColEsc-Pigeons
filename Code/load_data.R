# Helper function that loads the config file (JSON type) of a simulation
load_config <- function(path2config)
{
  d <- rjson::fromJSON(file = path2config)
  return(d)
}

# Helper function that loads the simulated csv data into a list of dataframes 
import_base_data <- function(folder_path, data_type = 'sim', types = c('self', 'neighb', 'flocks'))
{
  out <- list()
  
  if ('neighb' %in% types) out$neighb <- read.csv(paste0(folder_path, '\\', 'all_neighbors.csv'))
  if ('self' %in% types) out$self <- read.csv(paste0(folder_path, '\\', 'time_series.csv'))
  if ('flocks' %in% types)out$flock <- read.csv(paste0(folder_path, '\\', 'flocks.csv'))
  if ('forces' %in% types)out$forces <- read.csv(paste0(folder_path, '\\', 'forces.csv'))
  
  if (data_type == 'sim')
  {
    cfg <- load_config(paste0(folder_path, '\\', 'composed_config.json'))
    out$config <- cfg
    out$sim_info$sim_id <- basename(dirname(paste0(folder_path, '\\', 'time_series.csv')))
    out$sim_info$N <- 1
  }
  
  return(out)
}
