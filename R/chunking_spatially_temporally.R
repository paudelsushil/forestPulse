lapply(c("terra", "sf", "snow"), library, character.only = T)

sample_plot <-

# parameters
points_df <- sf::st_read(file.path(here::here("external/analyzed_data/plots/updated_plots/sample_plots_30.gpkg")))
raster_data <- file.path(here::here("external/analyzed_data/climate_data/pet_daily.tif"))
n_cores = 4,








extract_points <- sample_plot %>%
  dplyr::select(plot_id, plot_label, geom) %>%
  makeChunks(., 10000)


# Initialize cluster
cl <- makeCluster(n_cores, type = "SOCK")

# Export necessary functions and data to cluster
clusterExport(cl, c("climate_dir", "vars", "dates", "points_dt", "chunk_size"),
              envir = environment())


calculate.seasonal(data)
