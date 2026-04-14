# ##############################################################################
# Extract Values by features with respect to time applying parallel

parExtract.valuesbyTime <- function(rasters, sf_layers, ncores, date.column){
  fireMnth <- sort(unique(sf_layers[[date.column]]))

  layer_index <- sapply(fireMnth, function(m){
    raster_index <- which(terra::time(rasters) == m)
  })

  matched_layers <- rasters[[layer_index]]

  cl <- parallel::makeCluster(ncores)

  varlists <- c("rasters", "sf_layers", "date.column", "matched_layers")

  parallel::clusterExport(cl, varlist = varlists, envir = .GlobalEnv)

  parallel::clusterEvalQ(cl, {library(terra); library(sf); library(parallel)})

  layer_values <- pbapply::pblapply(cl = ncores,
                                    1:nrow(sf_layers),
                                    function(r){

                                      val <- terra::extract(matched_layers,
                                                            sf_layers[r, ],
                                                            method = "simple",
                                                            layer = which(sf_layers[[date.column]][r] == terra::time(matched_layers)),
                                                            bind = T
                                      )
                                      return(val)

                                    })
  parallel::stopCluster(cl)


  layer_values <- do.call(rbind, layer_values)
  layer_values <- sf::st_as_sf(layer_values)


  # st_write(layer_values,
  #            file.path(analyzed_src, paste0("plots/updated_plots/"), "file.name", ".gpkg"),
  #            append = F)

  return(layer_values)
}
