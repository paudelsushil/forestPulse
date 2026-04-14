#' Create spatial samples from feature geometries
#' @title Create Spatial Samples
#' @param feature sf object containing geometries to sample
#' @param area_col character, name of column containing area values
#' @param type character, type of sampling ("regular", "random", or "hexagonal")
#' @param n_cores integer, number of cores for parallel processing
#' @param size integer, optional fixed sample size (default: NULL)
#' @return sf object containing point samples with attributes
#' @import sf dplyr future future.apply progressr
#' @export
create_samples <- function(feature, area_col, type = "regular", n_cores, size = NULL) {
  # Input validation
  if (!inherits(feature, "sf")) {
    stop("feature must be an sf object")
  }
  if (!area_col %in% names(feature)) {
    stop(sprintf("Column '%s' not found in feature", area_col))
  }
  
  # Configure parallel processing
  future.seed <- TRUE
  future::plan("multisession", workers = n_cores)
  on.exit(future::plan("sequential", .cleanup = TRUE))
  
  # Filter valid geometries
  feature <- feature[!sf::st_is_empty(feature) & 
                    !is.na(sf::st_is_valid(feature)) & 
                    feature[[area_col]] > 0, ]
  
  # Setup progress tracking
  progressr::handlers(global = TRUE)
  progressr::handlers("progress")
  
  # Create samples
  samples <- progressr::with_progress({
    pb <- progressr::progressor(steps = nrow(feature))
    
    future.apply::future_lapply(seq_len(nrow(feature)), function(x) {
      # Extract row data
      fire_id <- feature$event_id[x]
      fire_date <- feature$fireDate[x]
      area <- feature[[area_col]][x]
      
      # Calculate sample size
      n_samples <- if(is.null(size)) {
        as.integer(ceiling(sqrt(area)))
      } else {
        size
      }
      
      # Create points
      points <- sf::st_sample(
        feature[x, ],
        size = n_samples,
        type = type
      ) %>%
        sf::st_as_sf() %>%
        dplyr::mutate(
          fire_id = fire_id,
          fire_date = fire_date,
          event = "fire"
        )
      
      pb()
      points
    })
  })
  
  # Combine results
  samples <- do.call(rbind, samples) %>%
    sf::st_as_sf() %>% 
    rename(geom = x)
  
  gc()
  return(samples)
}




