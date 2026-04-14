calculate.fireAnalysis<- function(outbreak_lodgepole, fire_lodgepole, fuel_lodgepole, study_area) {
  # Ensure we're working with data.frames
  # Load required packages
  require(sf)
  require(dplyr)
  require(tidyr)

  # Ensure all inputs are sf objects
  if (!all(c(inherits(outbreak_lodgepole, "sf"),
             inherits(fire_lodgepole, "sf"),
             inherits(fuel_lodgepole, "sf")))) {
    stop("All input data must be sf objects (spatial polygons)")
  }

  # Intersect fires with managed areas to get fires in managed areas
  managed_fires <- st_intersection(fire_lodgepole, fuel_lodgepole) %>%
    mutate(FireA_km2 = calculate.area(., "km2"),
           FireA_ha = calculate.area(., "ha"),
           Mstatus = "managed")

  # For each fire polygon, check for subsequent outbreaks
  # by spatial intersection with outbreak polygons
  fire_outbreak_status <- managed_fires %>%
    # Keep track of fire year and area
    dplyr::select(fire_year, FireA_km2) %>%
    # Spatial join with outbreaks
    st_join(outbreak_lodgepole %>% dplyr::select(outbreak_year, OutbreakA_km2),
            left = TRUE) %>%
    # Consider only outbreaks that occurred after fires
    dplyr::filter(is.na(outbreak_year) | outbreak_year > fire_year) %>%
    # Calculate areas
    mutate(
      area = calculate.area(., "km2"), # Convert to hectares
      outbreak_status = if_else(is.na(outbreak_year),
                                "No Outbreak",
                                "Outbreak")
    ) %>%
    # Group by year and outbreak status
    group_by(fire_year, outbreak_status) %>%
    summarise(
      total_area = sum(area),
      .groups = 'drop'
    ) %>%
    # Calculate percentages
    group_by(fire_year) %>%
    mutate(
      percent_area = total_area / sum(study_area$studyA_km2)
    ) %>%
    # Rename for plotting
    rename(year = fire_year)

  # Convert back to regular dataframe for plotting
  fire_outbreak_status <- fire_outbreak_status %>%
    st_drop_geometry()

  return(fire_outbreak_status)
}

