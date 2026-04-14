daily_to_monthly_precpt <- function(nc_file_path, study_area_path){
  # Loading the packages
  lib <- c("terra", "sf", "tidyverse")
  lapply(lib, require, character.only = TRUE)

  # states extent
  states <- sf::st_read(study_area_path)
  # Transform to EPSG:4326
  states_wgs84 <- states %>% st_transform(crs = "epsg:4326")

  # transform to (https://epsg.io/?q=5070)
  states_alb <- states %>% st_transform(5070)
  p4string <- crs(states_alb, proj = TRUE)


  # masking all temperature file by study area boundary
  nc <- rast(nc_file_path) %>% mask(states) %>% crop(states)

  # nc <- terra::rast(nc_file_path)

  has.time(nc)
  monthly_total <- tapp(nc, "yearmonths", fun = sum)
  # Projecting the raster to EPSG:5070
  monthly_total_proj <- terra::project(monthly_total, "epsg:5070", res = 4000)


  # Creating new raster with the monthly average temperature
  # Create new working directories if not exist
  if(!dir.exists(file.path(pr_analyzed_src))){
    dir.create(file.path(pr_analyzed_src))}
  # Create a file path with an extension
  filename <- file.path(pr_analyzed_src,
                        paste0("pr", "_",names(monthly_total_proj), "_total",".tif"))
  writeRaster(monthly_total_proj, filename = filename, overwrite = TRUE)



}

# ==============================================================================

