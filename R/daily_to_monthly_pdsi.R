daily_to_monthly_pdsi <- function(nc_file_path, study_area_path){
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

  file_split <- basename(nc_files) %>% strsplit(split="_") %>% unlist

  # extracting the variable name
  var <- file_split[1]

  has.time(nc)
  monthly_total <- tapp(nc, "yearmonths", fun = mean)
  # Projecting the raster to EPSG:5070
  monthly_total_proj <- terra::project(monthly_total, "epsg:5070", res = 4000)


  # Creating new raster with the monthly average temperature
  # Create new working directories if not exist
  if(!dir.exists(file.path(pr_analyzed_src))){
    dir.create(file.path(pr_analyzed_src))}
  # Create a file path with an extension
  filename <- file.path(pr_analyzed_src,
                        paste0(var, "_",names(monthly_total_proj), "_mean",".tif"))
  writeRaster(monthly_total_proj, filename = filename, overwrite = TRUE)


}

pet_src <- dir(file.path(here::here("external/analyzed_data/climate_data/pet")), pattern = ".tif", full.names = T)
vpd_src <- dir(file.path(here::here("external/analyzed_data/climate_data/vpd")), pattern = ".tif", full.names = T)


pet_daily <- terra::rast(pet_src_nc)
vpd_daily <- terra::rast(vpd_src)

pet_daily$
plot(pet_daily[[1]])
library(tidync)
library(RNetCDF)
library(ncdf4)

RNetCDF::open.nc(pet_src_nc)


src <- tidync::tidync(pet_src_nc[1])
src
ncmeta::nc_grids(pet_src_nc[1]) |>
tidyr::unnest(cols = c(variables))

ncmeta::nc_atts(pet_src_nc[1])


ncmeta::nc_vars(pet_src_nc[1])


time_ex <- lapply(pet_src_nc, function(x){
  pet.nc <- ncdf4::nc_open(x)
  pet <- ncdf4::ncvar_get(pet.nc, "potential_evapotranspiration")

  pet.nc$dim$lon$vals -> lon
  pet.nc$dim$lat$vals -> lat
})


tunit <- ncmeta::nc_atts(pet_src_nc[1], "day") |>
  tidyr::unnest(cols = c(value)) |>
  dplyr::filter(name == "units")


