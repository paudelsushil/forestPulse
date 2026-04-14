daily_to_monthly_mean_temp <- function(netcdf_file_path, study_area){
  # Loading the packages
  lib <- c("foreach", "parallel", "terra", "sf", "tidyverse",
           "doParallel", "ncdf4")
  lapply(lib, require, character.only = TRUE)

  # states extent
  states <- study_area

  # transform to (https://epsg.io/?q=5070)
  states_alb <- states %>% st_transform(5070)
  p4string <- crs(states_alb, proj = TRUE)


  # masking all temperature file by study area boundary
  nc <- rast(netcdf_file_path) %>% mask(states) %>% crop(states)


  # Extracting variable names and year from the file name
  file_split <- basename(netcdf_file_path) %>% strsplit(split="_") %>% unlist
  # extracting the variable name
  var <- file_split[1]
  # extracting the year value
  year <- substr(file_split[2], start = 1, stop = 4)
  # Starting date
  start_date <- as.Date(paste(year, "01", "01", sep = "-"))
  # ending date
  end_date <- as.Date(paste(year, "12", "31", sep = "-"))
  # date sequence
  date_seq <- seq(start_date, end_date, by = "1 day")
  date_seq <- date_seq[1:nlyr(nc)]
  # Month sequence in numeric vectors
  month_seq <- month(date_seq)
  # Month as datetime format
  months <- seq(start_date, end_date, by = "1 months")
  # Day sequence
  day_seq <- day(date_seq)

  # Mean function
  # Apply a function to get monthly average temperature from daily
  monthly_mean <- tapp(nc, month_seq, fun = mean)
  # Projecting the raster to EPSG:5070
  monthly_mean <- project(monthly_mean, "epsg:5070", res = 4000)
  # Renaming the layers name to "tmmx_1984_mean.tif" format
  names(monthly_mean) <- paste(var, year,
                               unique(month(date_seq, label = TRUE)),
                               sep = "_")

  # Creating new raster with the monthly average temperature
  # Create new working directories if not exist
  if(!dir.exists(here::here("external/analyzed_data/climate_data",var))){
    dir.create(here::here("external/analyzed_data/climate_data",var))}
  # Create a file path with an extension
  filename <- here::here("external/analyzed_data/climate_data", var,
                         paste0(var, "_", year, "_mean",".tif"))
  writeRaster(monthly_mean, filename = filename, overwrite = TRUE)
}



