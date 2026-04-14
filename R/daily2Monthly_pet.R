daily2Monthly_pet <- function(nc_file_path, study_area_path){

  # masking all temperature file by study area boundary
  nc <- rast(nc_file_path)

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
pet_src_nc <- dir(file.path(here::here("external/raw_data/climate_data/pet")), pattern = ".nc", full.names = T)
vpd_src_nc <- dir(file.path(here::here("external/raw_data/climate_data/vpd")), pattern = ".nc", full.names = T)

pet_nc <- rast(pet_src) %>% mask(conus_states_wgs84) %>% crop(conus_states_wgs84)
vpd_nc <- rast(vpd_src) %>% mask(conus_states_wgs84) %>% crop(conus_states_wgs84)


writeRaster(pet_nc, file.path((here::here("external/analyzed_data/climate_data/pet/pet_daily.tif"))))
writeRaster(vpd_nc, file.path((here::here("external/analyzed_data/climate_data/vpd/vpd_daily.tif"))))



var_name <- names(pet_daily)
reg_index <- regexpr(pattern = "\\d{5}", var_name)
days <- regmatches(var_name, reg_index)
days_date <- as.Date(as.numeric(days), origin = "1900-01-01")
days_date






