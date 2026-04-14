


gridmetDaily2Monthly <- function(nc_file, var_name, output_dir) {
  require(ncdf4)
  require(here)
  require(lubridate)

 # Open NetCDF file
  nc <- nc_open(nc_file)

  # Get dimensions
  times <- ncvar_get(nc, "day")

  dates <- as.Date("1900-01-01") + times
  months <- floor_date(dates, "month")

  # Read variable data
  var_data <- ncvar_get(nc, var_name)

  # Calculate monthly means (or sums for precipitation)
  unique_months <- unique(months)

  monthly_data <- array(NA, dim = c(dim(var_data)[1],
                                    dim(var_data)[2],
                                    length(unique_months)))

  for(i in seq_along(unique_months)) {
    month_idx <- which(months == unique_months[i])
    if(var_name == "precipitation_amount") {
      monthly_data[,,i] <- apply(var_data[ , ,month_idx], c(1, 2), sum, na.rm = TRUE)
    } else {
      monthly_data[,,i] <- apply(var_data[, ,month_idx], c(1, 2), mean, na.rm = TRUE)
    }
  }
  # Days since 1900-01-01 in Gregorian calender

  days_since_1900 <- as.numeric(unique_months - as.Date("1900-01-01"))


  # Create output NetCDF
  londim <- ncdim_def("lon", "degrees_east", ncvar_get(nc, "lon"))
  latdim <- ncdim_def("lat", "degrees_north", ncvar_get(nc, "lat"))
  timdim <- ncdim_def("time", "days since 1900-01-01", days_since_1900)

  var_def <- ncvar_def(var_name, nc$var[[var_name]]$units,
                       list(londim, latdim, timdim),
                       nc$var[[var_name]]$missval)

  output_file <- file.path(output_dir, paste0("monthly_", basename(nc_file)))

  # Write to file
  nc_out <- nc_create(output_file, var_def)
  ncvar_put(nc_out, var_def, monthly_data)

  # Copy attributes
  att_names <- names(nc$var[[var_name]]$att)
  for(att in att_names) {
    ncatt_put(nc_out, var_name, att, nc$var[[var_name]]$att[[att]])
  }

  # Close files
  nc_close(nc)
  nc_close(nc_out)
}

