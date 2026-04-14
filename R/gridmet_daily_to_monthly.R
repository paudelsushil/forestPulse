#' Convert daily gridMET data to monthly aggregates
#'
#' @description Reads a NetCDF file containing daily gridMET data, aggregates
#'   to monthly values (sum for precipitation, mean for other variables), and
#'   writes the result to a new NetCDF file.
#'
#' @param nc_file Character string specifying the path to the input NetCDF file.
#' @param var_name Character string specifying the variable name to aggregate.
#'   If `"precipitation_amount"`, values are summed; otherwise, values are averaged.
#' @param output_dir Character string specifying the output directory. If `NULL`
#'   (default), creates "rawClimateData" in the current working directory.
#' @param overwrite Logical; overwrite existing output file? Default is `FALSE`.
#'
#' @return Invisibly returns the path to the output NetCDF file.
#'
#' @details
#' The function assumes gridMET's time dimension is named "day" and uses a

#' reference date of 1900-01-01. Monthly aggregates are assigned to the first
#' day of each month.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Convert temperature data
#' gridmet_daily_to_monthly("tmmx_2020.nc", "air_temperature")
#'
#' # Convert precipitation (uses sum instead
#'
#' gridmet_daily_to_monthly("pr_2020.nc", "precipitation_amount", "~/monthly_data")
#' }
gridmet_daily_to_monthly <- function(nc_file,
                                     var_name,
                                     output_dir = NULL,
                                     overwrite = FALSE) {
 
  # Check for required packages
  check_package("ncdf4")
  check_package("lubridate")
 
 
  # Validate inputs
 
  if (!is.character(nc_file) || length(nc_file) != 1) {
    stop("`nc_file` must be a single character string", call. = FALSE)
  }
 
  if (!file.exists(nc_file)) {
    stop("File not found: ", nc_file, call. = FALSE)
  }
 
  if (!is.character(var_name) || length(var_name) != 1) {
    stop("`var_name` must be a single character string", call. = FALSE)
  }
 
 
  # Set up output directory
 
  if (is.null(output_dir)) {
    output_dir <- file.path(getwd(), "rawClimateData")
  }
 
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
 
  output_file <- file.path(output_dir, paste0("monthly_", basename(nc_file)))
 
  if (file.exists(output_file) && !overwrite) {
    stop(
      "Output file already exists: ", output_file, "\n",
      "Set `overwrite = TRUE` to replace it.",
      call. = FALSE
    )
  }
 
 
  # Open and validate NetCDF file
 
  nc <- tryCatch(
    ncdf4::nc_open(nc_file),
    error = function(e) {
      stop("Failed to open NetCDF file: ", conditionMessage(e), call. = FALSE)
    }
  )
 
  # Ensure file is closed on exit (success or error)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
 
  # Check that variable exists
  if (!var_name %in% names(nc$var)) {
    available_vars <- paste(names(nc$var), collapse = ", ")
    stop(
      "Variable '", var_name, "' not found in NetCDF file.\n",
      "Available variables: ", available_vars,
      call. = FALSE
    )
  }
 
  # Check for required dimensions
  required_dims <- c("day", "lon", "lat")
  missing_dims <- setdiff(required_dims, names(nc$dim))
 
  if (length(missing_dims) > 0) {
    stop(
      "Missing required dimensions: ", paste(missing_dims, collapse = ", "),
      call. = FALSE
    )
  }
 
 
  # Extract and process time dimension
 
  times <- ncdf4::ncvar_get(nc, "day")
  reference_date <- as.Date("1900-01-01")
  dates <- reference_date + times
  months <- lubridate::floor_date(dates, "month")
  unique_months <- unique(months)
 
 
  # Read variable data
 
  var_data <- tryCatch(
    ncdf4::ncvar_get(nc, var_name),
    error = function(e) {
      stop("Failed to read variable '", var_name, "': ", conditionMessage(e),
           call. = FALSE)
    }
  )
 
  # Validate dimensions
  if (length(dim(var_data)) != 3) {
    stop(
      "Expected 3D array (lon x lat x time), got ",
      length(dim(var_data)), " dimensions",
      call. = FALSE
    )
  }
 
 
  # Aggregate to monthly values
 
  n_lon <- dim(var_data)[1]
  n_lat <- dim(var_data)[2]
  n_months <- length(unique_months)
 
  monthly_data <- array(NA_real_, dim = c(n_lon, n_lat, n_months))
 
  # Choose aggregation function based on variable type
  agg_fun <- if (var_name == "precipitation_amount") sum else mean
 
  for (i in seq_along(unique_months)) {
    month_idx <- which(months == unique_months[i])
   
    monthly_data[, , i] <- apply(
      var_data[, , month_idx, drop = FALSE],
      c(1, 2),
      agg_fun,
      na.rm = TRUE
    )
  }
 
 
  # Create output NetCDF file
 
  # Define dimensions
  lon_vals <- ncdf4::ncvar_get(nc, "lon")
  lat_vals <- ncdf4::ncvar_get(nc, "lat")
  days_since_ref <- as.numeric(unique_months - reference_date)
 
  lon_dim <- ncdf4::ncdim_def("lon", "degrees_east", lon_vals)
  lat_dim <- ncdf4::ncdim_def("lat", "degrees_north", lat_vals)
  time_dim <- ncdf4::ncdim_def("time", "days since 1900-01-01", days_since_ref)
 
  # Get variable metadata from input
  input_var <- nc$var[[var_name]]
  var_units <- input_var$units %||% ""
  var_missval <- input_var$missval %||% -9999
 
  var_def <- ncdf4::ncvar_def(
    name = var_name,
    units = var_units,
    dim = list(lon_dim, lat_dim, time_dim),
    missval = var_missval,
    longname = paste("Monthly", input_var$longname %||% var_name)
  )
 
  # Write output file
  nc_out <- tryCatch(
    ncdf4::nc_create(output_file, var_def),
    error = function(e) {
      stop("Failed to create output file: ", conditionMessage(e), call. = FALSE)
    }
  )
 
  # Ensure output file is closed on exit
  on.exit(ncdf4::nc_close(nc_out), add = TRUE)
 
  ncdf4::ncvar_put(nc_out, var_def, monthly_data)
 
  # Copy relevant attributes from input
  copy_ncdf_attributes(nc, nc_out, var_name)
 
  # Add processing metadata
  ncdf4::ncatt_put(nc_out, 0, "source_file", basename(nc_file))
  ncdf4::ncatt_put(nc_out, 0, "processing", "Aggregated from daily to monthly")
  ncdf4::ncatt_put(nc_out, 0, "aggregation_method",
                   if (var_name == "precipitation_amount") "sum" else "mean")
  ncdf4::ncatt_put(nc_out, 0, "created_by", "globalClimData::gridmet_daily_to_monthly")
  ncdf4::ncatt_put(nc_out, 0, "created_on", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
 
  message("Created: ", output_file)
 
  invisible(output_file)
}


# ----
#  Internal helper functions (not exported) ----
#' Check if a package is available
#'
#' @param pkg Package name
#' @noRd
check_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Package '", pkg, "' is required but not installed.",
      call. = FALSE
    )
  }
}


#' Copy NetCDF attributes from input to output
#'
#' @param nc_in Input ncdf4 object
#' @param nc_out Output ncdf4 object
#' @param var_name Variable name
#' @noRd
copy_ncdf_attributes <- function(nc_in, nc_out, var_name) {
 
  # Attributes to skip (already set or not relevant for monthly data)
  skip_attrs <- c("_FillValue", "missing_value")
 
  att_names <- names(nc_in$var[[var_name]]$att)
  att_names <- setdiff(att_names, skip_attrs)
 
  for (att in att_names) {
    tryCatch(
      ncdf4::ncatt_put(nc_out, var_name, att, nc_in$var[[var_name]]$att[[att]]),
      error = function(e) {
        warning("Could not copy attribute '", att, "': ", conditionMessage(e),
                call. = FALSE)
      }
    )
  }
}


#' Null coalescing operator
#'
#' @param x Value to check
#' @param y Default value if x is NULL
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}