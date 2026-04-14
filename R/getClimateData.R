#' Download PRISM and gridMET climate data
#'
#' @description Downloads PRISM and gridMET climate data for specified 
#'   variables and date ranges. Skips files that already exist locally.
#'
#' @param names Character vector of climate variables. PRISM options: 
#'   "pdsi", "pzi", "scpdsi". Any other names are treated as gridMET variables.
#' @param start_year Integer start year (>= 1979 for gridMET).
#' @param end_year Integer end year.
#' @param base_path Directory for downloads. Defaults to "rawClimateData" 
#'   in current working directory.
#' @param quiet Logical; suppress progress messages?
#'
#' @return Invisibly returns the base_path where files were downloaded.
#' @export
#'
#' @examples
#' \donttest{
#' # Download PDSI for 2020-2021
#' getClimateData("pdsi", 2020, 2021)
#'
#' # Download multiple variables
#' getClimateData(c("pdsi", "pr", "tmmx"), 2015, 2020, "~/climate_data")
#' }
getClimateData <- function(names, 
                           start_year, 
                           end_year, 
                           base_path = NULL,
                           quiet = FALSE) {
  

  # Validate inputs

validate_years(start_year, end_year)
  
  # Set up base path
  if (is.null(base_path)) {
    base_path <- file.path(getwd(), "rawClimateData")
  }
  
  if (!dir.exists(base_path)) {
    dir.create(base_path, recursive = TRUE)
  }
  
  # Split into PRISM vs gridMET
  valid_prism <- c("pdsi", "pzi", "scpdsi")
  prism_vars <- names[names %in% valid_prism]
  gridmet_vars <- names[!names %in% valid_prism]
  
  # Download each source
  if (length(prism_vars) > 0) {
    download_prism(prism_vars, start_year, end_year, base_path, quiet)
  }
  
  if (length(gridmet_vars) > 0) {
    download_gridmet(gridmet_vars, start_year, end_year, base_path, quiet)
  }
  
  invisible(base_path)
}


# Internal function - not exported
download_prism <- function(variables, start_year, end_year, base_path, quiet) {
  
  base_urls <- list(
    pdsi = "http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi/",
    pzi = "http://www.wrcc.dri.edu/wwdt/data/PRISM/pzi/",
    scpdsi = "http://www.wrcc.dri.edu/wwdt/data/PRISM/scpdsi/"
  )
  
  # Generate year-month combinations
  dates <- expand.grid(
    year = start_year:end_year,
    month = 1:12
  )
  
  for (var in variables) {
    loc <- file.path(base_path, var)
    if (!dir.exists(loc)) dir.create(loc, recursive = TRUE)
    
    if (!quiet) message("Downloading PRISM ", var, "...")
    
    for (i in seq_len(nrow(dates))) {
      filename <- sprintf("%s_%d_%d_PRISM.nc", var, dates$year[i], dates$month[i])
      url <- paste0(base_urls[[var]], filename)
      destfile <- file.path(loc, filename)
      
      if (file.exists(destfile)) {
        if (!quiet) message("  Skipping ", filename, " (exists)")
        next
      }
      
      tryCatch({
        if (!quiet) message("  ", filename)
        utils::download.file(url, destfile, mode = "wb", quiet = TRUE)
      }, error = function(e) {
        warning("Failed to download ", filename, ": ", conditionMessage(e))
      })
    }
  }
}


# Internal function - not exported
download_gridmet <- function(variables, start_year, end_year, base_path, quiet) {
  
  base_url <- "https://www.northwestknowledge.net/metdata/data/"
  
  for (var in variables) {
    loc <- file.path(base_path, var)
    if (!dir.exists(loc)) dir.create(loc, recursive = TRUE)
    
    if (!quiet) message("Downloading gridMET ", var, "...")
    
    for (year in start_year:end_year) {
      filename <- paste0(var, "_", year, ".nc")
      url <- paste0(base_url, filename)
      destfile <- file.path(loc, filename)
      
      if (file.exists(destfile)) {
        if (!quiet) message("  Skipping ", filename, " (exists)")
        next
      }
      
      tryCatch({
        if (!quiet) message("  ", filename)
        utils::download.file(url, destfile, mode = "wb", quiet = TRUE)
      }, error = function(e) {
        warning("Failed to download ", filename, ": ", conditionMessage(e))
      })
    }
  }
  
  if (!quiet) message("gridMET download complete: ", base_path)
}


# Validation helper
validate_years <- function(start_year, end_year) {
  if (!is.numeric(start_year) || !is.numeric(end_year)) {
    stop("start_year and end_year must be numeric", call. = FALSE)
  }
  if (start_year > end_year) {
    stop("start_year cannot be greater than end_year", call. = FALSE)
  }
  if (start_year < 1979) {
    warning("gridMET data only available from 1979 onwards")
  }
}