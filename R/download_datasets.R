#' Download Forest Disturbance Datasets
#'
#' @description Functions to download various forest disturbance datasets
#'   including MTBS fire perimeters, elevation data, US state boundaries,
#'   USFS regional boundaries, forest type maps, and ADS outbreak data.
#'
#' @name download_datasets
NULL


# ============================================================================
# Main Download Function
# ============================================================================

#' Download Forest Disturbance Datasets
#'
#' @description Downloads one or more forest disturbance datasets to a
#'   specified directory. Supports MTBS fire data, elevation, climate,
#'   administrative boundaries, and insect outbreak data.
#'
#' @param datasets Character vector of datasets to download. Options:
#'   `"mtbs"`, `"elevation"`, `"us_states"`, `"usfs_regions"`,
#'   `"forest_types"`, `"outbreaks"`, or `"all"` for everything.
#' @param base_path Character string specifying the base directory for
#'   downloads. Default creates `"external"` in current working directory.
#' @param regions Character vector of USFS regions for outbreak data.
#'   Options: `"r1"` through `"r6"`, or `"all"`. Only used when
#'   `"outbreaks"` is in datasets. Default is `"all"`.
#' @param include_historical Logical; include historical outbreak data for
#'   regions 5 and 6? Only used when `"outbreaks"` is in datasets.
#'   Default is `TRUE`.
#' @param overwrite Logical; re-download existing files? Default is `FALSE`.
#' @param quiet Logical; suppress progress messages? Default is `FALSE`.
#'
#' @return Invisibly returns a named list of download paths.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Download MTBS fire data only
#' download_datasets("mtbs", base_path = "~/forest_data")
#'
#' # Download multiple datasets
#' download_datasets(c("mtbs", "elevation", "us_states"))
#'
#' # Download everything
#' download_datasets("all")
#'
#' # Download outbreaks for specific regions
#' download_datasets("outbreaks", regions = c("r1", "r4"))
#'
#' # Download outbreaks without historical data
#' download_datasets("outbreaks", regions = "r6", include_historical = FALSE)
#' }
download_datasets <- function(datasets = "all",
                              base_path = NULL,
                              regions = "all",
                              include_historical = TRUE,
                              overwrite = FALSE,
                              quiet = FALSE) {

  # Validate inputs

  valid_datasets <- c("mtbs", "elevation", "us_states", "usfs_regions",
                      "forest_types", "outbreaks", "all")

  if ("all" %in% datasets) {
    datasets <- setdiff(valid_datasets, "all")
  }

  invalid <- setdiff(datasets, valid_datasets)
  if (length(invalid) > 0) {
    stop(
      "Invalid dataset(s): ", paste(invalid, collapse = ", "), "\n",
      "Valid options: ", paste(valid_datasets, collapse = ", "),
      call. = FALSE
    )
  }

 
  # Setup directory structure
 
  paths <- setup_data_directories(base_path)

  # Download requested datasets
  results <- list()

  if ("mtbs" %in% datasets) {
    results$mtbs <- download_mtbs(paths$raw$fire, overwrite, quiet)
  }

  if ("elevation" %in% datasets) {
    results$elevation <- download_elevation(paths$raw$topo, overwrite, quiet)
  }

  if ("us_states" %in% datasets) {
    results$us_states <- download_us_states(paths$raw$us_state, overwrite, quiet)
  }

  if ("usfs_regions" %in% datasets) {
    results$usfs_regions <- download_usfs_regions(paths$raw$usfs_region, overwrite, quiet)
  }

  if ("forest_types" %in% datasets) {
    results$forest_types <- download_forest_types(paths$raw$forest_types, overwrite, quiet)
  }

  if ("outbreaks" %in% datasets) {
    results$outbreaks <- download_outbreaks(
      paths$raw$outbreaks,
      regions = regions,
      include_historical = include_historical,
      overwrite = overwrite,
      quiet = quiet
    )
  }

  invisible(results)
}


# ============================================================================
# Directory Setup
# ============================================================================

#' Setup Data Directory Structure
#'
#' @description Creates the standard directory structure for forest
#'   disturbance data analysis.
#'
#' @param base_path Base directory path. If NULL, uses "external" in
#'   current working directory.
#'
#' @return Named list of directory paths.
#'
#' @export
#'
#' @examples
#' \donttest{
#' paths <- setup_data_directories("~/my_project")
#' paths$raw$fire
#' paths$analyzed$climate
#' }
setup_data_directories <- function(base_path = NULL) {

  if (is.null(base_path)) {
    base_path <- file.path(getwd(), "external")
  }

 
  # Define directory structure
 
  raw_path <- file.path(base_path, "raw_data")
  analyzed_path <- file.path(base_path, "analyzed_data")

  # Raw data directories
  raw_dirs <- list(
    us_state = file.path(raw_path, "us_state"),
    fire = file.path(raw_path, "mtbs_fire_data"),
    topo = file.path(raw_path, "topo_data"),
    climate = file.path(raw_path, "climate_data"),
    outbreaks = file.path(raw_path, "outbreaks_data"),
    usfs_region = file.path(raw_path, "usfs_region"),
    forest_types = file.path(raw_path, "forest_types")
  )

  # Climate subdirectories
  climate_vars <- c("tmmx", "tmmn", "pr", "pdsi", "scpdsi", "pzi")
  raw_dirs$climate_vars <- stats::setNames(
    lapply(climate_vars, function(v) file.path(raw_dirs$climate, v)),
    climate_vars
  )

  # Analyzed data directories
  analyzed_dirs <- list(
    study_area = file.path(analyzed_path, "study_area"),
    fire = file.path(analyzed_path, "mtbs_fire_data"),
    fuel = file.path(analyzed_path, "fuel_mgmt"),
    outbreaks = file.path(analyzed_path, "outbreaks_data"),
    climate = file.path(analyzed_path, "climate_data"),
    topo = file.path(analyzed_path, "topo_data"),
    usfs_region = file.path(analyzed_path, "usfs_region"),
    forest_types = file.path(analyzed_path, "forest_types")
  )

  # Climate subdirectories for analyzed data
  analyzed_dirs$climate_vars <- stats::setNames(
    lapply(climate_vars, function(v) file.path(analyzed_dirs$climate, v)),
    climate_vars
  )

 
  # Create all directories
 
  all_dirs <- c(
    unlist(raw_dirs, use.names = FALSE),
    unlist(analyzed_dirs, use.names = FALSE)
  )

  for (dir in all_dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
  }

  list(
    base = base_path,
    raw = raw_dirs,
    analyzed = analyzed_dirs
  )
}


# ============================================================================
# Individual Download Functions
# ============================================================================

#' Download MTBS Fire Perimeter Data
#'
#' @description Downloads the Monitoring Trends in Burn Severity (MTBS)
#'   fire perimeter shapefile from USGS.
#'
#' @param dest_dir Destination directory for the download.
#' @param overwrite Logical; re-download if file exists?
#' @param quiet Logical; suppress messages?
#'
#' @return Path to the downloaded shapefile.
#'
#' @export
#'
#' @examples
#' \donttest{
#' download_mtbs("~/data/fire")
#' }
download_mtbs <- function(dest_dir, overwrite = FALSE, quiet = FALSE) {

  check_package("curl")

  output_file <- file.path(dest_dir, "mtbs_perims_DD.shp")

  if (file.exists(output_file) && !overwrite) {
    if (!quiet) message("MTBS data already exists. Skipping download.")
    return(invisible(output_file))
  }

  if (!quiet) message("Downloading MTBS fire perimeter data...")

  base_url <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data"
  url <- paste0(base_url, "/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip")
  zip_file <- file.path(dest_dir, "mtbs_perimeter_data.zip")

  tryCatch({
    curl::multi_download(url, zip_file, resume = TRUE, timeout = 1e6, progress = !quiet)
    utils::unzip(zip_file, exdir = dest_dir)
    unlink(zip_file)

    if (!file.exists(output_file)) {
      stop("Download completed but shapefile not found", call. = FALSE)
    }

    if (!quiet) message("MTBS download complete: ", output_file)

  }, error = function(e) {
    stop("Failed to download MTBS data: ", conditionMessage(e), call. = FALSE)
  })

  invisible(output_file)
}


#' Download GridMET Elevation Data
#'
#' @description Downloads the elevation data from the GridMET data source.
#'
#' @param dest_dir Destination directory for the download.
#' @param overwrite Logical; re-download if file exists?
#' @param quiet Logical; suppress messages?
#'
#' @return Path to the downloaded NetCDF file.
#'
#' @export
#'
#' @examples
#' \donttest{
#' download_elevation("~/data/topo")
#' }
download_elevation <- function(dest_dir, overwrite = FALSE, quiet = FALSE) {

  check_package("curl")

  output_file <- file.path(dest_dir, "metdata_elevationdata.nc")

  if (file.exists(output_file) && !overwrite) {
    if (!quiet) message("Elevation data already exists. Skipping download.")
    return(invisible(output_file))
  }

  if (!quiet) message("Downloading GridMET elevation data...")

  url <- "https://climate.northwestknowledge.net/METDATA/data/metdata_elevationdata.nc"

  tryCatch({
    curl::multi_download(url, output_file, resume = TRUE, timeout = 1e6, progress = !quiet)

    if (!file.exists(output_file)) {
      stop("Download completed but file not found", call. = FALSE)
    }

    if (!quiet) message("Elevation download complete: ", output_file)

  }, error = function(e) {
    stop("Failed to download elevation data: ", conditionMessage(e), call. = FALSE)
  })

  invisible(output_file)
}


#' Download US State Boundaries
#'
#' @description Downloads US state administrative boundary shapefile from
#'   the National Weather Service.
#'
#' @param dest_dir Destination directory for the download.
#' @param overwrite Logical; re-download if file exists?
#' @param quiet Logical; suppress messages?
#'
#' @return Path to the downloaded shapefile.
#'
#' @export
#'
#' @examples
#' \donttest{
#' download_us_states("~/data/boundaries")
#' }
download_us_states <- function(dest_dir, overwrite = FALSE, quiet = FALSE) {

  check_package("curl")

  output_file <- file.path(dest_dir, "us_state.shp")

  if (file.exists(output_file) && !overwrite) {
    if (!quiet) message("US states data already exists. Skipping download.")
    return(invisible(output_file))
  }

  if (!quiet) message("Downloading US state boundaries...")

  url <- "https://www.weather.gov/source/gis/Shapefiles/County/s_05mr24.zip"
  zip_file <- file.path(dest_dir, "us_states.zip")

  tryCatch({
    curl::multi_download(url, zip_file, resume = TRUE, timeout = 1e6, progress = !quiet)
    utils::unzip(zip_file, exdir = dest_dir)
    unlink(zip_file)

   
    # Rename files to standard names
   
    rename_shapefile_components(dest_dir, "us_state", quiet)

    if (!file.exists(output_file)) {
      stop("Download completed but shapefile not found", call. = FALSE)
    }

    if (!quiet) message("US states download complete: ", output_file)

  }, error = function(e) {
    stop("Failed to download US states data: ", conditionMessage(e), call. = FALSE)
  })

  invisible(output_file)
}


#' Download USFS Regional Boundaries
#'
#' @description Downloads US Forest Service administrative region boundaries.
#'
#' @param dest_dir Destination directory for the download.
#' @param overwrite Logical; re-download if file exists?
#' @param quiet Logical; suppress messages?
#'
#' @return Path to the downloaded shapefile.
#'
#' @export
#'
#' @examples
#' \donttest{
#' download_usfs_regions("~/data/usfs")
#' }
download_usfs_regions <- function(dest_dir, overwrite = FALSE, quiet = FALSE) {

  check_package("curl")

  output_file <- file.path(dest_dir, "usfs_region.shp")

  if (file.exists(output_file) && !overwrite) {
    if (!quiet) message("USFS regions data already exists. Skipping download.")
    return(invisible(output_file))
  }

  if (!quiet) message("Downloading USFS regional boundaries...")

  url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.AdministrativeRegion.zip"
  zip_file <- file.path(dest_dir, "usfs_regions.zip")

  tryCatch({
    curl::multi_download(url, zip_file, resume = TRUE, timeout = 1e6, progress = !quiet)
    utils::unzip(zip_file, exdir = dest_dir)
    unlink(zip_file)

    # Rename files to standard names
    rename_shapefile_components(dest_dir, "usfs_region", quiet)

    if (!file.exists(output_file)) {
      stop("Download completed but shapefile not found", call. = FALSE)
    }

    if (!quiet) message("USFS regions download complete: ", output_file)

  }, error = function(e) {
    stop("Failed to download USFS regions data: ", conditionMessage(e), call. = FALSE)
  })

  invisible(output_file)
}


#' Download Forest Type Map
#'
#' @description Downloads the CONUS forest group/type raster from USFS.
#'
#' @param dest_dir Destination directory for the download.
#' @param overwrite Logical; re-download if file exists?
#' @param quiet Logical; suppress messages?
#'
#' @return Path to the downloaded raster file.
#'
#' @export
#'
#' @examples
#' \donttest{
#' download_forest_types("~/data/forest")
#' }
download_forest_types <- function(dest_dir, overwrite = FALSE, quiet = FALSE) {

  check_package("curl")

  output_file <- file.path(dest_dir, "conus_forestgroup.img")

  if (file.exists(output_file) && !overwrite) {
    if (!quiet) message("Forest types data already exists. Skipping download.")
    return(invisible(output_file))
  }

  if (!quiet) message("Downloading forest type map...")

  url <- "https://data.fs.usda.gov/geodata/rastergateway/forest_type/conus_forestgroup.zip"
  zip_file <- file.path(dest_dir, "forest_types.zip")

  tryCatch({
    curl::multi_download(url, zip_file, resume = TRUE, timeout = 1e6, progress = !quiet)
    utils::unzip(zip_file, exdir = dest_dir)
    unlink(zip_file)

    if (!file.exists(output_file)) {
      stop("Download completed but raster file not found", call. = FALSE)
    }

    if (!quiet) message("Forest types download complete: ", output_file)

  }, error = function(e) {
    stop("Failed to download forest types data: ", conditionMessage(e), call. = FALSE)
  })

  invisible(output_file)
}


#' Download ADS Insect Outbreak Data
#'
#' @description Downloads Aerial Detection Survey (ADS) insect and disease
#'   outbreak data for specified USFS regions.
#'
#' @param dest_dir Destination directory for the download.
#' @param regions Character vector of regions to download. Options:
#'   `"r1"` through `"r6"`, or `"all"` for all regions. Default is `"all"`.
#' @param include_historical Logical; include historical data for regions
#'   5 and 6? Default is `TRUE`.
#' @param overwrite Logical; re-download if files exist?
#' @param quiet Logical; suppress messages?
#'
#' @return Named list of paths to downloaded geodatabases.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Download all regions
#' download_outbreaks("~/data/outbreaks")
#'
#' # Download specific regions
#' download_outbreaks("~/data/outbreaks", regions = c("r1", "r4"))
#' }
download_outbreaks <- function(dest_dir,
                               regions = "all",
                               include_historical = TRUE,
                               overwrite = FALSE,
                               quiet = FALSE) {

  check_package("curl")

  # Define URLs
  urls <- get_outbreak_urls()

  # Determine which regions to download
  valid_regions <- c("r1", "r2", "r3", "r4", "r5", "r6")

  if ("all" %in% regions) {
    regions <- valid_regions
  }

  invalid <- setdiff(regions, valid_regions)
  if (length(invalid) > 0) {
    stop("Invalid region(s): ", paste(invalid, collapse = ", "), call. = FALSE)
  }

  results <- list()

  for (region in regions) {
    region_dir <- file.path(dest_dir, region)

    if (!dir.exists(region_dir)) {
      dir.create(region_dir, recursive = TRUE)
    }

    region_urls <- urls[[region]]

    # Optionally exclude historical data
    if (!include_historical && region %in% c("r5", "r6")) {
      region_urls <- region_urls[1]  # Keep only current data
    }

    if (!quiet) message("Downloading outbreak data for ", toupper(region), "...")

    for (url in region_urls) {
      zip_file <- file.path(dest_dir, paste0(region, "_temp.zip"))

      tryCatch({
        curl::multi_download(url, zip_file, resume = TRUE, timeout = 1e6, progress = !quiet)
        utils::unzip(zip_file, exdir = region_dir)
        unlink(zip_file)
      }, error = function(e) {
        warning("Failed to download ", url, ": ", conditionMessage(e), call. = FALSE)
      })
    }

    results[[region]] <- region_dir
  }

  if (!quiet) message("Outbreak data download complete.")

  invisible(results)
}


# ============================================================================
# Helper Functions (Internal)
# ============================================================================

#' Get ADS Outbreak Data URLs
#'
#' @return Named list of URLs for each region.
#' @noRd
get_outbreak_urls <- function() {

  base_url <- "https://www.fs.usda.gov/foresthealth/docs/IDS_Data_for_Download/"
  base_url2 <- "https://www.fs.usda.gov/Internet/FSE_DOCUMENTS/"

  list(
    r1 = paste0(base_url, "CONUS_Region1_AllYears.gdb.zip"),
    r2 = paste0(base_url, "CONUS_Region2_AllYears.gdb.zip"),
    r3 = paste0(base_url, "CONUS_Region3_AllYears.gdb.zip"),
    r4 = paste0(base_url, "CONUS_Region4_AllYears.gdb.zip"),
    r5 = c(
      paste0(base_url, "CONUS_Region5_AllYears.gdb.zip"),
      paste0(base_url2, "stelprd3856634.zip")  # 1978-2004
    ),
    r6 = c(
      paste0(base_url, "CONUS_Region6_AllYears.gdb.zip"),
      paste0(base_url2, "fsbdev2_025913.zip"),  # 1996
      paste0(base_url2, "fsbdev2_025823.zip"),  # 1995
      paste0(base_url2, "fsbdev2_026303.zip"),  # 1994
      paste0(base_url2, "fsbdev2_026206.zip"),  # 1993
      paste0(base_url2, "fsbdev2_025717.zip"),  # 1992
      paste0(base_url2, "fsbdev2_025842.zip"),  # 1991
      paste0(base_url2, "fsbdev2_026135.zip"),  # 1990
      paste0(base_url2, "fsbdev2_025734.zip"),  # 1989
      paste0(base_url2, "fsbdev2_026440.zip"),  # 1988
      paste0(base_url2, "fsbdev2_026327.zip"),  # 1987
      paste0(base_url2, "fsbdev2_026132.zip"),  # 1986
      paste0(base_url2, "fsbdev2_025841.zip"),  # 1985
      paste0(base_url2, "fsbdev2_025840.zip")   # 1984
    )
  )
}


#' Rename Shapefile Components
#'
#' @description Renames all components of a shapefile to a standard name.
#'
#' @param dir Directory containing shapefile components.
#' @param new_name New base name for the shapefile.
#' @param quiet Logical; suppress messages?
#'
#' @return Invisibly returns TRUE on success.
#' @noRd
rename_shapefile_components <- function(dir, new_name, quiet = FALSE) {

  # Common shapefile extensions
  shp_extensions <- c(".shp", ".shx", ".dbf", ".prj", ".sbn", ".sbx",
                      ".fbn", ".fbx", ".ain", ".aih", ".cpg", ".xml")

  old_files <- list.files(dir, full.names = TRUE)

  for (old_file in old_files) {
    ext <- tools::file_ext(old_file)
    if (paste0(".", ext) %in% shp_extensions) {
      new_file <- file.path(dir, paste0(new_name, ".", ext))

      if (old_file != new_file) {
        tryCatch({
          file.rename(old_file, new_file)
          if (!quiet) message("Renamed: ", basename(old_file), " -> ", basename(new_file))
        }, error = function(e) {
          warning("Failed to rename ", old_file, ": ", conditionMessage(e), call. = FALSE)
        })
      }
    }
  }

  invisible(TRUE)
}


#' Check if Package is Available
#'
#' @param pkg Package name.
#' @noRd
check_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Package '", pkg, "' is required but not installed.",
      call. = FALSE
    )
  }
}