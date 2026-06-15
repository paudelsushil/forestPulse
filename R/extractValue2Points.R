#' Extract Raster Values to Point Features
#'
#' Extracts values from one or more rasters at point locations. The function
#' handles three layouts: a directory of per-month NetCDF files (matched by a
#' \code{_YEAR_MONTH_PRISM.nc} naming pattern), per-year MTBS GeoTIFFs
#' (\code{mtbs_CONUS_YEAR.tif}), and single- or multi-layer rasters where the
#' layer is selected by the month or year of each point's date.
#'
#' @param pointFeature An \code{sf} point object, or a path to a vector file
#'   readable by \code{\link[sf]{st_read}}.
#' @param rastDir Either a path to a single raster file or a directory
#'   containing rasters of a single file type.
#' @param variable Character scalar. Name of the output column to add to
#'   \code{pointFeature} and, for some layouts, a selector (e.g. \code{"mtbsI"}).
#' @param match.column Character scalar naming a date column used to pick the
#'   matching file or layer per point, or \code{FALSE} (default) for layouts
#'   that do not need a date.
#'
#' @return \code{pointFeature} with an added column named \code{variable}
#'   containing the extracted values (\code{NA} where no match was found).
#'
#' @examples
#' \dontrun{
#' pts <- extractValue2Points(plots, "rasters/ndvi.tif", variable = "ndvi")
#' }
#'
#' @importFrom sf st_read st_transform st_crs st_coordinates
#' @importFrom terra rast extract has.time nlyr
#' @importFrom tools file_ext
#' @importFrom lubridate year month
#' @export
extractValue2Points <- function(pointFeature,
                                rastDir,
                                variable,
                                match.column = FALSE) {
  if (inherits(pointFeature, "character")) {
    pointFeature <- sf::st_read(pointFeature)
  }
  if (!isFALSE(match.column)) {
    pointFeature[[match.column]] <- as.Date(pointFeature[[match.column]])
  }
  # Create vector to store extracted values
  varValues <- numeric(nrow(pointFeature))
  if (tools::file_ext(rastDir) == "") {
    fileList <- dir(rastDir, full.names = TRUE)
    fileExt <- unique(tools::file_ext(fileList))
    if (length(fileExt) > 1) {
      stop(paste("Directory consists of more than one types of files",
                 fileExt))
    }
  } else {
    fileList <- rastDir
    fileExt <- tools::file_ext(rastDir)
  }

  # For monthly separate netCDF files (drought indices)
  if (any(grepl("_\\d+_\\d+_PRISM\\.nc$", fileList))) {
    for (i in seq_len(nrow(pointFeature))) {
      date <- pointFeature[[match.column]][i]
      year <- lubridate::year(date)
      month <- lubridate::month(date)
      # Find the corresponding file
      file_pattern <- paste0("_", year, "_", month, "_PRISM\\.nc$")
      matching_file <- fileList[grep(file_pattern, fileList)]
      if (length(matching_file) == 1) {
        rastFile <- terra::rast(matching_file)
        pointFeature_i <- pointFeature[i, ] |>
          sf::st_transform(sf::st_crs(rastFile))
        point_coords <- pointFeature_i |> sf::st_coordinates()
        varValues[i] <- terra::extract(rastFile,
                                       matrix(point_coords, ncol = 2))[, 1]
      } else {
        warning(paste("No matching file found for", year, "month", month))
        varValues[i] <- NA
      }
    }
  } else if (fileExt == "tif" && variable == "mtbsI") {
    # Handle separate MTBS files with specific pattern mtbs_CONUS_YYYY.tif
    for (i in seq_len(nrow(pointFeature))) {
      date <- pointFeature[[match.column]][i]
      year <- lubridate::year(date)

      # Find the corresponding MTBS file for the year using exact pattern
      file_pattern <- paste0("mtbs_CONUS_", year, "\\.tif$")
      matching_file <- fileList[grep(file_pattern, fileList, ignore.case = TRUE)]

      if (length(matching_file) == 1) {
        rastFile <- terra::rast(matching_file)
        pointFeature_i <- pointFeature[i, ] |>
          sf::st_transform(sf::st_crs(rastFile))
        point_coords <- pointFeature_i |> sf::st_coordinates()
        varValues[i] <- terra::extract(rastFile,
                                       matrix(point_coords, ncol = 2))[, 1]
      } else {
        warning(paste("No MTBS file found for year:", year))
        varValues[i] <- NA
      }
    }
  } else {
    # Original functionality for other files
    rastFile <- terra::rast(fileList)
    pointFeature <- pointFeature |>
      sf::st_transform(sf::st_crs(rastFile))
    if (fileExt == "nc" && terra::has.time(rastFile) && terra::nlyr(rastFile) > 1) {
      for (i in seq_len(nrow(pointFeature))) {
        year <- lubridate::year(pointFeature[[match.column]][i])
        month <- lubridate::month(pointFeature[[match.column]][i])
        varMonth <- rastFile[[month]]
        point_coords <- pointFeature[i, ] |> sf::st_coordinates()
        varValues[i] <- terra::extract(varMonth,
                                       matrix(point_coords, ncol = 2))[, 1]
      }
    } else if (terra::nlyr(rastFile) == 1) {
      for (i in seq_len(nrow(pointFeature))) {
        point_coords <- pointFeature[i, ] |> sf::st_coordinates()
        varValues[i] <- terra::extract(rastFile,
                                       matrix(point_coords, ncol = 2))[, 1]
      }
    } else if (terra::nlyr(rastFile) >= 1) {
      for (i in seq_len(nrow(pointFeature))) {
        year <- lubridate::year(pointFeature[[match.column]][i])
        varYear <- rastFile[[year]]
        point_coords <- pointFeature[i, ] |> sf::st_coordinates()
        varValues[i] <- terra::extract(varYear,
                                       matrix(point_coords, ncol = 2))[, 1]
      }
    }
  }
  # Add varValues to the original data
  pointFeature[[variable]] <- varValues
  return(pointFeature)
}
