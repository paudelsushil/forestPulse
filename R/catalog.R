# ============================================================================
# ecoPulse :: Spatial Raster Catalog Module
# ============================================================================


# ---- Internal Helpers ------------------------------------------------------

#' Check if a File is GDAL-Readable
#'
#' @param path Character. File path.
#' @return Logical.
#' @keywords internal
#' @noRd
.is_readable <- function(path) {

  if (!file.exists(path)) return(FALSE)

  tryCatch(
    {
      info <- terra::describe(path, sds = FALSE)
      length(info) > 0L
    },
    error = function(e) FALSE
  )
}


#' Extract Spatial Metadata from a Raster Header
#'
#' @param path Character. Path to a GDAL-readable raster.
#' @return Named list of metadata fields.
#' @keywords internal
#' @noRd
.extract_meta <- function(path) {

  r   <- terra::rast(path)
  ext <- terra::ext(r)
  crs_raw <- terra::crs(r)

  epsg <- tryCatch(
    terra::crs(r, describe = TRUE)$code,
    error = function(e) NA_character_
  )

  driver <- tryCatch(
    terra::describe(path, sds = FALSE)[1],
    error = function(e) NA_character_
  )

  list(
    filepath     = normalizePath(path, mustWork = FALSE),
    filename     = basename(path),
    n_bands      = terra::nlyr(r),
    n_rows       = terra::nrow(r),
    n_cols       = terra::ncol(r),
    res_x        = terra::res(r)[1],
    res_y        = terra::res(r)[2],
    crs_wkt      = crs_raw,
    crs_epsg     = epsg,
    xmin         = ext$xmin,
    xmax         = ext$xmax,
    ymin         = ext$ymin,
    ymax         = ext$ymax,
    driver       = driver,
    datatype     = terra::datatype(r),
    file_size_mb = round(file.info(path)$size / 1048576, 3)
  )
}


#' Convert Bounding Box to sf Polygon
#'
#' @param xmin,xmax,ymin,ymax Numeric. Coordinates.
#' @param crs_wkt Character. CRS as WKT.
#' @return An \code{sfc} polygon.
#' @keywords internal
#' @noRd
.extent_to_poly <- function(xmin, xmax, ymin, ymax, crs_wkt) {

  coords <- matrix(
    c(xmin, ymin,
      xmax, ymin,
      xmax, ymax,
      xmin, ymax,
      xmin, ymin),
    ncol = 2, byrow = TRUE
  )

  sfc <- sf::st_sfc(sf::st_polygon(list(coords)))
  sf::st_set_crs(sfc, crs_wkt)
}


#' Transform CRS with Graceful Fallback
#'
#' @param geom An \code{sf} or \code{sfc} object.
#' @param to Target CRS.
#' @return Transformed geometry, or original on failure.
#' @keywords internal
#' @noRd
.safe_transform <- function(geom, to) {

  tryCatch(
    sf::st_transform(geom, to),
    error = function(e) {
      warning(
        "CRS transformation failed; returning native CRS.\n",
        "  Reason: ", conditionMessage(e),
        call. = FALSE
      )
      geom
    }
  )
}


# ---- Exported Functions ----------------------------------------------------

#' Get the Spatial Footprint of a Raster
#'
#' Reads only the header of any GDAL-readable raster and returns its
#' bounding-box footprint as an \code{sf} polygon.
#'
#' @param path Character. Path to a raster file.
#' @param target_crs Optional CRS to reproject into (e.g. \code{"EPSG:4326"}).
#'   \code{NULL} keeps the native CRS.
#'
#' @return An \code{sf} object with one polygon feature.
#'
#' @examples
#' \dontrun{
#' fp <- footprint("scene.tif")
#' plot(sf::st_geometry(fp))
#' }
#'
#' @export
footprint <- function(path, target_crs = NULL) {

  stopifnot(
    "'path' must be a single character string." =
      is.character(path) && length(path) == 1L
  )

  if (!.is_readable(path)) {
    stop(
      "'", basename(path), "' is not GDAL-readable or does not exist.",
      call. = FALSE
    )
  }

  meta <- .extract_meta(path)
  geom <- .extent_to_poly(
    meta$xmin, meta$xmax, meta$ymin, meta$ymax, meta$crs_wkt
  )

  result <- sf::st_sf(
    filepath = meta$filepath,
    filename = meta$filename,
    n_bands  = meta$n_bands,
    crs_epsg = meta$crs_epsg,
    res_x    = meta$res_x,
    res_y    = meta$res_y,
    geometry = geom
  )

  if (!is.null(target_crs)) {
    result <- .safe_transform(result, target_crs)
  }

  result
}


#' Build a Spatial Catalog from Geotagged Rasters
#'
#' Scans files, reads spatial headers (no pixel data), and writes a GeoJSON
#' FeatureCollection that serves as a lightweight spatial catalog.
#'
#' @param input Character. A directory path or a vector of file paths.
#' @param output Character. Output GeoJSON path.
#'   Defaults to \code{"catalog.geojson"}.
#' @param target_crs CRS for the catalog. Defaults to \code{"EPSG:4326"}.
#' @param recursive Logical. Recurse into subdirectories? Defaults to
#'   \code{TRUE}.
#' @param overwrite Logical. Overwrite existing output? Defaults to
#'   \code{FALSE}.
#' @param quiet Logical. Suppress messages? Defaults to \code{FALSE}.
#'
#' @return An \code{sf} object (invisibly). GeoJSON is written as a side
#'   effect.
#'
#' @examples
#' \dontrun{
#' cat <- build_catalog("data/imagery/", output = "catalog.geojson")
#' }
#'
#' @export
build_catalog <- function(input,
                          output     = "catalog.geojson",
                          target_crs = "EPSG:4326",
                          recursive  = TRUE,
                          overwrite  = FALSE,
                          quiet      = FALSE) {

  # --- validate inputs ------------------------------------------------------
  stopifnot(
    "'input' must be a character vector." =
      is.character(input) && length(input) >= 1L,
    "'output' must be a single character string." =
      is.character(output) && length(output) == 1L,
    "'target_crs' must be character or numeric." =
      is.character(target_crs) || is.numeric(target_crs),
    "'recursive' must be logical." = is.logical(recursive),
    "'overwrite' must be logical."  = is.logical(overwrite),
    "'quiet' must be logical."      = is.logical(quiet)
  )

  if (file.exists(output) && !overwrite) {
    stop(
      "Output '", output, "' exists. Set overwrite = TRUE.",
      call. = FALSE
    )
  }

  # --- discover files -------------------------------------------------------
  if (length(input) == 1L && dir.exists(input)) {
    files <- list.files(input, full.names = TRUE, recursive = recursive)
    if (length(files) == 0L) {
      stop("No files found in '", input, "'.", call. = FALSE)
    }
  } else {
    files <- input
  }

  # --- filter readable rasters ---------------------------------------------
  valid   <- vapply(files, .is_readable, logical(1))
  n_skip  <- sum(!valid)

  if (sum(valid) == 0L) {
    stop(
      "None of the ", length(files), " file(s) are GDAL-readable.",
      call. = FALSE
    )
  }

  if (n_skip > 0L && !quiet) {
    warning(
      "Skipping ", n_skip, " unreadable file(s): ",
      paste(basename(files[!valid]), collapse = ", "),
      call. = FALSE
    )
  }

  raster_files <- files[valid]
  n <- length(raster_files)

  if (!quiet) message("Indexing ", n, " raster(s)...")

  # --- extract metadata -----------------------------------------------------
  meta_list <- vector("list", n)

  for (i in seq_len(n)) {
    if (!quiet) message("  [", i, "/", n, "] ", basename(raster_files[i]))

    meta_list[[i]] <- tryCatch(
      .extract_meta(raster_files[i]),
      error = function(e) {
        warning(
          "Failed to read '", basename(raster_files[i]),
          "': ", conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )
  }

  meta_list <- Filter(Negate(is.null), meta_list)

  if (length(meta_list) == 0L) {
    stop("All files failed during metadata extraction.", call. = FALSE)
  }

  # --- assemble sf ----------------------------------------------------------
  geom_list <- lapply(meta_list, function(m) {
    .extent_to_poly(m$xmin, m$xmax, m$ymin, m$ymax, m$crs_wkt)
  })

  attrs <- data.frame(
    filepath     = vapply(meta_list, `[[`, character(1), "filepath"),
    filename     = vapply(meta_list, `[[`, character(1), "filename"),
    n_bands      = vapply(meta_list, `[[`, integer(1),   "n_bands"),
    n_rows       = vapply(meta_list, `[[`, integer(1),   "n_rows"),
    n_cols       = vapply(meta_list, `[[`, integer(1),   "n_cols"),
    res_x        = vapply(meta_list, `[[`, numeric(1),   "res_x"),
    res_y        = vapply(meta_list, `[[`, numeric(1),   "res_y"),
    crs_epsg     = vapply(meta_list, `[[`, character(1), "crs_epsg"),
    datatype     = vapply(meta_list, `[[`, character(1), "datatype"),
    file_size_mb = vapply(meta_list, `[[`, numeric(1),   "file_size_mb"),
    stringsAsFactors = FALSE
  )

  attrs$catalog_crs <- as.character(target_crs)
  attrs$indexed_at  <- format(Sys.time(), tz = "UTC", usetz = TRUE)

  geom_out <- lapply(geom_list, .safe_transform, to = target_crs)
  catalog  <- sf::st_sf(attrs, geometry = do.call(c, geom_out))

  # --- write ----------------------------------------------------------------
  out_dir <- dirname(output)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  sf::st_write(catalog, output, driver = "GeoJSON",
               delete_dsn = overwrite, quiet = quiet)

  if (!quiet) {
    message("Catalog written to '", output, "' (", nrow(catalog), " features).")
  }

  invisible(catalog)
}


#' Read an Existing Catalog
#'
#' @param path Character. Path to a GeoJSON catalog.
#' @return An \code{sf} object.
#'
#' @examples
#' \dontrun{
#' cat <- read_catalog("catalog.geojson")
#' }
#'
#' @export
read_catalog <- function(path) {

  stopifnot(
    "'path' must be a single character string." =
      is.character(path) && length(path) == 1L
  )

  if (!file.exists(path)) {
    stop("Catalog '", path, "' does not exist.", call. = FALSE)
  }

  catalog <- tryCatch(
    sf::st_read(path, quiet = TRUE),
    error = function(e) {
      stop("Failed to read catalog: ", conditionMessage(e), call. = FALSE)
    }
  )

  if (!"filepath" %in% names(catalog)) {
    warning("Missing 'filepath' column — may not be an ecoPulse catalog.",
            call. = FALSE)
  }

  catalog
}


#' Query a Catalog by Plot Locations
#'
#' Finds every raster whose footprint intersects one or more field-plot
#' locations. Plots can be an \code{sf} object, a \code{data.frame} with
#' coordinate columns, or a file path.
#'
#' @param catalog An \code{sf} catalog or a path to one.
#' @param plots Plot locations: \code{sf}, \code{data.frame}, or file path.
#' @param x_col,y_col Column names for coordinates when \code{plots} is a
#'   data.frame. Defaults: \code{"x"}, \code{"y"}.
#' @param plot_crs CRS of data.frame coordinates. Default \code{"EPSG:4326"}.
#' @param buffer Numeric. Buffer distance in catalog CRS units. Default
#'   \code{0}.
#'
#' @return An \code{sf} subset of the catalog with an added
#'   \code{n_plots_matched} column.
#'
#' @examples
#' \dontrun{
#' matched <- query_catalog("catalog.geojson", plots_df,
#'                          x_col = "lon", y_col = "lat",
#'                          buffer = 50)
#' }
#'
#' @export
query_catalog <- function(catalog,
                          plots,
                          x_col    = "x",
                          y_col    = "y",
                          plot_crs = "EPSG:4326",
                          buffer   = 0) {

  # --- resolve catalog ------------------------------------------------------
  if (is.character(catalog) && length(catalog) == 1L) {
    catalog <- read_catalog(catalog)
  }
  stopifnot("'catalog' must be an sf object." = inherits(catalog, "sf"))

  if (nrow(catalog) == 0L) {
    warning("Catalog is empty.", call. = FALSE)
    return(catalog)
  }

  # --- resolve plots --------------------------------------------------------
  plots_sf <- .resolve_plots(plots, x_col, y_col, plot_crs)

  # --- align CRS ------------------------------------------------------------
  cat_crs  <- sf::st_crs(catalog)
  plt_crs  <- sf::st_crs(plots_sf)

  if (!is.na(cat_crs) && !is.na(plt_crs) && cat_crs != plt_crs) {
    plots_sf <- .safe_transform(plots_sf, cat_crs)
  }

  # --- buffer ---------------------------------------------------------------
  stopifnot(
    "'buffer' must be a single non-negative number." =
      is.numeric(buffer) && length(buffer) == 1L && buffer >= 0
  )
  if (buffer > 0) plots_sf <- sf::st_buffer(plots_sf, dist = buffer)

  # --- intersect ------------------------------------------------------------
  hits <- tryCatch(
    suppressMessages(sf::st_intersects(catalog, plots_sf)),
    error = function(e) {
      stop("Spatial intersection failed: ", conditionMessage(e),
           call. = FALSE)
    }
  )

  matched <- lengths(hits) > 0L
  result  <- catalog[matched, ]
  result$n_plots_matched <- lengths(hits)[matched]

  if (nrow(result) == 0L) {
    msg <- if (buffer > 0) {
      paste0("No intersections found with buffer = ", buffer,
             ". Try a larger buffer.")
    } else {
      "No intersections found. Try setting a buffer."
    }
    message(msg)
  }

  result
}


# ---- Internal: resolve plot input ------------------------------------------

#' Coerce Various Plot Inputs to sf
#'
#' @param plots sf, data.frame, or file path.
#' @param x_col,y_col Column names.
#' @param crs CRS for data.frame input.
#' @return An \code{sf} object.
#' @keywords internal
#' @noRd
.resolve_plots <- function(plots, x_col, y_col, crs) {

  if (is.character(plots) && length(plots) == 1L) {
    if (!file.exists(plots)) {
      stop("Plot file '", plots, "' not found.", call. = FALSE)
    }
    return(
      tryCatch(
        sf::st_read(plots, quiet = TRUE),
        error = function(e) {
          stop("Cannot read '", plots, "': ", conditionMessage(e),
               call. = FALSE)
        }
      )
    )
  }

  if (inherits(plots, "sfc")) {
    return(sf::st_sf(geometry = plots))
  }

  if (inherits(plots, "sf")) {
    return(plots)
  }

  if (is.data.frame(plots)) {
    if (!x_col %in% names(plots)) {
      stop("Column '", x_col, "' not found. Set x_col.", call. = FALSE)
    }
    if (!y_col %in% names(plots)) {
      stop("Column '", y_col, "' not found. Set y_col.", call. = FALSE)
    }
    return(sf::st_as_sf(plots, coords = c(x_col, y_col), crs = crs))
  }

  stop(
    "'plots' must be sf, data.frame, or a file path.",
    call. = FALSE
  )
}
