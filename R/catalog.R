# ============================================================================
# ecoPulse :: catalog.R
# ============================================================================
#
# Spatial raster catalog: build, read, query, match.
#
# Exported:
#   build_catalog()
#   read_catalog()
#   query_catalog()
#   match_imagery()
#   footprint()
#
# Internal:
#   .to_sf()
#   .is_readable()
#   .extract_meta()
#   .extent_to_poly()
#   .safe_transform()
#
# ============================================================================


# ---- Internal Helpers ------------------------------------------------------

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


.extract_meta <- function(path) {

  r   <- terra::rast(path)
  ext <- terra::ext(r)

  crs_raw <- terra::crs(r)
  if (is.null(crs_raw) || nchar(crs_raw) == 0L) {
    crs_raw <- NA_character_
  }

  driver <- tryCatch(
    terra::describe(path, sds = FALSE)[1],
    error = function(e) NA_character_
  )

  dtype <- terra::datatype(r)
  dtype <- if (length(dtype) > 1L) dtype[1] else dtype

  list(
    filepath     = normalizePath(path, mustWork = FALSE),
    filename     = basename(path),
    n_bands      = as.integer(terra::nlyr(r)),
    n_rows       = as.integer(terra::nrow(r)),
    n_cols       = as.integer(terra::ncol(r)),
    res_x        = terra::res(r)[1],
    res_y        = terra::res(r)[2],
    crs          = crs_raw,
    xmin         = ext$xmin,
    xmax         = ext$xmax,
    ymin         = ext$ymin,
    ymax         = ext$ymax,
    driver       = driver,
    datatype     = dtype,
    file_size_mb = round(file.info(path)$size / 1048576, 3)
  )
}


.extent_to_poly <- function(xmin, xmax, ymin, ymax, crs) {

  coords <- matrix(
    c(xmin, ymin,
      xmax, ymin,
      xmax, ymax,
      xmin, ymax,
      xmin, ymin),
    ncol = 2, byrow = TRUE
  )

  sfc <- sf::st_sfc(sf::st_polygon(list(coords)))
  sf::st_set_crs(sfc, crs)
}


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


.to_sf <- function(x,
                   x_col = NULL,
                   y_col = NULL,
                   crs   = "EPSG:4326",
                   label = "input") {

  if (inherits(x, "sf")) return(x)
  if (inherits(x, "sfc")) return(sf::st_sf(geometry = x))
  if (inherits(x, "SpatVector")) return(sf::st_as_sf(x))

  if (is.character(x) && length(x) == 1L) {
    if (!file.exists(x)) {
      stop("File '", x, "' not found.", call. = FALSE)
    }
    return(
      tryCatch(
        sf::st_read(x, quiet = TRUE),
        error = function(e) {
          stop(
            "Cannot read '", label, "' file '", basename(x), "': ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )
    )
  }

  if (is.data.frame(x)) {
    geom_col <- attr(x, "sf_column")
    if (!is.null(geom_col) && geom_col %in% names(x)) {
      return(sf::st_as_sf(x))
    }
    if (is.null(x_col) || is.null(y_col)) {
      stop("'", label, "' is a data.frame. Provide x_col and y_col.",
           call. = FALSE)
    }
    if (!x_col %in% names(x)) {
      stop("Column '", x_col, "' not found in ", label, ".", call. = FALSE)
    }
    if (!y_col %in% names(x)) {
      stop("Column '", y_col, "' not found in ", label, ".", call. = FALSE)
    }
    return(sf::st_as_sf(x, coords = c(x_col, y_col), crs = crs))
  }

  stop(
    "'", label, "' must be sf, SpatVector, data.frame, or file path.",
    call. = FALSE
  )
}


# ---- Exported Functions ----------------------------------------------------

#' @title Get Spatial Footprint of a Raster
#' @param path Character. Path to a raster file.
#' @param target_crs Optional CRS to reproject into.
#' @return An \code{sf} object with one polygon.
#' @export
footprint <- function(path, target_crs = NULL) {

  stopifnot(
    "'path' must be a single character string." =
      is.character(path) && length(path) == 1L
  )

  if (!.is_readable(path)) {
    stop("'", basename(path), "' is not GDAL-readable or missing.",
         call. = FALSE)
  }

  meta <- .extract_meta(path)
  geom <- .extent_to_poly(meta$xmin, meta$xmax, meta$ymin, meta$ymax, meta$crs)

  result <- sf::st_sf(
    filepath = meta$filepath,
    filename = meta$filename,
    n_bands  = meta$n_bands,
    crs_info = meta$crs,
    res_x    = meta$res_x,
    res_y    = meta$res_y,
    geometry = geom
  )

  if (!is.null(target_crs)) result <- .safe_transform(result, target_crs)
  result
}


#' @title Build a Spatial Catalog from Geotagged Rasters
#' @param input Directory path or vector of file paths.
#' @param output Output GeoJSON path. Defaults to \code{"catalog.geojson"}.
#' @param target_crs Catalog CRS. Defaults to \code{"EPSG:4326"}.
#' @param recursive Recurse into subdirectories? Default \code{TRUE}.
#' @param overwrite Overwrite existing output? Default \code{FALSE}.
#' @param quiet Suppress messages? Default \code{FALSE}.
#' @return An \code{sf} object (invisibly). GeoJSON written as side effect.
#' @export
build_catalog <- function(input,
                          output     = "catalog.geojson",
                          target_crs = "EPSG:4326",
                          recursive  = TRUE,
                          overwrite  = FALSE,
                          quiet      = FALSE) {

  stopifnot(
    "'input' must be character."   = is.character(input) && length(input) >= 1L,
    "'output' must be character."  = is.character(output) && length(output) == 1L,
    "'recursive' must be logical." = is.logical(recursive),
    "'overwrite' must be logical." = is.logical(overwrite),
    "'quiet' must be logical."     = is.logical(quiet)
  )

  # force .geojson extension
  if (!grepl("\\.geojson$", output, ignore.case = TRUE)) {
    output <- sub("\\.[^.]*$", ".geojson", output)
    if (!grepl("\\.geojson$", output)) output <- paste0(output, ".geojson")
    if (!quiet) message("Output set to '", output, "'.")
  }

  if (file.exists(output) && !overwrite) {
    stop("'", output, "' exists. Set overwrite = TRUE.", call. = FALSE)
  }

  # discover files
  if (length(input) == 1L && dir.exists(input)) {
    files <- list.files(input, full.names = TRUE, recursive = recursive)
    if (length(files) == 0L) {
      stop("No files found in '", input, "'.", call. = FALSE)
    }
  } else {
    files <- input
  }

  # filter readable
  valid  <- vapply(files, .is_readable, logical(1))
  n_skip <- sum(!valid)

  if (sum(valid) == 0L) {
    stop("No GDAL-readable files found.", call. = FALSE)
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

  # extract metadata
  meta_list <- vector("list", n)
  for (i in seq_len(n)) {
    if (!quiet) message("  [", i, "/", n, "] ", basename(raster_files[i]))
    meta_list[[i]] <- tryCatch(
      .extract_meta(raster_files[i]),
      error = function(e) {
        warning("Failed: '", basename(raster_files[i]), "': ",
                conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  }

  meta_list <- Filter(Negate(is.null), meta_list)
  if (length(meta_list) == 0L) {
    stop("All files failed during metadata extraction.", call. = FALSE)
  }

  # assemble sf
  geom_list <- lapply(meta_list, function(m) {
    .extent_to_poly(m$xmin, m$xmax, m$ymin, m$ymax, m$crs)
  })

  attrs <- data.frame(
    filepath     = vapply(meta_list, `[[`, character(1), "filepath"),
    filename     = vapply(meta_list, `[[`, character(1), "filename"),
    n_bands      = vapply(meta_list, `[[`, integer(1),   "n_bands"),
    n_rows       = vapply(meta_list, `[[`, integer(1),   "n_rows"),
    n_cols       = vapply(meta_list, `[[`, integer(1),   "n_cols"),
    res_x        = vapply(meta_list, `[[`, numeric(1),   "res_x"),
    res_y        = vapply(meta_list, `[[`, numeric(1),   "res_y"),
    crs          = vapply(meta_list, `[[`, character(1), "crs"),
    datatype     = vapply(meta_list, `[[`, character(1), "datatype"),
    file_size_mb = vapply(meta_list, `[[`, numeric(1),   "file_size_mb"),
    stringsAsFactors = FALSE
  )

  attrs$catalog_crs <- as.character(target_crs)
  attrs$indexed_at  <- format(Sys.time(), tz = "UTC", usetz = TRUE)

  geom_out <- lapply(geom_list, .safe_transform, to = target_crs)
  catalog  <- sf::st_sf(attrs, geometry = do.call(c, geom_out))

  # write geojson
  out_dir <- dirname(output)
  if (nchar(out_dir) > 0L && !dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  sf::st_write(
    catalog, output,
    driver        = "GeoJSON",
    delete_dsn    = overwrite,
    quiet         = quiet,
    layer_options = "RFC7946=YES"
  )

  if (!quiet) {
    message("Catalog written to '", output, "' (", nrow(catalog), " features).")
  }

  invisible(catalog)
}


#' @title Read an Existing Catalog
#' @param path Path to a GeoJSON catalog.
#' @return An \code{sf} object.
#' @export
read_catalog <- function(path) {

  stopifnot(
    "'path' must be a single character string." =
      is.character(path) && length(path) == 1L
  )
  if (!file.exists(path)) {
    stop("Catalog '", path, "' not found.", call. = FALSE)
  }

  catalog <- tryCatch(
    sf::st_read(path, quiet = TRUE),
    error = function(e) {
      stop("Failed to read catalog: ", conditionMessage(e), call. = FALSE)
    }
  )

  if (!"filepath" %in% names(catalog)) {
    warning("Missing 'filepath' column \u2014 may not be a valid catalog.",
            call. = FALSE)
  }
  catalog
}


#' @title Query Catalog by Plot Locations
#' @param catalog sf object, SpatVector, or path to catalog file.
#' @param plots sf, SpatVector, data.frame, or path to spatial file.
#' @param x_col,y_col Coordinate columns for data.frame input.
#' @param plot_crs CRS for data.frame coordinates.
#' @param buffer Buffer distance in catalog CRS units.
#' @return Subset of catalog with \code{n_plots_matched} column.
#' @export
query_catalog <- function(catalog,
                          plots,
                          x_col    = "x",
                          y_col    = "y",
                          plot_crs = "EPSG:4326",
                          buffer   = 0) {

  catalog  <- .to_sf(catalog, label = "catalog")
  plots_sf <- .to_sf(plots, x_col = x_col, y_col = y_col,
                     crs = plot_crs, label = "plots")

  if (nrow(catalog) == 0L) {
    warning("Catalog is empty.", call. = FALSE)
    return(catalog)
  }

  cat_crs <- sf::st_crs(catalog)
  plt_crs <- sf::st_crs(plots_sf)
  if (!is.na(cat_crs) && !is.na(plt_crs) && cat_crs != plt_crs) {
    plots_sf <- .safe_transform(plots_sf, cat_crs)
  }

  stopifnot(
    "'buffer' must be non-negative numeric." =
      is.numeric(buffer) && length(buffer) == 1L && buffer >= 0
  )
  if (buffer > 0) plots_sf <- sf::st_buffer(plots_sf, dist = buffer)

  hits <- tryCatch(
    suppressMessages(sf::st_intersects(catalog, plots_sf)),
    error = function(e) {
      stop("Intersection failed: ", conditionMessage(e), call. = FALSE)
    }
  )

  matched <- lengths(hits) > 0L
  result  <- catalog[matched, ]
  result$n_plots_matched <- lengths(hits)[matched]

  if (nrow(result) == 0L) {
    msg <- if (buffer > 0) {
      paste0("No matches with buffer = ", buffer, ". Try larger.")
    } else {
      "No matches. Try setting a buffer."
    }
    message(msg)
  }
  result
}


#' @title Match Imagery Paths to Plot Locations
#' @param catalog sf object, SpatVector, or path to catalog file.
#' @param plots sf, SpatVector, data.frame, or path to spatial file.
#' @param x_col,y_col Coordinate columns for data.frame input.
#' @param plot_crs CRS for data.frame coordinates.
#' @param buffer Buffer distance in catalog CRS units.
#' @param col_name Name for the new column. Default \code{"matched_imagery"}.
#' @param match_value Which catalog value to attach for matches.
#'   Use \code{"filepath"} (default), \code{"filename"},
#'   or \code{"filename_no_ext"}.
#' @return Input plots as \code{sf} with matched file paths attached.
#' @export
match_imagery <- function(catalog,
                          plots,
                          x_col    = "x",
                          y_col    = "y",
                          plot_crs = "EPSG:4326",
                          buffer   = 0,
                          col_name = "matched_imagery",
                          match_value = "filepath") {

  catalog  <- .to_sf(catalog, label = "catalog")
  plots_sf <- .to_sf(plots, x_col = x_col, y_col = y_col,
                     crs = plot_crs, label = "plots")

  cat_crs <- sf::st_crs(catalog)
  plt_crs <- sf::st_crs(plots_sf)
  if (!is.na(cat_crs) && !is.na(plt_crs) && cat_crs != plt_crs) {
    plots_sf <- .safe_transform(plots_sf, cat_crs)
  }

  if (buffer > 0) {
    query_geom <- sf::st_buffer(plots_sf, dist = buffer)
  } else {
    query_geom <- plots_sf
  }

  stopifnot(
    "'match_value' must be a single character string." =
      is.character(match_value) && length(match_value) == 1L
  )

  value_vec <- switch(
    match_value,
    filepath = {
      if (!"filepath" %in% names(catalog)) {
        stop("Catalog does not contain a 'filepath' column.", call. = FALSE)
      }
      as.character(catalog$filepath)
    },
    filename = {
      if ("filename" %in% names(catalog)) {
        as.character(catalog$filename)
      } else if ("filepath" %in% names(catalog)) {
        basename(as.character(catalog$filepath))
      } else {
        stop("Catalog does not contain 'filename' or 'filepath' columns.",
             call. = FALSE)
      }
    },
    filename_no_ext = {
      if ("filename" %in% names(catalog)) {
        tools::file_path_sans_ext(as.character(catalog$filename))
      } else if ("filepath" %in% names(catalog)) {
        tools::file_path_sans_ext(basename(as.character(catalog$filepath)))
      } else {
        stop("Catalog does not contain 'filename' or 'filepath' columns.",
             call. = FALSE)
      }
    },
    stop(
      "Invalid 'match_value': ", match_value,
      ". Use one of: 'filepath', 'filename', 'filename_no_ext'.",
      call. = FALSE
    )
  )

  hits <- tryCatch(
    suppressMessages(sf::st_intersects(query_geom, catalog)),
    error = function(e) {
      stop("Intersection failed: ", conditionMessage(e), call. = FALSE)
    }
  )

  paths <- vapply(hits, function(idx) {
    if (length(idx) == 0L) return(NA_character_)
    paste(value_vec[idx], collapse = " | ")
  }, character(1))

  plots_sf[[col_name]] <- paths

  n_matched <- sum(!is.na(paths))
  n_total   <- nrow(plots_sf)
  message(n_matched, " of ", n_total, " plots matched to imagery.")

  if (n_matched < n_total) {
    message(
      n_total - n_matched, " plots had no intersecting imagery.",
      if (buffer == 0) " Try setting a buffer." else ""
    )
  }

  plots_sf
}