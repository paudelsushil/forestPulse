#' Compute RGB-Based Vegetation Indices from Raster Images
#'
#' Calculates one of 37 RGB-derived vegetation indices from a raster image.
#' Accepts either a file path or an in-memory \code{SpatRaster} object as input
#' and writes the result to disk.
#'
#' @param index_name Character string. Name of the vegetation index to compute
#'   (case-insensitive), or \code{"list"} to print all available indices and
#'   return them invisibly.
#' @param image_path A file path (character) to a raster image, or a
#'   \code{\link[terra]{SpatRaster}} object. Required unless
#'   \code{index_name = "list"}.
#' @param red_band Integer or character. Band number or name corresponding to
#'   the red channel. Default \code{1}.
#' @param green_band Integer or character. Band number or name corresponding to
#'   the green channel. Default \code{2}.
#' @param blue_band Integer or character. Band number or name corresponding to
#'   the blue channel. Default \code{3}.
#' @param output_path Character string. Full path (including filename and
#'   extension) for the output raster. If \code{NULL} (default), the output is
#'   written to the same directory as the input, with \code{_<index_name>}
#'   appended to the base filename. Required when \code{image_path} is an
#'   in-memory \code{SpatRaster}.
#' @param overwrite Logical. Should an existing output file be overwritten?
#'   Default \code{FALSE}.
#'
#' @return A single-layer \code{\link[terra]{SpatRaster}} containing the
#'   computed index values, invisibly when writing to disk. When
#'   \code{index_name = "list"}, returns the character vector of supported
#'   index names invisibly.
#'
#' @details
#' ## Supported indices
#' Call \code{vegetation_index("list")} to see all 37 supported indices.
#' Index names are matched case-insensitively.
#'
#' ## Band selection
#' When the image has more than three bands and the default band positions
#' (\code{1, 2, 3}) are used, the function first attempts to match band names
#' \code{"red"}, \code{"green"}, and \code{"blue"} (case-insensitive). If no
#' match is found, bands 1, 2, and 3 are used and a diagnostic message is
#' emitted. Suppress all messages with \code{suppressMessages()}.
#'
#' ## DESCRIPTION \code{Imports}
#' This function requires \pkg{terra} (>= 1.7-0). Add the following to your
#' package \code{DESCRIPTION}:
#' \preformatted{
#' Imports:
#'     stats,
#'     terra (>= 1.7-0),
#'     tools
#' }
#'
#' @examples
#' # List all available indices
#' vegetation_index("list")
#'
#' \dontrun{
#' # Compute ExG from a GeoTIFF (output auto-named alongside input)
#' result <- vegetation_index("ExG", image_path = "field_rgb.tif")
#'
#' # Specify bands by name, custom output path
#' result <- vegetation_index(
#'   index_name  = "NGRDI",
#'   image_path  = "multispectral.tif",
#'   red_band    = "Red",
#'   green_band  = "Green",
#'   blue_band   = "Blue",
#'   output_path = "output/ngrdi.tif",
#'   overwrite   = TRUE
#' )
#'
#' # Pass an in-memory SpatRaster
#' r <- terra::rast(system.file("ex/logo.tif", package = "terra"))
#' result <- vegetation_index("GLI", image_path = r, output_path = "gli.tif")
#' }
#'
#' @export
#' @importFrom stats setNames
#' @importFrom tools file_ext file_path_sans_ext
vegetation_index <- function(index_name,
                             image_path  = NULL,
                             red_band    = 1L,
                             green_band  = 2L,
                             blue_band   = 3L,
                             output_path = NULL,
                             overwrite   = FALSE) {

  # ── Input validation ───────────────────────────────────────────────────────
  if (!is.character(index_name) || length(index_name) != 1L) {
    stop("'index_name' must be a single character string.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L) {
    stop("'overwrite' must be a single logical value (TRUE or FALSE).")
  }

  # ── Supported index registry ───────────────────────────────────────────────
  supported_indices <- c(
    "BCC", "BGI", "BI", "BRVI", "CIVE", "ExB", "ExG", "ExGR", "ExR",
    "GCC", "GLI", "GR", "GRVI", "HI", "HUE", "IKAW", "IOR", "IPCA",
    "MGRVI", "MPRI", "MVARI", "NDI", "NGBDI", "NGRDI", "RCC", "RGBVI",
    "PRI", "SAVI", "SCI", "SI", "TGI", "VARI", "VDVI", "VEG",
    "VIgreen", "vNDVI", "WI"
  )

  # Case-insensitive lookup: UPPERCASE key -> canonical name
  index_map <- stats::setNames(supported_indices, toupper(supported_indices))

  # ── Special call: list all available indices ───────────────────────────────
  if (tolower(index_name) == "list") {
    message("Available vegetation indices:\n",
            paste(sort(supported_indices), collapse = ", "))
    return(invisible(supported_indices))
  }

  # ── Validate and canonicalise index name ───────────────────────────────────
  lookup_key <- toupper(index_name)

  if (!lookup_key %in% names(index_map)) {
    stop(sprintf(
      "'%s' is not a recognised index.\nCall vegetation_index('list') to see all supported indices.",
      index_name
    ))
  }

  index_name <- index_map[[lookup_key]]  # e.g. "exgr" -> "ExGR"

  # ── Check for terra package ────────────────────────────────────────────────
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(
      "Package 'terra' is required but is not installed.\n",
      "Install it with: install.packages(\"terra\")"
    )
  }

  # ── Load or accept a SpatRaster ───────────────────────────────────────────
  if (inherits(image_path, "SpatRaster")) {
    img        <- image_path
    image_file <- terra::sources(img)[1L]
    if (is.na(image_file) || identical(image_file, "")) {
      image_file <- "in_memory_raster"
    }
  } else if (is.character(image_path) && length(image_path) == 1L) {
    if (!file.exists(image_path)) {
      stop(sprintf(
        "File not found: '%s'\nPlease check the path and try again.",
        image_path
      ))
    }
    img        <- terra::rast(image_path)
    image_file <- image_path
  } else {
    stop("'image_path' must be a single file path (character) or a SpatRaster object.")
  }

  # ── Validate band count ────────────────────────────────────────────────────
  n_bands    <- terra::nlyr(img)
  band_names <- tolower(names(img))

  if (n_bands < 3L) {
    stop(sprintf(
      "Image has %d band(s); a minimum of 3 bands (R, G, B) is required.\nFile: %s",
      n_bands, image_file
    ))
  }

  # ── Resolve band arguments (name or integer -> integer index) ─────────────
  resolve_band <- function(band_arg, label) {
    if (is.character(band_arg)) {
      idx <- which(band_names == tolower(band_arg))
      if (length(idx) != 1L) {
        stop(sprintf(
          "Band name '%s' for %s not found (or is ambiguous) among image bands: %s",
          band_arg, label, paste(names(img), collapse = ", ")
        ))
      }
      return(idx)
    }
    as.integer(band_arg)
  }

  red_band   <- resolve_band(red_band,   "Red")
  green_band <- resolve_band(green_band, "Green")
  blue_band  <- resolve_band(blue_band,  "Blue")

  # ── Auto-detect RGB bands by name (only when defaults 1, 2, 3 are used) ───
  if (n_bands > 3L &&
      red_band == 1L && green_band == 2L && blue_band == 3L) {

    rgb_lookup <- c(red = NA_integer_, green = NA_integer_, blue = NA_integer_)

    for (colour in names(rgb_lookup)) {
      idx <- which(band_names == colour)
      if (length(idx) == 1L) rgb_lookup[[colour]] <- idx
    }

    if (!anyNA(rgb_lookup)) {
      red_band   <- rgb_lookup[["red"]]
      green_band <- rgb_lookup[["green"]]
      blue_band  <- rgb_lookup[["blue"]]
      message(sprintf(
        "Image has %d bands. Auto-detected RGB from band names: Red=%d, Green=%d, Blue=%d.",
        n_bands, red_band, green_band, blue_band
      ))
    } else {
      message(sprintf(
        "Image has %d bands. Could not auto-detect by name; using band %d (Red), %d (Green), %d (Blue).",
        n_bands, red_band, green_band, blue_band
      ))
    }
  }

  # ── Validate band indices are within range ─────────────────────────────────
  band_args <- c(Red = red_band, Green = green_band, Blue = blue_band)
  invalid   <- band_args[band_args < 1L | band_args > n_bands]

  if (length(invalid) > 0L) {
    stop(sprintf(
      "Band index out of range for %s (requested %s, but image has only %d band(s)).",
      paste(names(invalid), collapse = ", "),
      paste(invalid,        collapse = ", "),
      n_bands
    ))
  }

  # ── Extract individual bands ───────────────────────────────────────────────
  R <- img[[red_band]]
  G <- img[[green_band]]
  B <- img[[blue_band]]

  message(sprintf("Computing '%s' from: %s", index_name, basename(image_file)))

  # ── Index calculation ──────────────────────────────────────────────────────
  result <- switch(
    index_name,
    "BCC"     = B / (R + G + B),
    "BGI"     = B / G,
    "BI"      = sqrt((R^2 + G^2 + B^2) / 3),
    "BRVI"    = (B - R) / (B + R),
    "CIVE"    = 0.441 * R - 0.881 * G + 0.385 * B + 18.78745,
    "ExB"     = 1.4 * B - G,
    "ExG"     = 2 * G - R - B,
    "ExGR"    = (2 * G - R - B) - (1.4 * R - G),
    "ExR"     = 1.4 * R - G,
    "GCC"     = G / (R + G + B),
    "GLI"     = (2 * G - R - B) / (2 * G + R + B),
    "GR"      = G / R,
    "GRVI"    = (G - R) / (G + R),
    "HI"      = (2 * R - G - B) / (G - B),
    "HUE"     = atan(2 * (B - G - R) / (3.5 * (G - R))),
    "IKAW"    = (R - B) / (R + B),
    "IOR"     = R / B,
    "IPCA"    = 0.994 * abs(R - B) + 0.961 * abs(G - B) + 0.914 * abs(G - R),
    "MGRVI"   = (G^2 - R^2) / (G^2 + R^2),
    "MPRI"    = (G - R) / (G + R),
    "MVARI"   = (G - B) / (G + R - B),
    "NDI"     = 128 * (((G - R) / (G + R)) + 1),
    "NGBDI"   = (G - B) / (G + B),
    "NGRDI"   = (G - R) / (G + R),
    "RCC"     = R / (R + G + B),
    "RGBVI"   = (G^2 - B * R) / (G^2 + B * R),
    "PRI"     = R / G,
    "SAVI"    = (1.5 * (G - R)) / (G + R + 0.5),
    "SCI"     = (R - G) / (R + G),
    "SI"      = (R - B) / (R + B),
    "TGI"     = G - 0.39 * R - 0.61 * B,
    "VARI"    = (G - R) / (G + R - B),
    "VDVI"    = (2 * G - R - B) / (2 * G + R + B),
    "VEG"     = G / (R^0.667 * B^0.334),
    "VIgreen" = (G - R) / (G + R),
    "vNDVI"   = 0.5268 * (R^(-0.1294) * G^0.3389 * B^(-0.3118)),
    "WI"      = (G - B) / (R - G)
  )

  names(result) <- index_name

  # ── Build output path if not provided ─────────────────────────────────────
  if (is.null(output_path)) {
    if (identical(image_file, "in_memory_raster")) {
      stop("'output_path' must be provided when 'image_path' is an in-memory SpatRaster.")
    }
    output_path <- file.path(
      dirname(image_file),
      paste0(
        tools::file_path_sans_ext(basename(image_file)),
        "_", index_name, ".",
        tools::file_ext(image_file)
      )
    )
  }

  # ── Save and return ────────────────────────────────────────────────────────
  terra::writeRaster(result, filename = output_path, overwrite = overwrite)
  message(sprintf("Output saved to: %s", output_path))

  invisible(result)
}