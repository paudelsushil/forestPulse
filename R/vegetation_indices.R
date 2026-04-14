vegetation_index <- function(index_name,
                             image_path   = NULL,
                             red_band     = 1,
                             green_band   = 2,
                             blue_band    = 3,
                             output_path  = NULL,
                             overwrite    = FALSE) {

  # ── Supported index registry ───────────────────────────────────────────────
  # Single source of truth for canonical names (casing matters for switch/output)
  supported_indices <- c(
    "BCC", "BGI", "BI", "BRVI", "CIVE", "ExB", "ExG", "ExGR", "ExR",
    "GCC", "GLI", "GR", "GRVI", "HI", "HUE", "IKAW", "IOR", "IPCA",
    "MGRVI", "MPRI", "MVARI", "NDI", "NGBDI", "NGRDI", "RCC", "RGBVI",
    "PRI", "SAVI", "SCI", "SI", "TGI", "VARI", "VDVI", "VEG",
    "VIgreen", "vNDVI", "WI"
  )

  # ── Case-insensitive lookup map: UPPERCASE key -> canonical name ───────────
  # This is the core fix: resolves any casing the user types to the one true
  # canonical name, without any hardcoded per-index if() patches.
  index_map <- stats::setNames(supported_indices, toupper(supported_indices))

  # ── Special call: list all available indices ───────────────────────────────
  if (tolower(index_name) == "list") {
    cat("Available vegetation indices:\n")
    cat(paste(sort(supported_indices), collapse = ", "), "\n")
    return(invisible(supported_indices))
  }

  # ── Validate and canonicalise index name ───────────────────────────────────
  # Lookup key is always uppercase; canonical casing is restored from the map.
  lookup_key <- toupper(index_name)

  if (!lookup_key %in% names(index_map)) {
    stop(
      sprintf(
        "'%s' is not a recognised index.\nCall vegetation_index('list') to see all supported indices.",
        index_name          # show what the user actually typed in the error
      )
    )
  }

  index_name <- index_map[[lookup_key]]   # e.g. "EXGR" -> "ExGR", "vigreen" -> "VIgreen"

  # ── Check for terra package ────────────────────────────────────────────────
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required.")
  }

  # ── Load or accept a SpatRaster ────────────────────────────────────────────
  if (inherits(image_path, "SpatRaster")) {
    img        <- image_path
    image_file <- terra::sources(img)[1]
    if (is.na(image_file) || image_file == "") {
      image_file <- "in_memory_raster"
    }
  } else if (is.character(image_path)) {
    if (!file.exists(image_path)) {
      stop(sprintf("File not found: '%s'\nPlease check the path and try again.", image_path))
    }
    img        <- terra::rast(image_path)
    image_file <- image_path
  } else {
    stop("'image_path' must be a file path (character) or a SpatRaster object.")
  }

  # ── Validate band count ────────────────────────────────────────────────────
  n_bands    <- terra::nlyr(img)
  band_names <- tolower(names(img))

  if (n_bands < 3) {
    stop(
      sprintf(
        "Image has %d band(s). A minimum of 3 bands (R, G, B) is required.\nFile: %s",
        n_bands, image_file
      )
    )
  }

  # ── Resolve band arguments (name -> index) ─────────────────────────────────
  resolve_band <- function(band_arg, label) {
    if (is.character(band_arg)) {
      idx <- which(band_names == tolower(band_arg))
      if (length(idx) != 1) {
        stop(sprintf("Band name '%s' for %s not found (or is ambiguous) in image band names: %s",
                     band_arg, label, paste(names(img), collapse = ", ")))
      }
      return(idx)
    }
    as.integer(band_arg)
  }

  red_band   <- resolve_band(red_band,   "Red")
  green_band <- resolve_band(green_band, "Green")
  blue_band  <- resolve_band(blue_band,  "Blue")

  # ── Auto-detect RGB bands by name (only when defaults 1,2,3 are used) ──────
  if (n_bands > 3 &&
      red_band == 1 && green_band == 2 && blue_band == 3) {

    rgb_lookup <- c(red = NA_integer_, green = NA_integer_, blue = NA_integer_)
    for (colour in names(rgb_lookup)) {
      idx <- which(band_names == colour)
      if (length(idx) == 1) rgb_lookup[colour] <- idx
    }

    if (!anyNA(rgb_lookup)) {
      red_band   <- rgb_lookup[["red"]]
      green_band <- rgb_lookup[["green"]]
      blue_band  <- rgb_lookup[["blue"]]
      message(
        sprintf("Image has %d bands. Auto-detected RGB from band names: Red=%d, Green=%d, Blue=%d.",
                n_bands, red_band, green_band, blue_band)
      )
    } else {
      message(
        sprintf("Image has %d bands. Could not auto-detect by name; using band %d (Red), %d (Green), %d (Blue).",
                n_bands, red_band, green_band, blue_band)
      )
    }
  }

  # ── Validate band indices ──────────────────────────────────────────────────
  band_args <- c(Red = red_band, Green = green_band, Blue = blue_band)
  invalid   <- band_args[band_args < 1 | band_args > n_bands]

  if (length(invalid) > 0) {
    stop(
      sprintf(
        "Band index out of range for %s (requested band %s, but image has %d bands).",
        paste(names(invalid), collapse = ", "),
        paste(invalid, collapse = ", "),
        n_bands
      )
    )
  }

  # ── Extract individual bands ───────────────────────────────────────────────
  R <- img[[red_band]]
  G <- img[[green_band]]
  B <- img[[blue_band]]

  message(sprintf("Computing index '%s' from: %s", index_name, basename(image_file)))

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

  # ── Name the output layer ──────────────────────────────────────────────────
  names(result) <- index_name

  # ── Build output path if not provided ─────────────────────────────────────
  if (is.null(output_path)) {
    if (image_file == "in_memory_raster") {
      stop("'output_path' must be provided when 'image_path' is an in-memory SpatRaster.")
    }
    input_dir  <- dirname(image_file)
    input_base <- tools::file_path_sans_ext(basename(image_file))
    input_ext  <- tools::file_ext(image_file)
    output_path <- file.path(input_dir, paste0(input_base, "_", index_name, ".", input_ext))
  }

  # ── Save the result raster ─────────────────────────────────────────────────
  terra::writeRaster(result, filename = output_path, overwrite = overwrite)
  message(sprintf("Output saved to: %s", output_path))

  return(result)
}
