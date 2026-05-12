# ============================================================================
# forestPulse :: gap_metrics.R
# ============================================================================
#
# Canopy gap geometry metrics from binary rasters.
#
# Exported:
#   classify_gaps()
#   gap_metrics()
#
# Internal:
#   .validate_binary()
# ============================================================================


# ---- Internal Helpers ------------------------------------------------------

#' Check That a Raster is Binary (canopy/gap)
#'
#' @param r SpatRaster.
#' @return The validated raster.
#' @keywords internal
#' @noRd
.validate_binary <- function(r) {

  if (is.character(r) && length(r) == 1L) {
    if (!file.exists(r)) stop("File '", r, "' not found.", call. = FALSE)
    r <- terra::rast(r)
  }

  if (!inherits(r, "SpatRaster")) {
    stop("Input must be a SpatRaster or file path.", call. = FALSE)
  }

  if (terra::nlyr(r) > 1) {
    warning("Multi-band input. Using band 1.", call. = FALSE)
    r <- r[[1]]
  }

  vals <- unique(terra::values(r, mat = FALSE))
  vals <- vals[!is.na(vals)]

  if (length(vals) > 2) {
    stop(
      "Raster has ", length(vals), " unique values. ",
      "Expected binary (2 classes). Use classify_gaps() first.",
      call. = FALSE
    )
  }

  r
}


# ---- Exported Functions ----------------------------------------------------

#' Classify a CHM or RGB Raster into Binary Canopy/Gap
#'
#' Creates a binary raster where gap = 1 and canopy = 0 using a height
#' or brightness threshold.
#'
#' @param r SpatRaster or file path. CHM, single-band index, or DN image.
#' @param threshold Numeric. Pixels below this value are classified as gap.
#'   For CHM: typical value 2-5 m. For DN/index: depends on your data.
#' @param method Character. Thresholding method:
#'   \code{"fixed"} (default) uses the supplied threshold value.
#'   \code{"otsu"} computes the threshold automatically.
#' @param min_area Numeric. Minimum gap area in square meters. Gaps smaller
#'   than this are removed. Default \code{0} (keep all).
#' @param output Optional file path to save the binary raster.
#' @param overwrite Logical. Default \code{FALSE}.
#'
#' @return A binary SpatRaster (gap = 1, canopy = 0).
#'
#' @examples
#' # synthesise a small CHM
#' set.seed(1)
#' chm <- terra::rast(nrows = 30, ncols = 30, vals = runif(900, 0, 25))
#'
#' # fixed threshold
#' gaps <- classify_gaps(chm, threshold = 3)
#'
#' # automatic Otsu threshold
#' gaps_otsu <- classify_gaps(chm, method = "otsu")
#'
#' @export
classify_gaps <- function(r,
                          threshold = NULL,
                          method    = "fixed",
                          min_area  = 0,
                          output    = NULL,
                          overwrite = FALSE) {

  # load if path
  if (is.character(r) && length(r) == 1L) {
    if (!file.exists(r)) stop("File '", r, "' not found.", call. = FALSE)
    r <- terra::rast(r)
  }

  if (terra::nlyr(r) > 1) {
    warning("Multi-band input. Using band 1.", call. = FALSE)
    r <- r[[1]]
  }

  # --- determine threshold --------------------------------------------------
  stopifnot(
    "'method' must be 'fixed' or 'otsu'." = method %in% c("fixed", "otsu")
  )

  if (method == "otsu") {
    vals <- terra::values(r, mat = FALSE)
    vals <- vals[!is.na(vals)]
    threshold <- .otsu_threshold(vals)
    message("Otsu threshold: ", round(threshold, 3))
  }

  if (is.null(threshold)) {
    stop("Provide a threshold value or use method = 'otsu'.", call. = FALSE)
  }

  # --- classify -------------------------------------------------------------
  binary <- terra::ifel(r <= threshold, 1L, 0L)
  names(binary) <- "gap"

  # --- remove small gaps ----------------------------------------------------
  if (min_area > 0) {
    # label connected patches
    patches <- terra::patches(binary, directions = 8, zeroAsNA = TRUE)

    # compute area per patch
    px_area <- terra::res(binary)[1] * terra::res(binary)[2]
    freq_tbl <- terra::freq(patches)
    small_ids <- freq_tbl$value[freq_tbl$count * px_area < min_area]

    if (length(small_ids) > 0) {
      binary <- terra::ifel(patches %in% small_ids, 0L, binary)
      message(length(small_ids), " gaps smaller than ", min_area, " m2 removed.")
    }
  }

  # --- write ----------------------------------------------------------------
  if (!is.null(output)) {
    out_dir <- dirname(output)
    if (nchar(out_dir) > 0 && !dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }
    terra::writeRaster(binary, output, overwrite = overwrite)
    message("Binary gap raster written to '", output, "'.")
  }

  binary
}


#' Compute Otsu's Threshold
#'
#' @param vals Numeric vector of pixel values.
#' @return Single numeric threshold.
#' @keywords internal
#' @noRd
.otsu_threshold <- function(vals) {

  h <- graphics::hist(vals, breaks = 256, plot = FALSE)
  counts <- h$counts
  mids   <- h$mids
  total  <- sum(counts)

  sum_all <- sum(counts * mids)
  sum_bg  <- 0
  w_bg    <- 0
  max_var <- 0
  best_t  <- mids[1]

  for (i in seq_along(counts)) {
    w_bg   <- w_bg + counts[i]
    if (w_bg == 0) next

    w_fg <- total - w_bg
    if (w_fg == 0) break

    sum_bg  <- sum_bg + counts[i] * mids[i]
    mean_bg <- sum_bg / w_bg
    mean_fg <- (sum_all - sum_bg) / w_fg

    between_var <- w_bg * w_fg * (mean_bg - mean_fg)^2

    if (between_var > max_var) {
      max_var <- between_var
      best_t  <- mids[i]
    }
  }

  best_t
}


#' Compute Canopy Gap Geometry Metrics
#'
#' Calculates gap area, shape index, edge density, and fractal dimension
#' (crown-edge complexity) from a binary canopy/gap raster using
#' \code{landscapemetrics}.
#'
#' @param r SpatRaster or file path. Binary raster where gap = 1,
#'   canopy = 0. Use \code{\link{classify_gaps}} to create this.
#' @param level Character. Metric aggregation level:
#'   \code{"patch"} (default) returns per-gap metrics.
#'   \code{"class"} returns summary metrics for all gaps combined.
#'   \code{"landscape"} returns whole-raster summary.
#' @param min_area Numeric. Ignore gap patches smaller than this (m2).
#'   Default \code{0}.
#'
#' @return A \code{data.frame} with columns depending on level:
#'   \describe{
#'     \item{patch level}{patch_id, area_m2, shape_index, frac_dim}
#'     \item{class level}{class, n_gaps, total_area_m2, mean_area_m2,
#'       mean_shape, edge_density, mean_frac}
#'     \item{landscape level}{all of the above aggregated}
#'   }
#'
#' @examples
#' # synthesise a small CHM and classify gaps
#' set.seed(1)
#' chm  <- terra::rast(nrows = 30, ncols = 30, vals = runif(900, 0, 25))
#' gaps <- classify_gaps(chm, threshold = 3)
#'
#' # per-gap metrics
#' metrics <- gap_metrics(gaps)
#'
#' # class-level summary
#' summary <- gap_metrics(gaps, level = "class")
#'
#' @export
gap_metrics <- function(r, level = "patch", min_area = 0) {

  r <- .validate_binary(r)

  stopifnot(
    "'level' must be 'patch', 'class', or 'landscape'." =
      level %in% c("patch", "class", "landscape")
  )

  # landscapemetrics needs raster with integer classes
  r <- terra::as.int(r)

  # --- patch-level metrics --------------------------------------------------
  if (level == "patch") {

    area  <- landscapemetrics::lsm_p_area(r)
    shape <- landscapemetrics::lsm_p_shape(r)
    frac  <- landscapemetrics::lsm_p_frac(r)

    # filter to gap class only (class = 1)
    area  <- area[area$class == 1, ]
    shape <- shape[shape$class == 1, ]
    frac  <- frac[frac$class == 1, ]

    result <- data.frame(
      patch_id    = area$id,
      area_m2     = area$value * 10000,    # lsm returns hectares
      shape_index = shape$value,
      frac_dim    = frac$value,
      stringsAsFactors = FALSE
    )

    # filter by minimum area
    if (min_area > 0) {
      result <- result[result$area_m2 >= min_area, ]
    }

    # sort by area descending
    result <- result[order(-result$area_m2), ]
    rownames(result) <- NULL

    return(result)
  }

  # --- class-level metrics --------------------------------------------------
  if (level == "class") {

    n_patches <- landscapemetrics::lsm_c_np(r)
    total_a   <- landscapemetrics::lsm_c_ca(r)
    mean_a    <- landscapemetrics::lsm_c_area_mn(r)
    mean_sh   <- landscapemetrics::lsm_c_shape_mn(r)
    ed        <- landscapemetrics::lsm_c_ed(r)
    mean_fr   <- landscapemetrics::lsm_c_frac_mn(r)

    # gap class only
    gap <- function(df) df[df$class == 1, "value", drop = TRUE]

    result <- data.frame(
      class           = "gap",
      n_gaps          = gap(n_patches),
      total_area_m2   = gap(total_a) * 10000,
      mean_area_m2    = gap(mean_a) * 10000,
      mean_shape      = gap(mean_sh),
      edge_density    = gap(ed),
      mean_frac       = gap(mean_fr),
      stringsAsFactors = FALSE
    )

    return(result)
  }

  # --- landscape-level metrics ----------------------------------------------
  if (level == "landscape") {

    n_patches <- landscapemetrics::lsm_l_np(r)
    ed        <- landscapemetrics::lsm_l_ed(r)
    mean_sh   <- landscapemetrics::lsm_l_shape_mn(r)
    mean_fr   <- landscapemetrics::lsm_l_frac_mn(r)
    gap_frac  <- landscapemetrics::lsm_c_pland(r)

    gf <- gap_frac[gap_frac$class == 1, "value", drop = TRUE]
    if (length(gf) == 0) gf <- 0

    result <- data.frame(
      n_patches       = n_patches$value,
      edge_density    = ed$value,
      mean_shape      = mean_sh$value,
      mean_frac       = mean_fr$value,
      gap_fraction_pct = gf,
      stringsAsFactors = FALSE
    )

    return(result)
  }
}