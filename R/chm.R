# ============================================================================
# forestPulse :: chm.R
# ============================================================================
#
# Canopy Height Model computation.
#
# Exported:
#   compute_chm()
#
# Internal:
#   .align_rasters()
#   .validate_surface()
# ============================================================================


# ---- Internal Helpers ------------------------------------------------------

#' Check That a Raster is a Valid Single-Band Surface Model
#'
#' @param r A SpatRaster.
#' @param label Name for error messages.
#' @keywords internal
#' @noRd
.validate_surface <- function(r, label = "raster") {

  if (!inherits(r, "SpatRaster")) {
    stop("'", label, "' must be a SpatRaster.", call. = FALSE)
  }

  if (terra::nlyr(r) > 1) {
    warning(
      "'", label, "' has ", terra::nlyr(r),
      " bands. Using band 1 only.",
      call. = FALSE
    )
    r <- r[[1]]
  }

  if (all(is.na(terra::values(r, mat = FALSE)))) {
    stop("'", label, "' contains only NA values.", call. = FALSE)
  }

  r
}


#' Align Two Rasters to Matching Extent and Resolution
#'
#' @param target Reference raster (keeps its grid).
#' @param source Raster to resample.
#' @param method Resampling method.
#' @return Resampled source raster.
#' @keywords internal
#' @noRd
.align_rasters <- function(target, source, method = "bilinear") {

  needs_align <- !terra::compareGeom(target, source,
                                      stopOnError = FALSE,
                                      messages = FALSE)

  if (!needs_align) return(source)

  message("Resampling to match extents and resolution...")

  # reproject if CRS differs
  if (!identical(terra::crs(target), terra::crs(source))) {
    source <- terra::project(source, target, method = method)
  }

  # resample to match grid
  terra::resample(source, target, method = method)
}


# ---- Exported Function -----------------------------------------------------

#' Compute a Canopy Height Model (CHM)
#'
#' Calculates CHM = DSM - DTM. Accepts file paths or SpatRaster objects.
#' Automatically aligns grids if the inputs differ in extent, resolution,
#' or CRS. Optionally clamps negative values and writes the result to disk.
#'
#' @param dsm Character path or SpatRaster. Digital Surface Model.
#' @param dtm Character path or SpatRaster. Digital Terrain Model.
#' @param output Character. Optional output file path. If \code{NULL}
#'   (default) the CHM is returned in memory only.
#' @param clamp Logical. Set negative CHM values to zero? Defaults to
#'   \code{TRUE}. Negative values typically arise from alignment
#'   artifacts or temporal mismatch between DSM and DTM.
#' @param method Character. Resampling method when grids don't match.
#'   One of \code{"bilinear"} (default) or \code{"near"}.
#' @param overwrite Logical. Overwrite existing output? Default \code{FALSE}.
#'
#' @return A SpatRaster containing the CHM (in same units as inputs,
#'   typically meters).
#'
#' @examples
#' \dontrun{
#' # from file paths
#' chm <- compute_chm("dsm.tif", "dtm.tif", output = "chm.tif")
#'
#' # from SpatRaster objects
#' dsm <- terra::rast("dsm.tif")
#' dtm <- terra::rast("dtm.tif")
#' chm <- compute_chm(dsm, dtm)
#'
#' # keep negatives (e.g. for QA inspection)
#' chm_raw <- compute_chm(dsm, dtm, clamp = FALSE)
#' }
#'
#' @export
compute_chm <- function(dsm,
                        dtm,
                        output    = NULL,
                        clamp     = TRUE,
                        method    = "bilinear",
                        overwrite = FALSE) {

  # --- load if paths --------------------------------------------------------
  if (is.character(dsm) && length(dsm) == 1L) {
    if (!file.exists(dsm)) stop("DSM file '", dsm, "' not found.", call. = FALSE)
    dsm <- terra::rast(dsm)
  }

  if (is.character(dtm) && length(dtm) == 1L) {
    if (!file.exists(dtm)) stop("DTM file '", dtm, "' not found.", call. = FALSE)
    dtm <- terra::rast(dtm)
  }

  # --- validate -------------------------------------------------------------
  dsm <- .validate_surface(dsm, "dsm")
  dtm <- .validate_surface(dtm, "dtm")

  # --- align grids ----------------------------------------------------------
  stopifnot(
    "'method' must be 'bilinear' or 'near'." =
      method %in% c("bilinear", "near")
  )

  dtm <- .align_rasters(target = dsm, source = dtm, method = method)

  # --- compute CHM ----------------------------------------------------------
  chm <- dsm - dtm

  # --- clamp negatives ------------------------------------------------------
  if (clamp) {
    n_neg <- terra::global(chm < 0, "sum", na.rm = TRUE)[[1]]
    if (n_neg > 0) {
      message(n_neg, " negative pixels clamped to 0.")
    }
    chm <- terra::clamp(chm, lower = 0)
  }

  # --- set metadata ---------------------------------------------------------
  names(chm) <- "chm"

  # --- write if requested ---------------------------------------------------
  if (!is.null(output)) {
    stopifnot(
      "'output' must be a single character string." =
        is.character(output) && length(output) == 1L
    )

    if (file.exists(output) && !overwrite) {
      stop("'", output, "' exists. Set overwrite = TRUE.", call. = FALSE)
    }

    out_dir <- dirname(output)
    if (nchar(out_dir) > 0 && !dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }

    terra::writeRaster(chm, output, overwrite = overwrite)
    message("CHM written to '", output, "'.")
  }

  chm
}