# ============================================================================
# forestPulse :: extract_plot_metrics.R
# ============================================================================
#
# Plot-level gap metric extraction aligned to field plot footprints.
#
# Exported:
#   extract_plot_metrics()
#
# Internal:
#   .clip_to_plot()
#   .safe_gap_metrics()
# ============================================================================


# ---- Internal Helpers ------------------------------------------------------

#' Clip a Raster to a Circular Plot
#'
#' @param r SpatRaster.
#' @param center sf or SpatVector point.
#' @param radius Numeric. Buffer radius in meters.
#' @return Clipped SpatRaster (circular mask applied).
#' @keywords internal
#' @noRd
.clip_to_plot <- function(r, center, radius) {

  # ensure same CRS
  if (inherits(center, "sf")) {
    center_vect <- terra::vect(center)
  } else {
    center_vect <- center
  }

  if (!identical(terra::crs(r), terra::crs(center_vect))) {
    center_vect <- terra::project(center_vect, terra::crs(r))
  }

  # create circular buffer
  circle <- terra::buffer(center_vect, width = radius)

  # crop then mask
  cropped <- terra::crop(r, circle)
  masked  <- terra::mask(cropped, circle)

  masked
}


#' Safely Compute Gap Metrics with Error Handling
#'
#' @param binary_clip Clipped binary SpatRaster.
#' @param min_area Minimum gap area in m2.
#' @return Named list of class-level metrics, or NAs on failure.
#' @keywords internal
#' @noRd
.safe_gap_metrics <- function(binary_clip, min_area = 0) {

  empty <- list(
    n_gaps          = 0L,
    total_gap_m2    = 0,
    mean_gap_m2     = NA_real_,
    max_gap_m2      = NA_real_,
    mean_shape      = NA_real_,
    edge_density    = NA_real_,
    mean_frac       = NA_real_,
    gap_fraction    = 0
  )

  # check if any gap pixels exist
  vals <- terra::values(binary_clip, mat = FALSE)
  vals <- vals[!is.na(vals)]

  if (length(vals) == 0 || sum(vals == 1) == 0) {
    return(empty)
  }

  tryCatch(
    {
      r_int <- terra::as.int(binary_clip)

      # patch level for gap class
      area  <- landscapemetrics::lsm_p_area(r_int)
      area  <- area[area$class == 1, ]
      area$m2 <- area$value * 10000

      # filter small gaps
      if (min_area > 0) {
        area <- area[area$m2 >= min_area, ]
      }

      if (nrow(area) == 0) return(empty)

      # keep only valid patch IDs after area filter
      valid_ids <- area$id

      shape <- landscapemetrics::lsm_p_shape(r_int)
      shape <- shape[shape$class == 1 & shape$id %in% valid_ids, ]

      frac <- landscapemetrics::lsm_p_frac(r_int)
      frac <- frac[frac$class == 1 & frac$id %in% valid_ids, ]

      ed <- landscapemetrics::lsm_c_ed(r_int)
      ed <- ed[ed$class == 1, ]

      # total pixel area
      px_area <- terra::res(binary_clip)[1] * terra::res(binary_clip)[2]
      total_pixels <- sum(!is.na(vals))
      plot_area <- total_pixels * px_area

      list(
        n_gaps       = nrow(area),
        total_gap_m2 = sum(area$m2),
        mean_gap_m2  = mean(area$m2),
        max_gap_m2   = max(area$m2),
        mean_shape   = mean(shape$value, na.rm = TRUE),
        edge_density = if (nrow(ed) > 0) ed$value else 0,
        mean_frac    = mean(frac$value, na.rm = TRUE),
        gap_fraction = sum(area$m2) / plot_area * 100
      )
    },
    error = function(e) {
      warning("Metrics failed: ", conditionMessage(e), call. = FALSE)
      empty
    }
  )
}


# ---- Exported Function -----------------------------------------------------

#' Extract Gap Metrics Matched to Field Plot Footprints
#'
#' Clips a CHM (or binary gap raster) to each field plot's circular
#' footprint and computes gap geometry metrics within that footprint.
#' This ensures spatial correspondence between remotely sensed gap
#' metrics and field-measured variables (regeneration, fuel loads,
#' ground vegetation).
#'
#' @param chm SpatRaster or file path. Canopy Height Model.
#' @param plots sf object, data.frame, or file path. Plot center locations.
#' @param plot_id Character. Column name for plot identifiers.
#'   Default \code{"plot_id"}.
#' @param x_col,y_col Coordinate columns if \code{plots} is a data.frame.
#' @param plot_crs CRS for data.frame coordinates. Default \code{"EPSG:4326"}.
#' @param radius Numeric. Plot radius in meters. Default \code{15.24}
#'   (50 ft USFS standard).
#' @param gap_threshold Numeric. CHM height below which pixels are
#'   classified as gap (meters). Default \code{3}.
#' @param threshold_method Character. \code{"fixed"} or \code{"otsu"}.
#'   Default \code{"fixed"}.
#' @param min_gap_area Numeric. Minimum gap patch size in m2.
#'   Default \code{1}.
#' @param multi_radius Numeric vector. Optional additional radii for
#'   sensitivity analysis. e.g. \code{c(25, 50, 75)}.
#'   Default \code{NULL} (primary radius only).
#' @param save_clips Logical. Save clipped binary rasters per plot?
#'   Default \code{FALSE}.
#' @param clip_dir Directory for saved clips. Default \code{"gap_clips"}.
#'
#' @return A \code{data.frame} with one row per plot (per radius if
#'   \code{multi_radius} is used) containing plot ID, radius, and
#'   all gap metrics.
#'
#' @examples
#' # synthesise a CHM and field plot centres (no CRS for a self-contained
#' # example; in practice both inputs should carry a projected CRS in metres)
#' set.seed(1)
#' chm <- terra::rast(
#'   nrows = 60, ncols = 60,
#'   xmin = 0, xmax = 60, ymin = 0, ymax = 60,
#'   vals = runif(3600, 0, 25)
#' )
#'
#' plots <- sf::st_as_sf(
#'   data.frame(plot_id = c("P1", "P2"), x = c(20, 40), y = c(30, 30)),
#'   coords = c("x", "y")
#' )
#'
#' metrics <- extract_plot_metrics(
#'   chm           = chm,
#'   plots         = plots,
#'   radius        = 10,
#'   gap_threshold = 3
#' )
#'
#' @export
extract_plot_metrics <- function(chm,
                                 plots,
                                 plot_id          = "plot_id",
                                 x_col            = "x",
                                 y_col            = "y",
                                 plot_crs         = "EPSG:4326",
                                 radius           = 15.24,
                                 gap_threshold    = 3,
                                 threshold_method = "fixed",
                                 min_gap_area     = 1,
                                 multi_radius     = NULL,
                                 save_clips       = FALSE,
                                 clip_dir         = "gap_clips") {

  # --- load CHM -------------------------------------------------------------
  if (is.character(chm) && length(chm) == 1L) {
    if (!file.exists(chm)) stop("CHM '", chm, "' not found.", call. = FALSE)
    chm <- terra::rast(chm)
  }

  if (terra::nlyr(chm) > 1) {
    warning("Multi-band CHM. Using band 1.", call. = FALSE)
    chm <- chm[[1]]
  }

  # --- resolve plots --------------------------------------------------------
  plots_sf <- .to_sf(plots, x_col = x_col, y_col = y_col,
                     crs = plot_crs, label = "plots")

  # ensure plot_id column exists
  if (!plot_id %in% names(plots_sf)) {
    plots_sf[[plot_id]] <- paste0("plot_", seq_len(nrow(plots_sf)))
    warning("'", plot_id, "' column not found. Generated IDs.", call. = FALSE)
  }

  # reproject plots to CHM CRS
  chm_crs <- terra::crs(chm)
  if (!is.na(sf::st_crs(plots_sf)) && !is.na(chm_crs)) {
    plots_sf <- sf::st_transform(plots_sf, chm_crs)
  }

  # --- define all radii to process ------------------------------------------
  all_radii <- radius
  if (!is.null(multi_radius)) {
    all_radii <- sort(unique(c(radius, multi_radius)))
  }

  # --- setup output ---------------------------------------------------------
  n_plots  <- nrow(plots_sf)
  n_radii  <- length(all_radii)
  results  <- vector("list", n_plots * n_radii)
  counter  <- 0L

  if (save_clips) dir.create(clip_dir, showWarnings = FALSE, recursive = TRUE)

  message("Processing ", n_plots, " plots x ", n_radii, " radii...")

  # --- main loop ------------------------------------------------------------
  for (i in seq_len(n_plots)) {

    pid <- plots_sf[[plot_id]][i]
    center <- plots_sf[i, ]

    for (r_idx in seq_along(all_radii)) {

      rad <- all_radii[r_idx]
      counter <- counter + 1L

      message(
        "  [", counter, "/", n_plots * n_radii, "] ",
        pid, " @ ", rad, "m"
      )

      # clip CHM to circular plot
      chm_clip <- tryCatch(
        .clip_to_plot(chm, center, rad),
        error = function(e) {
          warning("Clip failed for ", pid, ": ", conditionMessage(e),
                  call. = FALSE)
          NULL
        }
      )

      if (is.null(chm_clip)) {
        results[[counter]] <- data.frame(
          plot_id      = pid,
          radius_m     = rad,
          plot_area_m2 = pi * rad^2,
          threshold    = NA_real_,
          n_gaps       = NA_integer_,
          total_gap_m2 = NA_real_,
          mean_gap_m2  = NA_real_,
          max_gap_m2   = NA_real_,
          mean_shape   = NA_real_,
          edge_density = NA_real_,
          mean_frac    = NA_real_,
          gap_fraction = NA_real_,
          stringsAsFactors = FALSE
        )
        next
      }

      # classify gaps
      if (threshold_method == "otsu") {
        vals <- terra::values(chm_clip, mat = FALSE)
        vals <- vals[!is.na(vals)]
        if (length(vals) < 10) {
          thresh <- gap_threshold  # fallback
        } else {
          thresh <- .otsu_threshold(vals)
        }
      } else {
        thresh <- gap_threshold
      }

      binary_clip <- terra::ifel(chm_clip <= thresh, 1L, 0L)

      # save clip if requested
      if (save_clips) {
        clip_path <- file.path(
          clip_dir,
          paste0(pid, "_r", rad, "m_gap.tif")
        )
        terra::writeRaster(binary_clip, clip_path, overwrite = TRUE)
      }

      # compute metrics
      mets <- .safe_gap_metrics(binary_clip, min_area = min_gap_area)

      results[[counter]] <- data.frame(
        plot_id      = pid,
        radius_m     = rad,
        plot_area_m2 = pi * rad^2,
        threshold    = thresh,
        n_gaps       = mets$n_gaps,
        total_gap_m2 = mets$total_gap_m2,
        mean_gap_m2  = mets$mean_gap_m2,
        max_gap_m2   = mets$max_gap_m2,
        mean_shape   = mets$mean_shape,
        edge_density = mets$edge_density,
        mean_frac    = mets$mean_frac,
        gap_fraction = mets$gap_fraction,
        stringsAsFactors = FALSE
      )
    }
  }

  # --- combine and return ---------------------------------------------------
  output <- do.call(rbind, results)
  rownames(output) <- NULL

  message(
    "Done. ", sum(!is.na(output$n_gaps)), " of ",
    nrow(output), " plot-radius combinations processed."
  )

  output
}

