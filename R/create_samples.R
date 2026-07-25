#' Create Spatial Samples from Feature Geometries
#'
#' Draws point samples within each geometry of an \code{sf} object. The number
#' of points per feature is either fixed (\code{size}) or scaled to feature area
#' (the square root of the area column, rounded up). Selected attribute columns
#' from each source feature are carried onto the points it generates. Sampling
#' can optionally run in parallel via the \pkg{future} framework.
#'
#' @param feature An \code{sf} object containing the geometries to sample.
#' @param area_col Character scalar. Name of the column holding feature area,
#'   used to scale the per-feature sample size when \code{size} is \code{NULL}.
#' @param type Character scalar. Sampling type passed to
#'   \code{\link[sf]{st_sample}} (e.g. \code{"regular"}, \code{"random"},
#'   \code{"hexagonal"}).
#' @param size Integer scalar or \code{NULL}. Fixed number of points per
#'   feature. When \code{NULL} (default), the count is
#'   \code{ceiling(sqrt(area))}.
#' @param attr_cols Character vector or \code{NULL}. Columns of \code{feature}
#'   to copy onto each sampled point (e.g. an identifier or date column). When
#'   \code{NULL} (default), no attributes are carried over.
#' @param n_cores Integer scalar. Number of worker processes to use when
#'   \code{parallel = TRUE}. Default \code{1}.
#' @param parallel Logical. If \code{TRUE}, sample features in parallel using
#'   \pkg{future} and \pkg{future.apply} (falls back to sequential evaluation
#'   with a warning if those packages are not installed). Default \code{FALSE}.
#'
#' @return An \code{sf} object of sampled points with a \code{"geometry"} column
#'   and any requested \code{attr_cols}.
#'
#' @examples
#' # two square polygons with an area column and attributes to carry over
#' sq <- function(x0, y0, s) {
#'   sf::st_polygon(list(rbind(
#'     c(x0, y0), c(x0 + s, y0), c(x0 + s, y0 + s),
#'     c(x0, y0 + s), c(x0, y0))))
#' }
#' polygons <- sf::st_sf(
#'   event_id   = c("A", "B"),
#'   event_date = as.Date(c("2020-06-01", "2020-06-02")),
#'   area_ha    = c(100, 400),
#'   geometry   = sf::st_sfc(sq(0, 0, 10), sq(20, 0, 20))
#' )
#'
#' pts <- create_samples(
#'   feature   = polygons,
#'   area_col  = "area_ha",
#'   type      = "random",
#'   attr_cols = c("event_id", "event_date")
#' )
#' head(pts)
#'
#' @importFrom sf st_is_empty st_is_valid st_sample st_sf st_as_sf
#' @export
create_samples <- function(feature,
                           area_col,
                           type = "regular",
                           size = NULL,
                           attr_cols = NULL,
                           n_cores = 1L,
                           parallel = FALSE) {

  # Input validation -----------------------------------------------------------
  if (!inherits(feature, "sf")) {
    stop("'feature' must be an sf object.", call. = FALSE)
  }
  if (!is.character(area_col) || length(area_col) != 1L ||
      !area_col %in% names(feature)) {
    stop(sprintf("Column '%s' not found in 'feature'.", area_col),
         call. = FALSE)
  }
  if (!is.null(attr_cols)) {
    missing_cols <- setdiff(attr_cols, names(feature))
    if (length(missing_cols) > 0) {
      stop(sprintf("Column(s) not found in 'feature': %s",
                   paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
  }

  # Keep only valid, non-empty geometries with positive area -------------------
  keep <- !sf::st_is_empty(feature) &
    !is.na(sf::st_is_valid(feature)) &
    !is.na(feature[[area_col]]) &
    feature[[area_col]] > 0
  feature <- feature[keep, , drop = FALSE]
  if (nrow(feature) == 0) {
    stop("No valid geometries with positive area to sample.", call. = FALSE)
  }

  # Sample a single feature and attach the requested attributes ----------------
  sample_one <- function(i) {
    n_samples <- if (is.null(size)) {
      as.integer(ceiling(sqrt(feature[[area_col]][i])))
    } else {
      as.integer(size)
    }
    geom <- sf::st_sample(feature[i, ], size = n_samples, type = type)
    if (length(geom) == 0) {
      return(NULL)
    }
    pts <- sf::st_sf(geometry = geom)
    for (cc in attr_cols) {
      pts[[cc]] <- feature[[cc]][i]
    }
    pts
  }

  # Evaluate in parallel when requested and available --------------------------
  use_parallel <- isTRUE(parallel) &&
    requireNamespace("future", quietly = TRUE) &&
    requireNamespace("future.apply", quietly = TRUE)

  if (use_parallel) {
    future::plan("multisession", workers = n_cores)
    on.exit(future::plan("sequential"), add = TRUE)
    sample_list <- future.apply::future_lapply(
      seq_len(nrow(feature)), sample_one, future.seed = TRUE
    )
  } else {
    if (isTRUE(parallel)) {
      warning("'future'/'future.apply' not installed; sampling sequentially.",
              call. = FALSE)
    }
    sample_list <- lapply(seq_len(nrow(feature)), sample_one)
  }

  # Combine results ------------------------------------------------------------
  sample_list <- sample_list[!vapply(sample_list, is.null, logical(1))]
  if (length(sample_list) == 0) {
    stop("Sampling produced no points.", call. = FALSE)
  }
  sf::st_as_sf(do.call(rbind, sample_list))
}
