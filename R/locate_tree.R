#' Calculate Tree Coordinates from a Known Point, Azimuth, and Distance
#'
#' @description
#' Computes the coordinates of a target point (e.g., a tree) given the
#' coordinates of a known reference point, a compass azimuth (bearing from
#' north), and a horizontal distance. Supports both projected (e.g., UTM)
#' and geographic (lat/lon) coordinate systems.
#'
#' @param x Numeric. Easting or Longitude of the known point.
#' @param y Numeric. Northing or Latitude of the known point.
#' @param azimuth Numeric. Compass bearing from the known point to the tree,
#'   in degrees clockwise from north (0-360).
#' @param distance Numeric. Horizontal distance from the known point to the
#'   tree, in the same units as the coordinates (metres for UTM).
#' @param crs Character or NULL. Coordinate reference system string
#'   (e.g., "EPSG:32610" for UTM 10N). If NULL (default), assumes projected
#'   (planar) coordinates and uses simple trigonometry.
#' @param id Optional. An identifier for each point (e.g., tree tag number).
#'
#' @return A \code{data.frame} with columns: id, x_origin, y_origin, azimuth,
#'   distance, x_tree, y_tree. If \code{crs} is provided, also returns an
#'   \code{sf} object attribute.
#'
#' @details
#' For projected CRS (UTM, State Plane, etc.), the function uses planar
#' trigonometry:
#' \deqn{x_{tree} = x + d \cdot \sin(\theta)}
#' \deqn{y_{tree} = y + d \cdot \cos(\theta)}
#'
#' where \eqn{\theta} is the azimuth converted to radians.
#'
#' For geographic CRS (lat/lon), the function uses the Vincenty direct
#' formula via \code{geosphere::destPoint()} for accurate results over
#' the ellipsoid.
#'
#' @examples
#' # Single tree from a plot center (UTM coordinates)
#' locate_tree(x = 755250, y = 4214550, azimuth = 135, distance = 12.5)
#'
#' # Multiple trees from the same plot center
#' locate_tree(
#'   x        = rep(755250, 4),
#'   y        = rep(4214550, 4),
#'   azimuth  = c(45, 120, 210, 330),
#'   distance = c(8.2, 11.5, 6.7, 14.0),
#'   id       = c("T01", "T02", "T03", "T04")
#' )
#'
#' @export
locate_tree <- function(x,
                        y,
                        azimuth,
                        distance,
                        crs = NULL,
                        id  = NULL) {

  # ── Input validation ──────────────────────────────────────────────────────
  n <- length(x)
  if (!all(length(y) == n, length(azimuth) == n, length(distance) == n)) {
    stop("x, y, azimuth, and distance must all have the same length.")
  }

  if (any(azimuth < 0 | azimuth > 360, na.rm = TRUE)) {
    stop("Azimuth values must be between 0 and 360 degrees.")
  }

  if (any(distance < 0, na.rm = TRUE)) {
    stop("Distance values must be non-negative.")
  }

  if (is.null(id)) id <- seq_len(n)

  # ── Determine if geographic or projected ───────────────────────────────────
  is_geographic <- FALSE

  if (!is.null(crs)) {
    if (requireNamespace("terra", quietly = TRUE)) {
      crs_obj <- terra::crs(crs)
      is_geographic <- terra::is.lonlat(terra::rast(crs = crs_obj))
    }
  }

  # ── Compute new coordinates ────────────────────────────────────────────────
  if (is_geographic) {
    # Use geosphere for accurate ellipsoidal calculation
    if (!requireNamespace("geosphere", quietly = TRUE)) {
      stop("Package 'geosphere' is required for geographic CRS.\n",
          "Please install it and retry.")
    }

    dest <- geosphere::destPoint(
      p = cbind(x, y),       # lon, lat
      b = azimuth,
      d = distance
    )

    x_tree <- dest[, 1]
    y_tree <- dest[, 2]

  } else {
    # Planar trigonometry (projected CRS like UTM)
    azimuth_rad <- azimuth * pi / 180

    x_tree <- x + distance * sin(azimuth_rad)
    y_tree <- y + distance * cos(azimuth_rad)
  }

  # ── Build result data.frame ────────────────────────────────────────────────
  result <- data.frame(
    id       = id,
    x_origin = x,
    y_origin = y,
    azimuth  = azimuth,
    distance = distance,
    x_tree   = round(x_tree, 6),
    y_tree   = round(y_tree, 6),
    stringsAsFactors = FALSE
  )

  # ── Optionally return an sf object ─────────────────────────────────────────
  if (!is.null(crs) && requireNamespace("sf", quietly = TRUE)) {
    result_sf <- sf::st_as_sf(result,
                              coords = c("x_tree", "y_tree"),
                              crs    = crs,
                              remove = FALSE)
    attr(result, "sf") <- result_sf
    message(sprintf("%d tree location(s) computed. Access sf object with attr(result, 'sf').", n))
  } else {
    message(sprintf("%d tree location(s) computed.", n))
  }

  return(result)
}
