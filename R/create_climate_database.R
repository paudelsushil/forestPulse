# =============================================================================
# Create an iLand climate database
#
# Pipeline: compute climate variables -> assemble a daily climate table from a
# gridMET extraction -> write it to a per-cluster SQLite database that iLand
# reads via system.database.climate / model.climate.tableName.
#
# Conventions: roxygen2 docs, no library() calls (use pkg::fun()), no hardcoded
# band names / units / column names (all are arguments), informative errors,
# and vectorised maths for large daily series.
#
# Contents:
#   1. Climate-variable calculators
#      1.1 calc_saturation_vapor_pressure()  saturation vapour pressure
#      1.2 calc_vpd()                         vapour pressure deficit
#      1.3 convert_radiation()                shortwave radiation unit conversion
#   2. gridMET preprocessor
#      gridmet_preprocessing()                gridMET daily table -> iLand table
#   3. Climate-database writer
#      write_iland_climate()                  validated tables -> SQLite
#   4. Internal helpers
# =============================================================================


# =============================================================================
# 1. Climate-variable calculators
# =============================================================================

# -----------------------------------------------------------------------------
# 1.1 Saturation vapour pressure
# -----------------------------------------------------------------------------

#' Saturation vapour pressure
#'
#' Computes saturation vapour pressure (over water) from air temperature.
#'
#' @param temp_c Numeric vector of air temperature (degrees Celsius).
#' @param method One of `"FAO56"` (Tetens form as used in FAO-56) or `"Murray"`.
#'
#' @details
#' `"FAO56"`:  \eqn{e^0(T) = 0.6108 \exp[17.27 T / (T + 237.3)]}  (kPa).
#' `"Murray"`: \eqn{e_s(T) = 0.61078 \exp[17.2693882 T / (T + 237.3)]} (kPa).
#'
#' @return Numeric vector of saturation vapour pressure (kPa).
#' @references
#' Tetens, O. (1930). Über einige meteorologische Begriffe.
#'   Zeitschrift für Geophysik, 6, 297-309.
#' Allen, R.G., Pereira, L.S., Raes, D., Smith, M. (1998). Crop
#'   evapotranspiration: Guidelines for computing crop water requirements.
#'   FAO Irrigation and Drainage Paper 56, Eq. 11. FAO, Rome.
#' Murray, F.W. (1967). On the computation of saturation vapor pressure.
#'   Journal of Applied Meteorology, 6(1), 203-204.
#' @export
calc_saturation_vapor_pressure <- function(temp_c, method = c("FAO56", "Murray")) {
  method <- match.arg(method)
  if (!is.numeric(temp_c))
    stop("`temp_c` must be numeric (degrees Celsius).", call. = FALSE)
  coef <- switch(method,
                 FAO56  = c(a = 0.6108,  b = 17.27,       d = 237.3),
                 Murray = c(a = 0.61078, b = 17.2693882,  d = 237.3))
  coef[["a"]] * exp(coef[["b"]] * temp_c / (temp_c + coef[["d"]]))
}


# -----------------------------------------------------------------------------
# 1.2 Vapour pressure deficit
# -----------------------------------------------------------------------------

#' Daily vapour pressure deficit from temperature and relative humidity
#'
#' Computes mean daily vapour pressure deficit (VPD) following the FAO-56 dual
#' relative-humidity approach.
#'
#' @param tmin_c,tmax_c Numeric vectors: daily minimum / maximum temperature (deg C).
#' @param rh_min,rh_max Numeric vectors: daily minimum / maximum relative humidity (%).
#' @param svp_method Saturation-vapour-pressure method passed to
#'   [calc_saturation_vapor_pressure()] (`"FAO56"` or `"Murray"`).
#' @param clamp_nonneg Logical; clamp negative VPD to zero (default `TRUE`).
#'
#' @details
#' Mean saturation vapour pressure (FAO-56 Eq. 12):
#'   \eqn{e_s = [e^0(T_{max}) + e^0(T_{min})] / 2}.
#' Actual vapour pressure from RHmin/RHmax (FAO-56 Eq. 17):
#'   \eqn{e_a = [e^0(T_{min}) RH_{max}/100 + e^0(T_{max}) RH_{min}/100] / 2}.
#' Vapour pressure deficit: \eqn{VPD = e_s - e_a} (kPa).
#'
#' @return Numeric vector of VPD (kPa).
#' @references
#' Allen, R.G., Pereira, L.S., Raes, D., Smith, M. (1998). Crop
#'   evapotranspiration: Guidelines for computing crop water requirements.
#'   FAO Irrigation and Drainage Paper 56, Eqs. 12 and 17. FAO, Rome.
#' @export
calc_vpd <- function(tmin_c, tmax_c, rh_min, rh_max,
                     svp_method = c("FAO56", "Murray"),
                     clamp_nonneg = TRUE) {
  svp_method <- match.arg(svp_method)
  n <- length(tmin_c)
  if (!all(lengths(list(tmax_c, rh_min, rh_max)) == n))
    stop("`tmin_c`, `tmax_c`, `rh_min`, `rh_max` must have equal length.", call. = FALSE)

  es_min <- calc_saturation_vapor_pressure(tmin_c, svp_method)
  es_max <- calc_saturation_vapor_pressure(tmax_c, svp_method)
  es <- (es_max + es_min) / 2                                   # FAO-56 Eq. 12
  ea <- (es_min * (rh_max / 100) + es_max * (rh_min / 100)) / 2 # FAO-56 Eq. 17
  vpd <- es - ea
  if (clamp_nonneg) vpd[vpd < 0] <- 0
  vpd
}


# -----------------------------------------------------------------------------
# 1.3 Radiation unit conversion
# -----------------------------------------------------------------------------

#' Convert shortwave radiation units
#'
#' @param x Numeric vector of radiation.
#' @param from,to Units. Supported: `"W/m2"` (daily-mean flux) and
#'   `"MJ/m2/day"` (daily total). Defaults convert gridMET `srad` to the
#'   daily total iLand expects.
#'
#' @details
#' Uses the FAO-56 conversion \eqn{1\,W\,m^{-2} = 0.0864\,MJ\,m^{-2}\,day^{-1}}
#' (i.e. 86400 s/day divided by 1e6 J/MJ).
#'
#' @return Numeric vector in the target unit.
#' @references
#' Allen, R.G., Pereira, L.S., Raes, D., Smith, M. (1998). Crop
#'   evapotranspiration: Guidelines for computing crop water requirements.
#'   FAO Irrigation and Drainage Paper 56, conversion factors (Chapter 3). FAO, Rome.
#' @export
convert_radiation <- function(x, from = "W/m2", to = "MJ/m2/day") {
  if (!is.numeric(x)) stop("`x` must be numeric.", call. = FALSE)
  key <- paste(from, to, sep = "->")
  fac <- switch(key,
                "W/m2->MJ/m2/day" = 0.0864,
                "MJ/m2/day->W/m2" = 1 / 0.0864,
                "W/m2->W/m2"       = 1,
                "MJ/m2/day->MJ/m2/day" = 1,
                stop(sprintf("Unsupported conversion '%s'.", key), call. = FALSE))
  x * fac
}


# =============================================================================
# 2. gridMET preprocessor
# =============================================================================

#' Preprocess gridMET daily data into an iLand climate table
#'
#' Converts an extracted gridMET daily table (one row per cluster-day) into the
#' harmonized `data.table` consumed by [write_iland_climate()]: columns
#' `year, month, day, min_temp, max_temp, prec, rad, vpd` (+ optional `co2`),
#' alongside the cluster column.
#'
#' @param data A `data.frame`/`data.table` of daily gridMET values already
#'   extracted to climate clusters (e.g. via `terra`/`climateR`).
#' @param cluster_col Name of the column identifying the climate cluster/table.
#' @param date_col Name of a `Date` column (used to derive year/month/day).
#' @param bands Named list mapping gridMET variables to columns in `data`.
#'   Recognised names: `tmmn`, `tmmx`, `pr`, `srad`, `rmin`, `rmax`, `vpd`.
#'   gridMET native names are the defaults.
#' @param temp_unit Unit of `tmmn`/`tmmx` in `data`: `"K"` (converted to C) or `"C"`.
#' @param rad_unit Unit of `srad` in `data` (passed to [convert_radiation()]).
#' @param vpd_source `"gridmet"` to pass through the native `vpd` band, or
#'   `"compute"` to derive VPD from temperature and RH via [calc_vpd()].
#' @param svp_method Saturation-vapour-pressure method when computing VPD.
#' @param co2 Optional CO2: `NULL`, a single ppm value, or a `data.frame(year, co2)`.
#'
#' @return A `data.table` ready for `write_iland_climate(split_col = cluster_col)`.
#' @references
#' Abatzoglou, J.T. (2013). Development of gridded surface meteorological data
#'   for ecological applications and modelling. International Journal of
#'   Climatology, 33(1), 121-131. (gridMET / METDATA)
#' @examples
#' # small synthetic gridMET daily extraction (two clusters, three days)
#' days <- as.Date("2000-01-01") + 0:2
#' gridmet_daily <- data.frame(
#'   cluster = rep(c("c1", "c2"), each = 3),
#'   date    = rep(days, 2),
#'   tmmn    = c(272, 273, 274, 271, 272, 273),   # Kelvin
#'   tmmx    = c(282, 283, 284, 281, 282, 283),
#'   pr      = c(0, 2, 5, 1, 0, 3),
#'   srad    = c(120, 140, 160, 110, 130, 150),
#'   rmin    = c(35, 40, 45, 30, 35, 40),
#'   rmax    = c(90, 92, 95, 88, 90, 93)
#' )
#'
#' clim <- gridmet_preprocessing(
#'   data        = gridmet_daily,
#'   cluster_col = "cluster",
#'   date_col    = "date",
#'   temp_unit   = "K",
#'   vpd_source  = "compute")
#' head(clim)
#' @importFrom data.table as.data.table data.table set setnames setorderv
#' @export
gridmet_preprocessing <- function(data,
                                  cluster_col,
                                  date_col,
                                  bands = list(tmmn = "tmmn", tmmx = "tmmx",
                                               pr = "pr", srad = "srad",
                                               rmin = "rmin", rmax = "rmax",
                                               vpd = "vpd"),
                                  temp_unit = c("K", "C"),
                                  rad_unit = "W/m2",
                                  vpd_source = c("gridmet", "compute"),
                                  svp_method = c("FAO56", "Murray"),
                                  co2 = NULL) {
  temp_unit  <- match.arg(temp_unit)
  vpd_source <- match.arg(vpd_source)
  svp_method <- match.arg(svp_method)

  dt <- data.table::as.data.table(data)

  # ---- validate presence of the columns we will touch ----
  need_bands <- c("tmmn", "tmmx", "pr", "srad")
  if (vpd_source == "gridmet") need_bands <- c(need_bands, "vpd")
  if (vpd_source == "compute") need_bands <- c(need_bands, "rmin", "rmax")
  need_cols <- c(cluster_col, date_col, unlist(bands[need_bands]))
  missing <- setdiff(need_cols, names(dt))
  if (length(missing))
    stop("`data` is missing required column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  if (!inherits(dt[[date_col]], "Date"))
    stop(sprintf("`%s` must be a Date column.", date_col), call. = FALSE)

  # ---- pull vectors (avoids non-standard-evaluation fragility) ----
  g <- function(b) dt[[bands[[b]]]]

  tmin <- if (temp_unit == "K") g("tmmn") - 273.15 else g("tmmn")
  tmax <- if (temp_unit == "K") g("tmmx") - 273.15 else g("tmmx")

  vpd <- if (vpd_source == "gridmet") g("vpd")
         else calc_vpd(tmin, tmax, g("rmin"), g("rmax"), svp_method = svp_method)

  d <- dt[[date_col]]
  out <- data.table::data.table(
    cluster  = dt[[cluster_col]],
    year     = data.table::year(d),
    month    = data.table::month(d),
    day      = data.table::mday(d),
    min_temp = tmin,
    max_temp = tmax,
    prec     = g("pr"),
    rad      = convert_radiation(g("srad"), from = rad_unit, to = "MJ/m2/day"),
    vpd      = vpd
  )
  data.table::setnames(out, "cluster", cluster_col)

  # optional CO2 column, added by reference (no NSE / no copy)
  co2v <- .resolve_co2(co2, out$year)
  if (!is.null(co2v)) data.table::set(out, j = "co2", value = co2v)

  data.table::setorderv(out, c(cluster_col, "year", "month", "day"))
  out[]
}


# =============================================================================
# 3. Climate-database writer
# =============================================================================

#' Write an iLand climate database
#'
#' Creates (or appends to) an SQLite climate database in the exact format
#' expected by the iLand forest landscape model. Each climate cluster is
#' stored as its own table; the table names are later referenced from the
#' iLand environment file (column \code{model.climate.tableName}) and the
#' database file is referenced from the project file
#' (\code{system.database.climate}).
#'
#' iLand uses daily climate data with the fixed columns \code{year},
#' \code{month}, \code{day}, \code{min_temp}, \code{max_temp}, \code{prec},
#' \code{rad} and \code{vpd}. Mean temperature is derived internally by iLand,
#' so no mean-temperature column is supplied. CO2 is \emph{not} part of the
#' climate table; it is set in the project file.
#'
#' Store separate scenarios (e.g. historical, RCP4.5, RCP8.5) in separate
#' database files by calling this function once per scenario with a different
#' \code{path}.
#'
#' @param data Either a single \code{data.frame} of daily climate, or a
#'   \emph{named} list of such data frames. When a named list is supplied,
#'   each element is written to a table named after the list element
#'   (i.e. one table per climate cluster).
#' @param path Character scalar. Path to the SQLite file to create or open
#'   (conventionally one file per climate scenario, e.g.
#'   \code{"database/historic.sqlite"}).
#' @param table_name Character scalar. Table name to use when \code{data} is a
#'   single data frame. Ignored when \code{data} is a named list or when
#'   \code{split_col} is supplied.
#' @param split_col Character scalar or \code{NULL}. When \code{data} is a single
#'   data frame, the name of a column to split it into one table per distinct
#'   value (the column is dropped from each table). Use this for a long table
#'   that stacks several climate clusters, e.g. the output of
#'   \code{\link{gridmet_preprocessing}}. Ignored for a named list. Default
#'   \code{NULL}.
#' @param overwrite Logical. If \code{TRUE}, existing tables of the same name
#'   are dropped and rewritten. If \code{FALSE} (default), writing to an
#'   existing table is an error.
#' @param check_ranges Logical. If \code{TRUE} (default), perform plausibility
#'   checks on units (e.g. temperature in degrees Celsius, radiation in
#'   MJ/m2/day) and warn on suspicious values.
#'
#' @return Invisibly, a character vector of the table names written.
#'
#' @details
#' The required columns and units are:
#' \describe{
#'   \item{year}{Absolute calendar year (integer), e.g. 2009.}
#'   \item{month}{Month of year, 1 (January) to 12 (December).}
#'   \item{day}{Day of month, 1 to 31. February must have 29 days in leap years.}
#'   \item{min_temp}{Daily minimum temperature, degrees Celsius.}
#'   \item{max_temp}{Daily maximum temperature, degrees Celsius.}
#'   \item{prec}{Daily precipitation sum, mm.}
#'   \item{rad}{Daily global radiation sum, MJ/m2/day.}
#'   \item{vpd}{Mean daily vapour pressure deficit, kPa.}
#' }
#' Rows are written in the order supplied, so the data frame should already be
#' sorted chronologically within each table. All tables are written inside a
#' single database transaction.
#'
#' @examples
#' # RSQLite (Suggests) is needed to write the database
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   clim_df <- data.frame(
#'     year = 2001L, month = rep(1:2, each = 2), day = 1:2,
#'     min_temp = c(-2, -1, 0, 1), max_temp = c(4, 5, 6, 7),
#'     prec = c(0, 3, 1, 2), rad = c(7, 8, 9, 10),
#'     vpd = c(0.2, 0.3, 0.4, 0.5))
#'
#'   # write to a temporary database (never the user's home/working directory)
#'   db <- tempfile(fileext = ".sqlite")
#'   write_iland_climate(clim_df, db, table_name = "climate1")
#'   file.remove(db)
#' }
#'
#' @importFrom DBI dbConnect dbDisconnect dbExistsTable dbRemoveTable dbWriteTable
#'   dbBegin dbCommit dbRollback
#' @export
write_iland_climate <- function(data,
                                path,
                                table_name = "climate1",
                                split_col = NULL,
                                overwrite = FALSE,
                                check_ranges = TRUE) {

  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("Package 'RSQLite' is required. Install it with install.packages('RSQLite').",
         call. = FALSE)
  }
  if (!is.character(path) || length(path) != 1L) {
    stop("'path' must be a single character string.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L) {
    stop("'overwrite' must be a single logical value.", call. = FALSE)
  }

  # Normalise input to a named list of data frames -------------------------
  if (is.data.frame(data) && !is.null(split_col)) {
    if (!is.character(split_col) || length(split_col) != 1L ||
        !split_col %in% names(data)) {
      stop("'split_col' must name a single column in 'data'.", call. = FALSE)
    }
    data <- as.data.frame(data)   # normalise data.table/tibble for base split()
    keys <- as.character(data[[split_col]])
    data[[split_col]] <- NULL
    tables <- split(data, keys)
  } else if (is.data.frame(data)) {
    if (!is.character(table_name) || length(table_name) != 1L) {
      stop("'table_name' must be a single character string.", call. = FALSE)
    }
    tables <- stats::setNames(list(data), table_name)
  } else if (is.list(data)) {
    nm <- names(data)
    if (is.null(nm) || any(!nzchar(nm)) || anyDuplicated(nm)) {
      stop("When 'data' is a list it must have unique, non-empty names ",
           "(used as table names).", call. = FALSE)
    }
    if (!all(vapply(data, is.data.frame, logical(1)))) {
      stop("All elements of 'data' must be data frames.", call. = FALSE)
    }
    tables <- data
  } else {
    stop("'data' must be a data frame or a named list of data frames.",
         call. = FALSE)
  }

  # Validate every table before opening the connection --------------------
  required_cols <- c("year", "month", "day", "min_temp", "max_temp",
                     "prec", "rad", "vpd")
  for (tn in names(tables)) {
    tables[[tn]] <- .validate_iland_climate(tables[[tn]], required_cols,
                                            tn, check_ranges)
  }

  # Write all tables in a single transaction (one fsync, not one per table) --
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  committed <- FALSE
  on.exit({
    if (!committed) try(DBI::dbRollback(con), silent = TRUE)
    DBI::dbDisconnect(con)
  }, add = TRUE)

  DBI::dbBegin(con)
  for (tn in names(tables)) {
    if (DBI::dbExistsTable(con, tn)) {
      if (!overwrite) {
        stop(sprintf("Table '%s' already exists in '%s'. ", tn, path),
             "Use overwrite = TRUE to replace it.", call. = FALSE)
      }
      DBI::dbRemoveTable(con, tn)
    }
    DBI::dbWriteTable(con, tn, tables[[tn]], row.names = FALSE)
  }
  DBI::dbCommit(con)
  committed <- TRUE

  invisible(names(tables))
}


# =============================================================================
# 4. Internal helpers
# =============================================================================

#' Internal: resolve a CO2 series to a per-row vector
#' @keywords internal
#' @noRd
.resolve_co2 <- function(co2, years) {
  if (is.null(co2)) return(NULL)
  if (is.numeric(co2) && length(co2) == 1L) return(rep(co2, length(years)))
  if (inherits(co2, "data.frame")) {
    if (!all(c("year", "co2") %in% names(co2)))
      stop("`co2` data.frame must have columns `year` and `co2`.", call. = FALSE)
    return(co2$co2[match(years, co2$year)])
  }
  stop("`co2` must be NULL, a single number, or a data.frame(year, co2).", call. = FALSE)
}


#' Internal: validate and coerce one iLand climate table
#' @keywords internal
#' @noRd
.validate_iland_climate <- function(df, required_cols, tn, check_ranges) {

  df <- as.data.frame(df)   # robust column subsetting for data.table/tibble input
  missing <- setdiff(required_cols, names(df))
  if (length(missing)) {
    stop(sprintf("Table '%s' is missing required column(s): %s.",
                 tn, paste(missing, collapse = ", ")), call. = FALSE)
  }

  # keep only the required columns, in the canonical order
  df <- df[, required_cols, drop = FALSE]

  # type coercion
  df$year  <- as.integer(df$year)
  df$month <- as.integer(df$month)
  df$day   <- as.integer(df$day)
  for (cc in c("min_temp", "max_temp", "prec", "rad", "vpd")) {
    df[[cc]] <- as.numeric(df[[cc]])
  }

  if (anyNA(df)) {
    stop(sprintf("Table '%s' contains NA values; iLand expects complete daily series.",
                 tn), call. = FALSE)
  }

  # structural checks
  if (any(df$month < 1L | df$month > 12L)) {
    stop(sprintf("Table '%s': 'month' must be in 1..12.", tn), call. = FALSE)
  }
  if (any(df$day < 1L | df$day > 31L)) {
    stop(sprintf("Table '%s': 'day' must be in 1..31.", tn), call. = FALSE)
  }
  if (any(df$max_temp < df$min_temp)) {
    stop(sprintf("Table '%s': 'max_temp' is below 'min_temp' on some days.",
                 tn), call. = FALSE)
  }

  # leap-year sanity check on Feb 29
  feb29 <- df$month == 2L & df$day == 29L
  if (any(feb29) && any(!.is_leap(df$year[feb29]))) {
    warning(sprintf("Table '%s': Feb 29 present in non-leap year(s).", tn),
            call. = FALSE)
  }

  # plausibility checks on units (warn only)
  if (isTRUE(check_ranges)) {
    if (any(df$prec < 0)) {
      warning(sprintf("Table '%s': negative precipitation found.", tn),
              call. = FALSE)
    }
    if (any(df$rad < 0) || max(df$rad) > 45) {
      warning(sprintf("Table '%s': 'rad' outside ~0..45 MJ/m2/day; check units.",
                      tn), call. = FALSE)
    }
    if (any(df$vpd < 0) || max(df$vpd) > 10) {
      warning(sprintf("Table '%s': 'vpd' outside ~0..10 kPa; check units.",
                      tn), call. = FALSE)
    }
    if (min(df$min_temp) < -70 || max(df$max_temp) > 60) {
      warning(sprintf("Table '%s': temperatures outside plausible Celsius range.",
                      tn), call. = FALSE)
    }
  }

  df
}


#' Internal: vectorised leap-year test
#' @keywords internal
#' @noRd
.is_leap <- function(y) {
  (y %% 4L == 0L & y %% 100L != 0L) | (y %% 400L == 0L)
}
