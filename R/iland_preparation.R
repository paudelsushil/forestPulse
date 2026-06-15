# =============================================================================
# Build iLand input files (stand grid, environment grid/file, init, climate)
# from GIS layers and inventory data.
#
# Every code, resolution, column name, delimiter, unit and path is a function
# argument with a sensible, overridable default; nothing is hardcoded.
#
# Pipeline (each step is usable standalone; build_iland_landscape() chains them):
#   create_stand_grid()        -> stand grid (ESRI-ASCII)
#   create_environment_grid()  -> resource-unit grid
#   create_environment_file()  -> per-resource-unit soil/climate table
#   create_init_file()         -> standgrid-mode tree init file
#   write_iland_climate()      -> SQLite climate database (see create_climate_database.R)
#   validate_landscape()       -> cross-file consistency report
# =============================================================================

# data.table powers the fast binning/aggregation in create_init_file(); declare
# the symbols its non-standard evaluation uses so R CMD check does not flag them
# as undefined globals.
.datatable.aware <- TRUE
utils::globalVariables(c(
  ".", ":=", "bin_lo", "count", "species", "dbh_from", "dbh_to", "hd",
  "stand_id", "n_ha", "age", "density"
))


# ---------------------------------------------------------------------
# Internal validation helpers (not exported)
# ---------------------------------------------------------------------

#' Assert that required columns are present
#' @keywords internal
#' @noRd
.require_cols <- function(x, cols, arg = deparse(substitute(x))) {
  missing <- setdiff(cols, names(x))
  if (length(missing)) {
    stop(sprintf("`%s` is missing required column(s): %s",
                 arg, paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Assert object class
#' @keywords internal
#' @noRd
.require_class <- function(x, cls, arg = deparse(substitute(x))) {
  if (!inherits(x, cls)) {
    stop(sprintf("`%s` must be a <%s>, not <%s>.",
                 arg, paste(cls, collapse = "/"), paste(class(x), collapse = "/")),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Resolve a writer for ESRI-ASCII grids used by iLand
#' @keywords internal
#' @noRd
.write_grid <- function(r, filename, nodata, overwrite) {
  if (is.null(filename)) return(invisible(r))
  terra::writeRaster(r, filename, filetype = "AAIGrid",
                     datatype = "INT4S", NAflag = nodata,
                     overwrite = overwrite)
  invisible(r)
}


# ---------------------------------------------------------------------
# 1. Stand grid
# ---------------------------------------------------------------------

#' Create an iLand stand grid from stand polygons
#'
#' Rasterizes stand polygons to the iLand stand grid, encoding forested
#' stands (integer IDs > 0), forested-outside cells, and non-forested cells.
#'
#' @param polygons An `sf` polygon layer of stands.
#' @param id_field Name of the column holding integer stand IDs (> 0).
#' @param resolution Grid resolution in CRS units (default 10).
#' @param surrounding_forest Optional `sf` of forest outside the project area;
#'   these cells receive `forested_outside_code`.
#' @param outside_code Value for non-forested outside cells. Use `NA` to rely on
#'   the grid NODATA value (iLand treats NODATA as non-forested). Default `NA`.
#' @param forested_outside_code Value for forested-outside cells (default -2).
#' @param nodata NODATA flag written to the file (default -9999).
#' @param buffer Optional buffer (CRS units) added around the extent (default 0).
#' @param filename Optional output path (`.asc`/`.txt`). If `NULL`, nothing is written.
#' @param overwrite Overwrite an existing file (default `FALSE`).
#'
#' @return A `terra::SpatRaster` (returned invisibly when a file is written).
#' @examples
#' \dontrun{
#' stands <- sf::st_read("gis/stands.shp")
#' g <- create_stand_grid(stands, id_field = "stand_id",
#'                        filename = "gis/stand_grid.asc")
#' }
#' @export
create_stand_grid <- function(polygons, id_field,
                              resolution = 10,
                              surrounding_forest = NULL,
                              outside_code = NA_integer_,
                              forested_outside_code = -2L,
                              nodata = -9999L,
                              buffer = 0,
                              filename = NULL,
                              overwrite = FALSE) {
  .require_class(polygons, "sf")
  .require_cols(polygons, id_field)

  v   <- terra::vect(polygons)
  ext <- terra::ext(v)
  if (buffer > 0) ext <- terra::ext(ext[1] - buffer, ext[2] + buffer,
                                    ext[3] - buffer, ext[4] + buffer)

  template <- terra::rast(ext, resolution = resolution,
                          crs = terra::crs(v))

  r <- terra::rasterize(v, template, field = id_field, background = NA)

  if (!is.null(surrounding_forest)) {
    .require_class(surrounding_forest, "sf")
    sf_r <- terra::rasterize(terra::vect(surrounding_forest), template,
                             field = 1, background = NA)
    r[is.na(r) & !is.na(sf_r)] <- forested_outside_code
  }

  if (!is.na(outside_code)) r[is.na(r)] <- outside_code

  .write_grid(r, filename, nodata, overwrite)
  if (is.null(filename)) r else invisible(r)
}


# ---------------------------------------------------------------------
# 2. Environment (resource-unit) grid
# ---------------------------------------------------------------------

#' Create an iLand environment (resource-unit) grid
#'
#' Builds the 100 m resource-unit grid over the extent of a reference grid and
#' assigns sequential integer resource-unit IDs to stockable cells.
#'
#' @param reference A `terra::SpatRaster` (typically the stand grid) defining extent/CRS.
#' @param resolution Resource-unit resolution (default 100).
#' @param stockable_from Optional `terra::SpatRaster`; resource units overlapping
#'   valid (non-NODATA, > 0) cells of this layer get IDs, others are NODATA.
#'   Defaults to `reference`.
#' @param start_id First resource-unit ID (default 1).
#' @param nodata NODATA flag (default -9999).
#' @param filename Optional output path.
#' @param overwrite Overwrite existing file.
#'
#' @return A `terra::SpatRaster` of resource-unit IDs.
#' @export
create_environment_grid <- function(reference,
                                     resolution = 100,
                                     stockable_from = NULL,
                                     start_id = 1L,
                                     nodata = -9999L,
                                     filename = NULL,
                                     overwrite = FALSE) {
  .require_class(reference, "SpatRaster")
  stockable_from <- stockable_from %||% reference

  ru <- terra::rast(terra::ext(reference), resolution = resolution,
                    crs = terra::crs(reference))

  # mark resource units that overlap stockable area
  mask <- terra::resample(stockable_from, ru, method = "max")
  ids  <- terra::values(mask)
  keep <- which(!is.na(ids) & ids > 0)

  out <- terra::setValues(ru, NA_integer_)
  terra::values(out)[keep] <- seq.int(start_id, length.out = length(keep))

  .write_grid(out, filename, nodata, overwrite)
  if (is.null(filename)) out else invisible(out)
}


# ---------------------------------------------------------------------
# 3. Environment file (soil + climate link per resource unit)
# ---------------------------------------------------------------------

#' Create an iLand environment file
#'
#' Builds the per-resource-unit environment table (soil texture, depth, available
#' nitrogen, climate-table link, and any extra keys such as carbon pools).
#'
#' @param ru_grid A `terra::SpatRaster` of resource-unit IDs (see [create_environment_grid()]).
#' @param soil Either a `data.frame` keyed by resource-unit `id`, or a named list of
#'   `terra::SpatRaster` soil layers to summarise per resource unit by zonal mean.
#' @param climate_table Either a single climate-table name applied to all units, a
#'   `data.frame` with columns `id` and `model.climate.tableName`, or a `function(id)`.
#' @param keys Named character vector mapping output column names to `soil` columns,
#'   e.g. `c(model.site.pctSand = "sand", model.site.soilDepth = "depth")`.
#'   Defaults to the standard iLand keys with identical names.
#' @param extra Optional `data.frame` keyed by `id` of additional columns (e.g. C pools).
#' @param sep Field separator for the output (default ",").
#' @param filename Optional output path.
#' @param overwrite Overwrite existing file.
#'
#' @return A `data.frame` of the environment table.
#' @export
create_environment_file <- function(ru_grid, soil, climate_table,
                                     keys = c(
                                       model.site.availableNitrogen = "availableNitrogen",
                                       model.site.soilDepth         = "soilDepth",
                                       model.site.pctSand           = "pctSand",
                                       model.site.pctSilt           = "pctSilt",
                                       model.site.pctClay           = "pctClay"),
                                     extra = NULL,
                                     sep = ",",
                                     filename = NULL,
                                     overwrite = FALSE) {
  .require_class(ru_grid, "SpatRaster")

  ids <- stats::na.omit(unique(terra::values(ru_grid)[, 1]))
  ids <- sort(ids[ids > 0])
  df  <- data.frame(id = ids)

  # ---- soil: data.frame join OR zonal summary of rasters ----
  if (inherits(soil, "data.frame")) {
    .require_cols(soil, "id")
    df <- merge(df, soil, by = "id", all.x = TRUE)
  } else if (is.list(soil)) {
    for (nm in names(soil)) {
      .require_class(soil[[nm]], "SpatRaster", arg = paste0("soil$", nm))
      z <- terra::zonal(soil[[nm]], ru_grid, fun = "mean", na.rm = TRUE)
      names(z) <- c("id", nm)
      df <- merge(df, z, by = "id", all.x = TRUE)
    }
  } else {
    stop("`soil` must be a data.frame or a named list of SpatRasters.", call. = FALSE)
  }

  # ---- rename soil columns to iLand keys ----
  for (out_key in names(keys)) {
    src <- keys[[out_key]]
    if (!is.null(df[[src]])) names(df)[names(df) == src] <- out_key
  }

  # ---- climate-table link ----
  df[["model.climate.tableName"]] <- .resolve_climate_table(climate_table, df$id)

  # ---- extra keys ----
  if (!is.null(extra)) {
    .require_cols(extra, "id")
    df <- merge(df, extra, by = "id", all.x = TRUE)
  }

  if (!is.null(filename)) {
    utils::write.table(df, filename, sep = sep, row.names = FALSE,
                       quote = FALSE, na = "",
                       fileEncoding = "UTF-8")
  }
  df
}

#' @keywords internal
#' @noRd
.resolve_climate_table <- function(climate_table, ids) {
  if (is.character(climate_table) && length(climate_table) == 1L) {
    rep(climate_table, length(ids))
  } else if (is.function(climate_table)) {
    vapply(ids, climate_table, character(1))
  } else if (inherits(climate_table, "data.frame")) {
    .require_cols(climate_table, c("id", "model.climate.tableName"))
    climate_table[["model.climate.tableName"]][match(ids, climate_table$id)]
  } else {
    stop("`climate_table` must be a length-1 string, a function, or a data.frame.",
         call. = FALSE)
  }
}


# ---------------------------------------------------------------------
# 4. Init file (vegetation, standgrid mode)
# ---------------------------------------------------------------------

#' Create an iLand initialization file (standgrid mode)
#'
#' Produces a validated standgrid-mode init file from either ready cohort rows or
#' raw tree records (optionally binned into DBH classes and unit-converted), and
#' optionally cross-checks `stand_id` against a stand grid.
#'
#' @param trees A `data.frame`/`data.table` of either cohorts (with
#'   `count, species, dbh_from, dbh_to, hd, stand_id`) or raw tree records.
#' @param cohorts Logical; if `FALSE`, `trees` is raw records to be binned. Default `TRUE`.
#' @param cols Named list mapping roles to column names in `trees` when `cohorts = FALSE`,
#'   e.g. `list(dbh = "DIA", height = "HT", count = "TPA", species = "code", stand = "stand_id")`.
#' @param bin_width DBH class width (CRS/metric units) when binning raw records (default 5).
#' @param units One of `"metric"` or `"imperial"`; if `"imperial"`, converts inches->cm,
#'   feet->m, trees/acre->trees/ha. Default `"metric"`.
#' @param hd_default Fallback height/diameter ratio when height is missing (default 80).
#' @param species_set Optional character vector of allowed iLand species codes;
#'   rows with other codes raise an error.
#' @param stand_grid Optional `terra::SpatRaster` stand grid OR an `sf`/list spec passed
#'   to [create_stand_grid()]; used to validate `stand_id`.
#' @param sep Output field separator (default " ").
#' @param filename Optional output path.
#' @param overwrite Overwrite existing file.
#'
#' @return A `data.frame` with columns
#'   `count, species, dbh_from, dbh_to, hd, age, density, stand_id`.
#' @export
create_init_file <- function(trees,
                             cohorts = TRUE,
                             cols = list(),
                             bin_width = 5,
                             units = c("metric", "imperial"),
                             hd_default = 80,
                             species_set = NULL,
                             stand_grid = NULL,
                             sep = " ",
                             filename = NULL,
                             overwrite = FALSE) {
  units <- match.arg(units)
  dt <- data.table::as.data.table(trees)

  if (!cohorts) {
    need <- c("dbh", "count", "species", "stand")
    miss <- setdiff(need, names(cols))
    if (length(miss))
      stop("With `cohorts = FALSE`, `cols` must name: ",
           paste(miss, collapse = ", "), call. = FALSE)

    f <- function(role) dt[[cols[[role]]]]
    conv <- if (units == "imperial") c(d = 2.54, h = 0.3048, n = 2.4710538)
            else c(d = 1, h = 1, n = 1)

    dbh_cm <- f("dbh") * conv[["d"]]
    ht_m   <- if (!is.null(cols$height)) f("height") * conv[["h"]] else NA_real_
    n_ha   <- f("count") * conv[["n"]]
    hd     <- ht_m / (dbh_cm / 100)
    hd[is.na(hd) | hd < 20 | hd > 200] <- hd_default

    work <- data.table::data.table(
      species  = as.character(f("species")),
      dbh_cm   = dbh_cm,
      n_ha     = n_ha,
      hd       = hd,
      stand_id = f("stand")
    )
    work[, bin_lo := floor(dbh_cm / bin_width) * bin_width]
    out <- work[, .(count = round(sum(n_ha), 2),
                    hd    = round(stats::weighted.mean(hd, n_ha), 1)),
                by = .(stand_id, species, dbh_from = bin_lo,
                       dbh_to = bin_lo + bin_width)]
  } else {
    .require_cols(dt, c("count", "species", "dbh_from", "dbh_to", "hd", "stand_id"))
    out <- dt[, .(count, species = as.character(species),
                  dbh_from, dbh_to, hd, stand_id)]
  }

  # age/density: carry from cohort input (rows align 1:1) or default for binned
  # raw records (aggregation changes row count, so dt's columns no longer map)
  if (cohorts) {
    out[, age     := if ("age" %in% names(dt)) dt$age else ""]
    out[, density := if ("density" %in% names(dt)) dt$density else 0]
  } else {
    out[, age := ""]
    out[, density := 0]
  }
  out <- out[count > 0]
  data.table::setcolorder(out, c("count","species","dbh_from","dbh_to",
                                 "hd","age","density","stand_id"))
  data.table::setorder(out, stand_id, species, dbh_from)

  if (!is.null(species_set)) {
    bad <- setdiff(unique(out$species), species_set)
    if (length(bad))
      stop("Species not in `species_set` (parameterize or remap): ",
           paste(bad, collapse = ", "), call. = FALSE)
  }

  if (!is.null(stand_grid)) {
    grid <- if (inherits(stand_grid, "SpatRaster")) stand_grid
            else do.call(create_stand_grid, stand_grid)
    rpt <- validate_landscape(init = out, stand_grid = grid)
    if (length(rpt$init_ids_missing_from_grid))
      stop("stand_id values absent from the stand grid: ",
           paste(rpt$init_ids_missing_from_grid, collapse = ", "), call. = FALSE)
  }

  out <- as.data.frame(out)
  if (!is.null(filename)) {
    dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
    utils::write.table(out, filename, sep = sep, row.names = FALSE,
                       quote = FALSE, na = "")
  }
  out
}


# ---------------------------------------------------------------------
# 5. Cross-file validation
# ---------------------------------------------------------------------

#' Validate consistency across iLand inputs
#'
#' @param init Optional init `data.frame` (must contain `stand_id`).
#' @param stand_grid Optional `terra::SpatRaster` stand grid.
#' @param env_file Optional environment `data.frame` (must contain `id`).
#' @param env_grid Optional `terra::SpatRaster` resource-unit grid.
#' @param species_set Optional allowed species codes.
#'
#' @return A list report: stands without trees, init IDs missing from grid,
#'   resource-unit ID mismatches, and unknown species.
#' @export
validate_landscape <- function(init = NULL, stand_grid = NULL,
                               env_file = NULL, env_grid = NULL,
                               species_set = NULL) {
  rpt <- list()

  if (!is.null(init) && !is.null(stand_grid)) {
    gids <- stats::na.omit(unique(terra::values(stand_grid)[, 1]))
    gids <- gids[gids > 0]
    iids <- unique(init$stand_id)
    rpt$stands_without_trees       <- sort(setdiff(gids, iids))
    rpt$init_ids_missing_from_grid <- sort(setdiff(iids, gids))
  }
  if (!is.null(env_file) && !is.null(env_grid)) {
    rids <- stats::na.omit(unique(terra::values(env_grid)[, 1]))
    rids <- rids[rids > 0]
    rpt$env_ids_mismatch <- sort(symdiff(env_file$id, rids))
  }
  if (!is.null(init) && !is.null(species_set)) {
    rpt$unknown_species <- setdiff(unique(init$species), species_set)
  }
  class(rpt) <- "iland_validation"
  rpt
}


# ---------------------------------------------------------------------
# 6. Optional orchestrator (batch build)  -- not required to use the package
# ---------------------------------------------------------------------

#' Build a full iLand landscape input set (optional convenience wrapper)
#'
#' Runs any subset of the file creators from a single configuration list. Using
#' the individual functions standalone is equally supported; this wrapper only
#' chains them and validates the result.
#'
#' @param config A named list. Recognised elements (all optional): `stand_grid`,
#'   `env_grid`, `env_file`, `init`, `climate` — each a list of arguments passed
#'   to the corresponding builder (`create_stand_grid()`,
#'   `create_environment_grid()`, `create_environment_file()`,
#'   `create_init_file()`, and `write_iland_climate()` for `climate`). Use
#'   `outdir` to set a common output folder; per-step `filename`/`path` still
#'   override it.
#' @param steps Character vector selecting which steps to run; defaults to all
#'   present in `config`.
#' @param overwrite Passed to each creator (default `FALSE`).
#'
#' @return A named list of the created objects, plus `$validation`.
#' @examples
#' \dontrun{
#' cfg <- list(
#'   outdir     = "project",
#'   stand_grid = list(polygons = stands, id_field = "stand_id"),
#'   init       = list(trees = cohorts)
#' )
#' res <- build_iland_landscape(cfg, steps = c("stand_grid", "init"))
#' }
#' @export
build_iland_landscape <- function(config,
                                  steps = intersect(
                                    c("stand_grid","env_grid","env_file","init","climate"),
                                    names(config)),
                                  overwrite = FALSE) {
  .require_class(config, "list")
  outdir <- config$outdir
  res <- list()

  out_path <- function(args, key, default_name) {
    if (!is.null(args[[key]])) return(args)
    if (!is.null(outdir)) args[[key]] <- file.path(outdir, default_name)
    args
  }

  if ("stand_grid" %in% steps) {
    a <- out_path(config$stand_grid, "filename", "gis/stand_grid.asc")
    a$overwrite <- overwrite
    res$stand_grid <- do.call(create_stand_grid, a)
  }
  if ("env_grid" %in% steps) {
    a <- config$env_grid
    if (is.null(a$reference)) a$reference <- res$stand_grid
    a <- out_path(a, "filename", "gis/environment_grid.asc")
    a$overwrite <- overwrite
    res$env_grid <- do.call(create_environment_grid, a)
  }
  if ("env_file" %in% steps) {
    a <- config$env_file
    if (is.null(a$ru_grid)) a$ru_grid <- res$env_grid
    a <- out_path(a, "filename", "gis/environment.txt")
    a$overwrite <- overwrite
    res$env_file <- do.call(create_environment_file, a)
  }
  if ("init" %in% steps) {
    a <- config$init
    if (is.null(a$stand_grid)) a$stand_grid <- res$stand_grid
    a <- out_path(a, "filename", "init/init_trees.txt")
    a$overwrite <- overwrite
    res$init <- do.call(create_init_file, a)
  }
  if ("climate" %in% steps) {
    a <- out_path(config$climate, "path", "database/climate.sqlite")
    a$overwrite <- overwrite
    res$climate <- do.call(write_iland_climate, a)
  }

  res$validation <- validate_landscape(
    init = res$init, stand_grid = res$stand_grid,
    env_file = res$env_file, env_grid = res$env_grid)
  res
}


# ---------------------------------------------------------------------
# small internal utilities
# ---------------------------------------------------------------------
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

#' @keywords internal
#' @noRd
symdiff <- function(a, b) union(setdiff(a, b), setdiff(b, a))