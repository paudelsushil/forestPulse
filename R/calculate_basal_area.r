#' Calculate basal Area Summaries
#'
#' One parameterized entry point for basal area summaries.
#'
#' @param data Data frame containing tree measurements.
#' @param summary Summary type: `"plot"`, `"plot_species"`, or `"species"`.
#' @param plot_id Optional vector of plot IDs to filter.
#' @param dbh_col Name of DBH column in centimeters.
#' @param species_col Name of species column.
#' @param plot_col Name of plot ID column.
#' @param height_col Name of tree height column in meters.
#' @param plot_area_m2 Plot area in square meters used for per-hectare scaling.
#' @param include_dead If `FALSE`, trees with `deadAlive == "D"` are removed when present.
#'
#' @return Tibble (or data frame fallback) with basal area summaries including
#'   `total_basal_area_m2` and `total_basal_area_m2_ha`.
#' @export
calculate_basal_area <- function(data,
                                summary = c("plot", "plot_species", "species"),
                                plot_id = NULL,
                                dbh_col = "dbh_cm",
                                species_col = "species",
                                plot_col = "Plt_id",
                                height_col = "height_m",
                                plot_area_m2 = 400,
                                include_dead = FALSE) {
  summary <- match.arg(summary)

  require_species <- summary %in% c("plot_species", "species")
  require_plot <- summary %in% c("plot", "plot_species")

  prep <- .prepare_basal_area_data(
    data = data,
    dbh_col = dbh_col,
    species_col = species_col,
    plot_col = plot_col,
    height_col = height_col,
    plot_id = plot_id,
    require_species = require_species,
    require_plot = require_plot,
    include_dead = include_dead
  )

  if (!is.numeric(plot_area_m2) || length(plot_area_m2) != 1 || is.na(plot_area_m2) || plot_area_m2 <= 0) {
    stop("'plot_area_m2' must be a single positive number.")
  }

  clean_data <- prep$data
  has_height <- prep$has_height

  grouping_key <- switch(
    summary,
    plot = as.character(clean_data[[plot_col]]),
    plot_species = paste(clean_data[[plot_col]], clean_data[[species_col]], sep = "||"),
    species = as.character(clean_data[[species_col]])
  )

  split_groups <- split(clean_data, grouping_key)

  rows <- lapply(split_groups, function(x) {
    out <- data.frame(
      n_trees = nrow(x),
      total_basal_area_m2 = round(sum(x$basal_area_m2, na.rm = TRUE), 4),
      mean_basal_area_m2 = round(mean(x$basal_area_m2, na.rm = TRUE), 4),
      mean_dbh_cm = round(mean(x[[dbh_col]], na.rm = TRUE), 2),
      min_dbh_cm = round(min(x[[dbh_col]], na.rm = TRUE), 2),
      max_dbh_cm = round(max(x[[dbh_col]], na.rm = TRUE), 2),
      stringsAsFactors = FALSE
    )

    if (plot_col %in% names(x)) {
      out$n_plots <- length(unique(stats::na.omit(x[[plot_col]])))
    } else {
      out$n_plots <- 1
    }

    sample_area_ha <- (out$n_plots * plot_area_m2) / 10000
    out$total_basal_area_m2_ha <- round(out$total_basal_area_m2 / sample_area_ha, 4)

    if (has_height) {
      out$mean_height_m <- round(mean(x[[height_col]], na.rm = TRUE), 2)
      out$min_height_m <- round(min(x[[height_col]], na.rm = TRUE), 2)
      out$max_height_m <- round(max(x[[height_col]], na.rm = TRUE), 2)
      out$n_trees_with_height <- sum(!is.na(x[[height_col]]))
    }

    out
  })

  out <- do.call(rbind, rows)

  if (summary == "plot") {
    out <- cbind(data.frame(plot_value = names(split_groups), stringsAsFactors = FALSE), out)
    names(out)[1] <- plot_col
    out <- out[order(out[[plot_col]]), , drop = FALSE]
  } else if (summary == "plot_species") {
    meta <- do.call(rbind, strsplit(names(split_groups), "\\|\\|"))
    out <- cbind(data.frame(meta, stringsAsFactors = FALSE), out)
    names(out)[1:2] <- c(plot_col, species_col)
    out <- out[order(out[[plot_col]], -out$total_basal_area_m2_ha), , drop = FALSE]
  } else {
    out <- cbind(data.frame(species_value = names(split_groups), stringsAsFactors = FALSE), out)
    names(out)[1] <- species_col
    out <- out[order(out$total_basal_area_m2_ha, decreasing = TRUE), , drop = FALSE]
  }

  rownames(out) <- NULL

  if (!is.null(plot_id)) {
    attr(out, "plot_id") <- plot_id
  }

  .as_tidy_table(out)
}


#' Calculate Species-Wise basal Area from FBAT Plot Data
#'
#' @inheritParams calculate_basal_area
#' @param units Deprecated. Kept for compatibility.
#'
#' @return Tibble (or data frame fallback) with species-level basal area and
#'   optional height stats.
#' @export
calculate_species_basal_area <- function(data,
                                         plot_id = NULL,
                                         dbh_col = "dbh_cm",
                                         species_col = "species",
                                         plot_col = "Plt_id",
                                         height_col = "height_m",
                                         units = "m2",
                                         plot_area_m2 = 400,
                                         include_dead = FALSE) {
  if (!missing(units)) {
    if (!units %in% c("m2", "cm2", "ha")) {
      stop("'units' must be one of: 'cm2', 'm2', 'ha'.")
    }
  }

  calculate_basal_area(
    data = data,
    summary = "species",
    plot_id = plot_id,
    dbh_col = dbh_col,
    species_col = species_col,
    plot_col = plot_col,
    height_col = height_col,
    plot_area_m2 = plot_area_m2,
    include_dead = include_dead
  )
}


#' Calculate basal Area by Plot and Species
#'
#' @inheritParams calculate_basal_area
#' @param units Deprecated. Kept for compatibility.
#'
#' @return Tibble (or data frame fallback) with plot/species-level basal area
#'   and optional height stats.
#' @export
calculate_plot_species_basal_area <- function(data,
                                              dbh_col = "dbh_cm",
                                              species_col = "species",
                                              plot_col = "Plt_id",
                                              height_col = "height_m",
                                              units = "m2",
                                              plot_area_m2 = 400,
                                              include_dead = FALSE) {
  if (!missing(units)) {
    if (!units %in% c("m2", "cm2", "ha")) {
      stop("'units' must be one of: 'cm2', 'm2', 'ha'.")
    }
  }

  calculate_basal_area(
    data = data,
    summary = "plot_species",
    dbh_col = dbh_col,
    species_col = species_col,
    plot_col = plot_col,
    height_col = height_col,
    plot_area_m2 = plot_area_m2,
    include_dead = include_dead
  )
}


#' Calculate Plot-Level Summary Statistics
#'
#' @inheritParams calculate_basal_area
#' @param units Deprecated. Kept for compatibility.
#'
#' @return Tibble (or data frame fallback) with plot-level summary statistics.
#' @export
calculate_plot_summary <- function(data,
                                   dbh_col = "dbh_cm",
                                   species_col = "species",
                                   plot_col = "Plt_id",
                                   height_col = "height_m",
                                   units = "m2",
                                   plot_area_m2 = 400,
                                   include_dead = FALSE) {
  if (!missing(units)) {
    if (!units %in% c("m2", "cm2", "ha")) {
      stop("'units' must be one of: 'cm2', 'm2', 'ha'.")
    }
  }

  calculate_basal_area(
    data = data,
    summary = "plot",
    dbh_col = dbh_col,
    species_col = species_col,
    plot_col = plot_col,
    height_col = height_col,
    plot_area_m2 = plot_area_m2,
    include_dead = include_dead
  )
}


# Internal helpers -------------------------------------------------------------
.as_tidy_table <- function(x) {
  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(x))
  }
  x
}

.prepare_basal_area_data <- function(data,
                                     dbh_col,
                                     species_col,
                                     plot_col,
                                     height_col,
                                     plot_id,
                                     require_species,
                                     require_plot,
                                     include_dead) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }

  required <- c(dbh_col)
  if (require_species) {
    required <- c(required, species_col)
  }
  if (require_plot) {
    required <- c(required, plot_col)
  }

  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  has_height <- height_col %in% names(data)

  if (!is.null(plot_id)) {
    if (!plot_col %in% names(data)) {
      stop(sprintf("Plot column '%s' not found in data.", plot_col))
    }
    data <- data[data[[plot_col]] %in% plot_id, , drop = FALSE]
    if (nrow(data) == 0) {
      stop("No data found for the specified plot ID(s).")
    }
  }

  keep <- !is.na(data[[dbh_col]]) & data[[dbh_col]] > 0
  if (require_species) {
    keep <- keep & !is.na(data[[species_col]])
  }
  if (require_plot) {
    keep <- keep & !is.na(data[[plot_col]])
  }

  clean_data <- data[keep, , drop = FALSE]

  if (!include_dead && "deadAlive" %in% names(clean_data)) {
    alive <- clean_data$deadAlive != "D" | is.na(clean_data$deadAlive)
    clean_data <- clean_data[alive, , drop = FALSE]
  }

  clean_data$basal_area_m2 <- pi * (clean_data[[dbh_col]] / 200)^2

  list(data = clean_data, has_height = has_height)
}
