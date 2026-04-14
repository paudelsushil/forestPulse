#' Calculate Crown Density by Plot
#'
#' Calculates crown density as the arithmetic mean of spherical densitometer
#' readings for each plot.
#'
#' Required input columns:
#' - `FBAT_plot_id`
#' - `direction`
#' - `distance1`
#' - `distance2`
#' - `distance3`
#'
#' Data design assumes 4 rows per plot (N, E, S, W) and 3 readings per row,
#' for up to 12 readings per plot.
#'
#' @param df Data frame containing crown density readings.
#' @param na.rm Logical; if `TRUE`, remove missing values when calculating means.
#'
#' @return A data frame with one row per plot:
#' - `FBAT_plot_id`
#' - `crown_density`
#' - `sd`
#' - `n`
#' - `mean_distance1`
#' - `mean_distance2`
#' - `mean_distance3`
#' @export
calculate_crownDensity <- function(df, na.rm = TRUE) {
  required_cols <- c("FBAT_plot_id", "direction", "distance1", "distance2", "distance3")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  df <- df[!is.na(df$FBAT_plot_id), required_cols, drop = FALSE]

  rows_per_plot <- table(df$FBAT_plot_id)
  if (any(rows_per_plot != 4)) {
    warning(
      "Some plots do not have exactly 4 rows (directions). Calculations continue, but verify data completeness."
    )
  }

  # Long-format equivalent using base R, keeps NA values.
  n_rows <- nrow(df)
  long_df <- data.frame(
    FBAT_plot_id = rep(df$FBAT_plot_id, times = 3),
    distance_class = rep(c("distance1", "distance2", "distance3"), each = n_rows),
    canopy_reading = c(df$distance1, df$distance2, df$distance3),
    stringsAsFactors = FALSE
  )

  plot_ids <- sort(unique(long_df$FBAT_plot_id))

  summarize_plot <- function(pid) {
    vals <- long_df$canopy_reading[long_df$FBAT_plot_id == pid]
    c(
      crown_density = round(mean(vals, na.rm = na.rm), 2),
      sd = round(stats::sd(vals, na.rm = na.rm), 2),
      n = sum(!is.na(vals))
    )
  }

  out_main <- t(vapply(plot_ids, summarize_plot, numeric(3)))
  out_main <- data.frame(FBAT_plot_id = plot_ids, out_main, row.names = NULL)

  distance_means <- stats::aggregate(
    cbind(distance1, distance2, distance3) ~ FBAT_plot_id,
    data = df,
    FUN = function(x) mean(x, na.rm = na.rm)
  )

  names(distance_means) <- c("FBAT_plot_id", "mean_distance1", "mean_distance2", "mean_distance3")
  distance_means$mean_distance1 <- round(distance_means$mean_distance1, 2)
  distance_means$mean_distance2 <- round(distance_means$mean_distance2, 2)
  distance_means$mean_distance3 <- round(distance_means$mean_distance3, 2)

  results <- merge(out_main, distance_means, by = "FBAT_plot_id", all.x = TRUE, sort = TRUE)

  if (any(results$n != 12, na.rm = TRUE)) {
    warning("Some plots do not have all 12 measurements; crown_density is based on available values.")
  }

  results
}


#' Show Crown Density Calculation Details for One Plot
#'
#' @param df Input data frame with `distance1`, `distance2`, `distance3`.
#' @param plot_id Plot identifier to inspect.
#'
#' @return Invisible list with crown density, sd, and long-format measurements.
#' @export
show_calculation_details <- function(df, plot_id) {
  required_cols <- c("FBAT_plot_id", "direction", "distance1", "distance2", "distance3")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  plot_data <- df[df$FBAT_plot_id == plot_id, required_cols, drop = FALSE]
  if (nrow(plot_data) == 0) {
    stop(sprintf("Plot %s not found in data.", plot_id))
  }

  measurements <- data.frame(
    direction = rep(plot_data$direction, times = 3),
    distance = rep(c("distance1", "distance2", "distance3"), each = nrow(plot_data)),
    reading = c(plot_data$distance1, plot_data$distance2, plot_data$distance3),
    stringsAsFactors = FALSE
  )

  measurements <- measurements[order(measurements$distance, measurements$direction), , drop = FALSE]

  all_readings <- measurements$reading
  n_valid <- sum(!is.na(all_readings))
  sum_readings <- sum(all_readings, na.rm = TRUE)
  crown_density <- mean(all_readings, na.rm = TRUE)
  sd_value <- stats::sd(all_readings, na.rm = TRUE)

  cat(sprintf("Plot: %s\n", plot_id))
  cat(sprintf("Valid readings (n): %d\n", n_valid))
  cat(sprintf("Sum of valid readings: %.2f\n", sum_readings))
  cat(sprintf("Crown density: %.2f%%\n", crown_density))
  cat(sprintf("SD: %.2f\n", sd_value))

  invisible(list(
    crown_density = crown_density,
    sd = sd_value,
    measurements = measurements
  ))
}


#' Quick Plot-Level Summary by Direction
#'
#' @param df Input data frame with `distance1`, `distance2`, `distance3`.
#' @param plot_id Optional plot ID. If NULL, returns all plots.
#'
#' @return Data frame with directional readings by distance class.
#' @export
plot_summary <- function(df, plot_id = NULL) {
  required_cols <- c("FBAT_plot_id", "direction", "distance1", "distance2", "distance3")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  if (!is.null(plot_id)) {
    df <- df[df$FBAT_plot_id == plot_id, , drop = FALSE]
  }

  split_plots <- split(df, df$FBAT_plot_id)

  make_row <- function(plot_df) {
    get_val <- function(dir, dist_col) {
      vals <- plot_df[[dist_col]][plot_df$direction == dir]
      if (length(vals) == 0) {
        return(NA_real_)
      }
      vals[[1]]
    }

    data.frame(
      FBAT_plot_id = plot_df$FBAT_plot_id[[1]],
      N_distance1 = get_val("N", "distance1"),
      E_distance1 = get_val("E", "distance1"),
      S_distance1 = get_val("S", "distance1"),
      W_distance1 = get_val("W", "distance1"),
      N_distance2 = get_val("N", "distance2"),
      E_distance2 = get_val("E", "distance2"),
      S_distance2 = get_val("S", "distance2"),
      W_distance2 = get_val("W", "distance2"),
      N_distance3 = get_val("N", "distance3"),
      E_distance3 = get_val("E", "distance3"),
      S_distance3 = get_val("S", "distance3"),
      W_distance3 = get_val("W", "distance3"),
      row.names = NULL
    )
  }

  if (length(split_plots) == 0) {
    return(data.frame())
  }

  out <- do.call(rbind, lapply(split_plots, make_row))
  rownames(out) <- NULL
  out
}
