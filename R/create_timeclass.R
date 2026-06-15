#' Assign Rows to Consecutive Time Classes
#'
#' Bins the values of a numeric time column (e.g. calendar years) into
#' consecutive, non-overlapping windows of fixed width and labels each row with
#' the window it falls into. Useful for grouping observations into periods such
#' as 5-year or decadal classes prior to summarising.
#'
#' @param data A data frame (or tibble) containing the time column.
#' @param date_column Character scalar. Name of the column in \code{data} that
#'   holds the numeric time values (for example a column of years).
#' @param start_date Numeric scalar. Start of the first window (inclusive).
#' @param end_date Numeric scalar. Upper bound used to generate windows; the
#'   last window ends at or before this value.
#' @param by Numeric scalar. Width of each window in the units of
#'   \code{date_column} (for example \code{5} for 5-year classes).
#' @param types Character scalar. Prefix used to build the name of the new
#'   column, which is \code{paste0(types, "By", by, "yrs")}.
#'
#' @return \code{data} with one additional character column giving the time
#'   class of each row (formatted \code{"start-end"}), or \code{NA} for rows
#'   whose value falls outside every window.
#'
#' @examples
#' df <- data.frame(year = c(1986, 1991, 2003, 2018))
#' create_timeclass(df, date_column = "year",
#'                  start_date = 1985, end_date = 2020,
#'                  by = 5, types = "fire")
#'
#' @export
create_timeclass <- function(data,
                             date_column,
                             start_date,
                             end_date,
                             by,
                             types) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }
  if (!is.character(date_column) || length(date_column) != 1L) {
    stop("'date_column' must be a single column name.", call. = FALSE)
  }
  if (!date_column %in% names(data)) {
    stop(sprintf("Column '%s' not found in 'data'.", date_column),
         call. = FALSE)
  }
  if (!is.numeric(by) || length(by) != 1L || by <= 0) {
    stop("'by' must be a single positive number.", call. = FALSE)
  }
  if (!is.character(types) || length(types) != 1L) {
    stop("'types' must be a single character string.", call. = FALSE)
  }

  # Build consecutive, non-overlapping windows [start, end].
  starts <- seq(from = start_date, to = end_date - by + 1, by = by)
  ends   <- starts + (by - 1)
  labels <- paste0(starts, "-", ends)

  x <- data[[date_column]]
  class_label <- rep(NA_character_, length(x))
  for (i in seq_along(starts)) {
    in_window <- !is.na(x) & x >= starts[i] & x <= ends[i]
    class_label[in_window] <- labels[i]
  }

  new_col_name <- paste0(types, "By", by, "yrs")
  data[[new_col_name]] <- class_label
  data
}
