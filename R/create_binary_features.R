#' Create Binary Features for a Multi-class Variable
#'
#' @description
#' Adds one binary (0/1 factor) column per class of a categorical target
#' column. Each new column flags whether a row belongs to that class, which is
#' useful for one-vs-rest modelling.
#'
#' @param data Input data frame.
#' @param target_col Character scalar. Name of the categorical target column.
#' @param positive_classes Character vector of class values to encode, or
#'   \code{NULL} (default) to use every unique value of \code{target_col}.
#'
#' @return \code{data} with one added factor column (levels \code{c(0, 1)}) per
#'   requested class.
#'
#' @examples
#' df <- data.frame(severity = c("Low", "High", "Moderate", "Low"))
#' create_binary_features(df, target_col = "severity",
#'                        positive_classes = c("Low", "High"))
#'
#' @export
create_binary_features <- function(data, target_col, positive_classes = NULL) {

  # Input validation
  if (!target_col %in% names(data)) {
    stop(sprintf("Target column '%s' not found in data", target_col))
  }

  # If positive_classes is NULL, use all unique values from the target column
  if (is.null(positive_classes)) {
    positive_classes <- unique(as.character(data[[target_col]]))
    message(sprintf("Using all %d unique values from '%s' as positive classes: %s",
                   length(positive_classes),
                   target_col,
                   paste(positive_classes, collapse = ", ")))
  }

  # Create binary columns for each positive class
  for (class_name in positive_classes) {
    new_col_name <- class_name
    # Check if the class exists in the data
    if (!class_name %in% unique(as.character(data[[target_col]]))) {
      warning(sprintf("Class '%s' not found in target column. Creating column anyway.", class_name))
    }
    data[[new_col_name]] <- ifelse(data[[target_col]] == class_name, 1, 0)
    data[[new_col_name]] <- factor(data[[new_col_name]], levels = c(0, 1))
  }

  return(data)
}
