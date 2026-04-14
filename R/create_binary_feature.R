#' Create Binary Features for Multi-class Variable
#' @title Create Binary Features
#' @param data Input dataframe
#' @param target_col Name of the target column
#' @param positive_classes Vector of positive class names (NULL = use all unique values)
#' @return Dataframe with new binary columns
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

# # Example usage:
# data_with_binary <- create_binary_features(
#   data = train_data_mtbsi,
#   target_col = "severity",
#   positive_classes = c("Low", "Moderate", "High")
# )


