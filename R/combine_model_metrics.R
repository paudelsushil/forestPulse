#' Combine Model Evaluation Metrics
#' @title Combine Model Evaluation Metrics
#' @param evaluations List of model evaluation results
#' @param model_names Vector of model names
#' @param output_dir Directory to save the results
#' @param output_name Name for the output file
#' @return Combined dataframe of metrics
#' @export
combine_model_metrics <- function(evaluations, 
                               model_names,
                               output_dir,
                               output_name = "combined_model_metrics") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Extract metrics for each model
  metrics_list <- lapply(seq_along(evaluations), function(i) {
    metrics_df <- as.data.frame(evaluations[[i]]$metrics)
    # Add model name
    metrics_df$Model <- model_names[i]
    # Reshape to have metrics in rows
    metrics_df <- data.frame(
      Model = model_names[i],
      AUC = metrics_df$auc,
      Accuracy = metrics_df$accuracy,
      Sensitivity = metrics_df$sensitivity,
      Specificity = metrics_df$specificity
    )
    return(metrics_df)
  })
  
  # Combine metrics into a single dataframe
  combined_metrics <- do.call(rbind, metrics_list)
  
  # Save as CSV
  write.csv(combined_metrics, 
           file = file.path(output_dir, paste0(output_name, ".csv")),
           row.names = FALSE)
  
  # Create formatted table
  formatted_table <- knitr::kable(combined_metrics,
                               format = "html",
                               digits = 3,
                               caption = "Model Performance Metrics") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  
  # Save as HTML
  writeLines(formatted_table,
            file.path(output_dir, paste0(output_name, ".html")))
  
  return(combined_metrics)
}
