library(ranger)
library(vip)
library(dplyr)
library(scales) 
library(pROC)
library(ggplot2)

#' Evaluate Ranger model performance and create visualizations
#' @param model Trained ranger model
#' @param test_data Test dataset
#' @param actual_values Actual values from test data
#' @param output_dir Directory to save output files
#' @param model_name Name of the model for file naming
#' @param max_samples Maximum number of samples for MDS plot
#' @param x_limits X-axis limits for MDS plot
#' @param y_limits Y-axis limits for MDS plot
#' @param severity_mode Whether this is a severity model (TRUE) or fire/non-fire model (FALSE)
#' @return List with metrics, confusion matrix, predictions and MDS plot
#' @export
evaluate_ranger_model <- function(model, test_data, actual_values, 
                                output_dir, model_name, max_samples = 500,
                                x_limits = c(-0.4, 0.4),
                                y_limits = c(-0.4, 0.4),
                                severity_mode = FALSE) {
  # Get predictions
  predictions <- predict(model, data = test_data)$predictions
  
  # Calculate metrics
  roc_obj <- roc(actual_values, predictions[,2])
  confusion_mat <- table(Actual = actual_values, 
                        Predicted = predictions[,2] > 0.5)
  
  # Calculate performance metrics
  metrics <- list(
    auc = auc(roc_obj),
    accuracy = sum(diag(confusion_mat)) / sum(confusion_mat),
    sensitivity = confusion_mat[2,2] / sum(confusion_mat[2,]),
    specificity = confusion_mat[1,1] / sum(confusion_mat[1,])
  )
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Export metrics to text file
  metrics_file <- file.path(output_dir, paste0(model_name, "_metrics.txt"))
  cat("Model Performance Metrics\n",
      "=======================\n",
      sprintf("AUC: %.3f\n", metrics$auc),
      sprintf("Accuracy: %.3f\n", metrics$accuracy),
      sprintf("Sensitivity: %.3f\n", metrics$sensitivity),
      sprintf("Specificity: %.3f\n", metrics$specificity),
      file = metrics_file)
  
  # Export confusion matrix to text file
  conf_mat_file <- file.path(output_dir, paste0(model_name, "_confusion_matrix.txt"))
  capture.output(
    cat("Confusion Matrix\n",
        "================\n"),
    print(confusion_mat),
    file = conf_mat_file
  )
  
  # Sample predictions if too large
  n_samples <- nrow(predictions)
  if (n_samples > max_samples) {
      set.seed(123)  # for reproducibility
      sample_idx <- sample(n_samples, max_samples)
      pred_subset <- predictions[sample_idx, ]
      actual_subset <- actual_values[sample_idx]
  } else {
      pred_subset <- predictions
      actual_subset <- actual_values
  }
  
  # Calculate MDS with reduced dataset
  pred_dist <- dist(pred_subset)
  mds_result <- cmdscale(pred_dist, k = 2)
  
  # Create plotting data and add letter labels based on the model type
  if (severity_mode) {
    # For severity models: "H" for high severity, "N" for other
    plot_data <- data.frame(
        MDS1 = mds_result[,1],
        MDS2 = mds_result[,2],
        Class = actual_subset,
        Label = ifelse(actual_subset == "1", "H", "N")
    )
    # Colors for severity
    color_values <- c("1" = "red", "0" = "blue")
    subtitle_text <- sprintf("Based on %d samples (H=high severity, N=non-high)", nrow(plot_data))
  } else {
    # For fire/non-fire models: "F" for fire, "N" for non-fire
    plot_data <- data.frame(
        MDS1 = mds_result[,1],
        MDS2 = mds_result[,2],
        Class = actual_subset,
        Label = ifelse(actual_subset == "fire", "F", "N")
    )
    # Colors for fire/non-fire
    color_values <- c("fire" = "red", "non_fire" = "blue")
    subtitle_text <- sprintf("Based on %d samples (F=fire, N=non-fire)", nrow(plot_data))
  }
  
  # Create MDS plot with letters instead of points
  mds_plot <- ggplot(plot_data, aes(x = MDS1, y = MDS2, label = Label, color = Class)) +
      geom_text(alpha = 0.7, size = 2.5) +  # Use text instead of points
      scale_color_manual(values = color_values) +
      theme_minimal() +
      labs(title = "MDS Plot of Random Forest Predictions",
           subtitle = subtitle_text,
           x = "First Dimension",
           y = "Second Dimension") +
      # Set fixed X and Y limits
      # coord_cartesian(xlim = x_limits, ylim = y_limits) +
      # Add a light grid for better readability
      theme(
          panel.grid.minor = element_line(color = "gray90"),
          panel.grid.major = element_line(color = "gray85"),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "none"  # Remove legend
      )
  
  # Save plot
  ggsave(file.path(output_dir, paste0(model_name, "_mds_plot.png")),
         mds_plot, width = 6, height = 6, dpi = 500)
  
  return(list(
    metrics = metrics,
    confusion_matrix = confusion_mat,
    predictions = predictions,
    mds_plot = mds_plot
  ))
}
