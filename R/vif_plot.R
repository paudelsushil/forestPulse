#' Create VIF Plot
#' @title Create Variance Inflation Factors Plot
#' @param model Model object for VIF analysis
#' @param subtitle Optional subtitle for the plot (default: NULL)
#' @param save_path Optional path to save plot (default: NULL)
#' @param width Plot width in inches (default: 8)
#' @param height Plot height in inches (default: 6)
#' @param dpi Resolution of saved image in dots per inch.
#' @return ggplot object
vif_plot <- function(model, 
                     subtitle = NULL,
                     save_path = NULL,
                     width = 6,
                     height = 6,
                     dpi = 300) {
  
  # Input validation
  if (!inherits(model, c("lm", "glm", "mlm"))) {
    stop("Model must be a linear or generalized linear model object")
  }
  if (!requireNamespace("car", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Packages 'car' and 'ggplot2' are required.", call. = FALSE)
  }
  
  # Calculate VIF values
  vif_values <- car::vif(model)
  
  # Create data frame for plotting
  vif_df <- data.frame(
    Variable = names(vif_values),
    VIF = as.numeric(vif_values)
  )
  
  # Create the plot
  p <- ggplot2::ggplot(
    vif_df,
    ggplot2::aes(
      x = stats::reorder(ggplot2::.data$Variable, ggplot2::.data$VIF),
      y = ggplot2::.data$VIF
    )
  ) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
    ggplot2::geom_hline(yintercept = 10, linetype = "dashed", color = "darkred") +
    ggplot2::geom_text(
      ggplot2::aes(label = round(ggplot2::.data$VIF, 1)),
      hjust = -0.1,
      size = 2.5
    ) +
    ggplot2::annotate("text", x = 1, y = 6, 
             label = "Moderate concern", color = "red",
             angle = 90, hjust = 0, size = 2.5) +
    ggplot2::annotate("text", x = 1, y = 11, 
             label = "High concern", color = "darkred",
             angle = 90, hjust = 0, size = 2.5) +

    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Variance Inflation Factors",
      subtitle = subtitle,
      x = "",
      y = "VIF Value"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      axis.text.y = ggplot2::element_text(face = "bold")
    )
  
  # Save plot if path provided
  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, p, width = width, height = height, dpi = dpi)
    message(sprintf("Plot saved to: %s", save_path))
  }
  
  return(p)
}

# Example usage:
# vif_plot(FNF_model, 
#          subtitle = "using logistic Regression Model for Fire Occurrence",
#          save_path = "vif_plot.png",
#          width = 8,
#          height = 6)






