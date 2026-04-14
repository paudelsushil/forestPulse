#' Create Correlation Plot
#' @title Create Correlation Plot
#' @param data Input data frame
#' @param columns Columns to include in correlation
#' @param use Method for handling missing values
#' @param title Plot title.
#' @param save_path Optional path to save the output image file.
#' @param width Width of saved plot in inches when `save_path` is provided.
#' @param height Height of saved plot in inches when `save_path` is provided.
#' @return Correlation plot object
corrPlot <- function(data, columns, 
use = "pairwise.complete.obs", 
title = "Correlation Plot of Variables",
save_path = NULL, 
width = 10, 
height = 8) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame")
  }
  if (!all(columns %in% names(data))) {
    stop("Some specified columns not found in data")
  }
  if (!requireNamespace("corrplot", quietly = TRUE) ||
      !requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("Packages 'corrplot' and 'RColorBrewer' are required.", call. = FALSE)
  }

  # Process data and create correlation matrix
  col4cor <- sf::st_drop_geometry(data)
  col4cor[columns] <- lapply(col4cor[columns], as.numeric)
  col4cor <- col4cor[, columns, drop = FALSE]

  # Calculate correlation matrix
  cor_matrix <- stats::cor(col4cor, use = use)

# If save_path is provided, open PNG device
  if (!is.null(save_path)) {
    grDevices::png(filename = save_path, 
             width = width, 
             height = height, 
             units = "in", 
             res = 500)
  }
  # Create correlation plot
  corr_plot <- corrplot::corrplot(cor_matrix,
                    method = "color",
                    type = "lower",
                    col = grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdBu"))(100),
                    order = "hclust",
                    addCoef.col = "black",
                    tl.col = "black",
                    tl.srt = 45,
                    tl.cex = 0.70,
                    tl.offset = 0.8,
                    number.cex = 0.55,
                    title = title,
                    mar = c(2, 0, 3, 0),
                    diag = FALSE
  )
  # If saving, close the device
  if (!is.null(save_path)) {
    grDevices::dev.off()
    message(sprintf("Plot saved to: %s", save_path))
  }
  # Return the plot object
  return(invisible(corr_plot))
}
# Example usage
# plot <- corrPlot(data, numCol, 
# save_path = file.path(fig_src, "corrplot.png"), width = 8, height = 10)

