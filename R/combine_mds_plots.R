#' Combine MDS Plots
#' @title Combine MDS Plots
#' @param evaluations List of model evaluation results
#' @param model_names Vector of model names
#' @param output_dir Directory to save the combined plot
#' @param output_name Name for the output file
#' @return Combined ggplot object
#' @export
combine_mds_plots <- function(evaluations,
                            model_names,
                            output_dir,
                            output_name = "combined_mds_plots",
                            title = "MDS Plots of Random Forest Predictions of wildfire occurrence Across Decades") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Extract MDS plots and add facet labels
  mds_plots <- lapply(seq_along(evaluations), function(i) {
    mds_plot <- evaluations[[i]]$mds_plot +
      ggtitle(model_names[i]) +
      theme(plot.title = element_text(size = 10, face = "bold"))
  })
  
  # Combine plots using patchwork
  combined_plot <- patchwork::wrap_plots(mds_plots, ncol = 2) +
    patchwork::plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    )
  
  # Save the combined plot
  ggsave(
    file = file.path(output_dir, paste0(output_name, ".png")),
    plot = combined_plot,
    width = 12,
    height = 10,
    dpi = 300
  )
  
  return(combined_plot)
}
