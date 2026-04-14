#' Create Variable Importance Plots for Multiple Models
#' @param models List of models or single model
#' @param model_names Vector of model names for plot titles
#' @param output_dir Directory to save plots
#' @param num_features Number of top features to show
#' @param plot_dims List containing width and height
#' @param create_combined Whether to create a combined plot for all models
#' @param combined_name Filename for the combined plot
#' @param combined_title Title for the combined plot
#' @return List of ggplot objects
#' @export
create_vip_plots <- function(models, 
                           model_names,
                           output_dir,
                           num_features = 20,
                           plot_dims = list(width = 6, height = 8),
                           create_combined = FALSE,
                           combined_name = "combined_vip_plot",
                           combined_title = "Combined Variable Importance Across Models") {
    
    # Convert single model to list if necessary
    if (!is.list(models) || "ranger" %in% class(models)) {
        models <- list(models)
        model_names <- c(model_names[1])
    }
    
    # Create plots for each model
    vip_plots <- lapply(seq_along(models), function(i) {
        model <- models[[i]]
        model_name <- model_names[i]
        
        # Create VIP plot
        vip_plot <- vip::vip(model,
            scale = TRUE,
            num_features = num_features,
            geom = "point",
            all_permutation = TRUE,
            aesthetics = list(color = "blue", size = 2.5)) +
            geom_text(aes(label = sprintf("%.1f", Importance)),
                     hjust = -0.3,
                     size = 2,
                     color = "black") +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 8, face = "bold"),
                axis.text.y = element_text(size = 8),
                axis.text.x = element_text(size = 8),
                axis.title = element_text(size = 10),
                plot.margin = margin(1, 1, 1, 1, "pt")
            ) +
            labs(
                title = paste("Variable Importance Plot -", model_name),
                x = "Variable",
                y = "Variable Importance Score"
            )
        
        # Save plot
        ggsave(
            filename = file.path(output_dir, paste0("VIP_plot_", model_name, ".png")),
            plot = vip_plot,
            dpi = 500,
            width = plot_dims$width,
            height = plot_dims$height,
            units = "in"
        )
        
        return(vip_plot)
    })
    
    names(vip_plots) <- model_names
    
    # Create combined plot if requested
    if (create_combined && length(models) > 1) {
        # Extract variable importance from each model
        vip_data_list <- lapply(seq_along(models), function(i) {
            vi_scores <- vip::vi(models[[i]],
                            scale = TRUE, 
                   num_features = 25,
                   
                   all_permutation = TRUE,) %>%
                as.data.frame() %>%
                # Take top N features
                head(num_features) %>%
                # Add model identifier
                mutate(Model = model_names[i])
        })
        
        # Combine all importance scores
        combined_vi <- do.call(rbind, vip_data_list)
        
        # Ensure models are ordered correctly
        combined_vi$Model <- factor(combined_vi$Model, levels = model_names)
        
        # Create the combined plot
        combined_plot <- ggplot(combined_vi, 
                             aes(x = reorder(Variable, Importance), 
                                 y = Importance, 
                                 fill = Model)) +
            geom_col(position = position_dodge(width = 0.8), width = 0.7) +
            coord_flip() +
            scale_fill_brewer(palette = "Set1") +
            theme_minimal() +
            labs(
                title = combined_title,
                
                x = "Variable",
                y = "Importance Score"
            ) +
            theme(
                legend.position = "top",
                plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 12, hjust = 0.5),
                axis.text.y = element_text(size = 10),
                axis.text.x = element_text(size = 9),
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_blank()
            )
        
        # Save combined plot
        ggsave(
            filename = file.path(output_dir, paste0(combined_name, ".png")),
            plot = combined_plot,
            dpi = 500,
            width = plot_dims$width * 1.2,  # Slightly wider for the combined plot
            height = plot_dims$height * 1.2,
            units = "in"
        )
        
        # Add combined plot to the return list
        vip_plots$combined <- combined_plot
    }
    
    return(vip_plots)
}

# Example usage:
# decadal_vips <- create_vip_plots(
#     models = list(d1_rf_model_FNF, d2_rf_model_FNF, d3_rf_model_FNF, d4_rf_model_FNF),
#     model_names = c("1984-1993", "1994-2003", "2004-2013", "2014-2023"),
#     output_dir = file.path(fig_src, "figures_v2"),
#     num_features = 20,
#     plot_dims = list(width = 6, height = 8)
# )

