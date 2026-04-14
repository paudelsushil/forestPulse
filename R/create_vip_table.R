#' Combine Variable Importance Scores from Multiple Models
#' @param models List of ranger models
#' @param model_names Vector of names for each model/decade
#' @param output_dir Directory to save combined results
#' @param output_name Name for the output file
#' @return Combined dataframe of importance scores
#' @export
create_vip_table <- function(models, 
                                     model_names,
                                     output_dir,
                                     output_name = "variable_importance") {
    
    # Process each model's importance scores
    importance_list <- lapply(seq_along(models), function(i) {
        # Get variable importance
        vi_scores <- vip::vi(models[[i]],
                            scale = TRUE, 
                            num_features = 25,
                            all_permutation = TRUE) %>%
            as.data.frame() %>%
            arrange(desc(Importance)) %>%
            # Rename Importance column with decade
            rename(!!model_names[i] := Importance)
    })
    
    # Combine all importance scores
    combined_importance <- Reduce(function(x, y) {
        full_join(x, y, by = "Variable")
    }, importance_list)
    
    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }
    
    # Export as CSV
    write.csv(combined_importance,
              file = file.path(output_dir, paste0(output_name, ".csv")),
              row.names = FALSE)
    
    # Export as formatted table
    formatted_table <- knitr::kable(combined_importance,
                                  format = "html",
                                  digits = 3,
                                  caption = "Variable Importance Scores Across Decades") %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
    
    # # Save HTML table
    writeLines(formatted_table,
              file.path(output_dir, paste0(output_name, ".html")))
    
    return(combined_importance)
}

# Example usage:
# decadal_importance <- combine_variable_importance(
#     models = list(d1_rf_model_mtbsi_high, 
#                  d2_rf_model_mtbsi_high, 
#                  d3_rf_model_mtbsi_high, 
#                  d4_rf_model_mtbsi_high),
#     model_names = c("1984-1993", "1994-2003", "2004-2013", "2014-2023"),
#     output_dir = file.path(analyzed_src, filename, "tables"),
#     output_name = "mtbsi_high_importance_scores"
# )