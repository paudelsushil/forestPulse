#' Analyze VIF for Single or Multiple Predictor Sets
#' @title Analyze VIF for Models
#' @param train_data Training dataset
#' @param response_var Response variable name
#' @param predictors Single vector of predictors or list of predictor sets
#' @param output_dir Output directory path
#' @param title Base title for plots and output
#' @param plot_dims List with width and height
#' @return List of VIF results
#' @export
analyze_vif_models <- function(train_data, 
                              response_var,
                              predictors,
                              output_dir,
                              title = "VIF Analysis for Fire Occurrence",
                              plot_dims = list(width = 7, height = 8)) {
    
    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }
    
    # Check if predictors is a single vector or a list of vectors
    if (!is.list(predictors) || (is.list(predictors) && !is.list(predictors[[1]]))) {
        # Convert single vector to a list with one element
        if (!is.list(predictors)) {
            predictor_sets <- list(predictors)
        } else {
            predictor_sets <- list(unlist(predictors))
        }
    } else {
        predictor_sets <- predictors
    }
    
    results <- list()
    
    for(i in seq_along(predictor_sets)) {
        # Get current predictor set
        current_predictors <- predictor_sets[[i]]
        
        # Check for valid predictors (must exist in dataset)
        valid_predictors <- current_predictors[current_predictors %in% names(train_data)]
        
        if (length(valid_predictors) == 0) {
            warning(paste("No valid predictors found for set", i))
            next
        }
        
        # Check for factors with only one level
        for (pred in valid_predictors) {
            if (is.factor(train_data[[pred]]) && length(levels(train_data[[pred]])) < 2) {
                warning(paste("Factor", pred, "has fewer than 2 levels and will be removed"))
                valid_predictors <- valid_predictors[valid_predictors != pred]
            }
        }
        
        # If we have no valid predictors left, skip this set
        if (length(valid_predictors) == 0) {
            warning(paste("No valid predictors remaining for set", i))
            next
        }
        
        tryCatch({
            # Fit logistic model
            formula <- as.formula(paste(response_var, "~", 
                                      paste(valid_predictors, collapse = " + ")))
            
            model <- glm(formula, data = train_data, family = binomial(link = "logit"))
            
            # Calculate VIF
            vif_values <- car::vif(model)
            vif_df <- data.frame(
                Variable = names(vif_values),
                VIF = as.numeric(vif_values)
            )
            
            # Create suffix for filenames
            suffix <- ifelse(length(predictor_sets) > 1, paste0("_pred", i), "")
            
            # Create and save VIF plot
            vif_plot <- ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF)) +
                geom_bar(stat = "identity", fill = "steelblue") +
                geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
                geom_hline(yintercept = 10, linetype = "dashed", color = "darkred") +
                coord_flip() +
                labs(
                    title = title,
                    subtitle = ifelse(length(predictor_sets) > 1, paste("Model", i), ""),
                    x = "Variables",
                    y = "Variance Inflation Factor (VIF)"
                ) +
                theme_minimal() +
                theme(
                    plot.title = element_text(face = "bold"),
                    axis.text.y = element_text(size = 10)
                )
            
            ggsave(
                file.path(output_dir, paste0("vif_plot", suffix, ".png")),
                vif_plot,
                width = plot_dims$width,
                height = plot_dims$height,
                dpi = 300
            )
            
            # Save VIF values to CSV
            write.csv(vif_df, 
                     file.path(output_dir, paste0("vif_values", suffix, ".csv")),
                     row.names = FALSE)
            
            # Store results
            results[[i]] <- list(
                model = model,
                vif_values = vif_df,
                vif_plot = vif_plot,
                predictors_used = valid_predictors
            )
            
        }, error = function(e) {
            warning(paste("Error in model", i, ":", e$message))
            results[[i]] <- list(error = e$message)
        })
    }
    
    return(results)
}

# Example usage
# Assuming you have a data frame `train_data` with your training data
# and a response variable `response_var`
# and a list of predictor sets `predictor_sets`
# and an output directory `output_dir`
# and a title for the analysis `title`
# For a single set of predictors
# vif_fnf_analyze <- analyze_vif_models(
#     train_data = train_data_FNF, 
#     response_var = "event",
#     predictors = predictors,  # Just a single vector
#     output_dir = file.path(fig_src, "vif_fnf_analysis"),
#     title = "VIF Analysis for Fire Occurrence",
#     plot_dims = list(width = 7, height = 8)
# )

# For multiple sets of predictors
# vif_fnf_analyze_multi <- analyze_vif_models(
#     train_data = train_data_FNF, 
#     response_var = "event",
#     predictors = predictors_collection,  # A list of predictor sets
#     output_dir = file.path(fig_src, "vif_fnf_analysis_multi"),
#     title = "VIF Analysis for Fire Occurrence",
#     plot_dims = list(width = 7, height = 8)
# )    


# predictors  <-  c("event", "fuel_mgmt", "outbreak",  
#               "mpb_red", "mpb_young_gray", "mpb_old_gray",  "mpb_old",  
#                 "fuel_0_5",  "fuel_6_10",  "fuel_11_15",  "fuel_gt_15",
#                 "tmmxM", "tmmnM", "prM", "pdsiM",  "scPdsiM", "pzIM", 
#           "vpdM", "petM", "vsM","rmaxM", "rminM", "sradM",
#            "elev30M", "slpD30M", "aspD30M","tpi", "twi",
#            "awC")



tmmxM_df <- raster2table_annual(tmmxM_dir, "\\.nc$", "tmmxM", aoi = conusStates)
tmmnM_df <- raster2table_annual(tmmnM_dir, "\\.nc$", "tmmnM", aoi = conusStates)
rmaxM_df <- raster2table_annual(rmaxM_dir, "\\.nc$", "rmaxM", aoi = conusStates)
rminM_df <- raster2table_annual(rminM_dir, "\\.nc$", "rminM", aoi = conusStates)
sradM_df <- raster2table_annual(sradM_dir, "\\.nc$", "sradM", aoi = conusStates)
vpdM_df <- raster2table_annual(vpdM_dir, "\\.nc$", "vpdM", aoi = conusStates)
petM_df <- raster2table_annual(petM_dir, "\\.nc$", "petM", aoi = conusStates)
prM_df <- raster2table_annual(prM_dir, "\\.nc$", "prM", aoi = conusStates)
pdsiM_df <- raster2table_annual(pdsiM_rawdir, "\\.nc$", "pdsiM", aoi = conusStates)
scPdsiM_df <- raster2table_annual(scPdsiM_rawdir, "\\.nc$", "scPdsiM", aoi = conusStates)
pzIM_df <- raster2table_annual(pzIM_rawdir, "\\.nc$", "pzIM", aoi = conusStates)




plot_d1 <- plot_climate_trends(pdsiM_df, "PDSI")
plot_d2 <- plot_climate_trends(scPdsiM_df, "scPDSI")
plot_d3 <- plot_climate_trends(pzIM_df, "PZI")

plot_d4 <- plot_climate_trends(tmmxM_df, "tmmxM")
plot_d9 <- plot_climate_trends(vpdM_df, "vpdM")
plot_d10 <- plot_climate_trends(petM_df, "petM")
plot_d11 <- plot_climate_trends(prM_df, "prM") 

plot_d5 <- plot_climate_trends(tmmnM_df, "tmmnM")
plot_d6 <- plot_climate_trends(rmaxM_df, "rmaxM")
plot_d7 <- plot_climate_trends(rminM_df, "rminM")
plot_d8 <- plot_climate_trends(sradM_df, "sradM")
   



raster_files <- list.files(tmmxM_dir,
                             pattern = "\\.nc$",
                             full.names = T)

nc <- raster_files[1]



plot_d1
plot_d2
plot_d3

raster_files <-  list.files(tmmxM_rawdir, "\\.nc$", full.names = TRUE)
 r <- terra::rast(raster_files)

vLyr <- conus_states %>%
    sf::st_transform(., st_crs(r)) %>%
    terra::vect(.)


  results <- data.frame()

  for(i in 1:nlyr(r)) {
    # Extract single layer
    nc <- ncdf4::nc_open(raster_files[i])
    times <- ncdf4::ncvar_get(nc, timeUnit)
    dates <- as.Date("1900-01-01") + times
    ncdf4::nc_close(nc)

    r_layer <- r[[i]]
    r_state <- mask(r_layer, vLyr)
    # Calculate statistics
    stats <- data.frame(
      date = dates,
      variable = variable_name,
      min = global(r_state, min, na.rm = T)[1, 1],
      mean = global(r_state, mean, na.rm = T)[1, 1],
      max = global(r_state, max, na.rm = T)[1, 1],
      sd = global(r_state, sd, na.rm = T)[1, 1]

    )

    results <- rbind(results, stats)
  }
 nc <- ncdf4::nc_open(raster_files[1])
    times <- ncdf4::ncvar_get(nc, "day")
dates <- as.Date("1900-01-01") + times
dates
