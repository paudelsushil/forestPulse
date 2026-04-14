# Load required packages
library(mgcv)      # For GAM modeling
library(dplyr)     # For data manipulation
library(caret)     # For data splitting and model evaluation
library(ggplot2)   # For visualization
library(ROCR)      # For ROC curves
library(spatialEco) # For spatial cross-validation if needed

#' Prepare and fit GAM model for fire prediction
#' @param data DataFrame containing response and predictor variables
#' @param response_col Name of binary response column (fire/non-fire)
#' @param coords_cols Names of coordinate columns if spatial CV is needed
#' @return List containing model and evaluation metrics
prepare_fire_gam <- function(data, response_col, coords_cols = NULL) {

  # 1. Data Preparation ----

  # Convert response to binary (0/1)
  data[[response_col]] <- as.numeric(as.factor(data[[response_col]])) - 1

  # List of predictor variables
  climate_vars <- c("tmmxM","tmmnM", "tmean", "prM", "vpd", "pet")
  topo_vars <- c("dem", "slpD", "aspD", "tpi", "twi")

  # Combine all predictors
  predictors <- c(climate_vars, topo_vars)

  # Check for missing values
  missing_data <- colSums(is.na(data[c(response_col, predictors)]))
  if(any(missing_data > 0)) {
    warning("Missing values detected in the following columns: ",
            paste(names(missing_data[missing_data > 0]), collapse = ", "))
  }

  # 2. Data Splitting ----

  if(!is.null(coords_cols)) {
    # Spatial cross-validation if coordinates are provided
    set.seed(123)
    spatial_folds <- spatial.kfold(data[coords_cols], k = 5)
    train_indices <- unlist(spatial_folds[-1])  # Use first 4 folds for training
  } else {
    # Random split if no coordinates provided
    set.seed(123)
    train_indices <- createDataPartition(data[[response_col]], p = 0.8, list = FALSE)
  }

  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]

  # 3. Model Formula Creation ----

  # Create GAM formula with smooth terms for each predictor
  # Using different k values for climate (more flexible) and topographic variables
  climate_terms <- paste0("s(", climate_vars, ", k=10)", collapse = " + ")
  topo_terms <- paste0("s(", topo_vars, ", k=5)", collapse = " + ")

  formula_str <- paste0(response_col, " ~ ", climate_terms, " + ", topo_terms)
  gam_formula <- as.formula(formula_str)

  # 4. Model Fitting ----

  # Fit GAM model with binomial family (logistic GAM)
  gam_model <- gam(gam_formula,
                   data = train_data,
                   family = binomial,
                   method = "REML",  # Restricted Maximum Likelihood
                   select = TRUE)    # Include variable selection

  # 5. Model Evaluation ----

  # Generate predictions for test set
  predictions <- predict(gam_model, newdata = test_data, type = "response")

  # Calculate various performance metrics
  pred_obj <- prediction(predictions, test_data[[response_col]])
  auc <- performance(pred_obj, "auc")@y.values[[1]]

  # Calculate optimal threshold using Youden's J statistic
  roc <- performance(pred_obj, "tpr", "fpr")
  optimal_idx <- which.max(roc@y.values[[1]] - roc@x.values[[1]])
  optimal_threshold <- roc@alpha.values[[1]][optimal_idx]

  # Convert predictions to binary using optimal threshold
  binary_preds <- ifelse(predictions >= optimal_threshold, 1, 0)

  # Create confusion matrix
  conf_matrix <- confusionMatrix(
    factor(binary_preds),
    factor(test_data[[response_col]]),
    positive = "1"
  )

  # 6. Variable Importance ----

  # Extract smooth term significance
  summary_gam <- summary(gam_model)
  var_importance <- data.frame(
    variable = row.names(summary_gam$s.table),
    p_value = summary_gam$s.table[,"p-value"]
  )

  # 7. Return Results ----

  return(list(
    model = gam_model,
    threshold = optimal_threshold,
    auc = auc,
    confusion_matrix = conf_matrix,
    variable_importance = var_importance,
    test_predictions = predictions,
    test_actual = test_data[[response_col]]
  ))
}

# Function to plot model diagnostics
plot_gam_diagnostics <- function(model_results) {
  # Plot 1: ROC Curve
  pred_obj <- prediction(model_results$test_predictions,
                         model_results$test_actual)
  roc <- performance(pred_obj, "tpr", "fpr")

  plot(roc, main = "ROC Curve", col = "blue")
  abline(0, 1, lty = 2)
  text(0.8, 0.2, paste("AUC =", round(model_results$auc, 3)))

  # Plot 2: Variable Effects
  par(mfrow = c(2, 2))
  plot(model_results$model, pages = 1)
  par(mfrow = c(1, 1))
}

# Example usage:
# Assuming your data frame is called 'fire_data'
# fire_data should contain columns:
# - fire_occurrence (0/1 or TRUE/FALSE)
# - climate variables (temperature, precipitation, vpd, wind_speed)
# - topographic variables (elevation, slope, aspect, tpi)
# - coordinates (optional, for spatial cross-validation)

# Example:
# model_results <- prepare_fire_gam(
#   data = fire_data,
#   response_col = "fire_occurrence",
#   coords_cols = c("longitude", "latitude")
# )

# Plot diagnostics
# plot_gam_diagnostics(model_results)

# Print summary
# summary(model_results$model)
# print(model_results$confusion_matrix)
# print(model_results$variable_importance)
