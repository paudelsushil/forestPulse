# Load required packages
library(xgboost)
library(dplyr)
library(caret)
library(Matrix)
library(ROCR)
library(ggplot2)
library(tidyr)

#' Prepare and fit XGBoost model for fire prediction
#' @param data DataFrame containing response and predictor variables
#' @param response_col Name of binary response column (fire/non-fire)
#' @param coords_cols Names of coordinate columns if spatial CV is needed
#' @param tune_hyper Logical, whether to perform hyperparameter tuning
#' @return List containing model and evaluation metrics
#'
#'
prepare_fire_xgboost <- function(data, response_col, coords_cols = NULL,
                                 tune_hyper = TRUE) {

  # 1. Data Preparation ----

  # Convert response to numeric binary
  data[[response_col]] <- as.numeric(as.factor(data[[response_col]])) - 1

  # List of predictor variables (modify based on your data)
  climate_vars <- c("temperature", "precipitation", "vpd", "wind_speed")
  topo_vars <- c("elevation", "slope", "aspect", "tpi")
  predictors <- c(climate_vars, topo_vars)

  # Check for missing values
  missing_data <- colSums(is.na(data[c(response_col, predictors)]))
  if(any(missing_data > 0)) {
    warning("Missing values detected in: ",
            paste(names(missing_data[missing_data > 0]), collapse = ", "))
    # Fill missing values with median
    for(col in names(which(missing_data > 0))) {
      data[[col]] <- ifelse(is.na(data[[col]]),
                            median(data[[col]], na.rm = TRUE),
                            data[[col]])
    }
  }

  # 2. Data Splitting ----

  if(!is.null(coords_cols)) {
    # Spatial cross-validation
    set.seed(123)
    spatial_folds <- spatial.kfold(data[coords_cols], k = 5)
    train_indices <- unlist(spatial_folds[-1])
  } else {
    # Random split
    set.seed(123)
    train_indices <- createDataPartition(data[[response_col]], p = 0.8, list = FALSE)
  }

  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]

  # Create DMatrix objects
  dtrain <- xgb.DMatrix(data = as.matrix(train_data[predictors]),
                        label = train_data[[response_col]])
  dtest <- xgb.DMatrix(data = as.matrix(test_data[predictors]),
                       label = test_data[[response_col]])

  # 3. Hyperparameter Tuning ----

  if(tune_hyper) {
    # Define parameter grid
    param_grid <- expand.grid(
      max_depth = c(3, 5, 7),
      eta = c(0.01, 0.1, 0.3),
      subsample = c(0.7, 0.8, 0.9),
      colsample_bytree = c(0.7, 0.8, 0.9),
      min_child_weight = c(1, 3, 5),
      gamma = c(0, 0.1, 0.2)
    )

    # Perform cross-validation for each parameter combination
    cv_results <- apply(param_grid, 1, function(params) {
      set.seed(123)
      cv <- xgb.cv(
        params = list(
          objective = "binary:logistic",
          eval_metric = "auc",
          max_depth = params["max_depth"],
          eta = params["eta"],
          subsample = params["subsample"],
          colsample_bytree = params["colsample_bytree"],
          min_child_weight = params["min_child_weight"],
          gamma = params["gamma"]
        ),
        data = dtrain,
        nrounds = 100,
        nfold = 5,
        early_stopping_rounds = 10,
        verbose = 0
      )
      return(max(cv$evaluation_log$test_auc_mean))
    })

    # Get best parameters
    best_params <- param_grid[which.max(cv_results), ]
  } else {
    # Default parameters
    best_params <- list(
      max_depth = 5,
      eta = 0.1,
      subsample = 0.8,
      colsample_bytree = 0.8,
      min_child_weight = 1,
      gamma = 0
    )
  }

  # 4. Model Fitting ----

  # Train final model with best parameters
  xgb_model <- xgb.train(
    params = c(
      list(
        objective = "binary:logistic",
        eval_metric = "auc"
      ),
      best_params
    ),
    data = dtrain,
    nrounds = 100,
    watchlist = list(train = dtrain, test = dtest),
    early_stopping_rounds = 10,
    verbose = 0
  )

  # 5. Model Evaluation ----

  # Generate predictions
  predictions <- predict(xgb_model, dtest)

  # Calculate performance metrics
  pred_obj <- prediction(predictions, test_data[[response_col]])
  auc <- performance(pred_obj, "auc")@y.values[[1]]

  # Find optimal threshold
  roc <- performance(pred_obj, "tpr", "fpr")
  optimal_idx <- which.max(roc@y.values[[1]] - roc@x.values[[1]])
  optimal_threshold <- roc@alpha.values[[1]][optimal_idx]

  # Binary predictions
  binary_preds <- ifelse(predictions >= optimal_threshold, 1, 0)

  # Confusion matrix
  conf_matrix <- confusionMatrix(
    factor(binary_preds),
    factor(test_data[[response_col]]),
    positive = "1"
  )

  # 6. Feature Importance ----

  importance_matrix <- xgb.importance(
    feature_names = predictors,
    model = xgb_model
  )

  # 7. Return Results ----

  return(list(
    model = xgb_model,
    best_params = best_params,
    threshold = optimal_threshold,
    auc = auc,
    confusion_matrix = conf_matrix,
    feature_importance = importance_matrix,
    test_predictions = predictions,
    test_actual = test_data[[response_col]]
  ))
}

# Function to plot model diagnostics
plot_xgb_diagnostics <- function(model_results) {
  # Plot 1: ROC Curve
  par(mfrow = c(2, 2))

  pred_obj <- prediction(model_results$test_predictions,
                         model_results$test_actual)
  roc <- performance(pred_obj, "tpr", "fpr")
  plot(roc, main = "ROC Curve", col = "blue")
  abline(0, 1, lty = 2)
  text(0.8, 0.2, paste("AUC =", round(model_results$auc, 3)))

  # Plot 2: Feature Importance
  importance_data <- model_results$feature_importance
  barplot(importance_data$Gain,
          names.arg = importance_data$Feature,
          main = "Feature Importance",
          las = 2)

  # Plot 3: Precision-Recall Curve
  pr <- performance(pred_obj, "prec", "rec")
  plot(pr, main = "Precision-Recall Curve", col = "red")

  # Plot 4: Predicted Probabilities Distribution
  hist(model_results$test_predictions,
       main = "Prediction Distribution",
       xlab = "Predicted Probability",
       breaks = 30)

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
# model_results <- prepare_fire_xgboost(
#   data = fire_data,
#   response_col = "fire_occurrence",
#   coords_cols = c("longitude", "latitude"),
#   tune_hyper = TRUE
# )

# Plot diagnostics
# plot_xgb_diagnostics(model_results)

# Print summary
# print(model_results$confusion_matrix)
# print(model_results$feature_importance)
