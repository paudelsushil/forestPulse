#' Prepare data for decision tree modeling
#' @param data Input dataframe
#' @param predictors Vector of predictor names
#' @param response_var Response variable name
#' @return Cleaned and prepared dataframe
prepare_tree_data <- function(data, predictors, response_var) {
    # Convert binary factors to explicit 0/1 factors
    binary_vars <- names(data)[sapply(data, function(x) 
        is.factor(x) && all(levels(x) %in% c("0", "1")))]
    
    model_data <- data %>%
        # Select needed columns
        dplyr::select(all_of(c(response_var, predictors))) %>%
        # Ensure binary factors are properly coded
        mutate(across(all_of(binary_vars), ~factor(as.numeric(as.character(.)), 
                                                  levels = c(0, 1)))) %>%
        # Remove any NA rows
        na.omit()
    
    # Verify factor levels
    factor_cols <- names(model_data)[sapply(model_data, is.factor)]
    for(col in factor_cols) {
        message(sprintf("Levels in %s: %s", 
                       col, 
                       paste(levels(model_data[[col]]), collapse = ", ")))
    }
    
    # Print data dimensions
    message(sprintf("Final data dimensions: %d rows, %d columns", 
                   nrow(model_data), 
                   ncol(model_data)))
    
    return(model_data)
}



