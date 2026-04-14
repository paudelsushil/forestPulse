# Kaiser_Meyer_Olkin test to measure sampling adequasy for each features

#' Measure Sampling Adequacy using Kaiser-Meyer-Olkin Test
#' @title Measure Sampling Adequacy
#' @param data Input dataframe or data.table
#' @param cols Optional vector of column names to analyze
#' @param scale Whether to standardize variables before analysis
#' @param cor_threshold Threshold for identifying highly correlated features
#' @param output_dir Directory to save output tables (NULL = don't save)
#' @param file_prefix Prefix for output files
#' @return List containing KMO results and interpretation
#' @export
measure_sampling_adequacy <- function(data, cols = NULL, scale = TRUE, 
                                   cor_threshold = 0.9,
                                   output_dir = NULL, 
                                   file_prefix = "kmo") {
  # Ensure psych package is available
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Package 'psych' is required. Please install it.")
  }
  
  # Input validation
  if (!is.data.frame(data) && !data.table::is.data.table(data)) {
    stop("Input must be a data frame or data.table")
  }
  
  # Convert data.table to data.frame if necessary
  is_dt <- data.table::is.data.table(data)
  if (is_dt) {
    data <- as.data.frame(data)
  }
  
  # Get numeric columns
  if (is.null(cols)) {
    numeric_cols <- names(data)[sapply(data, function(x) is.numeric(x) && !is.factor(x))]
  } else {
    numeric_cols <- cols[sapply(data[, cols, drop = FALSE], function(x) is.numeric(x) && !is.factor(x))]
  }
  
  if (length(numeric_cols) < 2) {
    stop("Need at least 2 numeric columns for KMO analysis")
  }
  
  # Extract numeric data
  numeric_data <- data[, numeric_cols, drop = FALSE]
  
  # Remove zero variance columns
  var_cols <- apply(numeric_data, 2, var, na.rm = TRUE) > 0
  if (sum(!var_cols) > 0) {
    warning(sprintf("Removed %d zero-variance columns: %s", 
                   sum(!var_cols), 
                   paste(names(var_cols)[!var_cols], collapse=", ")))
  }
  numeric_data <- numeric_data[, var_cols, drop = FALSE]
  
  # Handle NA values
  complete_cases <- complete.cases(numeric_data)
  if (sum(!complete_cases) > 0) {
    warning(sprintf("Removed %d rows with missing values", sum(!complete_cases)))
  }
  numeric_data <- numeric_data[complete_cases, , drop = FALSE]
  
  # Scale the data if requested
  if (scale) {
    numeric_data <- scale(numeric_data)
  }
  
  # Check for highly correlated features
  cor_matrix <- cor(numeric_data)
  diag(cor_matrix) <- 0
  high_cor <- which(abs(cor_matrix) > cor_threshold, arr.ind = TRUE)
  if (nrow(high_cor) > 0) {
    high_cor_pairs <- data.frame(
      var1 = rownames(cor_matrix)[high_cor[,1]],
      var2 = colnames(cor_matrix)[high_cor[,2]],
      correlation = cor_matrix[high_cor]
    )
    warning("Found highly correlated features which may inflate MSA values")
    high_cor_vars <- unique(c(high_cor_pairs$var1, high_cor_pairs$var2))
  } else {
    high_cor_pairs <- NULL
    high_cor_vars <- NULL
  }
  
  # Perform KMO test with error handling
  kmo_result <- tryCatch({
    psych::KMO(numeric_data)
  }, error = function(e) {
    warning("KMO calculation failed: ", e$message)
    return(NULL)
  })
  
  if (is.null(kmo_result)) {
    return(list(
      error = "KMO calculation failed - correlation matrix may be singular",
      data_summary = list(
        n_variables = ncol(numeric_data),
        n_complete_cases = nrow(numeric_data)
      )
    ))
  }
  
  # Create interpretation
  interpret_kmo <- function(value) {
    if (value >= 0.90) return("Marvelous")
    if (value >= 0.80) return("Meritorious") 
    if (value >= 0.70) return("Middling")
    if (value >= 0.60) return("Mediocre")
    if (value >= 0.50) return("Miserable")
    return("Unacceptable")
  }
  
  # Identify variables with low MSA
  msa_values <- kmo_result$MSAi
  low_msa <- names(msa_values[msa_values < 0.5])
  
  # Create MSA data frame
  msa_df <- data.frame(
    Variable = names(msa_values),
    MSA = msa_values,
    Quality = sapply(msa_values, interpret_kmo)
  )
  msa_df <- msa_df[order(-msa_df$MSA), ]
  
  # Create summary data frame
  summary_df <- data.frame(
    Metric = c("Overall MSA", "Interpretation", "Variables Analyzed", 
              "Complete Cases", "Low MSA Variables"),
    Value = c(
      sprintf("%.3f", kmo_result$MSA),
      interpret_kmo(kmo_result$MSA),
      as.character(ncol(numeric_data)),
      as.character(nrow(numeric_data)),
      ifelse(length(low_msa) > 0, paste(low_msa, collapse=", "), "None")
    )
  )
  
  # Export tables if directory is specified
  if (!is.null(output_dir)) {
    # Create directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Export MSA values
    write.csv(msa_df, 
             file.path(output_dir, paste0(file_prefix, "_individual_msa.csv")),
             row.names = FALSE)
    
    # Export summary
    write.csv(summary_df,
             file.path(output_dir, paste0(file_prefix, "_summary.csv")),
             row.names = FALSE)
    
    # Export correlated variables if any
    if (!is.null(high_cor_pairs)) {
      high_cor_pairs <- high_cor_pairs[order(-abs(high_cor_pairs$correlation)), ]
      write.csv(high_cor_pairs,
               file.path(output_dir, paste0(file_prefix, "_high_correlations.csv")),
               row.names = FALSE)
    }
    
    # Create HTML tables with kableExtra if available
    if (requireNamespace("kableExtra", quietly = TRUE)) {
      # Individual MSA HTML table
      msa_html <- knitr::kable(msa_df, format = "html", digits = 3,
                             caption = "Individual MSA Values") %>% 
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
      
      writeLines(msa_html,
                file.path(output_dir, paste0(file_prefix, "_individual_msa.html")))
      
      # Summary HTML table
      summary_html <- knitr::kable(summary_df, format = "html",
                                 caption = "KMO Analysis Summary") %>% 
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
      
      writeLines(summary_html,
                file.path(output_dir, paste0(file_prefix, "_summary.html")))
      
      # Correlated variables HTML table
      if (!is.null(high_cor_pairs)) {
        cor_html <- knitr::kable(high_cor_pairs, format = "html", digits = 3,
                               caption = "Highly Correlated Variables") %>% 
          kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
        
        writeLines(cor_html,
                  file.path(output_dir, paste0(file_prefix, "_high_correlations.html")))
      }
    }
  }
  
  # Return results
  results <- list(
    kmo_test = kmo_result,
    overall_msa = kmo_result$MSA,
    interpretation = interpret_kmo(kmo_result$MSA),
    individual_msa = sort(msa_values, decreasing = TRUE),
    msa_df = msa_df,
    summary_df = summary_df,
    low_msa_vars = low_msa,
    high_cor_pairs = high_cor_pairs,
    high_cor_vars = high_cor_vars,
    n_variables = ncol(numeric_data),
    n_complete_cases = nrow(numeric_data)
  )
  
  return(results)
}

# Example usage:
# For your fire/non-fire dataset
kmo_results_fnf <- measure_sampling_adequacy(data_FNF, predictors, 
output_dir = file.path(tbl_src, "msa_results"),
scale = TRUE, cor_threshold = 0.9, file_prefix = "fnf_model")

# Print summary


# For severity dataset, only analyzing specific columns
kmo_results_severity <- measure_sampling_adequacy(data_mtbsI, 
                                       cols = predictors,
                                       scale = TRUE,
                                       cor_threshold = 0.9, 
                                       output_dir = file.path(tbl_src, "msa_results"),
                                       file_prefix = "severity_model")
# Print summary
print(kmo_results_severity)


