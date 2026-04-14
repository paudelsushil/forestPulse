create_balanced_strata <- function(samples, strata_cols, n_per_stratum = NULL) {
  # Calculate minimum stratum size if n_per_stratum not specified
  if(is.null(n_per_stratum)) {
    counts <- lapply(strata_cols, function(col) table(samples[[col]]))
    n_per_stratum <- min(unlist(counts)) 
  }
  
  # Balance each stratum
  balanced_samples <- samples
  for(col in strata_cols) {
    strata_levels <- unique(samples[[col]])
    
    # Sample within each stratum
    strata_samples <- lapply(strata_levels, function(level) {
      stratum_data <- samples[samples[[col]] == level,]
      
      # Ensure unique fires are maintained
      fire_ids <- unique(stratum_data$fire_id)
      sampled_fires <- sample(fire_ids, 
                            size = min(length(fire_ids), n_per_stratum))
      
      stratum_data[stratum_data$fire_id %in% sampled_fires,]
    })
    
    balanced_samples <- do.call(rbind, strata_samples)
  }
  
  return(balanced_samples)
}

# Example usage:
# if(FALSE) {
#   set.seed(123)
  
#   # Create balanced samples
#   balanced_samples <- create_balanced_samples(
#     samples = fireSamples,
#     strata_cols = c("outbreak", "fuelmgmt"),
#     n_per_stratum = 1000
#   )
  
#   # Validate results
#   lapply(c("outbreak", "fuelmgmt"), function(col) {
#     print(paste("Distribution for", col))
#     print(table(balanced_samples[[col]]))
#   })
# }



