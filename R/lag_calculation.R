calculate_outbreak_fire_lag <- function(data, outbreak_col, fire_col) {
  # Convert column names to strings if they're not already
  outbreak_col <- as.character(substitute(outbreak_col))
  fire_col <- as.character(substitute(fire_col))
  
  # Create a copy of the data
  result_df <- data
  
  # Calculate time lag for each row
  result_df$time_lag <- result_df[[fire_col]] - result_df[[outbreak_col]]
  
  # Create followed_by_fire column
  # TRUE if fire year is greater than outbreak year (positive lag)
  result_df$followed_by_fire <- ifelse(result_df$time_lag >=
   0, "Yes", "No")
  
  return(result_df)

}

# Example usage:
# Assuming your data frame is called 'df' with columns 'outbreak_year' and 'fire_year'
# new_df <- calculate_outbreak_fire_lag(df, outbreak_year, fire_year)
# Optional: Create histogram of time lags

