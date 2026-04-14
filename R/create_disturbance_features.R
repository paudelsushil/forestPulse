#' Create Binary Features for MPB and Fuel Lag Classes
#' @title Create Binary Features for Disturbance Classes
#' @param data Input dataframe
#' @param mpb_lag Column name for MPB lag
#' @param fuel_lag Column name for fuel lag
#' @return Dataframe with binary features for each class
#' @export
create_disturbance_features <- function(data, mpb_lag = "MPB_lag", fuel_lag = "fuel_lag") {
  # Input validation
  if (!all(c(mpb_lag, fuel_lag) %in% names(data))) {
    stop("MPB_lag or fuel_lag columns not found in data")
  }
  
  # Create MPB stage binary features
  data <- data %>%
    mutate(
      mpb_red = factor(ifelse(!!sym(mpb_lag) >= 0 & !!sym(mpb_lag) <= 3, 1, 0), levels = c(0, 1)),
      mpb_young_gray = factor(ifelse(!!sym(mpb_lag) >= 4 & !!sym(mpb_lag) <= 6, 1, 0), levels = c(0, 1)),
      mpb_old_gray = factor(ifelse(!!sym(mpb_lag) >= 7 & !!sym(mpb_lag) <= 14, 1, 0), levels = c(0, 1)),
      mpb_old = factor(ifelse(!!sym(mpb_lag) > 14, 1, 0), levels = c(0, 1))
    )
  
  # Create fuel management binary features
  data <- data %>%
    mutate(
      fuel_0_5 = factor(ifelse(!!sym(fuel_lag) >= 0 & !!sym(fuel_lag) <= 5, 1, 0), levels = c(0, 1)),
      fuel_6_10 = factor(ifelse(!!sym(fuel_lag) >= 6 & !!sym(fuel_lag) <= 10, 1, 0), levels = c(0, 1)),
      fuel_11_15 = factor(ifelse(!!sym(fuel_lag) >= 11 & !!sym(fuel_lag) <= 15, 1, 0), levels = c(0, 1)),
      fuel_gt_15 = factor(ifelse(!!sym(fuel_lag) >= 16, 1, 0), levels = c(0, 1))
    )
  
  # Print class distributions
  mpb_cols <- c("mpb_red", "mpb_young_gray", "mpb_old_gray", "mpb_old")
  fuel_cols <- c("fuel_0_5", "fuel_6_10", "fuel_11_15", "fuel_gt_15")
  
  message("MPB class distributions:")
  for(col in mpb_cols) {
    dist <- table(data[[col]])
    message(sprintf("%s: 0=%d, 1=%d", col, dist["0"], dist["1"]))
  }
  
  message("\nFuel class distributions:")
  for(col in fuel_cols) {
    dist <- table(data[[col]])
    message(sprintf("%s: 0=%d, 1=%d", col, dist["0"], dist["1"]))
  }
  
  return(data)
}




