#' Get CPU Information
#'
#' @description Retrieves logical and physical CPU core counts and a suggested
#'   worker count for parallel processing.
#'
#' @return A list with `logical_processors`, `physical_cores`, and
#'   `recommended_workers`.
get_cpu_info <- function() {
  if (.Platform$OS.type == "windows") {
    # Windows-specific command
    system_info <- system("wmic cpu get NumberOfLogicalProcessors", intern = TRUE)
    n_logical <- as.numeric(system_info[2])
    
    system_info <- system("wmic cpu get NumberOfCores", intern = TRUE)
    n_physical <- as.numeric(system_info[2])
    
    return(list(
      logical_processors = n_logical,
      physical_cores = n_physical,
      recommended_workers = max(1, n_logical - 1)
    ))
  } else {
    # Unix-like systems
    n_logical <- parallel::detectCores(logical = TRUE)
    n_physical <- parallel::detectCores(logical = FALSE)
    
    return(list(
      logical_processors = n_logical,
      physical_cores = n_physical,
      recommended_workers = max(1, n_logical - 1)
    ))
  }
}

