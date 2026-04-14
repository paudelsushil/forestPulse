#' Get Optimal Number of Workers
#' @title Calculate Optimal Number of Workers
#' @description Determines an optimal number of parallel workers based on CPU
#'   and RAM availability.
#' @param min_ram_per_worker Minimum RAM required per worker in GB (default: 2)
#' @param max_ram_percent Maximum percent of total RAM to use (default: 75)
#' @return Integer indicating optimal number of workers
get_optimal_workers <- function(min_ram_per_worker = 2, max_ram_percent = 50) {
  # Get system info
  cpu_info <- get_cpu_info()
  ram_info <- get_ram_info()
  
  # Calculate workers based on CPU
  cpu_workers <- cpu_info$logical_processors - 1
  
  # Calculate workers based on RAM
  available_ram <- ram_info$total_ram_gb * (max_ram_percent / 100)
  ram_workers <- floor(available_ram / min_ram_per_worker)
  
  # Use the minimum of CPU and RAM-based worker counts
  optimal_workers <- min(cpu_workers, ram_workers)
  
  # Ensure at least 1 worker and no more than 32
  optimal_workers <- min(max(1, optimal_workers), 32)
  
  # Print diagnostics
  message(sprintf("System configuration:"))
  message(sprintf("- CPU cores: %d logical, %d physical", 
                 cpu_info$logical_processors, 
                 cpu_info$physical_cores))
  message(sprintf("- RAM: %.1f GB total, %.1f GB available", 
                 ram_info$total_ram_gb,
                 ram_info$free_ram_gb))
  message(sprintf("Optimal workers: %d (limited by %s)", 
                 optimal_workers,
                 if(cpu_workers < ram_workers) "CPU" else "RAM"))
  
  return(optimal_workers)
}

# Example usage:
# get_optimal_workers(
#     min_ram_per_worker = 3,    # Minimum 2GB RAM per worker
#     max_ram_percent = 75       # Use up to 75% of total RAM
#   )
