#' Get System RAM Information
#' @title Get RAM Information
#' @description Retrieves current system RAM statistics including total, free and used memory
#' @return List containing RAM information in GB
#' @export
get_ram_info <- function() {
  tryCatch({
    if (.Platform$OS.type == "windows") {
      # Use more reliable Windows command
      mem_info <- system2("powershell", 
                         args = c("-command",
                                "[Math]::Round((Get-CimInstance Win32_OperatingSystem).TotalVisibleMemorySize/1MB, 2);",
                                "[Math]::Round((Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory/1MB, 2)"),
                         stdout = TRUE)
      
      # Convert to numeric values
      total_mem <- as.numeric(mem_info[1])
      free_mem <- as.numeric(mem_info[2])
      
      if (is.na(total_mem) || is.na(free_mem)) {
        stop("Invalid memory values retrieved")
      }
      
      list(
        total_ram_gb = total_mem,
        free_ram_gb = free_mem,
        used_ram_gb = total_mem - free_mem
      )
      
    } else {
      stop("System not supported")
    }
  }, error = function(e) {
    warning(sprintf("Failed to get RAM info: %s", e$message))
    list(
      total_ram_gb = NA_real_,
      free_ram_gb = NA_real_,
      used_ram_gb = NA_real_
    )
  })
}

# Example usage:
# get_ram_info()

