getPrismDataset <- function(indices = "pdsi",
                            start_year = 1979,
                            end_year = 2023,
                            base_path = NULL) {

  # Input validation
  if (is.null(base_path)) {
    stop("Please provide a base_path for downloading files")
  }

  valid_indices <- c("pdsi", "pzi", "scpdsi")
  if (!all(indices %in% valid_indices)) {
    invalid_indices <- indices[!indices %in% valid_indices]
    stop(sprintf("Invalid indices specified: %s\nUse any combination of 'pdsi', 'pzi', or 'scpdsi'",
                 paste(invalid_indices, collapse = ", ")))
  }

  # Generate dates
  years <- start_year:end_year
  months <- sprintf("%1d", 1:12)
  dates <- expand.grid(years, months)

  # Create date index
  date_index <- apply(dates, 1, function(date) {
    paste(date[1], date[2], sep = "_")
  })

  # Base URLs for different indices
  base_urls <- list(
    pdsi = "http://www.wrcc.dri.edu/wwdt/data/PRISM/pdsi/",
    pzi = "http://www.wrcc.dri.edu/wwdt/data/PRISM/pzi/",
    scpdsi = "http://www.wrcc.dri.edu/wwdt/data/PRISM/scpdsi/"
  )

  # Process each climate index
  for (index in indices) {
    # Get the correct base URL for this index
    base_url <- base_urls[[index]]

    # Create database names
    database <- paste0(index, "_", date_index, "_PRISM.nc")
    urls <- paste0(base_url, database)

    # Create directory for each index
    loc <- file.path(base_path, index)
    if (!dir.exists(loc)) {
      dir.create(loc, showWarnings = F, recursive = T)
    }

    # Download files
    message(paste("Downloading", index, "files..."))
    for (url in urls) {
      destfile <- file.path(loc, basename(url))
      if (!file.exists(destfile)) {
        tryCatch(
          {
            message(paste("Downloading:", basename(url)))
            curl::multi_download(
              url,
              destfile = destfile,
              resume = T,
              progress = T,
              timeout = Inf
            )
          },
          error = function(e) {
            message(paste("Error downloading", url, ":", e))
          }
        )
      } else {
        message(paste(basename(destfile), "already exists. Skipping download."))
      }
    }
  }

  message("Download process completed!")
}
