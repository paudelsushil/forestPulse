#' Extract Plot Numbers from PDF Filenames
#'
#' Finds PDF files in a directory and extracts numeric plot IDs from
#' filename patterns like "plot_12", "Plot-07", or "plot 3".
#'
#' @param folder_path Character path to the directory containing PDF files.
#'
#' @return Integer vector of unique plot numbers in ascending order.
#'   Returns `integer(0)` when no PDFs are found or no plot numbers match.
#' @export
extract_plot_numbers_from_pdfs <- function(folder_path) {
  if (!dir.exists(folder_path)) {
    stop(paste("Folder not found:", folder_path))
  }

  pdf_files <- list.files(
    path = folder_path,
    pattern = "\\.[Pp][Dd][Ff]$",
    full.names = FALSE,
    recursive = FALSE
  )

  if (length(pdf_files) == 0) {
    return(integer(0))
  }

  file_stems <- tools::file_path_sans_ext(pdf_files)

  pattern <- "(?i)plot\\s*[_-]?\\s*([0-9]+)"
  matches <- regexec(pattern, file_stems, perl = TRUE)
  captured <- regmatches(file_stems, matches)

  plot_numbers <- vapply(
    captured,
    FUN = function(x) {
      if (length(x) >= 2) as.integer(x[2]) else NA_integer_
    },
    FUN.VALUE = integer(1)
  )

  sort(unique(plot_numbers[!is.na(plot_numbers)]))
}

