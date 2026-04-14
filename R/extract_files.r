#' Batch Extract and Rename TIFs from ZIP files
#'
#' This function iterates through all .zip files in a specified directory,
#' extracts a single target .tif file from each, renames that .tif file
#' to match its parent .zip file, and saves it to a single output directory.
#'
#' @param zip_dir A string path to the folder *containing* all the .zip files.
#' @param output_dir A string path to the folder where all extracted .tif files
#'   will be saved. This folder will be created if it doesn't exist.
#' @param file_to_extract A string path *inside* the zip file to extract.
#'   Example: "odm_orthophoto/odm_orthophoto.tif".
#'
#' @return This function is called for its side effects (extracting and
#'   renaming files) and returns \code{invisible(NULL)}.

extract_and_rename_tifs <- function(zip_dir, 
                                    output_dir, 
                                    file_to_extract = "odm_orthophoto/odm_orthophoto.tif") {
  
  # --- 1. Setup & Validation ---
  
  # Create the output directory if it doesn't already exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message(paste("Created output directory:", output_dir))
  }
  
  # Get a list of all files ending in .zip in your directory
  zip_files_list <- list.files(
    path = zip_dir,
    pattern = "\\.zip$", # Regex to find files ending in .zip
    full.names = TRUE,   # Get the full path
    recursive = FALSE    # Don't look in subfolders
  )
  
  if (length(zip_files_list) == 0) {
    stop("No .zip files found. Please check your 'zip_dir' path: ", zip_dir)
  }
  
  message(paste("Found", length(zip_files_list), "zip files to process..."))
  
  # Get the original name of the file we're extracting (e.g., "odm_orthophoto.tif")
  original_tif_name <- basename(file_to_extract)
  
  
  # --- 2. Loop, Extract, and Rename ---
  
  message("Starting extraction process...")
  
  # Loop over each file in the zip_files_list
  # We use invisible() to suppress the NULL list output from lapply
  invisible(lapply(zip_files_list, function(current_zip_path) {
    
    # Use tryCatch to handle errors gracefully
    tryCatch({
      
      # --- a. Define new name ---
      zip_basename_no_ext <- tools::file_path_sans_ext(basename(current_zip_path))
      new_tif_filename <- paste0(zip_basename_no_ext, ".tif")
      
      # --- b. Extract the file ---
      utils::unzip(
        zipfile = current_zip_path,
        files = file_to_extract,
        exdir = output_dir,
        junkpaths = TRUE # This strips the internal folder path
      )
      
      # --- c. Rename the extracted file ---
      path_before_rename <- file.path(output_dir, original_tif_name)
      path_after_rename <- file.path(output_dir, new_tif_filename)
      
      # Check if the file was actually extracted before trying to rename
      if (file.exists(path_before_rename)) {
        file.rename(from = path_before_rename, to = path_after_rename)
        # Print a success message
        message(paste("SUCCESS:", basename(current_zip_path), "->", new_tif_filename))
      } else {
        # This can happen if 'file_to_extract' was not found in the zip
        stop(paste("Target file '", file_to_extract, "' not found in zip."), sep = "")
      }
      
    }, error = function(e) {
      
      # Print an error message if anything went wrong for this file
      warning(paste("!!! ERROR processing", basename(current_zip_path), ":", e$message))
      
    }) # end tryCatch
    
  })) # end lapply
  
  message("--- Batch extraction complete! ---")
  invisible(NULL)
  
} 

# Example usag