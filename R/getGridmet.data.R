getGridmet.data <- function(years, variable, save_dir = NULL){
  if(!is.numeric(years)){
    stop("Year must be a numeric value greater or equals to 1979")
  }

  if(!is.character(variable)){
    stop("Variable must be a character vector with at least one element")
  }

  if(is.null(save_dir)){
    save_dir <- getwd()
  } else{
    if(!dir.exists(save_dir)){
        dir.create(save_dir, recursive = T)
      }
    }

  base_url <- "https://www.northwestknowledge.net/metdata/data/"

  file.pattern <- function(var, year){
    paste0(var, "_", year, ".nc")
  }
  # Pattern of file
  # Process for multiple files
  # File download from internet sources

  for(year in years){
    file_pattern <- file.pattern(variable, year)

    file_url <- paste0(base_url, file_pattern)

    dest_path <- file.path(save_dir, file_pattern)

    file.download(file_url, dest_path)
  }

  return(paste0("Successfully downloaded all variables in ", dest_path))
}


