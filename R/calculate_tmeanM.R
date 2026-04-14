calculate_tmeanM <- function(tmmnMList, tmmxMList, outputDir = F){
  if(outputDir == F){
    outputDir <- here::here(paste0(indices))
    if(!dir.exists(outputDir)){
      dir.create(outputDir)
    }
  }
  tmmnM <- terra::rast(tmmnMList)
  tmmxM <- terra::rast(tmmxMList)

  dates <- terra::time(tmmnM)
  years <- unique(lubridate::year(dates))
  months <- unique(format(dates, "%Y-%m-01"))

  tmeanYrs <- lapply(years, function(yr){
    year_months <- months[format(as.Date(months), "%Y") == yr]

    # Fixed: Changed months to year_months in the inner lapply
    tmeanMnth <- lapply(year_months, function(mnth){
      dateIndex <- format(dates, "%Y-%m-01") == mnth
      tmmn_m <- tmmnM[[dateIndex]]
      tmmx_m <- tmmxM[[dateIndex]]
      mean <- terra::mean((tmmn_m + tmmx_m) / 2, na.rm = TRUE)
      terra::time(mean) <- as.Date(mnth)
      return(mean)  # Fixed: Added return statement
    })

    # Combine all months for the year into one SpatRaster
    year_stack <- terra::rast(tmeanMnth)

    # Create filename with year
    filename <- file.path(outputDir,
                          paste0("tmeanM_", yr, ".nc"))

    # Set time dimension properly
    terra::time(year_stack) <- as.Date(year_months)

    # Fixed: Changed 'mean' to 'year_stack' for variable attributes
    terra::varnames(year_stack) <- "mean_temperature"
    terra::longnames(year_stack) <- "Monthly Mean Temperature"
    terra::units(year_stack) <- "degrees_celsius"


    # Fixed: Changed 'mean' to 'year_stack' for writing NetCDF
    terra::writeCDF(year_stack,
                    filename = filename,
                    overwrite = TRUE,
                    compression = 4,
                    zname = "time"
                    )

    return(year_stack)
  })

  return(tmeanYrs)
}
