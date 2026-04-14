# Hello, world!

#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

.onAttach <- function(libname, pkgname) {
  required_packages <- c("dplyr", "ggplot2", "sf", "terra","tmap", "curl",
                         "purrr", "assertthat","httr","tmap", "plotly","funModeling")  # List of required packages

  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (interactive() && length(missing_packages) > 0) {
    packageStartupMessage(
      "Optional packages not installed: ",
      paste(unique(missing_packages), collapse = ", ")
    )
  }
}
# plot the polygon map
map.polygon <- function(data, text, map_title){
  tm_shape(data) +
    tm_text(text, col = "grey") +
    tm_borders("blue", lwd = 1) +
    tm_scale_bar(position = c("left", "bottom")) +
    tm_compass(position = c("right", "TOP"))+
    tm_layout(title = map_title,
              title.position = c("center", "top"),
              title.color = "black")
}

# plot the point map
map.point <- function(data, map_title, baseLayer = NULL,
                      point_col = "red", point_size = 0.1,
                      poly_border_col = "black", poly_border_lwd = 0.5,
                      poly_fill_col = "gray90", baseLable = NULL) {

  # Check if 'data' is a valid sf object
  if (!is.null(baseLayer)) {
    map <- tm_shape(baseLayer) +
      tm_polygons() +
      tm_text(baseLable, col = "blue") +

      tm_shape(data) +
      tm_dots(col = point_col, size = point_size)

  } else {
    # Add point layer with customizable appearance
    map <- tm_shape(data) +
      tm_dots(col = point_col, size = point_size)
  }



  # Scale bar and compass
  map <- map +
    tm_scale_bar(position = c("left", "bottom")) +
    tm_compass(position = c("right", "top"))

  # Layout elements
  map <- map +
    tm_layout(title = map_title,
              title.position = c("center", "top"),
              title.color = "black",
              frame = TRUE,
              frame.lwd = 1,
              inner.margins = c(0.01, 0.01, 0.06, 0.01))

  return(map)
}


# save the map
map.save <- function(map, filename){
  tmap_save(map, file.path(fig_src, filename))
}

# create timeclass class id feature
# ==============================================================================
# createTimclass()

calculate.timeclass <- function(data,
                             date_column,
                             start_date,
                             end_date,
                             by,
                             types) {
    # First create start dates
    start_dates <- seq(from = start_date, 
                      to = end_date - by + 1, 
                      by = by)
    
    # Then create end dates
    end_dates <- start_dates + (by - 1)
    
    # Create case arguments
    case_args <- lapply(seq_along(start_dates), function(i) {
        quo(
            .data[[date_column]] >= !!start_dates[i] & 
            .data[[date_column]] <= !!end_dates[i] ~
            !!paste0(start_dates[i], "-", end_dates[i])
        )
    })
    
    # Create the new column
    new_col_name <- paste0(types, "By", by, "yrs")
    
    data %>%
        mutate(
            !!new_col_name := case_when(
                !!!case_args,
                TRUE ~ NA_character_
            )
        )
}



# ========================================================================================


# Categorize the fire severity based on below mentioned paper
# https://www.fs.usda.gov/rm/pubs_series/rmrs/gtr/rmrs_gtr164/rmrs_gtr164_13_land_assess.pdf
create.dnbrindex <- function(data, dnbr_column){
  data <- data %>% mutate(!!paste0("dnbr_class") := case_when(
    dnbr_column >= -500 & dnbr_column <= -101 ~ "Increased Greenness",
    dnbr_column > -101 & dnbr_column <=100 ~ "Unburned/Low",
    dnbr_column > 100  & dnbr_column < 270 ~ "Low",
    dnbr_column >= 270 & dnbr_column < 660 ~ "Moderate",
    dnbr_column >= 660 & dnbr_column <= 1300 ~ "High",
    TRUE ~ "NULL"

  ))
  return(data)
}

# Clean Outbreak datasets
clean.outbreaks <- function(region_shp){
  clean_outbreaks <- region_shp %>%
    rename_all(., tolower) %>%
    subset(!is.na(survey_year)) %>%
    dplyr::filter(dca_common_name %in% c( "ips engraver beetles",
                                          "Jeffrey pine beetle",
                                          "mountain pine beetle",
                                          "western pine beetle")) %>%
    select(region_id, damage_area_id, survey_year, host, dca_common_name,
           damage_type, acres, legacy_no_trees, legacy_severity, legacy_forest_type, shape) %>%
    arrange(survey_year)

  return(clean_outbreaks)

}

# # Create samples with in a given polygons
# create.sample <- function(data, cellsize, crs){
#   grids <- lapply(1:nrow(data), function(x){
#     gr <- st_make_grid(data[x,], cellsize = cellsize, what = "centers") %>%
#       st_as_sf(.)
#       return(gr)
#   })
#   grids_coord <- do.call(rbind, grids) %>% st_coordinates(.)
#   poly_id <- data.frame("poly_id" = 1:nrow(grids_coord))
#   sample <- cbind(poly_id, grids_coord) %>%
#     st_as_sf(., coords = c("X", "Y"), crs = crs)
#
#   return(sample)
# }


# ------------------------------------------------------------------------------
# Function to load timeseries data and update layers time and name
# ------------------------------------------------------------------------------
create.nametime <- function(clim_var_src, start_date, end_date, pattern = NULL, by, var_name){

  if(is.character(clim_var_src) && file.exists(clim_var_src) && !is.null(pattern) ){
    clim_var <- rast(dir(clim_var_src, pattern = pattern, full.names = TRUE))

  }else if(inherits(clim_var_src, "SpatRaster")){
    clim_var = clim_var_src
    }else {
    stop("Invalid input: clim_var_src must be a valid file path or a SpatRaster object.")
  }

  # Create date sequence
  if(by == "month"){
    date_seq <- seq(
      from = as.Date(paste0(start_date, "-01-01")),
      to = as.Date(paste0(end_date, "-12-01")),
      by = "month"
    )
  } else if(by == "day") {
    date_seq <- seq(from = as.Date(start_date),
                    to = as.Date(end_date), by = "day")
  } else if(by == "year") {
    date_seq <- seq(from = as.Date(paste0(start_date, "-01-01")),
                     to = as.Date(paste0(end_date, "-01-01")), by = "year")
  } else {
    stop("Only 'year', 'month', and 'day' is supported.")
  }

  # Set layer names as variable names along with the date
  layer_names <- paste0(var_name, "_", date_seq)
  names(clim_var) <- layer_names

  if(terra::has.time(clim_var) && length(time(clim_var)) == nlyr(clim_var)){
    print("Raster already has valid time dimension. No changes made.")
  } else{
    if (length(date_seq) != nlyr(clim_var)) {
      stop(paste0("Number of layers (", nlyr(clim_var), ") does not match the number of dates (", length(date_seq), ")."))
    }
  # Create a time dimension for each layers
    terra::time(clim_var) <- date_seq
  }
  print("Time dimension is added to the layers.")

  return(clim_var)
}
nlyr
# ##############################################################################
# Extract Values by features with respect to time

extract.valuesbyTime <- function(rasters, sf_layers, date.column){

  fireMnth <- sort(unique(sf_layers[[date.column]]))

  layer_index <- sapply(fireMnth, function(m){
    raster_index <- which(terra::time(rasters) == m)
  })

  matched_layers <- rasters[[layer_index]]

  layer_values <- lapply(1:nrow(sf_layers), function(r){

    pb <- txtProgressBar(min = 0, max = nrow(sf_layers), style = 3)
    setTxtProgressBar(pb, r)

    val <- terra::extract(matched_layers,
                          sf_layers[r, ],
                          method = "simple",
                          lmtbsI_va,
                          bind = T)

    return(val)
    close(pb)

    })

  # layer_values <- do.call(rbind, layer_values)
  # layer_values <- sf::st_as_sf(layer_values)

  return(layer_values)

}

################################################################################
# Handling missing values
replace.values2na <- function(data, values, pattern, output.file = FALSE,
                              output.location = NULL, keep_range = NULL){
  library(foreach)
  library(doParallel)

  if(is.character(data) && file.exists(data)){
    raster <- terra::rast(dir(data, pattern = pattern, full.names = TRUE))

  }else if(inherits(data, "SpatRaster")){
    raster = data
  }else {
    stop("Invalid input: clim_var_src must be a valid file path or a SpatRaster object.")
  }
  # Extract the raster values
  raster_values <- terra::values(raster)

  cl <- parallel::makeCluster(4)
  doParallel::registerDoParallel(cl)

  # Apply the value replacement in parallel on the extracted values
  raster_values_new <- foreach(v = values, .combine = 'c',
                               .errorhandling = 'pass',
                               .export = 'raster_values') %dopar% {
    tryCatch({
      raster_values[raster_values == v] <- NA
      raster_values # Return the modified values
    }, error = function(e) {
      message(paste("Error replacing value", v, ":", e))
      return(raster_values) # Return the original values if an error occurs
    })
  }
  parallel::stopCluster(cl)

  terra::values(raster) <- raster_values_new

  if (output.file && is.character(output.location)) {
    terra::writeRaster(raster, filename = output.location, overwrite = TRUE)
    message(paste0("Raster saved to: ", output.location))
    } else if (output.file && !is.character(output.location)) {
    warning("Please enter a valid output location as a character string.")
      } else {
    message("Raster not saved.")
        }

  return(raster)  # Always return the modified raster
}

#===============================================================================
# Returns sf class point features
get.centroid <- function(raster, xy = FALSE){
  centers <- terra::xyFromCell((raster), 1:ncell(raster)) %>%
    terra::vect(., type = "points", crs = "epsg:5070") %>%
    st_as_sf(.) %>%
    mutate(plot_id = 1:nrow(.))



  if(xy == TRUE){
    xy <- centers %>%
          mutate(X = sf::st_coordinates(.)[1],
                 Y = sf::st_coordinates(.)[2])
    return(xy)
  }else{
    return(centers)

  }

}

# ==============================================================================
calculate.seasonal <- function(input_raster, output_raster, start_date, end_date, var_name){
  # Load the raster stack
  temperature_stack <- input_raster

  # Define seasons and corresponding months
  seasons <- list(
    DJF = c(12, 1, 2),
    MAM = c(3, 4, 5),
    JJA = c(6, 7, 8),
    SON = c(9, 10, 11)
  )

  # Initialize a list to store the seasonal rasters
  seasonal_rasters <- list()

  # Loop through each year and season
  for (year in start_date:end_date) {
    for (season in names(seasons)) {
      months <- seasons[[season]]

      # Calculate the correct indices for the months in the stack
      indices <- sapply(months, function(m) {
        if (m == 12) {
          return(12 + (year - start_date) * 12)
        } else {
          return(m + (year - start_date) * 12)
        }
      })

      # Ensure indices are within the range of the stack layers
      indices <- indices[indices <= nlyr(temperature_stack)]

      # Extract the seasonal data
      seasonal_data <- temperature_stack[[indices]]

      # Calculate the maximum temperature for the season
      max_temp <- app(seasonal_data, fun = mean, na.rm = TRUE)

      # Store the result with an appropriate name
      layer_name <- paste0(var_name,"_", year,"_", season)
      seasonal_rasters[[layer_name]] <- max_temp
    }
  }

  # Create a raster stack from the seasonal raster layers
  seasonal_stack <- rast(seasonal_rasters)

  # Write the raster stack to a file
  writeRaster(seasonal_stack, filename = output_raster, overwrite = TRUE)

  return(seasonal_stack)
}

# ==============================================================================
calculate.annual <- function(input_raster, output_raster, start_date, end_date, var_name){
  if(is.character(input_raster) && file.exists(input_raster)){
    raster <- rast(dir(input_raster, full.names = TRUE))

  }else if(inherits(input_raster, "SpatRaster")){
    raster = input_raster
  }else {
    stop("Invalid input: 'input_raster' must be a valid file path or a SpatRaster object.")
  }
  # Initialize a list to store the seasonal rasters
  annual_rasters <- list()

  years <- seq.Date(from = as.Date(start_date),
                    to = as.Date(end_date), by = "year")

  # Loop through each year and season
  for (year in years) {
    indices <- grep(paste0(var_name, "_", year), names(raster))

    if(length(indices) > 0){
      annual_data <- raster[[indices]]
      # Calculate the values for the rasters
      values <- app(annual_data, fun = mean, na.rm = TRUE)
      # Store the result with an appropriate name
      layer_name <- paste0(var_name,"_", year)
      annual_rasters[[layer_name]] <- values
    }else {
      print(paste("No data found for year:", year)) # Inform user about missing year
      }
  }
  if(length(annual_rasters) > 0){
    # Create a raster stack from the seasonal raster layers
    annual_stack <- rast(annual_rasters)
    names(annual_stack) <- paste0(var_name, "_mean_", year)
    # Write the raster stack to a file
    writeRaster(annual_stack, filename = output_raster, overwrite = TRUE)

  } else{
    warning("No annual rasters calculated")
    return(NULL)
  }
  return(annual_stack)
}




# Plot Seasonal ================================================================
values.seasonal <- function(input_raster, start_date, end_date){
  results <- data.frame(Year = integer(), Season = character(), mean_temp = numeric())

  # Define seasons and corresponding months
  seasons <- list(
    DJF = c(12, 1, 2),
    MAM = c(3, 4, 5),
    JJA = c(6, 7, 8),
    SON = c(9, 10, 11)
  )

  # Initialize a list to store the seasonal rasters
  seasonal_rasters <- list()

  # Loop through each year and season
  for (year in start_date:end_date) {
    for (season in names(seasons)) {
      months <- seasons[[season]]

      # Calculate the correct indices for the months in the stack
      indices <- sapply(months, function(m) {
        if (m == 12) {
          return(12 + (year - start_date) * 12)
        } else {
          return(m + (year - start_date) * 12)
        }
      })

      # Ensure indices are within the range of the stack layers
      indices <- indices[indices <= nlyr(temperature_stack)]

      # Extract the seasonal data
      seasonal_data <- temperature_stack[[indices]]

      # Calculate the maximum temperature for the season
      temp <- app(seasonal_data, fun = mean, na.rm = TRUE)-272.15

      # Store the results
      results <- rbind(results, data.frame(Year = year, Season = season, mean_temp = temp))
    }
  }
}

# Make a chunk of spatial data or just a table.
makeChunks <- function(data, chunk_size){
  chunks <- ceiling(nrow(data) / chunk_size)
  data_chunks <- vector("list", chunks)


  for (i in 1:(chunks)) {
    start <- (i - 1) * chunk_size + 1
    end <- i * chunk_size
    data_chunks[[i]] <- data[start:end, ]
  }

  data[[chunks]] <- data[((chunks-1) * chunk_size)+1:nrow(data), ]
  return(data_chunks)
}




# ==============================================================================
# Filter layers by year
filter.layerByTime <- function(data, time_vector, var, seasons){
  filtered_lyrs <- data[[paste0(var, "_", time_vector, "_", seasons)]]
}




#===============================================================================
# Area calculation
# ==============================================================================
calculate.area <- function(data, area.unit = NULL) {
  # Check if data is in a projected CRS
  if (sf::st_is_longlat(data)) {
    data <- sf::st_transform(data, "epsg:5070")
  }

  # Calculate area
  areas <- sf::st_area(data)

  # Convert based on unit specification
  converted_areas <- if (is.null(area.unit)) {
    as.numeric(areas)
  } else if (area.unit == "km2") {
    as.numeric(areas)/1e6
  } else if (area.unit == "ha") {
    as.numeric(areas)/1e4
  } else {
    stop("Unsupported area unit. Use 'km2' or 'ha'")
  }

  # Round to 2 decimal places
  round(converted_areas, 2)
}


#===============================================================================
# Separate date column into Year, Month, and day columns
# ==============================================================================

separate.date <- function(data, var_name = NULL, calculate.seasons = FALSE){
  date_col <- colnames(data)[sapply(data, function(x) inherits(x, "Date") | inherits(x, "POSIXct"))]

  if(length(date_col) > 1){
    date_col <- date_col[2]
  }


  if(is.null(var_name)){
    data <- data %>%
    mutate(year = format(data[[date_col]], "%Y"),
           month = format(data[[date_col]], "%m"),
           day = format(data[[date_col]], "%d")
           )
    } else{
      data <- data %>%
               mutate(
               !!paste0(var_name,"_year") := format(data[[date_col]], "%Y"),
               !!paste0(var_name,"_month") := format(data[[date_col]], "%m"),
               !!paste0(var_name,"_day") := format(data[[date_col]], "%d")
        )

}

  if(calculate.seasons == TRUE){
    monthly_col <- paste0(var_name,"_month")
    data <- create.seasonalFeature(data, monthly_col = monthly_col, var_name)

  }
  return(data)
}
#===============================================================================
# Separate date column into Year, Month, and day columns
# ==============================================================================
create.seasonalFeature <- function(data, monthly_col, var_name){

    data <- data %>%
      mutate(!!paste0(var_name, "_season") := case_when(
        .data[[monthly_col]] %in% c("12","01", "02") ~ "DJF",
        .data[[monthly_col]] %in% c("03", "04", "05") ~ "MAM",
        .data[[monthly_col]] %in% c("06", "07", "08") ~ "JJA",
        .data[[monthly_col]] %in% c("09", "10", "11") ~ "SON",
        TRUE ~ NA
      ))
}

#===============================================================================
# Balancing the number of sample plots for fire and nonfire label
# ==============================================================================

balance.plotSize <- function(data){
  # Filter plots with fire label
  Fire_plots <- data %>%
    dplyr::filter(plot_label == "fire") %>%
    sf::st_drop_geometry(.)

  # Count number of fire occurrences per date
  fire_tbl <- Fire_plots %>%
    group_by(fire_date) %>%
    count(name = "Freq") %>%
    pivot_wider(names_from = fire_date, values_from = Freq, values_fill = list(Freq = 0))

  # Filter plots with nonfire lable
  NonFire_plots <- data %>%
    dplyr::filter(plot_label == "nonfire") %>%
    sf::st_drop_geometry(.)

  NonFire_Finalplots <- lapply(names(fire_tbl), function(Fdate){

    n_samples <- fire_tbl[[Fdate]]

    if(n_samples > 0 & n_samples <= nrow(NonFire_plots)){

      # set.seed(643)
      sampled_indices <- sample(nrow(NonFire_plots), n_samples, replace = FALSE)
      sampledPlots <- NonFire_plots[sampled_indices, ]
      sampledPlots$plot_label <- "nonfire"
    }

    return(sampledPlots)
  })

  NonFire_Finalplot <- do.call(rbind, NonFire_Finalplots)

  # Convert to sf object
  NonFire_Finalplots_sf <- sf::st_as_sf(NonFire_Finalplot, coords = c("lon", "lat"), crs = st_crs(data))
  Fire_plots_sf <- sf::st_as_sf(Fire_plots, coords = c("lon", "lat"), crs = st_crs(data))

  # Bind by row the Fire plots and NonFire plots
  finalPlots <- rbind(Fire_plots_sf, NonFire_Finalplots_sf)


  return(finalPlots)
  names(d1_mtbsi_imp$importance)

}
# ==============================================================================
important.variable2df.mtbsi <- function(important_variable){
  if(inherits(important_variable, "varImp.train")){
    imp_df <- important_variable$importance %>%
      as.data.frame(.) %>%
      rownames_to_column("Features") %>%
      mutate(Importance = round(Overall, 1)) %>%
      arrange(desc(Importance)) %>%
      dplyr::select(Features, Importance)
    message("Done!!")
  } else{
    imp_df <- important_variable
    message(paste0("The features is already Data Frame."))
  }


  return(imp_df)
}
# ==============================================================================
important.variable2df.FNF <- function(important_variable){
  if(inherits(important_variable, "varImp.train")){
    imp_df <- important_variable$importance %>%
      as.data.frame(.) %>%
      rownames_to_column("Features") %>%
      mutate(Importance = round(fire, 1)) %>%
      arrange(desc(Importance)) %>%
      dplyr::select(Features, Importance)
    message("Done!!")
  } else{
    imp_df <- important_variable
    message(paste0("The features is already Data Frame."))
  }


  return(imp_df)
}

#===============================================================================
# MDS plot

mds.plot <- function(mds_dF, main, xlab, ylab) {
    mds_plot <- ggplot2::ggplot(mds_df, aes(x = MDS1, y = MDS2, color = Target)) +
    geom_point(alpha = 0.6) +
    theme_minimal() +
    labs(title = main,
         x = xlab,
         y = ylab)

    # Display the plot


    return(print(mds_plot))
}

#===============================================================================
# Compute proximity
calculate.proximity <- function(predictions, testing_datasets){
  terminals <- predictions
  n_obs <- nrow(testing_datasets)
  prox_matrix <- matrix(0, n_obs, n_obs)

  for(i in 1:rf_model$ntree) {
    prox_matrix <- prox_matrix + outer(terminals[,i], terminals[,i], "==")
  }

  prox_matrix <- prox_matrix / rf_model$ntree

  return(prox_matrix)
  }

#===============================================================================
# Raster Clip
raster_clip <- function(img_src, shp_src, output){
  if(!dir.exists(output)){
    dir.create(output, recursive = T)
  }

  # Read data
  img <- rast(img_src)
  shp <- vect(shp_src)

  clippedImg <- mask(crop(img, shp), shp)

  writeRaster(clippedImg,
              file.path(output, paste0("conus_",basename(img_src))),
              overwrite = T)

  return(paste0("Successfully clipped and export to output_dir!!"))
}


# =============================================================================
# File Download from cloud source
# -----------------------------------------------------------------------------
file.download <- function(url, dest_path){
  tryCatch({
    curl::multi_download(url, destfile = dest_path, resume = T, progress = T, timeout = Inf)
    message(paste("Successfully downloaded:", basename(dest_path)))
    return(T)
  }, error = function(e){
    warning(paste("Failed to download:", basename(dest_path), e$message))
    return(F)
  })
}
# ==============================================================================
# Create random samples using multicore process
# ==============================================================================

create.samples <- function(feature, areaCol, type = "regular", n_cores, size = NULL){
  future.seed <- TRUE
  future::plan("multisession", workers = n_cores)

  # Filter for valid geometries and area > 0
  feature <- feature[!sf::st_is_empty(feature) & 
                    !is.na(sf::st_is_valid(feature)) & 
                    feature[[areaCol]] > 0, ]
 

  # Create progress handler
  progressr::handlers(global = TRUE)
  progressr::handlers("progress")

  progressr::with_progress({
    pb <- progressr::progressor(steps = nrow(feature))

    samples <- future.apply::future_lapply(1:nrow(feature), function(x){

      fire_id <- feature$event_id[x]
      fireDate <- feature$fireDate[x]
      area <- (feature[[areaCol]][x])

        if(is.null(size)){
        area4no <- as.integer(ceiling(sqrt(area)))
        n_samples <- as.integer(ceiling(area4no)) 
        } else {
        n_samples <- size
      }
      
      points <- sf::st_sample(feature[x, ],
                                 size = n_samples,
                                 type = type) %>%
                st_as_sf(.) %>%
                mutate(fire_id = fire_id,
                fireDate = fireDate,
                event = "fire"
              ) 
               

      pb()
      return(points)

    })

  })
  future::plan("sequential", .cleanup = T)
  gc()

  randomSamples <- do.call(rbind, samples)
  samples <- st_as_sf(randomSamples)
  return(samples)

}

# ==============================================================================
# Maintaining sampling distance

maintainSampling.distance <- function(feature, buffer_size, n_cores = 20L){

  future::plan("multisession", workers = n_cores)

  # Create progress handler
  progressr::handlers(global = TRUE)
  progressr::handlers("progress")

  progressr::with_progress({
    pb <- progressr::progressor(steps = nrow(feature))

    samples <- future.apply::future_lapply(1:nrow(feature), function(x){

        buffer <- sf::st_buffer(feature[x, ], dist = buffer_size)

        offending <- feature %>% sf::st_intersects(buffer, sparse = F)

        offending[x] <- F

        feature <- feature[!offending, ]

      pb()
      return(feature)

    })

  })
  return(samples)
  future::plan("sequential", .cleanup = T)
}
# ==============================================================================
source(here::here("R/theme.R"))
# ==============================================================================
# Create train test dataset

create_train_test_split <- function(data, train_ratio = 0.8, seed = 123){
  set.seed(seed)

   # Create indices for splitting
  train_indices <- sample(1:nrow(data), 
                         size = floor(train_ratio * nrow(data)))
  
  # Split the data
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  return(list(
    train = train_data,
    test = test_data
  ))
}


# Plotting functions for importance scores
plot_scaled_importance <- function(importance_df) {
  ggplot(importance_df, aes(x = reorder(Feature, Scaled_Importance))) +
    geom_bar(aes(y = Scaled_Importance), stat = "identity", fill = "steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Scaled Variable Importance Plot (0-100)",
      x = "Features",
      y = "Importance Score"
    ) +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5)
    )
}

plot_vip_scores <- function(importance_df) {
  ggplot(importance_df, aes(x = reorder(Feature, VIP_Score))) +
    geom_bar(aes(y = VIP_Score), stat = "identity", fill = "darkred") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "VIP Scores",
      x = "Features",
      y = "VIP Score"
    ) +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5)
    )
}

# Plot performance metrics distribution
plot_performance_metrics <- function(metrics_df) {
  boxplot(metrics_df[,c("AUC", "Accuracy", "Sensitivity", "Specificity")],
          main = "Performance Metrics Distribution",
          ylab = "Value",
          col = "lightblue")
}

# Optional: Combined plot function
plot_all_importance <- function(importance_df) {
  # Reshape data for combined plot
  importance_long <- tidyr::pivot_longer(
    importance_df,
    cols = c(Scaled_Importance, VIP_Score),
    names_to = "Method",
    values_to = "Value"
  )
  
  ggplot(importance_long, 
         aes(x = reorder(Feature, Value), y = Value, fill = Method)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Feature Importance Comparison",
      x = "Features",
      y = "Importance Score"
    ) +
    theme(
      axis.text.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_fill_manual(values = c("steelblue", "darkred"))
}




# Pair scatter plot

plot_ggpairs <- function(data, color_by = NULL) {
  # Check if GGally is installed
  if (!requireNamespace("GGally", quietly = TRUE)) {
    stop("Package 'GGally' is required.", call. = FALSE)
  }
  
  # If a column for coloring is specified
  if (!is.null(color_by) && color_by %in% names(data)) {
    # Make sure the color column is treated as a factor
    data[[color_by]] <- as.factor(data[[color_by]])
    
    # Create the plot with coloring
    GGally::ggpairs(data, ggplot2::aes(color = .data[[color_by]]))
  } else {
    # Create the plot without coloring
    GGally::ggpairs(data)
  }
}

