library(terra)
library(whitebox)

# Method 1: Using whitebox package (recommended approach)
calculate_twi_whitebox <- function(dem_path, output_path) {
  # Step 1: Fill depressions in the DEM
  wbt_fill_depressions(
    dem = dem_path,
    output = "filled_dem.tif"
  )
  
  # Step 2: Calculate flow accumulation
  wbt_d8_flow_accumulation(
    input = "filled_dem.tif", 
    output = "flow_acc.tif",
    log = TRUE  # Log-transform the output
  )
  
  # Step 3: Calculate slope (in radians)
  wbt_slope(
    dem = "filled_dem.tif",
    output = "slope.tif",
    units = "radians"
  )
  
  # Step 4: Calculate TWI
  wbt_wetness_index(
    sca = "flow_acc.tif",
    slope = "slope.tif",
    output = output_path
  )
  
  # Return the resulting raster
  return(rast(output_path))
}

# Method 2: Manual calculation using terra
calculate_twi_terra <- function(dem_path, output_path) {
  # Read DEM
  dem <- rast(dem_path)
  
  # Fill sinks
  dem_filled <- fill(dem)
  
  # Calculate flow direction
  flow_dir <- terrain(dem_filled, "flowdir")
  
  # Calculate flow accumulation
  flow_acc <- flowaccum(flow_dir)
  
  # Calculate specific catchment area
  # (flow_acc + 1 to avoid log(0))
  sca <- (flow_acc + 1) * res(dem)[1]
  
  # Calculate slope in radians
  slope_rad <- terrain(dem_filled, "slope", unit = "radians")
  
  # Calculate TWI
  # Add small value to slope to prevent division by zero
  twi <- log(sca / (tan(slope_rad) + 0.01))
  
  # Write result
  writeRaster(twi, output_path, overwrite = TRUE)
  
  return(twi)
}

# Example usage
# dem_file <- "path/to/your/dem.tif"
# twi_output <- "path/to/output/twi.tif"

# # Using whitebox method (recommended)
# twi_whitebox <- calculate_twi_whitebox(dem_file, twi_output)

# # Using terra method
# twi_terra <- calculate_twi_terra(dem_file, twi_output)