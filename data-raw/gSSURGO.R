#=============================================================================
# Process gSSURGO California for iLand
#=============================================================================
# Data-preparation script (NOT part of the installed package; excluded from the
# build via .Rbuildignore). Reads the gSSURGO geodatabase, derives 0-30 cm
# weighted soil texture / depth per map unit, and writes property rasters.
# Edit the paths in section 1 before running.
#=============================================================================

library(terra)   # rast, values, classify, writeRaster, global
library(sf)      # st_read
library(dplyr)   # data wrangling
library(rlang)   # sym(), !!

#-----------------------------------------------------------------------------
# 1. SET PATHS
#-----------------------------------------------------------------------------
gSSURGO_CA_gdb_path <- "D:/Datasets/gSSURGO_CA/gSSURGO_CA.gdb"
snmr_gdb_path <- "D:\\Dissertation_Analysis\\Task4_analysis\\Task4_analysis.gdb"
output_dir <- "D:/Datasets/gSSURGO_CA/processed"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

#-----------------------------------------------------------------------------
# 2. LOAD THE MUKEY RASTER
#-----------------------------------------------------------------------------
mukey_raster <- rast(paste0("OpenFileGDB:", snmr_gdb_path, ":MURASTER_10m_SNMR"))
study_mukeys <- unique(values(mukey_raster, mat = FALSE, na.rm = TRUE)) |>
                as.integer()

# Pull only needed component cols, only study-area mukeys

#-----------------------------------------------------------------------------
# 3. LOAD SOIL PROPERTY TABLES
#-----------------------------------------------------------------------------
cat("\nLoading soil tables...\n")

# Load component table (links cokey to mukey)
mukey_in <- paste(study_mukeys, collapse = ",")
component <- st_read(
                gSSURGO_CA_gdb_path,
                query = sprintf(
                    "SELECT mukey, cokey, comppct_r, compname, compkind, majcompflag, taxorder, taxsuborder
                    FROM component
                    WHERE mukey IN (%s)", mukey_in),
                quiet = TRUE
            ) |> as.data.frame()

study_cokeys <- unique(component$cokey)
cat("  - component table:", nrow(component), "rows\n")

# Load chorizon table (has soil texture data)
chorizon_cols <- c("cokey", "chkey", "hzname", "hzdept_r", "hzdepb_r",
                   "sandtotal_r", "silttotal_r", "claytotal_r",
                   "om_r", "dbthirdbar_r", "ksat_r",
                   "ph1to1h2o_r", "cec7_r", "ec_r", "awc_r")

read_chorizon_by_cokey <- function(gdb, cokeys, cols, chunk = 1000) {
  col_sql <- paste(cols, collapse = ", ")
  splits <- split(cokeys, ceiling(seq_along(cokeys) / chunk))
  out <- lapply(splits, function(ck) {
    ck_in <- paste0("'", ck, "'", collapse = ",")
    st_read(
      gdb,
      query = sprintf("SELECT %s FROM chorizon WHERE cokey IN (%s)", col_sql, ck_in),
      quiet = TRUE
    ) |> as.data.frame()
  })
  dplyr::bind_rows(out)
}

chorizon <- read_chorizon_by_cokey(gSSURGO_CA_gdb_path, study_cokeys, chorizon_cols)
cat("  - chorizon table:", nrow(chorizon), "rows\n")
# Check column names for texture
cat("\nTexture columns in chorizon:\n")
texture_cols <- grep("sand|silt|clay|om_r", names(chorizon), value = TRUE, ignore.case = TRUE)
print(texture_cols)

#-----------------------------------------------------------------------------
# 4. PROCESS SOIL TEXTURE (0-30 cm weighted average)
#-----------------------------------------------------------------------------
cat("\nCalculating soil texture for 0-30 cm depth...\n")

# Filter horizons that overlap with 0-30 cm depth range
horizon_data <- chorizon %>%
  select(cokey, hzdept_r, hzdepb_r, sandtotal_r, silttotal_r, claytotal_r, om_r) %>%
  filter(!is.na(hzdept_r) & !is.na(hzdepb_r)) %>%
  mutate(
    # Clip to 0-30 cm range
    top = pmax(hzdept_r, 0),
    bottom = pmin(hzdepb_r, 30),
    thickness = pmax(bottom - top, 0)
  ) %>%
  filter(thickness > 0)

cat("  - Horizons in 0-30cm range:", nrow(horizon_data), "\n")

# Weighted average by horizon thickness within each component
texture_by_comp <- horizon_data %>%
  group_by(cokey) %>%
  summarize(
    sand = weighted.mean(sandtotal_r, w = thickness, na.rm = TRUE),
    silt = weighted.mean(silttotal_r, w = thickness, na.rm = TRUE),
    clay = weighted.mean(claytotal_r, w = thickness, na.rm = TRUE),
    om = weighted.mean(om_r, w = thickness, na.rm = TRUE),
    .groups = "drop"
  )

cat("  - Components with texture data:", nrow(texture_by_comp), "\n")

#-----------------------------------------------------------------------------
# 5. GET SOIL DEPTH FROM COMPONENT TABLE
#-----------------------------------------------------------------------------
cat("\nExtracting soil depth...\n")

# Check available depth columns
depth_cols <- grep("depth|resdept|restr", names(component), value = TRUE, ignore.case = TRUE)
cat("  Depth-related columns:", paste(depth_cols, collapse = ", "), "\n")

# Use resdept_r (depth to restriction) or alternative
depth_by_comp <- component %>%
  select(cokey, mukey, comppct_r, resdept_r) %>%
  filter(!is.na(comppct_r))

#-----------------------------------------------------------------------------
# 6. COMBINE AND AGGREGATE TO MAPUNIT LEVEL
#-----------------------------------------------------------------------------
cat("\nAggregating to map unit (MUKEY) level...\n")

# Join texture to components
comp_data <- depth_by_comp %>%
  left_join(texture_by_comp, by = "cokey")

# Weighted average by component percentage within each mapunit
soil_by_mukey <- comp_data %>%
  group_by(mukey) %>%
  summarize(
    pctSand = weighted.mean(sand, w = comppct_r, na.rm = TRUE),
    pctSilt = weighted.mean(silt, w = comppct_r, na.rm = TRUE),
    pctClay = weighted.mean(clay, w = comppct_r, na.rm = TRUE),
    pctOM = weighted.mean(om, w = comppct_r, na.rm = TRUE),
    soilDepth = weighted.mean(resdept_r, w = comppct_r, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Convert mukey to numeric for raster classification
  mutate(mukey_num = as.numeric(as.character(mukey)))

cat("  - Map units with data:", nrow(soil_by_mukey), "\n")

# Summary
cat("\nSoil property summary:\n")
print(summary(soil_by_mukey[, c("pctSand", "pctSilt", "pctClay", "soilDepth")]))

#-----------------------------------------------------------------------------
# 7. SAVE LOOKUP TABLE
#-----------------------------------------------------------------------------
write.csv(soil_by_mukey, 
          file.path(output_dir, "mukey_soil_properties.csv"), 
          row.names = FALSE)
cat("\nLookup table saved!\n")

#-----------------------------------------------------------------------------
# 8. CREATE SOIL PROPERTY RASTERS
#-----------------------------------------------------------------------------
cat("\nCreating soil property rasters (this may take a while for CA)...\n")

# Function to create classification matrix and reclassify
create_property_raster <- function(mukey_rast, soil_df, property_col, output_name) {
  cat("  Processing:", property_col, "...\n")
  
  # Create lookup matrix: from_value, to_value
  lookup <- soil_df %>%
    filter(!is.na(!!sym(property_col))) %>%
    select(mukey_num, !!sym(property_col)) %>%
    as.matrix()
  
  # Reclassify
  result <- classify(mukey_rast, lookup, othersNA = TRUE)
  
  # Save
  output_path <- file.path(output_dir, paste0(output_name, ".tif"))
  writeRaster(result, output_path, overwrite = TRUE)
  cat("    Saved:", output_path, "\n")
  
  return(result)
}

# Create each property raster
sand_rast <- create_property_raster(mukey_raster, soil_by_mukey, "pctSand", "sand_pct")
silt_rast <- create_property_raster(mukey_raster, soil_by_mukey, "pctSilt", "silt_pct")
clay_rast <- create_property_raster(mukey_raster, soil_by_mukey, "pctClay", "clay_pct")
depth_rast <- create_property_raster(mukey_raster, soil_by_mukey, "soilDepth", "soil_depth_cm")
om_rast <- create_property_raster(mukey_raster, soil_by_mukey, "pctOM", "organic_matter_pct")

#-----------------------------------------------------------------------------
# 9. VERIFY RESULTS
#-----------------------------------------------------------------------------
cat("\nVerification - Raster statistics:\n")

cat("\nSand (%):\n")
print(global(sand_rast, fun = c("mean", "min", "max"), na.rm = TRUE))

cat("\nClay (%):\n")
print(global(clay_rast, fun = c("mean", "min", "max"), na.rm = TRUE))

cat("\nSoil Depth (cm):\n")
print(global(depth_rast, fun = c("mean", "min", "max"), na.rm = TRUE))

#-----------------------------------------------------------------------------
# 10. PLOT RESULTS
#-----------------------------------------------------------------------------
cat("\nCreating visualization...\n")

# Set up plot
png(file.path(output_dir, "soil_properties_overview.png"), 
    width = 1200, height = 1000, res = 100)

par(mfrow = c(2, 2), mar = c(2, 2, 3, 1))

plot(sand_rast, main = "Sand Content (%)", 
     col = hcl.colors(50, "YlOrBr", rev = TRUE))

plot(clay_rast, main = "Clay Content (%)", 
     col = hcl.colors(50, "Reds"))

plot(depth_rast, main = "Soil Depth (cm)", 
     col = hcl.colors(50, "Greens"))

plot(om_rast, main = "Organic Matter (%)", 
     col = hcl.colors(50, "Purples", rev = TRUE))

dev.off()

cat("\nPlot saved to:", file.path(output_dir, "soil_properties_overview.png"), "\n")

cat("\n=== Processing Complete! ===\n")
cat("Output files are in:", output_dir, "\n")