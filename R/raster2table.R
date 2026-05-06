# Function to process monthly raster files and extract statistics
raster2table <- function(rasterDir, pattern, variable_name, aoi) {

  raster_files <- list.files(rasterDir,
                             pattern = pattern,
                             full.names = T)

  if(variable_name %in% c("tmmnM", "tmmxM", "prM", "vpdM", "petM", "vsM", "rmaxM",
                          "rminM", "sradM")){
    timeUnit <- "time"
  } else{
    timeUnit <- "day"
  }

  r <- terra::rast(raster_files)

  vLyr <- aoi %>%
    sf::st_transform(., st_crs(r)) %>%
    terra::vect(.)


  results <- data.frame()

  for(i in 1:terra::nlyr(r)) {
    # Extract single layer
    nc <- ncdf4::nc_open(raster_files[i])
    times <- ncdf4::ncvar_get(nc, timeUnit)
    dates <- as.Date("1900-01-01") + times
    ncdf4::nc_close(nc)

    r_layer <- r[[i]]
    r_state <- mask(r_layer, vLyr)
    # Calculate statistics
    stats <- data.frame(
      date = dates,
      variable = variable_name,
      min = global(r_state, min, na.rm = T)[1, 1],
      mean = global(r_state, mean, na.rm = T)[1, 1],
      max = global(r_state, max, na.rm = T)[1, 1],
      sd = global(r_state, sd, na.rm = T)[1, 1]

    )

    results <- rbind(results, stats)
  }

  # Sort by date
  results <- results[order(results$date), ]
  return(results)
}


# ==============================================================================
plot_climate_trends <- function(climate_data,
                                variable_name,
                                add_smoothing = FALSE,
                                add_sd = FALSE) {
  # Convert data to long format
  climate_long <- climate_data %>%
    pivot_longer(cols = c(min, mean, max),
                 names_to = "statistic",  # Changed from variable_name to "statistic"
                 values_to = "value")

  # Create base plot
  p <- ggplot(climate_long, aes(x = year, y = value, color = statistic)) +  # Changed to statistic
    geom_line(aes(linetype = statistic), alpha = 0.7)  # Changed to statistic

  # Add standard deviation bands if requested
  if(add_sd) {
    p <- p + geom_ribbon(data = climate_data,
                         aes(x = year,
                             ymin = mean - sd,
                             ymax = mean + sd),
                         fill = "grey70",
                         alpha = 0.2,
                         color = NA)
  }

  p <- p + theme_minimal() +
    labs(title = paste("(d)", "Annual mean", variable_name),
         x = "Year",
         y = variable_name,
         color = "Statistic",    # Changed legend title
         linetype = "Statistic") +  # Changed legend title
    scale_color_manual(values = c("min" = "blue", "mean" = "black", "max" = "red")) +
    scale_linetype_manual(values = c("min" = "dotdash", "mean" = "solid", "max" = "dashed")) +
    scale_x_continuous(
      breaks = seq(1984, 2023, by = 5)
    )

  # Add smoothing if requested
  if(add_smoothing) {
    p <- p + geom_smooth(aes(linetype = statistic),  # Changed to statistic
                         method = "loess",
                         se = FALSE,
                         alpha = 0.5)
  }

  # Customize theme
  p <- p + theme(
    legend.position = "top",
    legend.box.just = "right",
    legend.justification = c(1, 1),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing.x = unit(0.2, 'cm'),
    legend.background = element_rect(fill = "white", color = NA),



    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7),
    axis.text.y = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    plot.title = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 8),
    plot.margin = margin(t = 5, r = 10, b = 5, l = 10),
  ) +
    guides(color = guide_legend(override.aes = list(linetype = c("dotdash", "solid", "dashed"))))

  return(p)
}

# ==============================================================================
# Function to process monthly raster files and extract annual statistics
raster2table_annual <- function(rasterDir, pattern, variable_name, aoi) {
  # Get monthly results first
  monthly_results <- raster2table(rasterDir, pattern, variable_name, aoi)

  # Calculate annual statistics based on the variable type
  if(variable_name %in% c("prM")) {  # For precipitation, sum the values
    annual_results <- monthly_results %>%
      mutate(year = lubridate::year(date)) %>%
      group_by(year, variable) %>%
      summarise(
        min = sum(min, na.rm = TRUE),
        mean = sum(mean, na.rm = TRUE),
        max = sum(max, na.rm = TRUE)
      ) %>%
      ungroup()
  } else {  # For temperature and other variables, average the values
    annual_results <- monthly_results %>%
      mutate(year = lubridate::year(date)) %>%
      group_by(year, variable) %>%
      summarise(
        min = mean(min, na.rm = TRUE),
        mean = mean(mean, na.rm = TRUE),
        max = mean(max, na.rm = TRUE)
      ) %>%
      ungroup()
  }

  return(annual_results)
}

# pdsiM_df <- raster2table_annual(pdsiM_dir, "\\.nc$", "pdsiM", aoi = conusStates)

# pdsiM_df

# # scPdsiM_df <- raster2table_annual(scPdsiM_dir, "\\.nc$", "scPdsiM", aoi = conus_states)
# # pzIM_df <- raster2table_annual(pzIM_dir, "\\.nc$", "pzIM", aoi = conus_states)

# # vpdM_df <-  raster2table_annual(vpdM_dir, "\\.nc$", "vpdM", aoi = conus_states)
# # petM_df <- raster2table_annual(petM_dir, "\\.nc$", "petM", aoi = conus_states)

# # tmmnM_df <- raster2table_annual(tmmnM_dir, "\\.nc$", variable_name = "tmmnM", aoi = conus_states)
# # tmmxM_df <- raster2table_annual(tmmxM_dir, "\\.nc$", "tmmxM", aoi = conus_states )
# # prM_df <- raster2table_annual(prM_dir, "\\.nc$", "prM", aoi = conus_states )


# plot_climate_trends(tmmnM_df, "tmmxM",
#                     add_smoothing = F,
#                     add_sd = F
#                     )


# plot_climate_trends(pdsiM_df, "PDSI",
#                     add_smoothing = F,
#                     add_sd = F
#                     )


# plot_climate_trends(scPdsiM_df, "scPDSI",
#                     add_smoothing = F,
#                     add_sd = F)


