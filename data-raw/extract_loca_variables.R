#=============================================================================
# Extract LOCA / GCM climate variables from NetCDF for iLand
#=============================================================================
# Data-preparation helpers (NOT part of the installed package; excluded from the
# build via .Rbuildignore). Read NetCDF climate files, collapse to a single
# series, convert units, and remap a 360-day calendar onto the Gregorian year.
# The resulting data frames feed write_iland_climate() in the package.
#=============================================================================

library(ncdf4)       # nc_open, ncvar_get, ncatt_get, nc_close
library(lubridate)   # leap_year

extract_pr <- function(path) {
  nc <- nc_open(path)

  pr_raw <- ncvar_get(nc, "pr")             # kg m-2 s-1
  t_raw  <- ncvar_get(nc, "time")
  t_unit <- ncatt_get(nc, "time", "units")$value      # e.g. "days since 1850-01-01"
  t_cal  <- ncatt_get(nc, "time", "calendar")$value   # "360_day" for HadGEM3
  units_pr <- ncatt_get(nc, "pr", "units")$value

  nc_close(nc)

  # If pr has spatial dims (lon, lat, time), collapse — single county = mean
  if (length(dim(pr_raw)) > 1) {
    pr_raw <- apply(pr_raw, length(dim(pr_raw)), mean, na.rm = TRUE)
  }

  # Convert kg/m2/s -> mm/day
  if (grepl("kg", units_pr)) pr_raw <- pr_raw * 86400

  list(pr_mm_day = as.numeric(pr_raw),
       time_raw  = t_raw,
       time_unit = t_unit,
       calendar  = t_cal)
}
# generalized function for any variable with unit conversions as needed
extract_variable <- function(path, var_name) {
  nc <- nc_open(path)
  v_raw <- ncvar_get(nc, var_name)
  t_raw <- ncvar_get(nc, "time")
  t_unit <- ncatt_get(nc, "time", "units")$value
  t_cal  <- ncatt_get(nc, "time", "calendar")$value
  units_v <- ncatt_get(nc, var_name, "units")$value
  nc_close(nc)

  if (length(dim(v_raw)) > 1)
    v_raw <- apply(v_raw, length(dim(v_raw)), mean, na.rm = TRUE)

  # Unit conversions per variable
  if (var_name %in% c("tasmin", "tasmax", "tas") &&
      grepl("^K", units_v))               v_raw <- v_raw - 273.15        # K -> °C
  if (var_name == "pr" && grepl("kg", units_v)) v_raw <- v_raw * 86400   # -> mm/day
  if (var_name == "rsds" && grepl("W", units_v)) v_raw <- v_raw * 86400 / 1e6  # -> MJ/m2/day

  list(values = as.numeric(v_raw),
       time_raw = t_raw, time_unit = t_unit, calendar = t_cal,
       units_in = units_v)
}
# ---------------------------------------------------------------
# 3. HANDLE 360-DAY CALENDAR -> Gregorian
# ---------------------------------------------------------------
# Strategy: PCICt package handles 360_day natively, OR remap manually.
# Manual approach: build 360-day dates, then map to Gregorian by
# linear interpolation onto a 365-day grid.

# Option A — keep as 360-day and accept the offset (simplest, OK for
# climatology, NOT recommended for iLand which expects real dates)


build_dates_360 <- function(time_raw, time_unit) {
  # Parse origin from time_unit string (e.g., "days since 1850-01-01")
  origin_str   <- sub("days since ", "", time_unit)
  origin_str   <- sub(" .*", "", origin_str)            # strip any time portion
  origin_parts <- as.integer(strsplit(origin_str, "-")[[1]])
  o_year  <- origin_parts[1]
  o_month <- origin_parts[2]
  o_day   <- origin_parts[3]

  # In 360-day calendar: total days from "year 0"
  origin_total <- o_year * 360 + (o_month - 1) * 30 + (o_day - 1)
  total_days   <- origin_total + as.integer(time_raw)

  # Convert back to year / month / day in 360-day system
  yr  <- total_days %/% 360
  rem <- total_days %%  360
  mo  <- rem %/% 30 + 1
  dy  <- rem %%  30 + 1

  data.frame(year_360 = yr, month_360 = mo, day_360 = dy)
}



# Map 360-day series to a 365-day Gregorian series via interpolation.
# This preserves totals reasonably and gives iLand the dates it wants.
remap_360_to_greg <- function(values_360, dates_360_df,
                              var_name = "prec",
                              preserve_total = TRUE) {
  # values_360     : numeric vector aligned with dates_360_df rows
  # dates_360_df   : data.frame with year_360, month_360, day_360
  # var_name       : column name for the output variable
  # preserve_total : rescale interpolated year so annual sum matches 360-day sum
  #                  (recommended for precipitation; harmless for temperature)

  df_360 <- cbind(dates_360_df, val = values_360)
  unique_years <- sort(unique(df_360$year_360))
  out_list <- vector("list", length(unique_years))

  for (i in seq_along(unique_years)) {
    yr <- unique_years[i]
    yr_data <- df_360[df_360$year_360 == yr, ]

    if (nrow(yr_data) != 360) {
      message("Skipping incomplete year ", yr, " (", nrow(yr_data), " days)")
      next
    }

    # Order chronologically within the 360-day year
    yr_data <- yr_data[order(yr_data$month_360, yr_data$day_360), ]
    vals_360 <- yr_data$val

    # Stretch 360 source points across the Gregorian year length
    n_target <- if (leap_year(yr)) 366 else 365
    interp <- approx(
      x    = seq(1, n_target, length.out = 360),
      y    = vals_360,
      xout = seq_len(n_target),
      rule = 2                                  # extend ends, no NAs
    )
    vals_greg <- interp$y

    # Non-negativity (matters for precipitation, radiation)
    vals_greg[vals_greg < 0] <- 0

    # Optional: rescale so the Gregorian year preserves the 360-day annual sum.
    # Important for precipitation; for temperature pass preserve_total = FALSE.
    if (preserve_total) {
      s_src <- sum(vals_360,  na.rm = TRUE)
      s_tgt <- sum(vals_greg, na.rm = TRUE)
      if (s_tgt > 0) vals_greg <- vals_greg * (s_src / s_tgt)
    }

    greg_dates <- seq(as.Date(sprintf("%04d-01-01", yr)),
                      as.Date(sprintf("%04d-12-31", yr)),
                      by = "day")

    out_list[[i]] <- data.frame(
      year  = as.integer(format(greg_dates, "%Y")),
      month = as.integer(format(greg_dates, "%m")),
      day   = as.integer(format(greg_dates, "%d")),
      val   = vals_greg
    )
  }

  result <- do.call(rbind, out_list)
  names(result)[names(result) == "val"] <- var_name
  rownames(result) <- NULL
  result
}
