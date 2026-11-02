# R version of column_extractor_example.ipynb
#
# Plots salinity from an EMS column extraction: a depth profile at one time,
# and a time series at one depth.
#
# Needs the ncdf4 package. Install it once with: install.packages("ncdf4")
# Run from the folder holding the .nc file, so the relative path below works.
# In RStudio: Session > Set Working Directory > To Source File Location.
# Plots are saved as PNG files there, and also shown on screen in RStudio.

library(ncdf4)

data <- nc_open("column_extractor_example.nc")
#print(data)

# R reverses the dimension order shown by Python, and ncvar_get() drops
# dimensions of length 1. So salt, which is [nrecords, k, npoints] in xarray,
# arrives here as a matrix of [k, nrecords].
salinity <- ncvar_get(data, "salt")
#print(dim(salinity))

depth_from_msl <- ncvar_get(data, "z")
#print(depth_from_msl)

# xarray converts time to dates automatically; here it is done by hand.
# The units look like "days since 2010-01-01 00:00:00 +10".
time_days <- ncvar_get(data, "time")
time_units <- ncatt_get(data, "time", "units")$value
units_pattern <- "^days since (\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}) ([+-]\\d+)$"
stopifnot(grepl(units_pattern, time_units))

nc_close(data)

epoch <- as.POSIXct(sub(units_pattern, "\\1", time_units), tz = "UTC")
utc_offset_hours <- as.numeric(sub(units_pattern, "\\2", time_units))

# Times are converted to UTC, as xarray does. Rounding to the nearest second
# removes floating point error, which would otherwise turn 15:00 into 14:59.
time <- epoch - utc_offset_hours * 3600 + round(time_days * 86400)
time_labels <- format(time, "%d %b %Y, %H:%M", tz = "UTC")
#print(time)

blue <- "#1f77b4"  # matplotlib's default line colour
grid_colour <- adjustcolor("black", alpha.f = 0.25)


# plot salinity vs depth at a given time
record <- 12  # which time record to plot (R counts from 1: this is record 11 in Python)

plot_salinity_vs_depth <- function() {
  # Levels below the seabed have no salinity value. Unlike matplotlib, R would
  # still include their depths in the axis range, so set the range explicitly.
  has_value <- !is.na(salinity[, record])

  plot(
    salinity[, record], depth_from_msl,
    type = "l", col = blue, lwd = 2, las = 1,
    ylim = range(depth_from_msl[has_value]),
    main = paste0("Salinity vs Depth at ", time_labels[record]),
    xlab = "Salinity [psu]",
    ylab = "Depth from MSL [m]"
  )
  grid(lty = 2, col = grid_colour)
}

png("salinity_vs_depth.png", width = 8, height = 7, units = "in", res = 150)
plot_salinity_vs_depth()
invisible(dev.off())
if (interactive()) plot_salinity_vs_depth()


# plot salinity over time at a given depth
level <- 29  # which depth level to plot (R counts from 1: this is level 28 in Python)

plot_salinity_over_time <- function() {
  positions <- seq_along(time_labels)

  old_par <- par(mar = c(8, 4, 4, 2) + 0.1)  # extra room below for the time labels
  on.exit(par(old_par))

  plot(
    positions, salinity[level, ],
    type = "l", col = blue, lwd = 2, las = 1, xaxt = "n",
    main = paste0("Evolution of Salinity at z=", depth_from_msl[level]),
    xlab = "",
    ylab = "Salinity [psu]"
  )
  grid(nx = NA, ny = NULL, lty = 2, col = grid_colour)
  abline(v = positions, lty = 2, col = grid_colour)

  # Base R can only rotate axis labels by 90 degrees, so draw them as text.
  axis(1, at = positions, labels = FALSE)
  plot_area <- par("usr")
  text(
    positions, plot_area[3] - 0.03 * (plot_area[4] - plot_area[3]),
    labels = time_labels,
    srt = 75, adj = c(1, 1), xpd = TRUE, cex = 0.7
  )
}

png("salinity_over_time.png", width = 8, height = 7, units = "in", res = 150)
plot_salinity_over_time()
invisible(dev.off())
if (interactive()) plot_salinity_over_time()
