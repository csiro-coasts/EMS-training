# R version of particle_example_plot3d.ipynb
#
# Plots particle tracks in 3D from three viewpoints: from above (xy plane),
# from the side (xz plane), and from an angle.
#
# Needs the ncdf4 package. Install it once with: install.packages("ncdf4")
# Run from the folder holding the .nc file, so the relative path below works.
# In RStudio: Session > Set Working Directory > To Source File Location.
# The plot is saved as a PNG file there, and also shown on screen in RStudio.

library(ncdf4)

PT_ACTIVE <- 0x001  # particle is active

data <- nc_open("particle_tracking_example.nc")
#print(data)

# R reverses the dimension order shown by Python: x is [t, n] in xarray and
# [n, t] here. So each row is a track, and Python's x[:, track] is x[track, ].
x <- ncvar_get(data, "x")
y <- ncvar_get(data, "y")
z <- ncvar_get(data, "z")
flag <- ncvar_get(data, "flag")

nc_close(data)

num_tracks <- nrow(x)

# matplotlib's default colours, which it cycles through for successive lines
track_colours <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
)

# Axis ranges cover the active particle positions, plus a 5% margin each side
# so tracks don't touch the box. matplotlib works this out automatically.
active <- flag == PT_ACTIVE
range_with_margin <- function(values) {
  limits <- range(values)
  limits + c(-1, 1) * 0.05 * diff(limits)
}
xlim <- range_with_margin(x[active])
ylim <- range_with_margin(y[active])
zlim <- range_with_margin(z[active])

# Draws one panel. persp() is base R's 3D plot: it draws the box and axes
# (the surface itself is left invisible), and returns the viewing matrix that
# trans3d() uses to place the track lines.
#
# theta and phi set the viewpoint. They relate to matplotlib's view_init() as
# theta = azim + 90 and phi = elev. A large d gives a near-orthographic view,
# and expand = 0.75 matches the box proportions matplotlib uses.
plot_tracks <- function(theta, phi) {
  view <- persp(
    x = xlim, y = ylim, z = matrix(zlim[1], 2, 2), zlim = zlim,
    theta = theta, phi = phi, d = 1000, expand = 0.75,
    col = NA, border = NA,
    ticktype = "detailed", nticks = 8, cex.axis = 0.7,
    xlab = "Lon [deg]", ylab = "Lat [deg]", zlab = "Depth [m]"
  )

  for (track in seq_len(num_tracks)) {
    # extract [lon, lat, z] particle positions for a given track
    xlon <- x[track, ]
    ylat <- y[track, ]
    zpos <- z[track, ]

    # keep the parts of the arrays in which particles are active
    okay <- flag[track, ] == PT_ACTIVE

    # (track - 1) %% 10 + 1 cycles through the 10 colours
    lines(
      trans3d(xlon[okay], ylat[okay], zpos[okay], view),
      col = track_colours[(track - 1) %% 10 + 1],
      lwd = 2
    )
  }
}

plot_all_views <- function() {
  # 3 rows, 1 column, with margins wide enough to fit the axis labels
  old_par <- par(mfrow = c(3, 1), mar = c(6, 7, 2, 7))
  on.exit(par(old_par))

  plot_tracks(theta = 0, phi = 90)   # xy plane
  plot_tracks(theta = 0, phi = 0)    # xz plane
  plot_tracks(theta = 30, phi = 30)  # matplotlib's default view
}

png("particle_tracks_3d.png", width = 8, height = 24, units = "in", res = 150)
plot_all_views()
invisible(dev.off())

# Three rows don't fit in RStudio's plot pane (R can stop with "figure margins
# too large"), so on screen each view is its own plot. Use the arrows in the
# Plots pane to move between them.
if (interactive()) {
  old_par <- par(mar = c(4, 5, 1, 5))
  plot_tracks(theta = 0, phi = 90)   # xy plane
  plot_tracks(theta = 0, phi = 0)    # xz plane
  plot_tracks(theta = 30, phi = 30)  # matplotlib's default view
  par(old_par)
}
