# R version of particle_example_distance.ipynb
#
# Finds the furthest distance any active particle travels from its release point.
#
# Needs the ncdf4 package. Install it once with: install.packages("ncdf4")
# Run from the folder holding the .nc file, so the relative path below works.
# In RStudio: Session > Set Working Directory > To Source File Location.

library(ncdf4)

# Returns distance [m] between pairs of [lon, lat] points [deg]
distance <- function(lon_a, lat_a, lon_b, lat_b) {
  r_earth <- 6371220.0  # Earth's radius [m]

  lat_1 <- lat_a * pi / 180  # degrees-to-radians
  lon_1 <- lon_a * pi / 180
  lat_2 <- lat_b * pi / 180
  lon_2 <- lon_b * pi / 180

  dlon <- 0.5 * (lon_1 - lon_2)
  dlat <- 0.5 * (lat_1 - lat_2)

  2.0 * r_earth * asin(sqrt(
    sin(dlat)^2 +
      sin(dlon)^2 * cos(lat_1) * cos(lat_2)
  ))
}

PT_ACTIVE <- 0x001  # particle is active

data <- nc_open("particle_tracking_example.nc")
#print(data)

# where the particles are released
# source_locationX is [l, m] in xarray, and arrives here as a vector along l
# because m has length 1. R counts from 1, so [1] is Python's [0, 0].
src_xlon <- ncvar_get(data, "source_locationX")[1]
src_ylat <- ncvar_get(data, "source_locationY")[1]

# R reverses the dimension order shown by Python: x is [t, n] in xarray and
# [n, t] here. So each row is a track, and Python's x[:, track] is x[track, ].
x <- ncvar_get(data, "x")
y <- ncvar_get(data, "y")
flag <- ncvar_get(data, "flag")

nc_close(data)

dmax_m <- 0.0  # max distance [m] to any point on any track

num_tracks <- nrow(x)
for (track in seq_len(num_tracks)) {
  # extract [lon, lat] particle positions for a given track
  xlon <- x[track, ]
  ylat <- y[track, ]

  # keep the parts of the arrays in which particles are active
  okay <- flag[track, ] == PT_ACTIVE

  dist_m <- distance(src_xlon, src_ylat, xlon[okay], ylat[okay])

  # keep the max distance so far
  dmax_m <- max(dmax_m, dist_m)
}

cat("Max distance from release [km]:", format(dmax_m / 1000.0, digits = 17), "\n")
