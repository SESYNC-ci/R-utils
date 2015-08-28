library(raster)

# 'inverse' of raster::rotate
# takes a raster r with x extent form (-180, 180), changes it to (0, 360)
inv_rotate <- function(r) {
    xmin(r) <- 0
    xmax(r) <- 360
    r <- rotate(r)
    xmin(r) <- 0
    xmax(r) <- 360
    r
}
