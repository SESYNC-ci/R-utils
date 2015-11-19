library(sp)

# Create clipping rectangle around point p (SpatialPoints of length 1)
#  to guarantee at least dmax on each side. In unprojected (long/lat) coordinates,
#  it automatically cuts the polygon in two if it overlaps international date line
#
# Notes:
# - dmax either in meters (if p unprojected) or in the projection's coordinates
# - this uses the poly_rect function described below
get_clip_rect <- function(p, dmax) {
    if (is.projected(p)) {
        x <- coordinates(p)[1]
        y <- coordinates(p)[2]
        r1 <- poly_rect(x - dmax, y - dmax, x + dmax, y + dmax)
        clip_rect <- SpatialPolygons(list(Polygons(list(r1), ID = 1)),
                                     proj4string = CRS(proj4string(p)))
    } else {
        lat_dist <- 111600 # approx. distance (in m) between degrees of latitude
        long <- coordinates(p)[1]
        lat <- coordinates(p)[2]
        ybuf = dmax / lat_dist
        xbuf = ybuf / cospi(abs(lat) / 180)
        # Split clip_rect in two if it would overlap international date line
        if (long - xbuf < -180) {
            westr <- poly_rect(-180, lat - ybuf, long + xbuf, lat + ybuf)
            eastr <- poly_rect(long - xbuf + 360, lat - ybuf, 180, lat + ybuf)
            clip_rect <- SpatialPolygons(list(Polygons(list(westr), ID = 1),
                                              Polygons(list(eastr), ID = 2)),
                                         proj4string = CRS(proj4string(p)))
        } else if(long + xbuf > 180) {
            westr <- poly_rect(-180, lat - ybuf, long + xbuf - 360, lat + ybuf)
            eastr <- poly_rect(long - xbuf, lat - ybuf, 180, lat + ybuf)
            clip_rect <- SpatialPolygons(list(Polygons(list(westr), ID = 1),
                                              Polygons(list(eastr), ID = 2)),
                                         proj4string = CRS(proj4string(p)))
        } else {
            r1 <- poly_rect(long - xbuf, lat - ybuf, long + xbuf, lat + ybuf)
            clip_rect <- SpatialPolygons(list(Polygons(list(r1), ID = 1)),
                                         proj4string = CRS(proj4string(p)))
        }
    }
    clip_rect
}


# Create a Polygon object corresponding to rectangle with given coords
poly_rect <- function(xmin, ymin, xmax, ymax) {
    Polygon(cbind(c(rep(xmin, 2), rep(xmax, 2), xmin),
                  c(ymin, rep(ymax, 2), rep(ymin, 2))))
}


# Spatial operations on SpatialPolygons (e.g. rgeos::gUnion) that are nearly but
#  not perfectly contiguous may create very small or narrow 'sliver polygons'.
# 
# This function takes a SpatialPolygons object (sppoly_obj) and returns it after
#  removing any constituent polygons with less than 3 vertices or those
#  with area smaller than the specified min_area.
rm_small_polys <- function(sppoly_obj, min_area) {
    poly_list <- sppoly_obj@polygons
    # Looping through polygons in reverse order 
    #  since deletions of whole polygons could change later indices
    for (i in length(poly_list):1) {
        subpoly_list <- poly_list[[i]]@Polygons
        inval <- sapply(subpoly_list, function(x) {
            nrow(unique(x@coords)) < 3 || x@area < min_area 
        })
        subpoly_list[inval] <- NULL
        if (length(subpoly_list) == 0) {
            poly_list[[i]] <- NULL
        } else {
            poly_list[[i]] <- Polygons(subpoly_list, poly_list[[i]]@ID)
        }
    }
    if (length(poly_list) == 0) {
        NULL
    } else {
        SpatialPolygons(poly_list, proj4string = CRS(proj4string(sppoly_obj)))
    }
}