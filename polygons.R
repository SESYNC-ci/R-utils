library(sp)

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