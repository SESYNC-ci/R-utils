# R-utils

Please use this repository to share R functions that you write which may be useful to other SESYNC users. Functions dealing with the similar tasks can be grouped into a single .R file, with a short description of the file added to this README.

# Functions list by source file

*lists.R*

- `nest_vector`: create a list from a vector based on the position of headers

*polygons.R*

- `get_clip_rect`: create a rectangle of given size around a point, can be used with both projected and long/lat coordinates
- `poly_rect`: shortcut to create a rectangle Polygon given four numbers (min and max along x and y) 
- `rm_small_polys`: removes small polygons and polygons 'slivers' from a SpatialPolygons object 

*raster.R*

- `inv_rotate`: 'inverse' of `rotate` from `raster` package, takes a raster with longitudes in (-180, 180), converts it to (0, 360) 




