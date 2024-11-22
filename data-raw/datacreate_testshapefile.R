
testshapefilename = system.file("testdata/shapes/portland.gdb.zip", package = "EJAM")

testshape = shapefile_from_any(testshapefilename)

 
shp = testshape
# this will be in newer shapefile_from_any()
shp = shapefix(shp)[['shp']] # but may change format of output

# make it a smaller file, object
testshape <- shp[1:2,]


testshape <- metadata_add(testshape)

use_data(testshape, overwrite = TRUE)

# documentation...



################################# #

## These work:
# mapview(testshape)  # and has nice basemap selection button and popups automatically done
# map_shapes_mapview(testshape)
# map_shapes_plot(testshape)

## but this does not work and should fix the issue:
  
#  leaflet(testshape) %>% addPolygons() %>% addTiles()
# map_shapes_leaflet(testshape)
# Warning messages:
#   1: sf layer is not long-lat data 
# 2: sf layer has inconsistent datum (+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs).
# Need '+proj=longlat +datum=WGS84' 
