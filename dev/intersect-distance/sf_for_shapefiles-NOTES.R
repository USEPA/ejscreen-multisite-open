

# notes on key steps in using sf package


# also see C:/Users/mcorrale/R/mysource/EJAM/inst/notes_intersect_shape_or_dist_calc_and_FRS_density/NOTES_EPA_ATMOS_Highperformancecluster_parallelization.R 

########################################################################### # 
# READ SHAPEFILE ####

# https://r-spatial.github.io/sf/articles/sf2.html
# or using the DBI interface:
# https://r-spatial.github.io/sf/articles/sf2.html#reading-and-writing-directly-to-and-from-spatial-databases

library(sf)
sf::sf_use_s2(TRUE)
# library(dplyr) 

## e.g., read a shapefile that is the State of NC boundaries
nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
plot(nc)
par(mar = c(0,0,1,0))
plot(nc[1], reset = FALSE) # reset = FALSE: we want to add to a plot with a legend
plot(nc[1,1], col = 'grey', add = TRUE)

# or  from https://r-spatial.github.io/sf/articles/sf1.html#how-attributes-relate-to-geometries ...
nc <- sf::st_read(system.file("shape/nc.shp", package="sf"),
              agr = c(AREA = "aggregate", PERIMETER = "aggregate", CNTY_ = "identity",
                      CNTY_ID = "identity", NAME = "identity", FIPS = "identity", FIPSNO = "identity",
                      CRESS_ID = "identity", BIR74 = "aggregate", SID74 = "aggregate", NWBIR74 = "aggregate",
                      BIR79 = "aggregate", SID79 = "aggregate", NWBIR79 = "aggregate"))
print(nc[9:15], n = 3)


## e.g. read CEJST dataset ####
usa <- sf::st_read( "~/../../OneDrive - Environmental Protection Agency (EPA)/Downloads/1.0-shapefile-codebook/usa/" )
usa
# add FIPS.ST and ST columns, and 
# add that info for Island areas too
# usa$ST <- ejanalysis package file get.state.info(usa$FIPS.ST )[,'ST']
# get.state.info(c("60", "66", "69", "78"))$ST
# [1] "AS" "GU" "MP" "VI"
stateinfo2 <- EJAM::stateinfo
stateinfo2 <- rbind(stateinfo2, data.frame(ST = c("AS", "GU", "MP", "VI"), 
                                           statename = c("American Samoa", "Guam", "Northern Mariana Islands", "U.S. Virgin Islands"),
                                           ftpname   = c("American Samoa", "Guam", "Northern Mariana Islands", "U.S. Virgin Islands"), 
                                           FIPS.ST = c("60", "66", "69", "78"), 
                                           REGION  = c(9, 9, 9, 2) ))
stateinfo2 <- unique(stateinfo2)
usa$ST   <-  stateinfo2$ST[match(usa$FIPS.ST,  stateinfo2$FIPS.ST)]
usa$FIPS.ST = substr(usa$GEOID10,1,2)

########################################################################### # 
# WRITE/SAVE SHAPEFILE  as .rda ####
save(usa_cejst_shape, file = 'usa_cejst_shape.rda')
# load("~/../EJ 2021/CEJST DATA/usa_cejst_shape.rda")

########################################################################### # 
# WRITE/SAVE SHAPEFILE as .shp ####
sf::st_write(usa_cejst_shape, "usa_cejst_shape.shp")
## Writing layer `usa_cejst_shape' to data source `usa_cejst_shape.shp' using driver `ESRI Shapefile'
## Writing xxx features with xxx fields and geometry type Multi Polygon.

############################################# #
##  CHANGING UNITS ####

a <- sf::st_area(nc[1,])
attributes(a)
# The units package can be used to assign and convert between units:
  
  units::set_units(a, km^2) # result in square kilometers
## 1137.108 [km^2]

# ____ ####


# example my lat lon data ####

mydata <-  data.table::setDF(data.table::copy(EJAM::testpoints_1000_dt[-1, ])) # first row is NA, not allowed in st_as_sf.data.frame() 
mydata <- sf::st_as_sf(mydata, coords = c("lon", "lat"), crs = 4269, remove = FALSE)


########################################################################### # 
# MAPS ####

## mapview() to see tracts ####
usa
mapview::mapview(usa[usa$ST == 'DE', c( "GEOID10" ,"SF"   ,   "CF"  ,    "DF_PFS", 'FIPS.ST', 'ST', 'geometry') ])
mapview::mapview(usa[usa$ST == 'NY',  ])
usa_cejst_shape <- usa; rm(usa)
########################################################################### # 

#################################### #
# Example - BUFFER & MAP ####
# of sf::st_buffer() and mapview() ### #

library(units)
library(tidyverse)
library(sf)
library(mapview)
#  help("mapview", package = "mapview")

radius <- 5000
# should also use units::install_unit()  ?

#################################### #
## _Specify point to be center of circular buffer: ####

lon <- -76.6    # e.g. in Baltimore
lat <- 39.2
elev <- units::set_units(242.0, "ft") # just an example
site <- sf::st_point(x = c(lon, lat, elev), dim = "XYZ")
# site <- sf::st_point(x = c(lon, lat ), dim = "XY")
site <- site %>% sf::st_sfc(crs = 4326) 

#################################### #  
## _Create your buffer in meters around this point: ####

buffer <- sf::st_buffer(site, radius)

#################################### #
## _plot the site and circle ####
# plot(buffer)
# plot(site, add = TRUE)

################################### #
### _mapview for nice interactive map with basemaps ####

mapview::mapview(buffer, map.types = c("OpenStreetMap", "OpenTopoMap", "Esri.WorldImagery", "CartoDB.Positron"  ))
# see help("mapview", package = "mapview")

#################################### #
## _buffers as rings - annulus (ring) ####

buff1 <- sf::st_buffer(site, dist=1*radius)
buff2 <- sf::st_buffer(site, dist=2*radius)
ring  <- sf::st_difference(buff2, buff1)

## _mapview to map it ####
mapview::mapview(ring, map.types = c("OpenStreetMap", "OpenTopoMap", "Esri.WorldImagery", "CartoDB.Positron"  ))

#################################### #
## put grid on top of shapefile - NC example  ####

nc <- NULL
demo(nc, ask = FALSE, echo = FALSE)
sf::st_crs(nc)$proj4string
# [1] "+proj=longlat +datum=NAD27 +no_defs"
nc <- sf::st_transform(nc, crs =sf::st_crs(5070)) # NAD27
cs <- c(10000, 10000)
my_grid <- sf::st_make_grid(x = nc, cellsize=cs)
mapview::mapview(my_grid) + mapview::mapview(nc['NAME'])

#################################### #
#### plot state and grid using plot #########

# plot(nc['NAME'], main = 'North Carolina')
# plot(my_grid, add = TRUE)
#################################### #
#### plot just using ggplot #########

# ring %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_sf()

# ____ ####



########################################################################### # 
# JOINS & GEO OPS  ####

# Join on attributes, just like a regular merge() but for spatial objects, ignores the spatial info

x = sf::st_sf(a = 1:2, geom = st_sfc(st_point(c(0,0)), st_point(c(1,1))))
y = data.frame(a = 2:3)
merge(x, y)
# https://r-spatial.github.io/sf/articles/sf4.html


########################################################################### # 
# **  using sf:: ####
#
# like getblocksnearby()  but via sf package 

## ___* st_is_within_distance() fast but only in circle, vs st_buffer + intersect is slower? ### #

## ___ * st_buffer() using R2 vs S2 (s2 pkg, spherical approach) ### #

# ( "https://r-spatial.github.io/sf/articles/sf7.html")
# https://r-spatial.github.io/sf/articles/ 
# ?sf_use_s2()

########################################################################### # 
# * SPATIAL JOINS: st_join(x, join= st_covers, st_intersects, st_within, etc. ####

# Spatial join to find overlaps and do merge/join on attributes, for example:

# can join based on st_intersects, by default,
sf::st_join(x, y)
# or by other spatial relationships
# https://r-spatial.github.io/sf/articles/sf3.html#geometrical-operations
sf::st_join(x, y, join = st_covers) # sf::st_covers
# https://r-spatial.github.io/sf/articles/sf4.html

########################################################################### # 
# ** NEAR A POINT = IN A CIRCLE: st_is_within_distance() VS st_buffer() + st_intersects() ####
#
#  st_is_within_distance() may be faster than creating a circular buffer around a point and then using intersect to find blockpoints in the circle

########################################################################### # 
# ** IN A POLYGON: st_intersects() ####
#   WHAT POINTS ARE INSIDE SOME POLYGON LIKE A BUFFER/ LIKE HIGH-RISK ZONE:  

## sf::st_intersects() to find points inside polygon or buffered polygon.  

########################################################################### # 
# ** NEAR A POLYGON: buffer(), then intersects(); or else is_within_distance()  ####
#    (NONCIRCULAR) BUFFER AROUND A POLYGON = near a POLYGON OR ROAD LINE: 
#     CREATE A BUFFER THAT ADDS SOME DISTANCE FROM A SHAPE LIKE NPL SITE  with 
# 
## sf::st_buffer() to add buffer around polygon ####
## sf::st_intersects() to then find points in that buffer, i.e., near the polygon. ####

########################################################################### # 
# ** DISTANCES: st_distance() ####

# st_distance() 
# returns a dense numeric matrix with distances between geometries. 
x = sf::st_transform(nc, 32119)
sf::st_distance(x[c(1,4,22),], x[c(1, 33,55,56),])

########################################################################### # 
# ** DISSOLVE multiple circles or shapes into one ####
# geo with no doublecounting of overlaps
# use  st_union()   https://r-spatial.github.io/sf/reference/geos_combine.html 

y <- structure(list(structure(list(structure(
  c(-0.8, 0.8, 0.8, -0.8, -0.8, -0.8, -0.8, 0.8, 0.8, -0.8), dim = c(5L, 2L), 
  class = c("matrix", "array"))), class = c("XY", "POLYGON", "sfg")), structure(list(
    structure(
      c(1.6, 2.4, 2.4, 1.6, 1.6, 0.3, 0.3, 1.1, 1.1,  0.3), dim = c(5L, 2L), class = c("matrix", "array"))), 
    class = c("XY", "POLYGON", "sfg")), 
  structure(list(structure(
    c(0.2, 1.8, 1.8, 0.2, 0.2, 0.2, 0.2, 1.8, 1.8, 0.2), dim = c(5L, 2L), 
    class = c("matrix",  "array"))), class = c("XY", "POLYGON", "sfg")), 
  structure(list(
    structure(
      c(1.5, 2.5, 2.5, 1.5, 1.5, -1, -1, 0, 0, -1), dim = c(5L, 2L), class = c("matrix", "array"))), class = c("XY", "POLYGON",  "sfg"))), 
  class = c("sfc_POLYGON", "sfc"), precision = 0, bbox = structure(
    c(xmin = -0.8,  ymin = -1, xmax = 2.5, ymax = 1.8), class = "bbox"), 
  crs = structure(list(input = NA_character_, wkt = NA_character_), class = "crs"), n_empty = 0L)

plot(y)
plot(sf::st_union(y))

## _NOTE: picking st_buffer+intersect vs st_is_within_distance (only if circular buffer) ####
#
#   As discussed in the sf issue tracker, deciding on workflows and selecting 
# appropriate levels of level of geographic resolution can be an iterative process. 
# * st_buffer as powered by GEOS, for R2 data, are smooth and (nearly) exact. 
# * st_buffer as powered by S2 is rougher, complex, non-smooth, and may need tuning. 
# An common pattern where st_buffer is used is this:
#   
# 1.  compute buffers around a set of features x (points, lines, polygons)
# 2. within each of these buffers, find all occurances of some other spatial variable y 
# and aggregate them (e.g. count points, or average a raster variable like precipitation or population density)
# 3. work with these aggregated values (discard the buffer)
# 
# When this is the case, and you are working with geographic coordinates, 
# it may pay off to not compute buffers, but instead directly work with 
# *** st_is_within_distance to select, for each feature of x, 
# *** all features of y that are within a certain distance d from x. 
# *** The S2 version of this function uses spatial indexes, so is fast for large datasets. ***
# * ####


########################################################################### # 
## Example using OSM data ####

cycleway_osm = osmdata::osmdata_sf(osmdata::add_osm_feature(
  opq = osmdata::opq("leeds"), key = "name", 
  value = "Cycle Superhighway 1"))
cycleway = cycleway_osm$osm_lines 
geometry = cycleway_osm$osm_lines %>%  sf::st_union()
mysite <- sf::st_sf(data.frame(name = "cycleway"), geometry) 
class(mysite)


mysite <- sf::st_sf(data.frame(name = "cycleway"), geometry) 
plot(mysite, main='buffering around this shape')
buffer_lowres = sf::st_buffer(mysite, 100, max_cells = 500)
plot(buffer_lowres, main='low resolution buffer')
buffer_hires = sf::st_buffer(mysite, 100, max_cells = 9000)
plot(buffer_hires, main='slower but higher resolution buffer')

radius_miles <- 3
mypoints_geo <-  EJAMejscreenapi::testpoints_50 # need to convert to sf format?
blockpoints_geo <-  blockpoints  # need to convert to sf format?

sf::st_is_within_distance(x =  mypoints_geo , y= blockpoints_geo,
                      dist = units::set_units(radius_miles, value = US_survey_mile))

# in sf, dist is assumed to be in decimal degrees (arc_degrees), so need set_units() to specify.
#   pt = sf::st_sfc(st_point(c(0,0)), crs = 4326)
# buf = sf::st_buffer(polygon, 1)
# buf = sf::st_buffer(polygon, units::set_units(1, degree))



########################################################################################################## #
