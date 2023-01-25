
# FIND POINTS WITHIN A DISTANCE via sf:: ####
#
# like getblocksnearby but via sf package 
# * st_buffer() vs st_is_within_distance() ####
# * st_buffer() via R2 vs S2 (s2 pkg, spherical approach) ####
browseURL( "https://r-spatial.github.io/sf/articles/sf7.html")
# https://r-spatial.github.io/sf/articles/ 
# sf_use_s2()
# ____ ####

# Picking between st_buffer and st_is_within_distance ####
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

# old way of using sp to create a spatial dataframe (but sf will replace sp over time)
#   mydf <- EJAMejscreenapi::testoutput_ejscreenapi_plus_50
#   mysp   <- sp::SpatialPoints(mydf[,c('lon','lat')])
#   myspdf <- sp::SpatialPointsDataFrame(mysp, mydf)


########################################################################### # 
# sf to read SHAPEFILE ####

library(sf)
sf_use_s2(TRUE)
# library(dplyr) 

## e.g. read CEJST dataset ####
usa <- st_read( "~/../../OneDrive - Environmental Protection Agency (EPA)/Downloads/1.0-shapefile-codebook/usa/" )
usa
# add FIPS.ST and ST columns, and 
# add that info for Island areas too
# usa$ST <- ejanalysis::get.state.info(usa$FIPS.ST )[,'ST']
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
# mapview to see shapefile tracts ####
usa
mapview::mapview(usa[usa$ST == 'DE', c( "GEOID10" ,"SF"   ,   "CF"  ,    "DF_PFS", 'FIPS.ST', 'ST', 'geometry') ])
mapview::mapview(usa[usa$ST == 'NY',  ])
usa_cejst_shape <- usa; rm(usa)
########################################################################### # 
# Save shapefile as .rda ####
save(usa_cejst_shape, file = 'usa_cejst_shape.rda')
# ___ ####
########################################################################### # 
# Using sf:: ####

## get example data ####
cycleway_osm = osmdata::osmdata_sf(osmdata::add_osm_feature(
  opq = osmdata::opq("leeds"), key = "name", 
  value = "Cycle Superhighway 1"))
cycleway = cycleway_osm$osm_lines 
geometry = cycleway_osm$osm_lines %>%  sf::st_union()
mysite <- st_sf(data.frame(name = "cycleway"), geometry) 
class(mysite)

########################################################################### # 
# ** NEAR A POINT = in a circular buffer ####

#     sf::st_is_within_distance()   



########################################################################### # 
# ** INSIDE A POLYGON LIKE HIGH-RISK ZONE ####
#   WHAT POINTS ARE INSIDE SOME POLYGON LIKE A BUFFER:  

## sf::st_intersects() to find points inside polygon or buffered polygon.  



########################################################################### # 
# ** NEAR A POLYGON (buffer, then intersects) ####
#    (NONCIRCULAR) BUFFER AROUND A POLYGON = near a POLYGON OR ROAD LINE: 
#     CREATE A BUFFER THAT ADDS SOME DISTANCE FROM A SHAPE LIKE NPL SITE  with 

# sf::st_buffer() to add buffer around polygon ####
# sf::st_intersects() to then find points in that buffer, i.e., near the polygon. #


mysite <- st_sf(data.frame(name = "cycleway"), geometry) 
  plot(mysite, main='buffering around this shape')
buffer_lowres = st_buffer(mysite, 100, max_cells = 500)
  plot(buffer_lowres, main='low resolution buffer')
buffer_hires = st_buffer(mysite, 100, max_cells = 9000)
  plot(buffer_hires, main='slower but higher resolution buffer')

  radius_miles <- 3
  mypoints_geo <-  EJAMejscreenapi::testpoints_50 # need to convert to sf format?
  blockpoints_geo <- EJAMblockdata::blockpoints  # need to convert to sf format?
  st_is_within_distance(x =  mypoints_geo , y= blockpoints_geo,
                        dist = units::set_units(radius_miles, value = US_survey_mile))
 # in sf, dist is assumed to be in decimal degrees (arc_degrees), so need set_units() to specify.
 #   pt = st_sfc(st_point(c(0,0)), crs = 4326)
 # buf = st_buffer(polygon, 1)
 # buf = st_buffer(polygon, units::set_units(1, degree))
 
########################################################################################################## #
  
  # ___ ####
  # Another set of notes, separate:
  
  
  
  #################################### #
  ##### Example of sf::st_buffer() and mapview() ####
  
  library(units)
  library(tidyverse)
  library(sf)
  library(mapview)
  #  help("mapview", package = "mapview")
  
  radius <- 5000
  # should also use units::install_unit()  ?
  
  #################################### #
  ## Specify point to be center of circular buffer: ####

  lon <- -76.6    # e.g. in Baltimore
  lat <- 39.2
  elev <- units::set_units(242.0, "ft") # just an example
  
  site <- sf::st_point(x = c(lon, lat, elev), dim = "XYZ")
  site <- site %>% sf::st_sfc(crs = 4326)
  # # transform your coordinate to Irish Grid, if viewing Dublin  # site = sf::st_transform(site, 29902)

  #################################### #  
  ## Create your buffer in meters around this point: ####
  
  buffer <- sf::st_buffer(site, radius)
  
  #################################### #
  ## plot the site and circle ####
  # plot(buffer)
  # plot(site, add = TRUE)
  
  ################################### #
  #### mapview for nice interactive map with basemaps ####
  
  mapview::mapview(buffer, map.types = c("OpenStreetMap", "OpenTopoMap", "Esri.WorldImagery", "CartoDB.Positron"  ))
  # see help("mapview", package = "mapview")
  
  ####################################################################### #
  
  #################################### #
  ## annulus (ring) ####
  
  buff1 <- sf::st_buffer(site, dist=1*radius)
  buff2 <- sf::st_buffer(site, dist=2*radius)
  ring  <- sf::st_difference(buff2, buff1)
  
  mapview::mapview(ring, map.types = c("OpenStreetMap", "OpenTopoMap", "Esri.WorldImagery", "CartoDB.Positron"  ))
  
  #################################### #
  #### plot just using ggplot #########
  
  # ring %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_sf()
  
  ####################################################################### #
  
  #################################### #
  #### put grid on top of shapefile ####
  
  nc <- NULL
  demo(nc, ask = FALSE, echo = FALSE)
  sf::st_crs(nc)$proj4string
  # [1] "+proj=longlat +datum=NAD27 +no_defs"
  
  nc <- sf::st_transform(nc, crs=sf::st_crs(5070))
  cs <- c(10000, 10000)
  my_grid <- sf::st_make_grid(x = nc,
                              cellsize=cs)
  
  #################################### #
  #### mapview for plot of grid and States #######
  
  mapview::mapview(my_grid) + mapview::mapview(nc['NAME'])
  
  #################################### #
  #### plot state and grid using plot #########
  
  # plot(nc['NAME'], main = 'North Carolina')
  # plot(my_grid, add = TRUE)
  ###################################################################### ##
  
  
  
