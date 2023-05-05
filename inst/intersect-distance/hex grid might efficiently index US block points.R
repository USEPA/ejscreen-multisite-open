
# considered exploring the hex grid as a way to efficiently index US block points, 
# possibly more efficient than the quadtree index now used by EJAM?
# but did not have time to figure out how to implement this - do you "build an index?" and use it??
# not sure how to "index" all blockpoints to find nearby ones later quickly as getblocksnearby() does.
# - the point_to_cell() step crashed R even with just levels 0 to 3 of resolution which is not much. 


# https://cran.r-project.org/web/packages/h3jsr/vignettes/intro-to-h3jsr.html#General_information

local_options <- options()
library(sf)
library(dplyr)
library(ggplot2)
library(h3jsr)
options(stringsAsFactors = FALSE)

library(EJAMblockdata)
library(sf)

# blockpoints_sf <- EJAMblockdata::blockpoints |> sf::st_as_sf(coords = c("lon", "lat"), crs= 4326) # 
blockpoints_sf <- EJAMblockdata::blockpoints |> sf::st_as_sf(coords = c("lon", "lat"), crs= 4269) # Geodetic CRS:  NAD83
blockpoints_sf <- st_transform(blockpoints_sf, crs = 4326) # this takes forever. not sure it has to be read as nad83 and then transformed like this to I think its wgs84
blockpoints_sf

hexes <- point_to_cell(blockpoints_sf, res = seq(0, 3), simple = TRUE)
