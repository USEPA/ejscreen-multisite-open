
#' @inheritParams get_blockpoints_in_shape
#' @export
#'
shapefile2blockpoints <- function(...) {
  get_blockpoints_in_shape(...)  
}


#' get_blockpoints_in_shape - find blocks that are in a polygon, using internal point of block - WORK IN PROGRESS ****
#' @aliases shapefile2blockpoints
#' @description This is like getblocksnearby() but for a polygonal buffer area instead of 
#'   a circular buffer.  
#' @details This uses getblocksnearby() to get a very fast rough/good estimate of 
#'   which US block points are nearby (with a safety margin - see param below),
#'   before then using sf:: to carefully identify which of those candidate blocks are actually 
#'   inside each polygon (e.g., circle) according to sf:: methods.
#'   
#'   For circular buffers, just using getblocksnearby() should work and not need this function.
#'   
#'   For noncircular polygons, buffered or not, this function will provide a way to very quickly
#'   filter down to which of the millions of US blocks should be examined by the sf:: join / intersect,
#'   since otherwise it takes forever for sf:: to check all US blocks.
#' @param polys Spatial data as from sf::st_as_sf(), with a column called siteid, like 
#'   points as from [shapefile_from_sitepoints()],
#'   or a table of points with lat,lon columns that will first be converted here using that function,
#'   or polygons 
#' @param addedbuffermiles width of optional buffering to add to the points (or edges), in miles
#' @param blocksnearby optional table of blocks with blockid,siteid (from which lat,lon can be looked up in blockpoints dt)
#' @param dissolved If TRUE, use sf::st_union(polys) to find unique blocks inside any one or more of polys
#' @param safety_margin_ratio multiplied by addedbuffermiles, how far to search for 
#'   blocks nearby using getblocksnearby(), before using those found to do the intersection via sf::
#' @param crs used in st_as_sf() and st_transform() and shape_buffered_from_shapefile_points(), crs = 4269 or Geodetic CRS NAD83 
#' @import sf
#' @return Block points table for those blocks whose internal point is inside the buffer 
#'   which is just a circular buffer of specified radius if polys are just points. 
#'   
#' @examples  
#'   # y <- get_blockpoints_in_shape()
#'   
#'   # x = shapefile_from_sitepoints(testpoints_n(2))
#'   # y = get_blockpoints_in_shape(x, 1)  # very very slow
#' @seealso [get_blockpoints_in_shape()] [shapefile_from_sitepoints()] [shape_buffered_from_shapefile_points()]
#' @export
#'
get_blockpoints_in_shape <- function(polys, addedbuffermiles=0, blocksnearby=NULL, dissolved=FALSE, safety_margin_ratio=1.10, crs = 4269) {
  
  ############################################################################################################### #
  # NOTE: For comparison or validation one could get the results from the EJScreen API, for a polygon:
  #      Example of how the API could be used to analyze a polygon, which must use POST not GET:
  # HTTP POST URL: https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx
  # HTTP POST Body:
  #   namestr=
  #   geometry={"spatialReference":{"wkid":4326},"rings":[[[-76.6418006649668,39.41979061319584],[-76.54223706633402,39.403875492879656],[-76.48158343568997,39.32424541053687],[-76.45526191279846,39.24452456392063],[-76.63378974482964,39.202856485626576],[-76.74021979854052,39.284396329589654],[-76.74594187237864,39.37911140807963],[-76.6418006649668,39.41979061319584]]]}
  #   distance=
  #   unit=9035
  #   areatype=
  #   areaid=
  #   f=pjson
  ############################################################################################################ #
  
  ## overall bbox
  bbox <- sf::st_bbox(polys)
  
  ## filter blockpoints to overall bbox
  blockpoints_filt <-  blockpoints %>% 
    dplyr::filter(
      lon > bbox['xmin'],
      lon < bbox['xmax'],
      lat > bbox['ymin'],
      lat < bbox['ymax']
    ) 
  
  ## individual bbox per polygon
  bbox_polys <- lapply(polys$geometry, sf::st_bbox)
  
  ## filter blockpoints again to individual bboxes, keep unique points
  blockpoints_filt <- lapply(bbox_polys, function(a) blockpoints_filt[between(lon, a[1], a[3]) & 
                                                                        between(lat, a[2], a[4]), ]) %>% 
    rbindlist %>% 
    unique
  
  blockpoints_sf <- sf::st_as_sf(blockpoints_filt, coords = c('lon', 'lat'), crs = crs)
  
  if (!exists("blockpoints_sf")) {
    stop("requires the blockpoints   called blockpoints_sf  you can make like this: \n blockpoints_sf <-  blockpoints |> sf::st_as_sf(coords = c('lon', 'lat'), crs= 4269) \n # Geodetic CRS:  NAD83 ")
  }
  
  # CHECK FORMAT OF polys - ensure it is spatial object (with data.frame/ attribute table? ) 
  if (!("sf" %in% class(polys))) {
    polys <-  shapefile_from_sitepoints(polys, crs = crs)
  }
  ARE_POINTS <- "POINT" == names(which.max(table(sf::st_geometry_type(polys)))) 
  
  # ensure it has unique IDs called siteid column, or else add that column? getblocksnearby() adds it  
  
  
  
  if (addedbuffermiles > 0) {
    addedbuffermiles_withunits <- units::set_units(addedbuffermiles, "miles")
    polys <- shape_buffered_from_shapefile_points(polys,  addedbuffermiles_withunits, crs = crs)
    # addedbuffermiles_withunits  name used since below getblocksnearby( , radius=addedbuffermiles etc ) warns units not expected
  }  
  
  
  
  # use   sf::st_intersects() or st_join(, join=intersects) 
  
  if (is.null(blocksnearby) & ARE_POINTS) {
    #  calculate it here since not provided
    # get lat,lon of sites
    pts <-  data.table(sf::st_coordinates(polys))  # this is wasteful if they provided a data.frame or data.table and we convert it to sf and then here go backwards
    setnames(pts, c("lon","lat")) # I think in this case it must be lon first then lat, due to how st_coordinates() output is provided?
    # get blockid of each nearby census block
    blocksnearby <- getblocksnearby(pts, addedbuffermiles * safety_margin_ratio)  # blockid, distance, siteid # don't care which siteid was how this block got included in the filtered list
    # get lat,lon of nearby blocks
    blocksnearby <- (blockpoints[blocksnearby, .(lat,lon,blockid), on = "blockid"])  # blockid,      lat ,      lon
  }
  if (is.null(blocksnearby) & !ARE_POINTS) {
    # must use extremely slow method ?
    #stop("noncircular buffers not working yet - too slow to find all US blocks in each via simple sf::st_join   ")
    
    if (dissolved) {
      # warning("using getblocksnearby() to filter US blocks to those near each site must be done before a dissolve  ")
      polys <- sf::st_union(polys)
    }
    blocksinside <- sf::st_join(blockpoints_sf, sf::st_transform(polys, crs = crs), join = sf::st_intersects, left = 'FALSE' )
    
    
    # OR...  find centroid of each polygon and 
    # figure out bounding box (or max radius) of each 
    # and then use getblocksnearby() to find all in bounding box (based on some radius like 
    # farthest point from centroid, which is worst-case, distance to the furthest corner of bounding box??)
    
    
    
  }
  
  if (dissolved) {
    # warning("using getblocksnearby() to filter US blocks to those near each site must be done before a dissolve  ")
    polys <- sf::st_union(polys)
  }
  
  blocksinsidef <- unique(blocksinside)
  
  #standardize input shapes for doaggregate
  
  pts <-  data.table(
    sf::st_coordinates(blocksinsidef),
    blocksinsidef$ejam_uniq_id,
    #blocksinsidef$siteid,
    blocksinsidef$blockid,
    distance = 0
  )
  
  #setnames(pts, c("lon","lat","siteid","blockid","distance")) # it is lon then lat due to format of output of st_coordinates() I think
  #pts[blockwts,  `:=`(bgid = bgid, blockwt = blockwt), on = "blockid"]
  #data.table::setcolorder(pts, c('siteid', 'blockid', 'distance', 'blockwt', 'bgid', 'lat', 'lon')) # to make it same order as output of getblocksnearby(), plus latlon
  setnames(pts, c("lon","lat","ejam_uniq_id","blockid","distance")) # it is lon then lat due to format of output of st_coordinates() I think
  pts[blockwts,  `:=`(bgid = bgid, blockwt = blockwt), on = "blockid"]
  data.table::setcolorder(pts, c('ejam_uniq_id', 'blockid', 'distance', 'blockwt', 'bgid', 'lat', 'lon')) # to make it same order as output of getblocksnearby(), plus latlon
  
  return(list('pts' = pts, 'polys' = polys))
  
}

