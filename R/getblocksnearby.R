#' getblocksnearby - Fast way to find nearby points (distance to each Census block centroid near each site)
#' @description 
#'   Given a set of points and a specified radius, 
#'   this function quickly finds all the US Census blocks near each point. 
#'   For each point, it uses the specified radius distance and finds the distance to 
#'   every block within the circle defined by the radius. 
#'   Each block is defined by its Census-provided internal point, by latitude and longitude.
#'   
#'   Each point can be the location of a regulated facility or other type of site, and 
#'   the blocks are a high-resolution source of information about where
#'   residents live. 
#'   
#'   Finding which blocks have their internal points in a circle provides 
#'   a way to quickly estimate what fraction of a block group is 
#'   inside the circular buffer more accurately and more quickly than
#'   areal apportionment of block groups would provide. 
#'   
#' @details 
#'  See [ejamit()] for examples.
#'  
#'  getblocksnearby() is a wrapper redirecting to the right version, like [getblocksnearbyviaQuadTree()]
#'    Census block "internal points" (defined by Census Bureau) are actually what it looks for, 
#'    and they are like centroids. 
#'    The blocks are pre-indexed for the whole USA, via the data object quadtree aka localtree
#'
#' @inheritParams getblocksnearbyviaQuadTree
#' @param ...  passed to [getblocksnearbyviaQuadTree()] or other such functions
#' 
#' @seealso [ejamit()]  [getblocksnearbyviaQuadTree()] [getblocksnearbyviaQuadTree_Clustered()] [getblocksnearbyviaQuadTree2()]
#' @export
#'
getblocksnearby  <- function(sitepoints, radius=3, maxradius=31.07, 
                             avoidorphans=FALSE, 
                             # indexgridsize,
                             quadtree, 
                             quiet=FALSE,
                             parallel=FALSE,
                             ...
) {
  ################################################################################## #
  # select file if missing
  if (missing(sitepoints)) {
    if (interactive()) {
      sitepoints <- rstudioapi::selectFile()
    } else {
      stop("sitepoints (locations to analyze) is missing but required.")
    }
  }
  # if user entered a table, path to a file (csv, xlsx), or whatever, then read it to get the lat lon values from there
  sitepoints <- latlon_from_anything(sitepoints)
  ################################################################################## #
  # timed <- system.time({
  if (missing(quadtree)) {
    if (exists("localtree")) {
      quadtree <- localtree 
    } else {    #  SEE IF WE EVER NEED TO OR EVEN CAN CREATE THIS ON THE FLY HERE FOR SOME INTERACTIVE USERS, BUT SHOULD NOT BE AN ISSUE IF PKG LOADED
      if (!exists("quaddata") | !exists("blockwts") | !exists("blockpoints") ) {  #| !exists("bgid2fips")
        # should 
        cat('census block data file(s) not already loaded, so key data will now be downloaded (or loaded from a local copy if possible)...\n')
        dataload_from_aws() # loads quaddata needed to make localtree index, and several other large files pkg uses.
      }
      # need to pause here?
      # localtree <- SearchTrees::createTree( quaddata, treeType = "quad", dataType = "point")
      indexblocks() # not really tested yet in this context
      quadtree <- localtree 
      # stop(paste0("Nationwide index of block locations is required but missing (quadtree parameter default is called localtree but was not found). ",
      #             'Try this: \n\n',
      #             'localtree <- SearchTrees::createTree( quaddata, treeType = "quad", dataType = "point") \n\n'
      # ))
    }
  }
  cat("Analyzing", NROW(sitepoints), "points, radius of", radius, "miles around each.\n")
  
  ################################################################################## #
  # wrapper to make it simple to (later?) switch between functions to use for this, clustered vs not, etc.
  
  if (!parallel) {
    x <- getblocksnearbyviaQuadTree(sitepoints = sitepoints, radius = radius, maxradius = maxradius, 
                                    avoidorphans = avoidorphans, 
                                    # indexgridsize = indexgridsize,
                                    quadtree = quadtree, quiet = quiet,
                                    ...)
  } else {
    stop('parallel processing version not implemented yet')
    x <- getblocksnearbyviaQuadTree_Clustered(sitepoints = sitepoints, radius = radius, maxradius = maxradius,
                                              avoidorphans = avoidorphans,
                                              # indexgridsize = indexgridsize,
                                              quadtree = quadtree,
                                              ...)
  }
  
  #  DRAFT / WAS WORK IN PROGRESS: 
  # getblocksnearbyviaQuadTree2(sitepoints = sitepoints, radius = radius, maxradius = maxradius, 
  #                               avoidorphans = avoidorphans, 
  #                             # indexgridsize = indexgridsize,
  #                             quadtree = quadtree,
  #                             ...)
  
  # })
  # print(timed)
  
  return(x)
}
