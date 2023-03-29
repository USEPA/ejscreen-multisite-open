#' Fast way to find nearby points - finds distance to each Census block centroid nearby
#' @description 
#'   Given a set of points and a specified radius (cutoff), 
#'   this function quickly finds all the US Census blocks near each point. 
#'   For each point, it uses the specified cutoff distance and finds the distance to 
#'   every block within the circle defined by the radius (cutoff). 
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
#'  For all examples, see [ejamit()]
#'  getblocksnearby() is a wrapper redirecting to the right version like [getblocksnearbyviaQuadTree()]
#'    Census block "internal points" are actually what it looks for, like a centroid. 
#'    The blocks are pre-indexed for the whole USA, via the data object quadtree or localtree
#'
#' @inheritParams getblocksnearbyviaQuadTree
#' 
#' @param ...  passed to [getblocksnearbyviaQuadTree()] or other such functions
#' 
#' @seealso [ejamit()]  [getblocksnearbyviaQuadTree()] [getblocksnearbyviaQuadTree_Clustered()] [getblocksnearbyviaQuadTree2()]
#' @export
#'
getblocksnearby  <- function(sitepoints, cutoff=3, maxcutoff=31.07, 
                             avoidorphans=TRUE, 
                             # indexgridsize,
                             quadtree,
                             ...
) {
  
  cat("Analyzing", NROW(sitepoints), "points, radius of", cutoff, "miles.\n") 
  
  # wrapper to make it simple to (later?) switch between functions to use for this, clustered vs not, etc.
  
  # timed <- system.time({
  
  x <- getblocksnearbyviaQuadTree(sitepoints=sitepoints, cutoff=cutoff, maxcutoff=maxcutoff, 
                             avoidorphans=avoidorphans, 
                             # indexgridsize=indexgridsize,
                             quadtree=quadtree,
                             ...)
  
  # getblocksnearbyviaQuadTree_Clustered(sitepoints=sitepoints, cutoff=cutoff, maxcutoff=maxcutoff, 
  #                                        avoidorphans=avoidorphans, 
  #                                      # indexgridsize=indexgridsize,
  #                                      quadtree=quadtree,
  #                                      ...)
  
  # getblocksnearbyviaQuadTree2(sitepoints=sitepoints, cutoff=cutoff, maxcutoff=maxcutoff, 
  #                               avoidorphans=avoidorphans, 
  #                             # indexgridsize=indexgridsize,
  #                             quadtree=quadtree,
  #                             ...)
    
  # })
  # print(timed)
  
  return(x)
}
