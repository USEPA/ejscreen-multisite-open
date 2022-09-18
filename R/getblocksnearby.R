#' Key buffering function - wrapper redirecting to the right version of the getblocksnearby function
#' 
#'  As written it assumes that certain things are already in global environment. 
#'  
#' @param sitepoints   see \link{getblocksnearbyviaQuadTree} or other such functions
#' @param cutoff       see \link{getblocksnearbyviaQuadTree} or other such functions
#' @param maxcutoff    see \link{getblocksnearbyviaQuadTree} or other such functions
#' @param avoidorphans see \link{getblocksnearbyviaQuadTree} or other such functions
#' @param ...          see \link{getblocksnearbyviaQuadTree_Clustered} or other such functions
#'
#' @export
#'
getblocksnearby <- function(sitepoints, cutoff=1, maxcutoff=31.07, 
                            avoidorphans=TRUE, 
                            # indexgridsize,
                            quadtree,
                            ...
                            ) {
  # wrapper to make it simple to switch between functions to use for this, clustered vs not, etc.

  
  getblocksnearbyviaQuadTree(sitepoints=sitepoints, cutoff=cutoff, maxcutoff=maxcutoff, 
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

}
