#' Key buffering function - wrapper redirecting to the right version of getblocksnearby()
#'  As written it assumes that certain things are already in global environment. 
#'  
#' @param sitepoints   see [getblocksnearbyviaQuadTree()] or other such functions
#' @param cutoff       see [getblocksnearbyviaQuadTree()] or other such functions
#' @param maxcutoff    see [getblocksnearbyviaQuadTree()] or other such functions
#' @param avoidorphans see [getblocksnearbyviaQuadTree()] or other such functions
#' @param quadtree a large quadtree object created from the SearchTree package example:
#'    SearchTrees::createTree(EJAMblockdata::quaddata, treeType = "quad", dataType = "point")
#' @param ...          see [getblocksnearbyviaQuadTree_Clustered()] or other such functions
#' @seealso [getblocksnearby2()] that was work in progress
#' @export
#'
getblocksnearby  <- function(sitepoints, cutoff=1, maxcutoff=31.07, 
                             avoidorphans=TRUE, 
                             # indexgridsize,
                             quadtree,
                             ...
) {
  
  
  
  
  
  
  
  # wrapper to make it simple to (later?) switch between functions to use for this, clustered vs not, etc.
  
  
  
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
