#' Wrapper for getblocksnearby() plus doaggregate()
#'
#' @param sitepoints    see [getblocksnearbyviaQuadTree()] or other such functions
#' @param cutoff        see [getblocksnearbyviaQuadTree()] or other such functions
#' @param maxcutoff     see [getblocksnearbyviaQuadTree()] or other such functions
#' @param avoidorphans  see [getblocksnearbyviaQuadTree()] or other such functions
#' @param quadtree a large quadtree object created from the SearchTree package example:
#'    SearchTrees::createTree(EJAMblockdata::quaddata, treeType = "quad", dataType = "point")
#' @param ...  see [getblocksnearbyviaQuadTree_Clustered] or other such functions
#'
#' @export
getblocksnearby_and_doaggregate <- function(sitepoints, 
                                            cutoff=1, maxcutoff=31.07, 
                                            avoidorphans=TRUE, 
                                            quadtree,
                                            ...
) {
  cat('Finding blocks nearby, in buffers\n')
  cat('Aggregating at each buffer and overall\n')
  doaggregate(
    sitepoints=sitepoints,  
    getblocksnearby(sitepoints=sitepoints, 
                    cutoff=cutoff, maxcutoff=maxcutoff, 
                    avoidorphans=avoidorphans, 
                    quadtree=quadtree,
                    ...)
  )
}
