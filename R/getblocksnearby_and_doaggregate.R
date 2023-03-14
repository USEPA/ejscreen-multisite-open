#' Wrapper for getblocksnearby() plus doaggregate()
#'
#' @param sitepoints    see [getblocksnearbyviaQuadTree()] or other such functions
#' @param cutoff        see [getblocksnearbyviaQuadTree()] or other such functions
#' @param maxcutoff     see [getblocksnearbyviaQuadTree()] or other such functions
#' @param avoidorphans  see [getblocksnearbyviaQuadTree()] or other such functions
#' @param quadtree a large quadtree object created from the SearchTree package example:
#'    SearchTrees::createTree(EJAMblockdata::quaddata, treeType = "quad", dataType = "point")
#' @param ...  see [getblocksnearbyviaQuadTree_Clustered()] or other such functions
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
  mysites2blocks <- getblocksnearby(
    sitepoints=sitepoints, 
    cutoff=cutoff, maxcutoff=maxcutoff, 
    avoidorphans=avoidorphans, 
    quadtree=quadtree,
    ...)
  sites2states = data.frame(siteid = sitepoints$siteid, ST=NA)
  # sites2states$siteid <- sitepoints$siteid
  sites2states$ST <- state_from_latlon(lat = sitepoints$lat, lon = sitepoints$lon)[, "ST"]
  x <- suppressWarnings (doaggregate(sites2blocks = mysites2blocks, sites2states = sites2states))
  return(x)
}
