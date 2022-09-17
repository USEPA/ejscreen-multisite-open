#' Wrapper for getblocksnearby() plus doaggregate()
#'
#' @param sitepoints    see \link{getblocksnearbyviaQuadTree_Clustered} or other such functions
#' @param cutoff        see \link{getblocksnearbyviaQuadTree_Clustered} or other such functions
#' @param maxcutoff     see \link{getblocksnearbyviaQuadTree_Clustered} or other such functions
#' @param uniqueonly    see \link{getblocksnearbyviaQuadTree_Clustered} or other such functions
#' @param avoidorphans  see \link{getblocksnearbyviaQuadTree_Clustered} or other such functions
#' @param ...  see \link{getblocksnearbyviaQuadTree_Clustered} or other such functions
#'
#' @export
getblocksnearby_and_doaggregate <- function(sitepoints, cutoff=1, maxcutoff=31.07, 
                                   uniqueonly=FALSE, avoidorphans=TRUE, 
                                   # indexgridsize,
                                   quadtree,
                                   ...
) {
  cat('Finding blocks nearby, in buffers\n')
  blocks <- getblocksnearby(sitepoints=sitepoints, cutoff=cutoff, maxcutoff=maxcutoff, 
                            uniqueonly=uniqueonly, avoidorphans=avoidorphans, 
                            # indexgridsize=indexgridsize,
                            quadtree=quadtree,
                            ...)
  
  cat('Aggregating at each buffer and overall\n')
  doaggregate(sitepoints , blocks)
}
