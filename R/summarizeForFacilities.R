#' Wrapper for other functions doing the work - seems to duplicate getblocksnearby_and_doaggregate.R
#'
#' @param sitepoints see [getrelevantCensusBlocksviaQuadTree_Clustered] or other such functions
#' @param cutoff  see [getrelevantCensusBlocksviaQuadTree_Clustered] or other such functions
#' @param maxcutoff  see [getrelevantCensusBlocksviaQuadTree_Clustered] or other such functions
#' @param avoidorphans  see [getrelevantCensusBlocksviaQuadTree_Clustered] or other such functions
#' @param ...  see [getrelevantCensusBlocksviaQuadTree_Clustered] or other such functions
#'
#' @export

summarizeForFacilities <- function(sitepoints, cutoff, maxcutoff,  avoidorphans, ...) {
  
  blocks <- getrelevantCensusBlocksviaQuadTree(sitepoints, cutoff, maxcutoff,  avoidorphans, ...)
  cat('Aggreating results\n')
  doaggregate(sitepoints , blocks)
  # facilities
}
  
