#' Wrapper for other functions doing the work - seems to duplicate getblocksnearby_and_doaggregate.R
#'
#' @param sitepoints see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param cutoff  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param maxcutoff  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param avoidorphans  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param ...  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#'
#' @export

summarizeForFacilities <- function(sitepoints, cutoff, maxcutoff,  avoidorphans, ...) {
  
  blocks <- getrelevantCensusBlocksviaQuadTree(sitepoints, cutoff, maxcutoff,  avoidorphans, ...)
  cat('Aggreating results\n')
  doaggregate(sitepoints , blocks)
  # facilities
}
  
