#' Key buffering function - wrapper redirecting to the right version of getblocksnearby()
#' @details  For all examples, see [ejamit()]
#' 
#'   Like getblocksnearby() but tries to handle localtree and quadtree parameter differently 
#'   - not sure how to check if they are in the right environment.
#'  
#' @param sitepoints   see [getblocksnearbyviaQuadTree()] or other such functions
#' @param cutoff       see [getblocksnearbyviaQuadTree()] or other such functions
#' @param maxcutoff    see [getblocksnearbyviaQuadTree()] or other such functions
#' @param avoidorphans see [getblocksnearbyviaQuadTree()] or other such functions
#' @param quadtree a large quadtree object created from the SearchTree package example:
#'    SearchTrees::createTree(EJAMblockdata::quaddata, treeType = "quad", dataType = "point")
#' @param ...          see [getblocksnearbyviaQuadTree_Clustered()] or other such functions
#' @seealso [getblocksnearby_and_doaggregate()] [getblocksnearby()] [getblocksnearbyviaQuadTree()] [getblocksnearbyviaQuadTree_Clustered()] [getblocksnearbyviaQuadTree2()]
#' @export
#'
getblocksnearby2 <- function(sitepoints, cutoff=3, maxcutoff=31.07, 
                             avoidorphans=TRUE, 
                             # indexgridsize,
                             quadtree=is.null,
                             ...
) {
  
  # TRYING THIS TO SEE IF quadtree can be checked in global environment or calling environment or whatever
  # so this function could be used without specifying that index
  # and the index quadtree could be built using .onLoad() when package is first attached. 
  # not sure that would work on a server, 
  # and not sure it would work for an R user in RStudio who did library(EJAM)
  
  # wrapper to make it simple to (later?) switch between functions to use for this, clustered vs not, etc.
  if (is.null(quadtree)) {
    if (exists('localtree' )) { # not working yet?
      return(
        getblocksnearbyviaQuadTree(sitepoints=sitepoints, cutoff=cutoff, maxcutoff=maxcutoff, 
                                   avoidorphans=avoidorphans, 
                                   # indexgridsize=indexgridsize,
                                   quadtree=localtree,
                                   ...)
      )
    } else {
      stop('requires quadtree')
    }
  }
  
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
