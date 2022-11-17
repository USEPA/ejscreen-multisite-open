getblocksnearby2 <- function(sitepoints, cutoff=1, maxcutoff=31.07, 
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
    if (exists('localtree')) {
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
