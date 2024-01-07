#' indexblocks
#' Create localtree (a quadtree index of all US block centroids) in global environment
#' @details Note this is duplicated code in .onAttach() and also in global.R  
#' 
#'    .onAttach() can be edited to create this when the package loads, 
#'   but then it takes time each time a developer rebuilds/installs the package or others that load EJAM.
#'   
#' It also has to happen in global.R if it has not already.
#' @return Returns TRUE when done. Side effect is it creates the index in memory.
#' @export
#'
indexblocks <- function() {
  cat("Checking for index of Census blocks (localtree)...\n")
  if (!exists("localtree")) {
    cat('The index of Census block groups (localtree) has not been created yet...\n')
    if (!exists("quaddata")) {
      cat(    "The index of Census block groups (localtree) cannot be created until quaddata is loaded ... Trying dataload_from_pins() before indexblocks() \n")
      message("The index of Census block groups (localtree) cannot be created until quaddata is loaded ... Trying dataload_from_pins() before indexblocks()")
      dataload_from_pins("quaddata")
    } else {
      cat("Building index of Census Blocks (localtree)...\n")
      assign(
        "localtree",
        SearchTrees::createTree(quaddata, treeType = "quad", dataType = "point"),
        envir = globalenv()
        # need to test, but seems to work.
        # But takes a couple seconds at every reload of pkg.
      )
      cat("  Done building index.\n")
    }
    
    
  } else {
    cat('The index of Census block groups localtree index of blocks appears to have been created.\n')
  }
  return(TRUE)
}
