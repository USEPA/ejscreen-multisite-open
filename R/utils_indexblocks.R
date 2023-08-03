#' indexblocks
#' Create localtree (a quadtree index of all US block centroids) in global environment
#' @details Note this is duplicated code in .onLoad() and also in global.R  
#' 
#'    .onLoad() can be edited to create this when the package loads, 
#'   but then it takes time each time a developer rebuilds/installs the package or others that load EJAM.
#'   
#' It also has to happen in global.R if it has not already.
#' @return Side effect is it creates the index in memory
#' @export
#'
indexblocks <- function () {
  cat("Building index of Census Blocks (localtree)...\n")
  if (!exists("localtree")) {
    if (!exists("quaddata")) {stop("requires quaddata to be loaded - cannot build localtree without it.")}
    # It is obtained from AWS currently, via  data_load_from_aws()
    
    # This assign() below is the same as the function called  indexblocks() 
    # indexblocks() # this creates  localtree object
    assign(
      "localtree",
      SearchTrees::createTree(quaddata, treeType = "quad", dataType = "point"),
      envir = globalenv()
      # need to test, but seems to work.
      # But takes a couple seconds at every reload of pkg.
    )
    cat("  Done building index.\n")
  }
  
}
