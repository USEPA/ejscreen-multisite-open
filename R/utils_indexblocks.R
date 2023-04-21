#' indexblocks
#' Create localtree (a quadtree index of all US block centroids) in global environment
#' @details .onLoad() can be edited to create this when the package loads, 
#'   but then it takes time each time a developer rebuilds/installs the package or others that load EJAM.
#' @return Side effect is it creates the index in memory
#' @export
#'
indexblocks <- function () {
  assign(
    "localtree", 
    SearchTrees::createTree(EJAMblockdata::quaddata, treeType = "quad", dataType = "point"), 
    envir = globalenv()
    )
}
