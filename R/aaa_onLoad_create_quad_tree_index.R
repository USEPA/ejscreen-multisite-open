#' Creates index to all US blocks (internal point lat lon) at package load
#'
#' @param libname na
#' @param pkgname na
#'
.onLoad <- function(libname, pkgname) {
  
  # This should create the index of all US block points to enable fast queries 
  # This cannot be done during package build and saved, because of what this createTree function creates.
  # NOT TESTED in context of an app published on RStudio Server
  assign(
    "localtree", 
    SearchTrees::createTree(EJAMblockdata::quaddata, treeType = "quad", dataType = "point"), 
    envir = globalenv() 
    # need to test, but seems to work. 
    # But takes a couple seconds at every reload of pkg.
  )
  
  
  ### Maybe load the datasets now that are needed, 
  ### instead of when user does a query that triggers slow lazyloading?
  #
  # data(EJAMfrsdata::frs)
  # data(EJAM::blockgroupstats)
  # data(EJAMblockdata::blockpoints)
  # data(EJAMblockdata::blockwts)
  ###   etc.
  
}
 
