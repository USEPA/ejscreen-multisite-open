#' Creates index to all US blocks (internal point lat lon) at package load
#'
#' @param libname na
#' @param pkgname na
#'
.onLoad <- function(libname, pkgname) {
  
  # This should create the index of all US block points to enable fast queries 
  # This cannot be done during package build and saved, because of what this createTree function creates.
  # NOT TESTED in context of an app published on RStudio Server
  cat("Building Index of Census Blocks (localtree)...\n")
  assign(
    "localtree", 
    SearchTrees::createTree(EJAMblockdata::quaddata, treeType = "quad", dataType = "point"), 
    envir = globalenv() 
    # need to test, but seems to work. 
    # But takes a couple seconds at every reload of pkg.
  )
  cat("Done building index.\n")
  
  
  if (!interactive()) {
  ### Maybe load the datasets now that are needed, 
  ### instead of when user does a query that triggers slow lazyloading?
  cat("Loading data.tables of Census Blocks (blockpoints, blockwts)\n")
  data("blockgroupstats") # data(EJAM::blockgroupstats)  # 54 MB rda file
  #  statestats.rda is about 1.3 MB, usastats.rda is about 28 KB
  data(blockpoints, package="EJAMblockdata"); cat('blockpoints loaded\n')  
  data(blockwts,    package="EJAMblockdata"); cat('blockwts loaded\n') 
  # data(EJAMfrsdata::frs) # will be lazy loaded if/when needed
  
  } else {
    cat("You may want to modify this .onLoad() function, to have it immediately load data (which takes several seconds) instead of that happening only when you first run an analysis, ") 
    cat("as with quaddata (168 MB), blockgroupstats  (54 MB), blockpoints (86 MB) and blockwts (31 MB), blockid2fips (20 MB) files, ")
    cat("data.tables of Census Blocks or Block Groups.\n")
  }

}
 
