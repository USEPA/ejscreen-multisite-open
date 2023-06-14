#' Creates index to all US blocks (internal point lat lon) at package load
#'
#' @param libname na
#' @param pkgname na
#'
.onLoad <- function(libname, pkgname) {
    ok_to_do_slow_load_early <- TRUE # WHILE TESTING/Building often
  if (  ok_to_do_slow_load_early) {
  ### Maybe load the datasets now that are needed, 
  ### instead of when user does a query that triggers slow lazyloading?
  cat("Loading data.tables of Census Blocks...\n")
  data("blockgroupstats", package="EJAM") # data(EJAM::blockgroupstats)  # 54 MB rda file
  #  statestats.rda is about 1.3 MB, usastats.rda is about 28 KB
  
  # require(EJAMblockdata)
  data(blockpoints,   package="EJAMblockdata"); cat('  Done loading blockpoints.\n')  
  data(blockwts,      package="EJAMblockdata"); cat('  Done loading blockwts.\n') 
  data(quaddata,      package="EJAMblockdata"); cat('  Done loading quaddata.\n')  
  
  data(blockid2fips,  package="EJAMblockdata") 
  # used only in state_from_blocktable() and state_from_blockid(), which are not necessarily used, 
  # so maybe should not load this unless/until needed
  
  data(bgid2fips,     package="EJAMblockdata")
  data(lookup_states, package="EJAMblockdata")
  
  } else {
    cat("Developers may want to modify the .onLoad() function, \n")
    cat("to have it immediately load less or more data \n")
    cat("(which takes several seconds each time you rebuild or just load the package)\n") 
    cat("instead of that happening only when you first run an analysis,\n") 
    cat("as with these large datasets (data.tables of Census blocks or block groups):\n ")
    cat("\n- quaddata (168 MB), \n- blockgroupstats  (54 MB), \n- blockpoints (86 MB), \n- blockwts (31 MB), \n- blockid2fips (20 MB)\n ")
    cat("\n")
  }
  ok_to_do_slow_indexing_early <- TRUE # WHILE TESTING/Building often
  if (ok_to_do_slow_indexing_early) {
  # This should create the index of all US block points to enable fast queries 
  # This cannot be done during package build and saved, because of what this createTree function creates.
  # NOT TESTED in context of an app published on RStudio Server
  cat("Building index of Census Blocks (localtree)...\n")
  if (!exists("localtree")) {
    # This assign() below is the same as the function called  indexblocks() 
    
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
  


}
 
