.onLoad <- function(libname, pkgname) {
  # This should create the index of all US block points to enable fast queries 
  # This cannot be done during package build and saved, because of what this createTree function creates.
  assign(
    "localtree", 
    SearchTrees::createTree(EJAMblockdata::quaddata, treeType = "quad", dataType = "point"), 
    envir = globalenv() # need to test
  )
}
