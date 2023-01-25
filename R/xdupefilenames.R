#' look at several packages to spot conflicting exported names (functions or data)
#'
#' @param pkg one or more package names as vector of strings. 
#'   If "all" it checks all installed pkgs, but takes very very long potentially.
#' @param sortbypkg If TRUE, just returns same thing but sorted by package name
#'
#' @return data.frame with columns Package, Object name (or NA if no dupes)
#' @export
#'
dupenames <- function(pkg = EJAM::ejampackages, sortbypkg=FALSE) {
  
  # latlon_as.numeric
  # latlon_df_clean
  # latlon_infer
  # latlon_is.valid
  # 
  
  if (pkg == "all") {
    pkg <- as.vector(installed.packages()[,"Package"])
  } else {
    findPkgAll <- function(pkg) { # finds path to each installed package of those specified
      unlist(lapply(.libPaths(), function(lib)
        find.package(pkg, lib, quiet=TRUE, verbose=FALSE)))
    }
    installed.packages.among <- function(pkg) {
      fff <- findPkgAll(pkg) # ok if a pkg is not installed. finds path to installed not source location
      if (length(fff) == 0) {warning("none of those packages are installed"); return(NA)} 
      gsub(".*/", "", fff) # get just names of pkgs not full paths
    }
    pkg <- installed.packages.among(pkg)
  }
  # getNamespaceExports will get exported object names, but fails if any pkg is not installed, hence code above
  
  xnames <-  sapply(pkg, function(x) {
    y <- try(getNamespaceExports(x))
    if (class(y) == "try-error") {return(paste0("nothing_exported_by_", x))} else {return(y)}
  } ) # extremely slow if "all" packages checked
  
  counts <- sapply(xnames, length)
  xnames <- unlist(xnames)
  xnames_pkgs <- rep(names(counts), counts)
  names(xnames) <- xnames_pkgs
  ddd <-  data.frame(variable = xnames, package = names(xnames))
  duplicatednameslistedonceeach <- names(table(  xnames ))[(table(  xnames ) > 1)]
  if (length(duplicatednameslistedonceeach) > 0) {
    ddd <- ddd[ddd$variable %in% duplicatednameslistedonceeach, ]
    ddd <- ddd[order(ddd$variable), ]
    rownames(ddd) <- NULL
  } else {
    ddd <- NA
  }
  if (sortbypkg) ddd <- ddd[order(ddd$package), ]
  return(ddd)
}
