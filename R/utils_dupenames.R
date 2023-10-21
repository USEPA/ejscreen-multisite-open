#' setdiff_yx
#' utility just like setdiff except for y,x instead of x,y
#' @export
setdiff_yx = function(x,y) setdiff(y,x)
##################################################################### #

#' setdiff2 aka unshared
#' utility just like setdiff except for y,x and also x,y
#' Just shows which elements are in one and only one of the sets x and y
#' @export
setdiff2 <- function(x,y) {setdiff(union(x,y),intersect(x,y))}
##################################################################### #

#' setdiff2 aka unshared
#' utility just like setdiff except for y,x and also x,y
#' Just shows which elements are in one and only one of the sets x and y
#' @export
unshared <- function(x,y) {setdiff(union(x,y),intersect(x,y))}
##################################################################### #

#' helper function to look at several packages to spot conflicting exported names
#' See what objects (functions or data) are exported by a given (installed) package
#' @details This can help find duplicates/conflicts within source code 
#'   and make sure they are on search path, for when renaming / moving functions/packages
#' @param pkg one or more package names as vector of strings. 
#'   If "all" it checks all installed pkgs, but takes very very long potentially.
#' @param sortbypkg If TRUE, just returns same thing but sorted by package name
#' @param compare.functions If TRUE, sends to console inf about whether body and formals
#'   of the functions are identical between functions of same name from different packages.
#'   Only checks the first 2 copies, not any additional ones (where 3+ pkgs use same name)
#' @seealso [all_equal_functions()]
#' @return data.frame with columns Package, Object name (or NA if no dupes)
#' @export
#'
dupenames <- function(pkg = EJAM::ejampackages, sortbypkg=FALSE, compare.functions=TRUE) {
  
  # Get list of exported names in package1, then look in package1 to
  #   obs <- getNamespaceExports(pkg)
  # find those appearing in source code .R files without package1:: specified,
  # since code using those functions and code defining those have to both be in the same pkg,
  #  (or need to add xyz:: specified)
  # and maybe want to do global search replace within files, like this:
  #   xfun::gsub_file()
  
  if ("all" %in% pkg) {
    pkg <- as.vector(installed.packages()[,"Package"])
  } else {
    
    # THIS COULD/SHOULD BE REPLACED USING ::: and/or getFromNamespace(), etc.
    
    findPkgAll <- function(pkg) { # finds path to each installed package of those specified
      unlist(lapply(.libPaths(), function(lib)
        find.package(pkg, lib, quiet = TRUE, verbose = FALSE)))
    }
    installed.packages.among <- function(pkg) {
      fff <- findPkgAll(pkg) # ok if a pkg is not installed. finds path to installed not source location
      if (length(fff) == 0) {warning("none of those packages are installed")
        return(NA)
      }
      gsub(".*/", "", fff) # get just names of pkgs not full paths
    }
    pkg <- installed.packages.among(pkg)
  }
  # getNamespaceExports will get exported object names, but fails if any pkg is not installed, hence code above
  
  xnames <-  sapply(pkg, function(x) {
    
    # DO WE WANT TO CHECK EVEN NON-EXPORTED OBJECTS? see getFromNamespace() and :::
    
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
    return(ddd)
  }
  if (sortbypkg) ddd <- ddd[order(ddd$package), ]
  
  if (compare.functions) {
    ddd$problem = "ok"
    #  use all_equal_functions() here to compare all pairs (but ignores more 2d copy of a function, so misses check of trios)
    #  to see if identical names are actually identical functions 
    # ddd <- dupenames()
    for (var in unique(ddd$variable)) {
      ok <- all_equal_functions(
        fun = var,
        package1 = ddd$package[ddd$variable == var][1],
        package2 = ddd$package[ddd$variable == var][2]
      )
      cat(var, " identical? ", ok, " \n")
      if (any(!ok)) {ddd$problem[ddd$variable == var] <- "Copies of this function differ"}
    }
    cat(" \n\n") 
  } else {
    ddd$problem = "not checked"
  }
  
  return(ddd)
}
##################################################################### #

#' helper function for checking possibly different versions of a function with same name in 2 packages
#'
#' @param fun quoted name of function, like "latlon_infer"
#' @param package1 quoted name of package, like "EJAM"
#' @param package2 quoted name of package, like "EJAMejscreenapi"
#'
#' @return TRUE or FALSE
#' @seealso [dupenames()] [all.equal.function()]
#' @export
#'
all_equal_functions <- function(fun="latlon_infer", package1="EJAM", package2="EJAMejscreenapi") {
  
  # not the same as base R all.equal.function() see  ?all.equal.function
  
  # strange quirks did not bother to debug:
  
  # 1) Normally it checks the first two cases of dupe named functions from 2 packages,
  # and answers with FALSE or TRUE (1 value).
  # But it returns FALSE 3 times only in the case of run_app (but not latlon_is.valid) 
  # dupenames(ejampackages) # or just dupenames() 
  
  # 2) ### error when checking a package that is loaded but not attached. 
  # eg doing this:
  # all_equal_functions("get.distance.all", "proxistat", "EJAM") # something odd about proxistat pkg.
  ### or 
  # dupenames(c("proxistat", "EJAMejscreenapi"), compare.functions = T)
  # Error in all_equal_functions(fun = var, package1 = ddd$package[ddd$variable ==  : 
  #                                                                  get.distances.all not found in proxistat
  #                                                                Called from: all_equal_functions(fun = var, package1 = ddd$package[ddd$variable == 
  
  if (!(is.character(fun) & is.character(package1) & is.character(package2))) {
    stop("all params must be quoted ")
  }
  # we could attach
  f1 = try(
    silent=TRUE, 
    expr = getFromNamespace(fun, ns = package1) 
    # get((fun), envir = as.environment(paste0("package:", (package1)) ) ) # this would not work if the package were not already loaded, on search path. see ?getFromNamespace
  )
  if (class(f1) == "try-error"  ) {
    # warning("fails when checking a package that is loaded but not attached - library func allows it to work. ")
    stop(fun, " not found in ", package1 )
  }
  if (!(is.function(f1))) {warning(package1, "::", fun, " is not a function");return(NA)}
  
  f2 = try(
    silent=TRUE, 
    expr = getFromNamespace(fun, ns = package2) 
    # get((fun), envir = as.environment(paste0("package:", (package2)) ) )
  )
  
  if (class(f2) == "try-error") {
    # warning("fails when checking a package that is loaded but not attached - library func allows it to work. ")
    stop(fun, " not found in ",  package2)
  }
  if (!(is.function(f2))) {warning(package2, "::", fun, " is not a function");return(NA)}
  
  x <- (TRUE == all.equal(body(f1), body(f2))) & (TRUE == all.equal(formals(f1), formals(f2)))
  return(x)
}
##################################################################### #


