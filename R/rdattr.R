#' helper function in updating the package metadata
#' @description Quick and dirty helper during development, to check all the 
#'   attributes of all the data files in relevant packages. 
#'   It loads unloaded packages as needed, which you might not want it to do, 
#'   but it is not coded to be able to check attributes without doing that.
#' 
#' @param package Optional. e.g. 'EJAMejscreendata', or can be a vector of character strings, 
#'   and if not specified, default is to report on all packages with EJ as part of their name, 
#'   like EJAMblockdata or ejscreenapi
#' @param which Optional vector (not list) of strings, the attributes. 
#'   Default is some typical ones used in EJAM-related packages currently.
#' @param loadifnotloaded Optional to control if func should load packages not already loaded.
#'
#' @export
#'
rdattr <- function(packages=NULL, which=c(
  'census_version', 
  'acs_version', 'acs_releasedate', 'ACS', 
  'ejscreen_version', 'ejscreen_releasedate', 'ejscreen_pkg_data', 
  'year', 'released'),
  loadifnotloaded=TRUE) {
  # census_version = 2020,
  # acs_version = '2016-2020',
  # acs_releasedate = '3/17/2022',
  # ejscreen_version = '2.1',
  # ejscreen_releasedate = 'October 2022',
  # ejscreen_pkg_data = 'bg22'
  #
  # The 2017-2021 American Community Survey 5-year estimates are scheduled to be released on Thursday, December 8, 2022.
  # EJScreen might incorporate that in mid/late 2023.
  
  # utility to check if year attribute is set on each data file
  # does it really need to lazy load all these to check their attributes? that would be slow for big datasets, right?
  get1attribute <- function(x, which) {try(attr(get(x), which = which))}
  
  if (is.null(packages)) {
    # if not specified, report on all packages with EJ as part of their name, like EJAMblockdata or ejscreenapi.
    packages <- grep(pattern = 'EJ', ignore.case = TRUE, x = installed.packages(fields = 'Package'), value = TRUE)
  }
  
  allresults <- list()
  ii <- 0
  for (pkg in packages) {
    ii <- ii + 1
    if (!(pkg %in% installed.packages(fields = 'Package'))) {
      cat(paste0(pkg, ' package not installed\n'))
      # return a 1row data.frame with NA values, using the attributes listed in which as the colnames:
      results <- which; names(results) <- results; results[] <- NA
      next
    }
    if (!isNamespaceLoaded(pkg)) {
      
      cat(paste0(pkg, ' package not loaded\n'))
    }
    
    rdafiles <- data(package=pkg)
    rdafiles <- rdafiles$results[ , 'Item']
    # rdafiles <- gsub(' .*' , '', rdafiles) # obsolete
    if (length(which) == 1) {
      results <- cbind(sapply(rdafiles, FUN = get1attribute, which))
      colnames(results) <- which
    } else {
      results <- list()
      for (i in 1:length(which)) {
        results[[i]] <- cbind(sapply(rdafiles, FUN = get1attribute, which[i]))
      }
      results <- do.call(cbind, results)
      colnames(results) <- which
    }
    allresults[[ii]] <- results
  }
  #maybe...
  # allresults <- do.call(cbind, results)
  names(allresults) <- packages
  return(allresults)
}

# rdattr()
# library(ejscreen)
# rdattr(which=c('year', 'ACS'), packages = 'ejscreen')
# rdattr(which=c('year', 'ACS', 'released', 'ejscreen_version'), packages = 'ejscreen')
