#' helper function for package to set attributes of a dataset
#' 
#' @description This can be used annually to update some datasets in a package.
#'  It just makes it easier to set a few metadata attributes similarly
#'  for a number of data elements, for example,
#'  to add new or update existing attributes.
#' @param x dataset (or any object) whose metadata you want to update or create
#' @param metadata must be a named list, so that the function can do this for each i:
#'   `attr(x, which=names(metadata)[i]) <- metadata[[i]]`
#' @seealso metadata_check()
#'
#' @return returns x but with new or altered attributes
#' @examples
#'   x <- data.frame(a=1:10,b=1001:1010)
#'   metadata <- list(
#'   ejscreen_version =  '2.2',
#'   acs_version =          '2017-2021',
#'   census_version = 2020,
#'   ejscreen_releasedate = '2023-06-23',
#'   acs_releasedate =      '2022-12-08',
#'   ejscreen_pkg_data = NA
#'   )
#'   x <- metadata_add(x, metadata)
#'   attributes(x)
#'   x <- metadata_add(x, list(status='final'))
#'   attr(x,'status')
#'   
#' 
#' @keywords internal
#' 
metadata_add <- function(x, metadata) {
  
  if (missing(metadata)) {
    metadata <- list(
      ejscreen_version =  '2.2',
      acs_version =          '2017-2021',
      census_version = 2020,
      ejscreen_releasedate = '2023-08-21',
      acs_releasedate =      '2022-12-08',
      ejscreen_pkg_data = NA
    )
    txt <- paste0(paste0(names(metadata), "=", unlist(metadata)), collapse = ", ")
    warning("metadata not specified, so used defaults from source code of this function: ", txt, "\n")
    print(cbind(attributes = metadata))
  }
  if (!is.list(metadata)) {warning('metadata has to be a named list')
    return(NULL)}
  for (i in seq_along(metadata)) {
    attr(x, which = names(metadata)[i]) <- metadata[[i]]
  }
  invisible(x)
}
#################################################### #


#' helper function in updating the package metadata
#' 
#' @description Quick and dirty helper during development, to check all the 
#'   attributes of all the data files in relevant packages. 
#'   It loads unloaded packages as needed, which you might not want it to do, 
#'   but it is not coded to be able to check attributes without doing that.
#' 
#' @param packages Optional. e.g. 'EJAMejscreendata', or can be a vector of character strings, 
#'   and if not specified, default is to report on all packages with EJ as part of their name, 
#'   like EJAMejscreenapi
#' @param which Optional vector (not list) of strings, the attributes. 
#'   Default is some typical ones used in EJAM-related packages currently.
#' @param datasets optional, "all" means all data objects exported?
#'   can be a vector of character names of the ones to check like c("bgpts", "blockpoints")
#' @param grepdatasets optional, if set to TRUE, datasets should be a query to use
#'   via grep to identify which datasets to check. It always uses ignore.case=TRUE for this.
#' @param loadifnotloaded Optional to control if func should temporarily attach packages not already loaded.
#' @seealso [functions_in_pkg()]
#' 
#' @keywords internal
#' 
metadata_check <- function(packages = EJAM::ejampackages, 
                           which = c(
                             'census_version', 
                             'acs_version', 'acs_releasedate', 'ACS', 
                             'ejscreen_version', 'ejscreen_releasedate', 'ejscreen_pkg_data', 
                             'year', 'released'),
                           datasets = "all",
                           grepdatasets = FALSE,
                           loadifnotloaded = TRUE) {
  
  # ejscreen_version     "2.2"       
  # acs_version          "2017-2021" 
  # census_version       2020        
  # ejscreen_releasedate "2023-06-23"
  # acs_releasedate      "2022-12-08"
  # ejscreen_pkg_data    NA      
  
  # The 2017-2021 American Community Survey 5-year estimates were released on Thursday, December 8, 2022.
  # EJScreen incorporated that in July 2023.
  
  #     previously:
  # census_version = 2020,
  # acs_version = '2016-2020',
  # acs_releasedate = '3/17/2022',
  # ejscreen_version = '2.1',
  # ejscreen_releasedate = 'October 2022',
  # ejscreen_pkg_data = 'bg22'
  #
  
  # utility to check if year attribute is set on each data file
  # does it really need to lazy load all these to check their attributes? that would be slow for big datasets, right?
  get1attribute <- function(x, which) {try(attr(get(x), which = which))}
  
  if (is.null(packages)) {
    # if not specified, report on all packages with EJ as part of their name, like EJAMejscreenapi.
    packages <- grep(pattern = 'EJ', ignore.case = TRUE, x = installed.packages(fields = 'Package'), value = TRUE)
    packages <- unique(packages[!grepl(",", packages)])
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
    ############################################### # 
    # GET THE datasets TO CHECK ####
    
    ## also see  functions 
    # EJAM:::functions_in_pkg(pkg = pkg, internal_included = TRUE, exportedfuncs_included = TRUE, data_included = TRUE)
    
    # rdafiles <- datapack(pkg = pkg)$Item  # same thing as data(package = pkg)$results[ , "Item"]
    rdafiles <- data(package = pkg)$results[ , "Item"]
    if (datasets[1] != "all") {
      if (grepdatasets) {
        rdafiles <- rdafiles[grepl(datasets, rdafiles, ignore.case = TRUE)]
      } else {
        if (!(all(datasets %in% rdafiles))) {warning("not all specified datasets were found")}
        rdafiles = datasets[datasets %in% rdafiles]
      }
    }
    if (!isNamespaceLoaded(pkg) & loadifnotloaded) {
      wasnotloaded <- pkg
      cat(paste0(pkg, ' package was not loaded, loading and attaching now\n'))
      attachNamespace(pkg, include.only = rdafiles)   # library(pkg, character.only = TRUE)
    } else {
      wasnotloaded <- NULL
    }
    ############################################### # 
    
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
    if (!is.null(wasnotloaded)) {
      unloadNamespace(asNamespace(wasnotloaded))
    }
    
    some <- as.vector(apply(allresults[[ii]], 1, function(z) !all("NULL" == z)))
    allresults[[ii]] <- cbind(allresults[[ii]], has_metadata = FALSE)
    allresults[[ii]][some, "has_metadata"] <- TRUE
    
  }
  #maybe...
  # allresults <- do.call(cbind, results)
  names(allresults) <- packages
  cat(
    '\n 
    Also see  
    x = EJAM:::functions_in_pkg(pkg = "EJAM", internal_included = FALSE, exportedfuncs_included = FALSE, data_included = TRUE)$object  
    x[!grepl("^name", x)] 
    \n\n'
  )
  return(allresults)
}
