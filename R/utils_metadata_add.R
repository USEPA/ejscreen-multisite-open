#' helper function for package to set attributes of a dataset
#' 
#' @description This can be used annually to update some datasets in a package.
#'  It just makes it easier to set a few metadata attributes similarly
#'  for a number of data elements, for example,
#'  to add new or update existing attributes.
#' @details This utility would be used in scripts in EJAM/data-raw/ to 
#'   add metadata to objects like x before use_data(x, overwrite=T)
#' @param x dataset (or any object) whose metadata (stored as attributes) you want to update or create
#' @param metadata must be a named list, so that the function can do this for each i:
#'   `attr(x, which=names(metadata)[i]) <- metadata[[i]]`
#'  EJAM, EJScreen, and other dataset versions and release dates are tracked in DESCRIPTION
#' @seealso metadata_check()
#'
#' @return returns x but with new or altered attributes
#' @examples
#'   metadata_check()
#'   x <- data.frame(a=1:10,b=1001:1010)
#'   metadata <- list(
#'     data_downloaded = "2024-07-01",
#'     date_saved_in_package = as.character(Sys.Date())
#'   )
#'   x <- metadata_add(x, metadata)
#'   attributes(x)
#'   x <- metadata_add(x, list(status = 'final'))
#'   attr(x,'status')
#' 
#' @keywords internal
#'
library(desc)
description_file <- description$new("DESCRIPTION")
source("R/metadata_mapping.R")

metadata_add <- function(x) {
  metadata <- get_metadata_mapping(deparse(substitute(x)))
  if (missing(metadata)) {
    txt <- paste0(paste0(names(metadata), "=", unlist(metadata)), collapse = ", ")
    message("metadata not specified, so used defaults from source code of this function: ", txt, "\n")
    # print(cbind(attributes = metadata))
  }
  if (!is.list(metadata)) {stop('metadata has to be a named list')
    # return(NULL)
  }
  
  metadata$date_saved_in_package <- as.character(Sys.Date())
  metadata$EJAMversion           <- description_file$get("Version")
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
#' @examples 
#'   x <- metadata_check("EJAM")
#'   x[x$has_metadata == TRUE, ]
#'   table(x$has_metadata)
#'
#' @keywords internal
#'
metadata_check <- function(packages = EJAM::ejampackages, 
                           which = c(
                             "date_saved_in_package",
                             # "date_downloaded",
                             "ejscreen_version",
                             "ejscreen_releasedate",
                             "acs_releasedate",
                             "acs_version",
                             "census_version",
                             "EJAMversion"
                           ),
                           loadifnotloaded = TRUE) {
  
  # ejscreen_version =  '2.3',
  # ejscreen_releasedate = "2024-07-01",
  # acs_releasedate =      "2023-12-07",
  # acs_version =          "2018-2022",
  # census_version        = 2020
  
  #     previously:
  # The 2017-2021 American Community Survey 5-year estimates were released on Thursday, December 8, 2022.
  # EJScreen incorporated that in July 2023.
  # 
  # acs_version = '2016-2020',
  # acs_releasedate = '3/17/2022',
  # ejscreen_version = '2.1',
  # ejscreen_releasedate = 'October 2022',
  # ejscreen_pkg_data = 'bg22'
  # census_version = 2020,
  
  # utility to check if year attribute is set on each data file
  # does it really need to lazy load all these to check their attributes? that would be slow for big datasets, right?
  get1attribute <- function(x, which, dates_as_text=FALSE) {
    attribute1 <- try(attr(get(x), which = which))
    if (is.null(attribute1) || inherits(attribute1, "try-error")) {
      attribute1 <- NA
    }
    if (dates_as_text && "Date" %in% class(attribute1)) {attribute1 <- as.character(attribute1)}
    return(attribute1)
    }
  ################################# # 
  if (is.null(packages) || length(packages) == 0) {
    # if null or empty is specified, report on all packages with EJ as part of their name, like EJAMejscreenapi.
    packages <- grep(pattern = 'EJ', ignore.case = TRUE, x = installed.packages(fields = 'Package'), value = TRUE)
    packages <- unique(packages[!grepl(",", packages)])
  }
  
  allresults <- list()
  ii <- 0
  for (pkg in packages) {
    
    ii <- ii + 1
    if (!(pkg %in% as.vector(installed.packages()[, "Package"]))) {  # installed.packages() is slow but comprehensive
      cat(paste0(pkg, ' package not installed\n'))
      # packages <- packages[packages != pkg]
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
      results$has_metadata <- FALSE
      rownames(results) <- "package not installed"
      allresults[[ii]] <- results
      allresults[[ii]] <- data.frame(package = pkg, item = rownames(results), allresults[[ii]])
      rownames(allresults[[ii]]) <- NULL
    } else {
      
      rdafiles <- data(package = pkg)
      rdafiles <- rdafiles$results[ , 'Item']
      
      ## see also EJAM ::: #  datapack()
      # were_attached <- .packages() 
      # were_loaded <- loadedNamespaces()
      
      if (!isNamespaceLoaded(pkg) & loadifnotloaded) {
        wasnotloaded <- pkg
        cat(paste0(pkg, ' package was not loaded, loading and attaching now\n'))
        attachNamespace(pkg, include.only = rdafiles)   # library(pkg, character.only = TRUE)
      } else {
        wasnotloaded <- NULL
      }
      
      if (length(which) == 1) {
        results <- cbind(sapply(rdafiles, FUN = get1attribute, which, dates_as_text = TRUE))
        colnames(results) <- which
      } else {
        results <- list()
        for (i in 1:length(which)) {
          results[[i]] <- cbind(sapply(rdafiles, FUN = get1attribute, which[i], dates_as_text = TRUE))
        }
        results <- do.call(cbind, results)
        colnames(results) <- which
      }
      
      if (!is.null(wasnotloaded)) {
        unloadNamespace(asNamespace(wasnotloaded))
      }
    
    allresults[[ii]] <- results

    some <- as.vector(apply(allresults[[ii]], 1, function(z) !all(is.na(z))))
    allresults[[ii]] <- cbind(allresults[[ii]], has_metadata = FALSE)
    allresults[[ii]][some, "has_metadata"] <- TRUE
    allresults[[ii]] <- data.frame(package = pkg, item = rownames(allresults[[ii]]), allresults[[ii]])
    rownames(allresults[[ii]]) <- NULL
    }
    
    
  }
  
  names(allresults) <- packages

  cat(
    '\n 
    Also see  
    x = EJAM:::functions_in_pkg(pkg = "EJAM", internal_included = FALSE, exportedfuncs_included = FALSE, data_included = TRUE)$object  
    x[!grepl("^name", x)] 
    \n\n'
  )
  
  # replace the NULL values with NA values,
  # and make each column just a vector instead of a list
  
  
  
  allresults <- do.call(rbind, allresults)
  
  for (mycol in 1:NCOL(allresults)) {
    allresults[, mycol] <- as.vector(unlist(allresults[, mycol] ))
  }
  
  rownames(allresults) <- NULL
  
  return(allresults)
}
