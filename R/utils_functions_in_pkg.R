
#' utility to see which objects in a loaded/attached package are exported functions, internal (unexported) objects, or datasets
#'
#' @param pkg name of package as character like "EJAM"
#' @param alphasort_table default is FALSE, to show internal first as a group, then exported funcs, then datasets
#' @param internal_included default TRUE includes internal (unexported) objects in the list
#' @param exportedfuncs_included default TRUE includes exported functions (non-datasets, actually) in the list
#' @param data_included default TRUE includes datasets in the list, as would be seen via data(package=pkg)
#' @param vectoronly set to TRUE to just get a character vector of object names instead of the data.frame table output
#' @seealso [datapack()] [ls()] [getNamespace()] [getNamespaceExports()] [loadedNamespaces()]
#' 
#' @return data.table with colnames object, exported, data  where exported and data are 1 or 0 for T/F,
#'   unless vectoronly = TRUE in which case it returns a character vector
#' @examples  functions_in_pkg("datasets") # functions_in_pkg("EJAMejscreenapi")
#' 
#' @keywords internal
#'
functions_in_pkg <- function(pkg, alphasort_table=FALSE, internal_included=TRUE, exportedfuncs_included=TRUE, data_included=TRUE, vectoronly=FALSE) {
  
  # helper functions ####
  
  dataonly <- function(pkg) {EJAM:::datapack(pkg = pkg, simple = TRUE)$Item}

  exported_plus_internal_withdata <- function(pkg) {sort(union(dataonly(pkg), ls(getNamespace(pkg), all.names = TRUE)))} # all.names filters those starting with "."
  exported_only_withdata          <- function(pkg) {ls(paste0("package:", pkg))} 
    # same as ls(envir = as.environment(x = paste0("package:", pkg)))
    # same as  getNamespaceExports() except sorted 
  
  exported_plus_internal_nodata <- function(pkg) {sort(setdiff(
    exported_plus_internal_withdata(pkg = pkg), 
    dataonly(pkg = pkg)))}
  exported_only_nodata <- function(pkg) {sort(setdiff(
    exported_only_withdata(pkg = pkg), 
    dataonly(pkg = pkg)))}
  
  internal_only_withdata <- function(pkg) {sort(setdiff(
    exported_plus_internal_withdata(pkg = pkg), 
    exported_only_nodata(pkg = pkg)))}
  internal_only_nodata <- function(pkg) {sort(setdiff(
    internal_only_withdata(pkg = pkg), 
    dataonly(pkg = pkg)))}
  
  # # double-checks
  # 
  # setequal(      exported_plus_internal_withdata("EJAMejscreenapi"), 
  #          union(exported_plus_internal_nodata(  "EJAMejscreenapi"), dataonly("EJAMejscreenapi")))
  # 
  # setequal(      exported_only_withdata(         "EJAMejscreenapi"), 
  #          union(exported_only_nodata(           "EJAMejscreenapi"), dataonly("EJAMejscreenapi")))
  # 
  # setequal(      internal_only_withdata(         "EJAMejscreenapi"), 
  #          union(internal_only_nodata(           "EJAMejscreenapi"), dataonly("EJAMejscreenapi")))

  # table format output ####
  
  omni <- exported_plus_internal_withdata(pkg)
  y <- data.frame(
    object = omni,
    exported = ifelse(omni %in% exported_only_withdata(pkg), 1, 0),
    data = ifelse(omni %in% dataonly(pkg), 1, 0)
  )
  if (!data_included) {
    y <- y[y$data == 0, ]
  }
  if (!internal_included) {
    y <- y[!(y$exported == 0), ]
  }
  if (!exportedfuncs_included) {
    y <- y[!(y$exported == 1 & y$data == 0), ]
  }

  if (!vectoronly) {
    if (alphasort_table) {
      # already done by default
    } else {
      y <- y[order(y$exported, y$data, y$object), ]
    }
    return(y)
  }
  
  # vector format output ####
  
  if (vectoronly) {
    # cat('\n\n')
    # cat(pkg)
    # cat('\n\n')
    # print(y)
    # cat('\n\n')
    
    return(y$object) 
    
    ################# #
  # if (internal_included & data_included) {
  #   x <- exported_plus_internal_withdata(pkg)
  # }
  # if (internal_included & !data_included) {
  #   x <- exported_plus_internal_nodata(pkg)
  # }
  # if (!internal_included & data_included) {
  #   x <- exported_only_withdata(pkg)
  # }
  # if (!internal_included & !data_included) {
  #   x <- exported_only_nodata(pkg)
  # }
  #   return(x)
    ################# #
    
  }
}
