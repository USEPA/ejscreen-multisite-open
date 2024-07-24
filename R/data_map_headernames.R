#' @name map_headernames
#' @docType data
#' @title map_headernames is an essential dataset providing a table of metadata
#' about all the indicators and variables of interest used in EJAM and the ejscreen-related functions.
#' 
#' @details
#' 
#'   map_headernames is a n IMPORTANT TABLE THAT STORES INFORMATION ABOUT THE NAMES OF VARIABLES, 
#'   
#'   INCLUDING ALTERNATIVE VERSIONS OF THE NAMES AS USED IN GEODATABASE FILES,
#'   
#'   IN THE CODE, SHORT VERSIONS FOR LABELS OF GRAPHICS, 
#'   
#'   LONG VERSIONS TO PROVIDE FULL DESCRIPTIONS OF THE VARIABLES, 
#'   
#'   TYPE OF VARIABLE FOR PURPOSES OF GROUPING SIMILAR ONES, ETC. ETC.
#'   
#'   It was created from a spreadsheet of the same name, that is in a data-raw folder.
#'   
#'   Several helper functions are used to query it such as [fixcolnames()] and many functions rely on it.
#'  
#'   You can see examples of what it contains like this, for example: 
#'   
#'   
#'   `data.frame(t(map_headernames[1:2, ]))`
#'   
#'   
#'   See how many variables are on each list, for example: 
#'   
#'   
#'   data.table::setDT(copy(map_headernames))[, .(
#'     variables = .N, 
#'     has_apiname = sum(apiname != ""), 
#'     has_csvname = sum(csvname != ""), 
#'     has_acsname = sum(acsname != "")
#'     ), 
#'  keyby = c("raw_pctile_avg", "DEJ", "ratio.to", "pctile.", "avg.",  "varlist" )]
#'   
#'   
#'  # Which sources provide which key variables or indicators? 
#'  
#'  some <- unique(map_headernames$rname[map_headernames$varlist != "" 
#'    & map_headernames$varlist != "x_anyother"])
#'  
#'  info <- cbind(
#'    varinfo(some, info = c('api', 'csv', 'acs', 'varlist')), 
#'    usastats = some %in% names(usastats), 
#'    statestats = some %in% names(statestats))
#'  info <- info[nchar(paste0(info$api, info$csv, info$acs)) > 0, ]
#'  info
#'  
#'  # any others
#'  
#'  some <- unique(map_headernames$rname[map_headernames$varlist != "" 
#'    & map_headernames$varlist == "x_anyother"])
#'  
#'  info <- cbind(
#'    varinfo(some, info = c('api', 'csv', 'acs', 'varlist')), 
#'    usastats = some %in% names(usastats), 
#'    statestats = some %in% names(statestats))
#'  info <- info[nchar(paste0(info$api, info$csv, info$acs)) > 0, ]
#'  info
#'  
#'  
NULL
