#' @name map_headernames
#' @docType data
#' @title map_headernames is an essential dataset providing a table of metadata
#' about all the indicators and variables of interest used in EJAM and the ejscreen-related functions.
#' 
#' @details
#'   You can see examples of what it contains like this, for example: 
#'   `data.frame(t(map_headernames[1:2, ]))`
#'   
#'   and see help for [fixcolnames()]
#'   
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
NULL
