#' @name bgej
#' @docType data
#' @title bgej (DATA) EJScreen EJ Indexes for Census block groups
#' @details
#'   For documentation on the demographic and environmental data and indicators used by
#'   EJScreen and EJAM, see <https://www.epa.gov/ejscreen/understanding-ejscreen-results>.
#'   
#'   bgej is a table of all blockgroups, with the raw scores of the EJ Indexes
#'   and supplemental EJ Indexes for all the environmental indicators.
#'   
#'   See 
#'     
#'     dataload_from_pins('bgej')
#'     
#'     names(bgej)
#'   
#'   The column names are these:
#'   
#'     c("OBJECTID", "bgfips", "bgid", "ST", "pop", 
#'     
#'     names_ej, 
#'     
#'     names_ej_supp)
#'   
NULL
