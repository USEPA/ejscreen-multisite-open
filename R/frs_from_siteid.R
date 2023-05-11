#' Use registry ID to see FRS Facility Registry Service data on those EPA-regulated sites
#' 
#' @param siteid vector of one or more EPA Registry ID codes used by FRS 
#' @aliases [frs_from_regid()]
#' @return relevant rows of the data.table called frs, which has column names that are
#'   "lat" "lon" "REGISTRY_ID" "PRIMARY_NAME" "NAICS" "PGM_SYS_ACRNMS"
#' @export
#'
#' @examples frs_from_siteid(testids_registry_id)
frs_from_siteid <- function(siteid) {
  frs[REGISTRY_ID %in% siteid, ]
}
########################################## # 

#' @inherit frs_from_siteid
#' @aliases [frs_from_siteid()]
#' @export
frs_from_regid = frs_from_siteid

########################################## # 

#' Use EPA Program ID to see FRS Facility Registry Service data on those EPA-regulated sites
#' 
#' @param siteid vector of one or more EPA Program ID codes used by FRS 
#' @return relevant rows of the data.table called frs, which has column names that are
#'   "lat" "lon" "REGISTRY_ID" "PRIMARY_NAME" "NAICS" "PGM_SYS_ACRNMS"
#' @examples x=frs_from_programid(testids_program_sys_id)
#'   x
#'   mapfast(x)
#' @export
frs_from_programid <- function(programid) {
  
  frs[REGISTRY_ID %in% latlon_from_programid(programid)$REGISTRY_ID, ]
}
########################################## # 


#' Use EPA Program acronym like TRIS to see FRS Facility Registry Service data on those EPA-regulated sites
#' @description Get data.table based on given FRS Program System CATEGORY.
#'   Find all FRS sites in a program like RCRAINFO, TRIS, or others.
#' @param program vector of one or more EPA Program names used by FRS 
#' @return relevant rows of the data.table called frs, which has column names that are
#'   "lat" "lon" "REGISTRY_ID" "PRIMARY_NAME" "NAICS" "PGM_SYS_ACRNMS"
#'     
#' @export
frs_from_program  <- function(program) {
  
  frs[REGISTRY_ID %in% latlon_from_program(program)$REGISTRY_ID, ]
}
########################################## # 


#' Use NAICS code or industry title text search to see FRS Facility Registry Service data on those EPA-regulated sites
#'
#' @param naics_code_or_name 
#' @param ... passed to [naics_from_any()]
#' @return relevant rows of the data.table called frs, which has column names that are
#'   "lat" "lon" "REGISTRY_ID" "PRIMARY_NAME" "NAICS" "PGM_SYS_ACRNMS"
#' @seealso [siteid_from_naics()] [naics_from_any()]
#' @export
#'
#' @examples 
#'   frs_from_naics("uranium")
#'   mapfast(frs_from_naics(naics_from_any("nuclear")$code))
#'   naics_from_any("silver")
#'   naics_from_name("silver")
#'   naics_from_any(212222 )
#'   frs_from_naics(21222)
#'   siteid_from_naics(21222)
#'   latlon_from_naics(21222)
#'   
frs_from_naics <- function(naics_code_or_name, ...) {
  frs[REGISTRY_ID %in% siteid_from_naics(naics_from_any(naics_code_or_name, ...)$code, id_only = TRUE) , ]
}
########################################## # 

#' Use site name text search to see FRS Facility Registry Service data on those EPA-regulated sites
#' VERY SLOW search within PRIMARY_NAME of facilities for matching text
#' @param sitenames one or more strings in a vector, which can be regular expressions or query for exact match using fixed=TRUE
#' @param ignore.case logical, search is not case sensitive by default (unlike [grepl()] default)
#' @param fixed see [grepl()], if set to TRUE it looks for only exact matches
#'
#' @return relevant rows of the data.table called frs, which has column names that are
#'   "lat" "lon" "REGISTRY_ID" "PRIMARY_NAME" "NAICS" "PGM_SYS_ACRNMS"
#' @export
#'
#' @examples \dontrun{
#'  # very slow
#'  x=frs_from_sitename
#'  nrow(x)
#'  head(x)
#' }
frs_from_sitename <- function(sitenames, ignore.case=TRUE, fixed=FALSE) {
  results <- list()
  for (i in 1:length(sitenames)) {
    # VERY SLOW WAY:
    results[[i]] <- frs[grepl(sitenames[i], PRIMARY_NAME, ignore.case = ignore.case, fixed = fixed), ]
  }
  results  <- unique(data.table::rbindlist(results))
  return(results)
}
########################################## # 

