
#' Get lat lon, Registry ID, and NAICS, for given FRS Program System ID
#' 
#' @details The ID is the identification number, such as the permit number,
#'  assigned by an information management system that represents a 
#'  facility site, waste site, operable unit, or other feature tracked by that
#'  Environmental Information System.
#'  
#'  Also note the FRS API: 
#'   <https://www.epa.gov/frs/facility-registry-service-frs-api>
#'   <https://www.epa.gov/frs/frs-rest-services>
#'
#' @param programname acronym for EPA program, as found in `epa_programs`
#' @param programid program system ID, such as "XJW000012435"
#'
#' @return data.table with lat  lon  REGISTRY_ID  program   pgm_sys_id
#' @examples 
#'  test <- data.frame(programname = c('STATE','FIS','FIS'),
#'                     programid = c('#5005','0-0000-01097','0-0000-01103'))
#'  latlon_from_programid(test$programname, test$programid)
#'
#'  latlon_from_regid(latlon_from_programid(test$programname, test$programid)[,REGISTRY_ID])
#'  latlon_from_programid(test$programname, test$programid)[,.(lat,lon)]
#'
#' @export
#'
latlon_from_programid <- function(programname,programid) {

  if (missing(programid) | missing(programname)) {
    warning('Please provide both programname and programid.')
    return(NULL)
  }
  if (!exists("frs_by_programid")) dataload_from_pins("frs_by_programid")
  
  frs_by_programid[match(paste0(programname,':',programid), PGM_SYS_ACRNMS), ] # slower but retains order
  #frs_by_programid[match(programid, frs_by_programid$pgm_sys_id), ] # slower but retains order
  #  frs_by_programid[pgm_sys_id %in% programid, ] # faster but lose sort order of input
  
}
