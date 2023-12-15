#' latlon_from_programid - Get lat lon, Registry ID, and NAICS, for given FRS Program System ID
#' @details The ID is the identification number, such as the permit number,
#'  assigned by an information management system that represents a 
#'  facility site, waste site, operable unit, or other feature tracked by that
#'  Environmental Information System.
#'  
#'  Also note the FRS API: 
#'   <https://www.epa.gov/frs/facility-registry-service-frs-api>
#'   <https://www.epa.gov/frs/frs-rest-services>
#'   
#'   
#' @param programid like "XJW000012435"
#'
#' @return data.table with lat  lon  REGISTRY_ID  program   pgm_sys_id
#' @export
#' @examples 
#'  latlon_from_programid(c("XJW000012435", "00768SRTRSROAD1"))
#'  pids <- c("7-0540-00003", "354362", "1513529", "485659", "LAG750956", 
#'    "CAC002995519", "3601252181", "3601439158")
#'  latlon_from_siteid(latlon_from_programid(pids)[,REGISTRY_ID])
#'  latlon_from_programid(c("XJW000012435", "00768SRTRSROAD1", "asdfsdf"))[,.(lat,lon)]
#'  
latlon_from_programid <- function(programid) {
  if (missing(programid)) {return(NULL)}
  
  if (!exists("frs_by_programid")) dataload_from_pins("frs_by_programid")
  
  frs_by_programid[match(programid, frs_by_programid$pgm_sys_id), ] # slower but retains order
  #  frs_by_programid[pgm_sys_id %in% programid, ] # faster but lose sort order of input
}
