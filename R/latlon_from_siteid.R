#' latlon_from_siteid - Get lat lon (and NAICS) via Facility Registry ID
#' @param siteid Facility Registry Service ID like 110010052520
#'
#' @return data.table with columns 
#'   lat,lon,REGISTRY_ID,PRIMARY_NAME,NAICS,PGM_SYS_ACRNMS
#' @export
#' @examples 
#'  latlon_from_siteid(110070874073)
#' x = latlon_from_siteid(
#'     c(110071293460, 110070874073, 110070538057, 110044340807,
#'        110030509215, 110019033810, 110056111559, 110056982323)
#'         )
#' EJAMejscreenapi::mapfast(x)
latlon_from_siteid <- function(siteid) {
  if (missing(siteid)) {return(NULL)}
  frs[match(siteid, frs$REGISTRY_ID), ] # slower but retains order
  
}

#' @export
#' @inherit latlon_from_siteid
latlon_from_regid <- latlon_from_siteid
