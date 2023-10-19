#' url_4table - Create URLs in columns 
#'
#' @param lat vector of latitudes
#' @param lon vector of longitudes
#' @param radius vector of values for radius in miles
#' @param regid vector of FRS registry IDs if available to use to create links to detailed ECHO facility reports
#' @param as_html logical
#' @seealso url_ejscreen_report() url_ejscreenmap() url_echo_facility_webpage() from EJAMejscreenapi package
#' @return list of data.frames to append to the list of data.frames created by ejamit() or doaggregate(), 
#'   list(results_bysite = results_bysite, results_overall = results_overall, newcolnames=newcolnames)
#' @export
#'
url_4table <- function(lat, lon, radius, regid=NULL, as_html=TRUE) {
  
  # add error checking
  

  
  # Also could add other links such as these:
  #   url_frs_report()
  #   url_enviromapper()   
  #   url_envirofacts_data()  ?
  

  
  
  
  if (!is.null(regid)) {
    echolink = url_echo_facility_webpage(regid, as_html = as_html)
  } else {
    echolink = rep(NA,NROW(lat))
  }
  
  newcolnames <- c(
    "EJScreen Report", 
    "EJScreen Map", 
    # "ACS Report", 
    "ECHO report"
  )
  
  results_bysite <- data.table(
    `EJScreen Report` = url_ejscreen_report(    lat = lat, lon = lon, radius = radius, as_html = as_html), 
    `EJScreen Map`    = url_ejscreenmap(        lat = lat, lon = lon,                    as_html = as_html), 
    # `ACS Report`      = url_ejscreen_acs_report(lat = lat, lon = lon, radius = radius, as_html = as_html),
    `ECHO report` = echolink
  )
  
  results_overall <- data.table(
    `EJScreen Report` = NA,  
    `EJScreen Map`    = NA,  
    # `ACS Report`      = NA,  
    `ECHO report`     = NA   
  )
  
  # setcolorder(out$results_bysite,  neworder = newcolnames)
  # setcolorder(out$results_overall, neworder = newcolnames)
  # out$longnames <- c(newcolnames, out$longnames)
  return(list(
    results_bysite  = results_bysite,
    results_overall = results_overall,
    newcolnames = newcolnames
    ))
}
