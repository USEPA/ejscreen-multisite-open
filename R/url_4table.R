url_4table <- function(lat, lon, distance, regid=NULL, as_html=TRUE) {
  # add error checking
  
  
  if (!is.null(regid)) {
    echolink = url_echo_facility_webpage(regid, as_html = as_html)
  } else {
    echolink = rep(NA,NROW(lat))
  }
  
  newcolnames <- c(
    "EJScreen Report", 
    "EJScreen Map", 
    "ACS Report", 
    "ECHO report"
  )
  
  results_bysite <- data.table(
    `EJScreen Report` = url_ejscreen_report(    lat = lat, lon = lon, distance = distance, as_html = as_html), 
    `EJScreen Map`    = url_ejscreenmap(        lat = lat, lon = lon,                      as_html = as_html), 
    `ACS Report`      = url_ejscreen_acs_report(lat = lat, lon = lon, distance = distance, as_html = as_html),
    `ECHO report` = echolink
  )
  
  results_overall <- data.table(
    `EJScreen Report` = NA,  
    `EJScreen Map`    = NA,  
    `ACS Report`      = NA,  
    `ECHO report`     = NA   
  )
  
  # setcolorder(out$results_bysite,  neworder = newcolnames)
  # setcolorder(out$results_overall, neworder = newcolnames)
  # out$longnames <- c(newcolnames, out$longnames)
  return(list(
    results_bysite = results_bysite,
    results_overall = results_overall,
    newcolnames=newcolnames
    ))
}
