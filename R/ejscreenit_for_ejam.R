#' ejscreenit_for_ejam
#' Wrapper for EJAMejscreenapi::ejscreenit(), to use in EJAM app
#' @param sitepoints table with lat and lon columns
#' @param radius in miles
#' @param ... passed to ejscreenit() but not tested and probably should not use/ not needed
#'
#' @return list of results from [EJAMejscreenapi::ejscreenit()]
#'   i.e., list of these: results_bysite (data.table), map, plot, us.ratios
#' @export
#'
ejscreenit_for_ejam <- function(sitepoints, radius=3, ...) {
  # in ejamit() sitepoints is the first parameter
  out <- EJAMejscreenapi::ejscreenit(
    sitepoints, 
    radius = radius, 
    save_map = F, 
    see_map = F, 
    save_plot = F, 
    see_plot = F, 
    save_table = F, 
    interactiveprompt = F
  )
  out$table <- data.table::as.data.table(out$table)
  names(out) <- gsub("table", "results_bysite", names(out))

  # create empty placeholder for overall results to emulate structure of output of ejam
  ov <- data.frame(matrix(data = 0, nrow = 1, ncol = NCOL(out$results_bysite)))
  ov <- data.table::setDT(ov)
  data.table::setnames(ov, names(out$results_bysite))
  out$results_overall <- ov
  
  # do the same for results_bybg_people ???

  
  #  ****  these are the short names and need to be renamed - this is a placeholder to be fixed *****
  out$longnames <- names(out$results_bysite)
    
# add URLs, but this duplicates code in server code
  echolink = rep(NA,nrow(out$results_bysite))
 out$results_bysite[ , `:=`(
  `EJScreen Report` = url_ejscreen_report(    lat = sitepoints$lat, lon = sitepoints$lon, radius = radius, as_html = TRUE), 
  `EJScreen Map`    = url_ejscreenmap(        lat = sitepoints$lat, lon = sitepoints$lon,                  as_html = TRUE), 
  # `ACS Report`      = url_ejscreen_acs_report(lat = sitepoints$lat, lon = sitepoints$lon, radius = radius, as_html = TRUE),
  `ECHO report` = echolink
)]
out$results_overall[ , `:=`(
  `EJScreen Report` = NA, 
  `EJScreen Map`    = NA, 
  # `ACS Report`      = NA,
  `ECHO report`     = NA
)]
newcolnames <- c(
  "EJScreen Report", 
  "EJScreen Map", 
  # "ACS Report", 
  "ECHO report")
# put those up front as first columns
setcolorder(out$results_bysite, neworder = newcolnames)
setcolorder(out$results_bysite, neworder = newcolnames)
out$longnames <- c(newcolnames, out$longnames)

  out$results_bysite[      , radius.miles := radius]
  out$results_overall[     , radius.miles := radius]
  # out$results_bybg_people[ , radius.miles := radius]
  out$longnames <- c(out$longnames , "Radius (miles)")

  
  return(out)
}
