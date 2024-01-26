#' Get EJScreen results directly from EJScreen servers via their API
#' 
#' A wrapper for EJAM to use ejscreenit() from the EJAMejscreenapi package
#' 
#' @param sitepoints table with lat and lon columns
#' @param radius in miles
#' @param ... passed to ejscreenit() 
#' @seealso [ejscreen_vs_ejam()]
#' @return a data.table that looks like output of ejamit()$results_bysite
#' @export
#'
ejscreenit_for_ejam <- function(sitepoints, radius=3, fillmissingcolumns = TRUE, ...) {

  out <- ejscreenit(sitepoints, radius = radius, ...)
  out <- ejscreenapi2ejam_format(out, fillmissingcolumns = fillmissingcolumns)  # , ejamcolnames = ejamcolnames
  
  return(out)
}
