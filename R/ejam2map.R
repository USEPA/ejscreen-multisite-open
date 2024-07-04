

#' Show EJAM results as a map of points
#'
#' Takes the output of ejamit() and uses [mapfastej()] to 
#' create a map of the points.
#' @details Gets radius by checking ejamitout$results_overall$radius.miles
#' 
#' @inheritParams mapfast
#' @return like what [mapfastej()] returns
#' 
#' @examples
#' # out = ejamit(testpoints_100, radius = 1)
#' out = testoutput_ejamit_100pts_1miles
#' x = ejam2map(out)
#' map2browser(x)
#' # to see it in the local browser instead of RStudio viewer pane
#'  
#' @export
#'
ejam2map <- function(ejamitout, ...) {
  
  mapfast(mydf = ejamitout$results_bysite,
          radius = ejamitout$results_overall$radius.miles,
          column_names = 'ej',
          ...)
}
############################################################################ #


#' quick way to open a map html widget in the local browser (by saving it as a tempfile)
#'
#' @param x output of [ejam2map()] or [mapfastej()] or [mapfast()]
#'
#' @return launches local browser to show x, but also returns
#'   name of tempfile that is the html widget
#'
#' @examples
#' x = ejam2map(testoutput_ejamit_100pts_1miles)
#' map2browser(x)
#' # to see it in the local browser instead of RStudio viewer pane
#'    
#' @export
#' 
map2browser = function(x) {
  
  if (!interactive()) {
    stop("must be in interactive mode in R to view html widget this way")
  }
  mytempfilename = file.path(tempfile("map", fileext = ".html"))
  htmlwidgets::saveWidget(x, file = mytempfilename)
  browseURL(mytempfilename)
  return(mytempfilename)
}
############################################################################ #
