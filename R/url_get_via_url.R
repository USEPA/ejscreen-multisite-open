#' helper function work in progress: GET json via url of ejscreen ejquery map services
#'
#' @param url the url for an EJScreen ejquery request
#'
#' @return json
#' @export
#'
url_get_via_url <- function(url) { 
  
  # function to GET json via url of ejscreen ejquery map services ### #
  
  x <- httr::GET(url)
  if (x$status_code == 400) {stop('Query failed with status code 400: ', url)}
  if (x$status_code == 404) {stop('Query failed with status code 404, possibly requesting too many locations at once: ', url)}
  x <- try(rawToChar(x$content))
  x <- try(jsonlite::fromJSON(x))
  alldata <- x$features$attributes
  # print(str(x)) # nothing else useful except possible x$features$geometry data.frame
  return(alldata)
}
