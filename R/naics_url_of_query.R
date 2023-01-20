#' Get URL for page with info about industry sectors by text query term
#' See [naics.com] for more information on NAICS codes
#' 
#' @param query string query term like "gasoline" or "copper smelting"
#'
#' @return URL as string
#' @export
#' 
naics_url_of_query <- function(query) {
  query <- gsub(" ", "+", query)
  
  paste0("https://www.naics.com/code-search/?trms=", query, "&v=2017&styp=naics")
}
