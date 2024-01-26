#' Get URL for page with info about industry sectors by text query term
#' 
#' See (https://naics.com) for more information on NAICS codes
#' 
#' @param query string query term like "gasoline" or "copper smelting"
#' @param as_html Whether to return as just the urls or as html hyperlinks to use in a DT::datatable() for example
#' @param linktext used as text for hyperlinks, if supplied and as_html=TRUE
#' @return URL as string
#' @export
#' 
url_naics.com <- function(query, as_html=FALSE, linktext) {
  query <- gsub(" ", "+", query)
  urlout = paste0("https://www.naics.com/code-search/?trms=", query, "&v=2017&styp=naics")
  if (as_html) {
    if (missing(linktext)) {linktext <- query}  #   paste0("EJScreen Map ", 1:length(lon)) 
    urlout <- EJAMejscreenapi::url_linkify(urlout, text = linktext)
  }   
  return(urlout)
  }
