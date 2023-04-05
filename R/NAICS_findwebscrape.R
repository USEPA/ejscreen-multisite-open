#' for query term, show list of roughly matching NAICS, scraped from web
#' This finds more than just [naics_from_any()] does, since that needs an exact match
#'   but this looks at naics.com website which lists various aliases for a sector.
#' 
#' @param query text like "gasoline" or "copper smelting"
#' @seealso [naics_from_any()]  [url_naics.com()]
#' @return data.frame of info on what was found, naics and title
#' @export
#'
#' @examples
#'  # naics_from_any("copper smelting")
#'  # naics_from_any("copper smelting", website_scrape=TRUE)
#'  # browseURL(naics_from_any("copper smelting", website_url=TRUE) )
#'  
#'   url_naics.com("copper smelting")
#'   \dontrun{
#'   naics_findwebscrape("copper smelting")
#'   browseURL(url_naics.com("copper smelting"))
#'   browseURL(naics_url_of_code(326))
#'   }
naics_findwebscrape <- function(query) {
 
 
  myurl <- url_naics.com(query)
    
  htm <- rvest::read_html(myurl)
  x <- htm |> rvest::html_elements(css = ".first_child a") |> rvest::html_text2()
  x <- data.frame(matrix(x, ncol=2, byrow = TRUE), stringsAsFactors = FALSE)
  names(x) <- c("code", "name")
  x
}

