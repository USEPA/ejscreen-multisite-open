#' for query term, show list of roughly matching NAICS, scraped from web
#' This finds more than just [naics_find()] does, since that needs an exact match
#'   but this looks at naics.com website which lists various aliases for a sector.
#' 
#' @param query text like "gasoline" or "copper smelting"
#' @seealso [naics_find()] [naics_url_of_query()]
#' @return data.frame of info on what was found, naics and title
#' @export
#'
#' @examples
#'  naics_find("copper smelting", search_on_naics_website=FALSE)
#'  naics_find("copper smelting", search_on_naics_website=TRUE)
#'   naics_url_of_query("copper smelting")
#'   \dontrun{
#'   naics_findwebscrape("copper smelting")
#'   browseURL(naics_url_of_query("copper smelting"))
#'   browseURL(naics_url_of_code(326))
#'   }
naics_findwebscrape <- function(query) {
  # query <- "copper smelting"
  # query <- gsub(" ", "+", query)
  # myurl <- EJAM::naics_find(query, search_on_naics_website=TRUE)
  myurl <- naics_url_of_query(query)
    
  htm <- rvest::read_html(myurl)
  x <- htm %>% rvest::html_elements(css = ".first_child a") %>% rvest::html_text2()
  x <- data.frame(matrix(x, ncol=2, byrow = TRUE), stringsAsFactors = FALSE)
  names(x) <- c("naics", "title")
  x
}
