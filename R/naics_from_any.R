
#' find subcategories of the given overall NAICS industry code(s)
#' 
#' Given 3-digit NAICS code, for example, get all NAICS that start with those digits. 
#' 
#' @details  similar idea was naics2children() but this is more robust 
#' See [naics_from_any()] which uses this
#' @param mycodes NAICS codes vector, of 2 to 6 digits each. See <https://naics.com>
#'
#' @return a subset of the [naicstable] data.table (not just the codes column)
#' @seealso [naics_subcodes_from_code()] [naics_from_code()]  [naics_from_name()]  [naics_from_any()]
#' @examples 
#'   naics_categories()
#'   
#' @keywords internal
#'
naics_subcodes_from_code <- function(mycodes) {
  
  len <- nchar(mycodes)
  cnames  <- paste0("n", 1:6)
  results <- list()
  results[[1]] <- NULL
  for (digits in 2:6) {
    mycolname = cnames[digits]
    myvalues = unlist(as.vector(naicstable[ , ..mycolname])) # this seems like a crazy workaround, but can't see how to subset data.table by specifying mycolname = 1123 when the column name is stored in mycolname
    results[[digits]] <-  naicstable[ myvalues %in% mycodes[len == digits] ,] # subset(naicstable, mycolname %in% mycodes[len == digits] )
  }
  results <- data.table::rbindlist(results)
  return(results)
}
################################################################## #


#' search for industry names by NAICS code(s), 2-6 digits long each
#' 
#' See [naics_from_any()] which uses this
#' 
#' @param mycodes vector of numeric NAICS codes. see <https://naics.com>
#' @param children logical, if TRUE, also return all the subcategories - where NAICS starts with the same digits
#' @seealso [naics_subcodes_from_code()] [naics_from_code()]  [naics_from_name()]  [naics_from_any()]
#'
#' @return a subset of the [naicstable] data.table (not just the codes column)
#' 
#' @keywords internal
#'
naics_from_code <- function(mycodes, children=FALSE) {
  
  # find naicstable data.table rows by exact matches on numeric NAICS codes vector
  results <- NULL
  results <- naicstable[code %in% mycodes, ]
  if (children) {
    # add subcategories
    results <- naics_subcodes_from_code(results$code)
  }
  return(results)
}
################################################################## #


#' search for industry names and NAICS codes by query string
#' 
#' query by parts of words, etc. in the industry name. 
#' 
#' See [naics_from_any()] which uses this
#' @param mynames query string, vector of NAICS industry names or any regular expression or partial words. See <https://naics.com>
#' @param children logical, if TRUE, also return all the subcategories - where NAICS starts with the same digits
#' @param ignore.case see [grepl()]
#' @param fixed should it be an exact match? see [grepl()]
#' @param search_on_naics_website whether to query on naics website for more hits than just search for text in industry title
#' @seealso [naics_subcodes_from_code()] [naics_from_code()]  [naics_from_name()]  [naics_from_any()]
#' @examples 
#'  data.table::fintersect(naics_from_any( "manufac"), naics_from_any("chem"))
#' @return a subset of the [naicstable] data.table (not just the codes column)
#' 
#' @keywords internal
#'
naics_from_name <- function(mynames, children=FALSE, ignore.case = TRUE, fixed = FALSE) {
  
  # find naicstable data.table rows by text search in NAICS industry names via grepl()
  hits <- vector()
  results <- NULL
  for (i in 1:length(mynames)) {
    hits <- c(hits, which(grepl(mynames[i], naicstable$name, ignore.case = ignore.case,  fixed = fixed)) )
  }
  results <- naicstable[unique(hits),]
  if (children) {
    # add subcategories
    results <- naics_subcodes_from_code(results$code)
  }
  return(results)
} 
################################################################## #


#' General way to search for industry names and NAICS codes
#' 
#' Find industry names and codes by searching for queried code(s) or text
#' 
#' @param query query string(s) and/or number(s), vector of NAICS codes or industry names or any regular expression or partial words
#' @param children logical, if TRUE, also return all the subcategories - where NAICS starts with the same digits
#' @param ignore.case see [grepl()]
#' @param fixed should it be an exact match? see [grepl()]
#' @param website_scrape whether to scrape info from the NAICS website to return a table of codes and names that match (web query uses synonyms so gets more hits)
#' @param website_url whether to return the URL of the webpage with info on the NAICS (web query uses synonyms so gets more hits)
#' @seealso [naics_subcodes_from_code()] [naics_from_code()]  [naics_from_name()]  [naics_from_any()]
#'
#' @return a subset of the [naicstable] data.table (not just the codes column)
#' @examples # Also see vignette for examples
#'   naics_categories()
#'   naics_from_any(naics_categories(3))[order(name),.(name,code)][1:10,] 
#'   naics_from_any(naics_categories(3))[order(code),.(code,name)][1:10,] 
#'   naics_from_code(211)
#'   naicstable[code==211,]
#'   naics_subcodes_from_code(211)
#'   naics_from_code(211,  children = TRUE)
#'   naicstable[n3==211,]
#'   NAICS[211][1:3] # wrong
#'   NAICS[NAICS == 211]
#'   NAICS["211 - Oil and Gas Extraction"]
#'   
#'  naics_from_any("plastics and rubber")[,.(name,code)]
#'  naics_from_any(326)
#'  naics_from_any(326, children = T)[,.(code,name)]
#'  naics_from_any("plastics", children=T)[,unique(n3)] 
#'  naics_from_any("pig")
#'  naics_from_any("pig ") # space after g
#'  
#'  # naics_from_any("copper smelting")
#'  # naics_from_any("copper smelting", website_scrape=TRUE)
#'  # browseURL(naics_from_any("copper smelting", website_url=TRUE) )
#'  
#'  a = naics_from_any("plastics")
#'  b = naics_from_any("rubber") 
#'  fintersect(a,b)[,.(name,code)] #  a AND b
#'  funion(a,b)[,.(name,code)]     #  a OR  b
#'  naics_subcodes_from_code(funion(a,b)[,code])[,.(name,code)]   #  plus children
#'  naics_from_any(funion(a,b)[,code], children=T)[,.(name,code)] #  same
#'  
#'  NROW(naics_from_any(325))
#' #[1] 1
#'  NROW(naics_from_any(325, children = T))
#' #[1] 54
#'  NROW(naics_from_any("chem"))
#' #[1] 20
#'  NROW(naics_from_any("chem", children = T))
#' [1] 104
#' 
#' @export
#'
naics_from_any <- function(query, children=FALSE, ignore.case = TRUE, fixed = FALSE, 
                           website_scrape=FALSE, website_url=FALSE) {
  
  # find naicstable data.table rows by vector of text queries and/or numeric NAICS codes
  # returns subset of naicstable, not in any particular order and number of rows may be longer than number of query terms
  isnum <- suppressWarnings( !is.na(as.numeric(query)) )
  
  if (website_url) {
    return(naics_url_of_code(query))
  }
  if (website_scrape) {
    return(naics_findwebscrape(query))
  }
  
  query_codes <- query[isnum]
  if (length(query_codes) != 0) {
    via_codes <- naics_from_code(query_codes, children = children)
  } else {
    via_codes <- NULL
  }
  query_text <- query[!isnum]
  if (length(query_text) != 0) {
    via_text  <- naics_from_name(query_text,  children = children, ignore.case = ignore.case, fixed = fixed)
  } else {
    via_text <- NULL
  }
  results <- data.table::rbindlist(list(via_codes, via_text))
  # if (children) {
  #   # add subcategories
  #   results <- naics_subcodes_from_code(results$code)
  # }
  return(results)
}
 
