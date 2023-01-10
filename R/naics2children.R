#' See NAICS codes queried plus all children of any of those
#' Used by NAICS_find()
#' @details 
#' start with shortest (highest level) codes. since tied for nchar, these branches have zero overlap, so do each.
#' for each of those, get its children = all rows where parentcode == substr(allcodes, 1, nchar(parentcode))
#' put together list of all codes we want to include so far.
#' now for the next longest set of codes in original list of codes, 
#' do same thing. 
#' etc. until did it for 5 digit ones to get 6digit children.
#' take the unique(allthat)
#' table(nchar(as.character(NAICS)))
#'    2    3    4    5    6 
#'   17   99  311  709 1057 
#' 
#' @param codes vector of numerical or character
#' @param allcodes Optional (already loaded with package) - dataset with all the codes
#'
#' @return vector of codes and their names
#' @seealso NAICS_find() NAICS
#' @export
#'
#' @examples 
#'   naics2children(211)
#'   NAICS_find(211, exactnumber=TRUE)
#'   NAICS_find(211, exactnumber=TRUE, add_children = TRUE)
#'   NAICS[211][1:3] # wrong
#'   NAICS[NAICS == 211]
#'   NAICS["211 - Oil and Gas Extraction"]
naics2children <- function(codes, allcodes=EJAM::NAICS) {
  # if (missing(allcodes)) {allcodes <- NAICS} # data from this package
  codes <- as.character(codes)
  kidrows <- NULL
  for (digits in 2:5) {
    sibset <- codes[nchar(codes) == digits]
    kidrows <- union(kidrows, which(substr(allcodes, 1, digits) %chin% sibset))
    # if that were a data.table then funion() could be used which is faster
  }
  x <- c(codes, allcodes[kidrows])
  x <- x[!duplicated(x)]
  x <- allcodes[allcodes %in% x] # cannot use %chin% unless using as.character(allcodes). fast enough anyway.
  cat(paste0('\n', names(x)), '\n')
  invisible(x)
}
