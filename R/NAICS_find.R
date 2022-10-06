#' Search for an industrial sector in the list of NAICS codes, see subsectors
#'
#' Just a utility, quick way to view NAICS industrial sectors that contain queried word or phrase,
#' but can also see all the subcategories within the matching one.
#'
#' @details
#' See \url{https://www.census.gov/naics/}
#'  NOTE: By default, this shows NAICS that match the text query,
#'  and also can include all the children NAICS even if they do not match based on text query.
#'  So it first finds NAICS that match the text (or code) query via grep(),
#'  and then can also include all subcategories within those categories.
#'  
#'  So NAICS_find('soap', add_children=TRUE) shows "325612 - Polish and Other Sanitation Good Manufacturing",
#'  and others, not just "3256 - Soap, Cleaning Compound, and Toilet Preparation Manufacturing",
#'  because 3256 matches 'soap' and 325612 is a subcategory of 3256.
#'  
#'  The format of NAICS, the naics_dataset, in this package is 
#'  dput(NAICS[1:4]) 
#'  c(`11 - Agriculture, Forestry, Fishing and Hunting` = 11, `111 - Crop Production` = 111, 
#'    `1111 - Oilseed and Grain Farming` = 1111, `11111 - Soybean Farming` = 11111
#'    )
#'
#' @param query a single word or phrase such as "chemical manufacturing" or "cement"
#' @param add_children default is FALSE, so it does NOT chidren (subcategories) of those that match the query.
#' @param naics_dataset Should default to the dataset NAICS, installed with this package. see \link{NAICS}
#' @param ignore.case default TRUE, ignoring whether query is upper or lower case
#'
#' @seealso  \link{NAICS_categories} \link{NAICS} 
#' @examples
#'  # NAICS_find('paper')
#'  NAICS_find('pulp', add_children = FALSE)
#'  NAICS_find('pulp', add_children = TRUE)
#'  NAICS_find('asdfasdf', add_children = TRUE)
#'  NAICS_find('asdfasdf', add_children = FALSE)
#'  
#' @export
#'
NAICS_find <- function(query, add_children=FALSE, naics_dataset=NULL, ignore.case=TRUE) {
  # NAICS would be from installed package, EJAM::NAICS  
  if (is.null(naics_dataset) & !exists('NAICS')) {warning('missing NAICS dataset and not passed as a parameter to NAICS_find'); return(NA)}
  if (is.null(naics_dataset) &  exists('NAICS')) {naics_dataset <- NAICS}
  if (class(naics_dataset) != 'numeric' | length(naics_dataset) < 2000) {warning('naics_dataset does not seem to be what is expected')}
  # browser()
  
  # Find all industry entries that match the query at all, including say 4 digit and 5 or 6 digit codes as well,
  #  ( not any parent or children entries unless they each match, themselves )
  suppressWarnings( rownum <-     which(grepl(query, names(naics_dataset), ignore.case = ignore.case)) )
  if (!add_children) {
    
    if (0 == length(rownum)) {
      return(NA) # none found
    } else {
      # prefix <- naics_dataset[[rownum]]
      found <- rownum
    }
    
  } else {
    # add children as well
    if (0 == length(rownum)) {
      return(NA)
    } else {
      # maybe get JUST ONE PARENT CATEGORY - find the entry that is highest up in the tree (fewest naics digits) and also smallest of all with that many digits.
      # rownum_1 <- rownum[which.min(naics_dataset[rownum])] # want the rownum of the smallest NAICS code number  (not the smallest row number)
      # prefix <- naics_dataset[[rownum_1]]
      #   ALSO NOW ADD IN ALL THOSE BELOW the 1 parent (THOSE WITH LONGER NAICS CODES THAT START WITH THE 1 HIT CODE)
      # found <- substr(names(naics_dataset), 1, nchar(prefix)) == prefix
      
      # ADD IN ALL THOSE BELOW ANY OF THESE HITS
      codes_matching <- naics_dataset[rownum]
      codes_plus_kids <- naics2children(codes_matching, naics_dataset)
      
      found <- which(naics_dataset %in% codes_plus_kids)
    }
  }
  
  return( (naics_dataset[found]) )
}


naics2children <- function(codes, allcodes) {
  # return the codes queried plus all children of any of those.
  #
  # start with shortest (highest level) codes. since tied for nchar, these branches have zero overlap, so do each.
  # for each of those, get its children = all rows where parentcode == substr(allcodes, 1, nchar(parentcode))
  # put together list of all codes we want to include so far.
  # now for the next longest set of codes in original list of codes, 
  # do same thing. 
  # etc. until did it for 5 digit ones to get 6digit children.
  # take the unique(allthat)
  # table(nchar(as.character(NAICS)))
  # 
  # 2    3    4    5    6 
  # 17   99  311  709 1057 
  if (missing(allcodes)) {allcodes <- NAICS} # data from this package
  kidrows <- NULL
  for (digits in 2:5) {
    sibset <- codes[nchar(codes) == digits]
    kidrows <- union(kidrows, which(substr(allcodes,1,digits) %in% sibset))
  }
  x <- c(codes, allcodes[kidrows])
  x <- x[!duplicated(x)]
  x <- allcodes[allcodes %in% x]
  
  return(x)
}
