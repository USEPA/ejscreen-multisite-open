#' See the names of industrial categories and their NAICS code
#' Easy way to list the 2-digit NAICS (17 categories), or other level 
#' @details 
#'  Also see <https://www.naics.com/search/>
#'  
#' There are this many NAICS codes roughly by number of digits in the code:
#' 
#'   table(nchar(NAICS))
#'   
#'    2    3    4    5    6
#'    
#'   17   99  311  709 1057 
#'   
#'   See <https://www.census.gov/naics/>
#'   
#' @param digits default is 2, for 2-digits NAICS, the top level, but could be up to 6.
#' @param dataset Should default to the dataset called NAICS, installed with this package. 
#'   see [NAICS]  Check attr(NAICS, 'year')
#' @examples  naics_categories()
#' @export
#'
#' @seealso [naics_from_any]  [NAICS]
naics_categories <- function(digits=2, dataset=EJAM::NAICS) {
  cat("Also see https://www.naics.com/search/ \n")
  if (is.null(dataset)) {warning('missing NAICS dataset'); return(NA)}
  cbind(cbind(dataset[nchar(as.character(dataset)) == digits]))
}

# make separate columns for the codes, titles, indented code-title strings, 
#  and pulldown selection code-title (was the names of code vector).

# indent <- data.frame(title = gsub(".* - ", "", names(NAICS)), code = NAICS, stringsAsFactors = FALSE, row.names = NULL)
# 
# cbind(paste0(
#   stringr::str_pad(ndent$code, width = 6, side = 'right'),
#   ifelse(nchar(ndent$code) == 2, ' ** ', "   "), 
#   stringr::str_pad(
#     stringr::str_pad(
#       ndent$title, pad = ifelse(nchar(ndent$code) == 4, '.', ifelse(nchar(ndent$code) < 4, '*', ' ')), 
#       width = -3 + (2 * nchar(ndent$code)) + nchar(ndent$title), 
#       side = 'left'
#       ), 
#     width = ifelse(nchar(ndent$code) < 4, 75, 1), 
#     pad = '*', 
#     side = 'right'
#   )
# ))
