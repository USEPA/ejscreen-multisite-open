
#' utility to convert aliases to proper colnames of map_headernames
#'
#' @param x character vector of colnames of map_headernames, or aliases like "long"
#'
#' @return vector where aliases are replaced with actual colnames and unmatched ones left as-is
#' @export
#'
#' @examples 
#'   fixmapheadernamescolname(c('long', 'csv', 'api', 'r'))
#' 
fixmapheadernamescolname <- function(x) {
  # interpret shorthand/aliases for column names of map_headernames
  akas <- list(api = 'apiname',
               csv = 'csvname2.2',
               r =   'rname',
               original = 'oldnames',
               friendly = 'newnames_ejscreenapi',
               long = 'longname_tableheader')
  x[x %in% names(akas)] <- as.vector(unlist(akas[match(x[x %in% names(akas)], names(akas))]))
  return(x)
}
#################################################################### # 
