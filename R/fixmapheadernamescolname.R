
#' utility to convert aliases to proper colnames of map_headernames
#' used by varinfo() and fixcolnames()
#' @param x character vector of colnames of map_headernames, or aliases like "long"
#' @param alias_list optional named list where canonical names (colnames in map_headernames)
#'   are the names of vectors of alternative names
#' @return vector where aliases are replaced with actual colnames and unmatched ones left as-is
#' @seealso [fixnames_aliases()]
#' @examples 
#'   fixmapheadernamescolname(c('long', 'csv', 'api', 'r'))
#' 
#' @seealso [varinfo()] [fixnames_aliases()]
#' 
#' @keywords internal
#'
fixmapheadernamescolname <- function(x, 
                                     alias_list = list(
                                       rname = "r",
                                       longname_tableheader = c("long", "longname", "longnames", "full", "description", "longname_tableheader", "header"),
                                       shortlabel = c("short", "shortname", "shortnames", "shortlabel", "shortlabels", "labels", "label"),
                                       apiname = c('api', 'apiname'),
                                       csvname2.2 = c("csv", "csvname"),  # *** change with v 2.2 vs 2.3
                                       newnames_ejscreenapi = "friendly",
                                       oldnames = c("original", "old", "oldname", "oldnames")
                                     )) {
  
  # long is potentially a problem! 
  # be careful as we already interpret "long" as "lon" (longitude) elsewhere

  x <- fixnames_aliases(x, 
                        na_if_no_match = FALSE, ignore.case = TRUE,
                        alias_list = alias_list
  )
  
  # akas <- list(api = 'apiname',
  #              csv = 'csvname2.2',
  #              r =   'rname',
  #              original = 'oldnames',
  #              friendly = 'newnames_ejscreenapi',
  #              long = 'longname_tableheader')
  # x[x %in% names(akas)] <- as.vector(unlist(akas[match(x[x %in% names(akas)], names(akas))]))
  return(x)
}
#################################################################### # 
