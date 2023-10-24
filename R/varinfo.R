#' varinfo - Get metadata for a variable, like its type, glossary definition, decimal places rounding, etc.
#'
#' @param var vector of variable names such as c("pctlowinc", "cancer") or c(names_d, names_d_subgroups)
#' @param info one type of metadata/info needed, such as "decimals" or "long", 
#'   which should be the name of a column in map_headernames, to pass to fixcolnames()
#'   which is the function that actually looks up the info.
#'   Or a vector of those, in which case a table is returned, one column for each.
#' @param varnametype which column of map_headernames to use when looking for var, like "rname" or "api" or "long"
#' @seealso fixcolnames() [table_rounding_info()]  
#' @return named vector same size as var, with var as names; 
#'    unless info is a vector in which case a table is returned, one col per value of info.
#'    Results can be character, numeric, etc. depending on what info is requested
#' @export
#'
#' @examples  
#' varinfo("traffic.score", "decimals")
#' varinfo(names_d, "long")
#' myvars <- c(names_d, names_d_subgroups, names_e)
#' myinfo <- "percentage"
#' cbind(  is.a.percentage = varinfo(myvars, myinfo) )
#' cbind(varinfo(names_all_r, "pctile."))
#' myinfo <- "long"
#' cbind(varinfo(myvars, myinfo) )
#' table_rounding_info(names_e)
#'  
#' varinfo(
#'  var = c(names_these, names_d_pctile),
#'  info = c(
#'  "topic_root_term", "varcategory", "vartype", "percentage", "pctile.", "calculation_type"
#' ))
#' 
#' varinfo(names_all_r, c("varcategory", "varlist", "in_api", "in_bgcsv"))
#' 
varinfo <- function(var, info, varnametype="rname") {
  if (length(info) > 1) {
    x <- sapply(info, function(info1) fixcolnames(var, oldtype = varnametype, newtype = info1))
    rownames(x) <- var
    return(x)
  }
  x <- fixcolnames(var, oldtype = varnametype, newtype = info)
  names(x) <- var
  x
}
