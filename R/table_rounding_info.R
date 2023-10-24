############################################################################# #


#' table_rounding_info - how many decimal places to round to for given variable(s)
#'
#' @param var vector of variable names such as c("pctlowinc", "cancer") or c(names_d, names_d_subgroups)
#' @param varnametype which column of map_headernames to use when looking for var, like "rname" or "api" or "long"
#' @seealso [varinfo()] [table_round()]
#' @return named vector same size as var, with var as names.
#' @examples
#'   table_rounding_info("pm")
#'   table_round(8.252345, "pm")
#'   table_round(8, "pm")
#'   
#'   cbind(table_rounding_info(names_all_r), fixcolnames(names_all_r, "r", "long"))
#'  
#' @export
#'
table_rounding_info <- function(var, varnametype="rname") {
  as.numeric(  # in case it was still stored as character in map_headernames
    varinfo(var = var, info = "decimals", varnametype = varnametype)
  )
}





