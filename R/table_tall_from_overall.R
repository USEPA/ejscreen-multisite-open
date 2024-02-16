#' Format the results_overall part of the output of ejamit() or doaggregate()
#' 
#' Take a quick look at results in the RStudio console
#' 
#' @param results_overall data.table of 1 row, from output of ejamit() or doaggregate()
#' @param longnames vector of names of variables in results_overall, 
#'   from output of ejamit() or doaggregate()
#'
#' @return data.table that is one row per indicator
#' @examples 
#'  table_tall_from_overall(testoutput_ejamit_10pts_1miles$results_overall)
#'  table_tall_from_overall(x$results_bysite[1, ])
#' @export
#'
table_tall_from_overall <- function(results_overall, longnames) {
  # just a slightly easier to read view of the results
  
  if (missing(longnames)) {longnames <- fixcolnames(names(results_overall), "r", "long")}
  
  if (is.list(results_overall)) {
    if (is.data.frame(results_overall$results_overall)) {
      # looks like the entire output of ejamit() or doaggregate() was passed as 1st param, not just the 1 table needed
      if ("results_overall" %in% names(results_overall) & "longnames" %in% names(results_overall)) {
        longnames <- results_overall$longnames
        results_overall <- results_overall$results_overall
      } else {
        warning("requires results_overall and longnames, such as from output of ejamit() or doaggregate() ")
        return(NULL)
      }
    }
  } else {
    if (missing(longnames)) {
      warning("requires longnames")
    }
  }
  
  x <- copy(results_overall)
  x <- fix_pctcols_x100(x)
  x <- table_round(x)
  
  x <- as.vector(unlist(x))
  x[!var_is_numeric_ish(x)] <- NA # easier to just drop the info like state name/abbrev.
  # a vector has to be numeric or not, cannot have just some elements numeric some char.
  # x[var_is_numeric_ish(x)] <- as.numeric(x[var_is_numeric_ish(x)] )
  x  <- as.numeric(x  )
  x <- cbind(
    value = x, 
    indicator = longnames
  )
  x <- data.frame(x)
  return(x)
  # data.frame(table_tall_from_overall( table_round( ejamit_a$results_overall) ))
  
}
