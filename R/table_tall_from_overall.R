#' table_tall_from_overall
#' Format the results_overall part of the output of ejamit() or doaggregate()
#' @param results_overall data.table of 1 row, from output of ejamit() or doaggregate()
#' @param longnames vector of names of variables in results_overall, 
#'   from output of ejamit() or doaggregate()
#'
#' @return data.table that is one row per indicator
#' @export
#'
table_tall_from_overall <- function(results_overall, longnames) {
  # just a slightly easier to read view of the results

  if (is.list(results_overall)) {
    if (is.data.frame(results_overall$results_overall)) {
      # looks like the entire output of ejamit() or doaggregate() was passed as 1st param, not just the 1 table needed
      if ("results_overall" %in% names(results_overall) & "longnames" %in% names(results_overall)) {
      longnames <- results_overall$longnames
      results_overall <- results_overall$results_overall
      } else {stop("requires results_overall and longnames, such as from output of ejamit() or doaggregate() ")}
    }
  } else {
    if (missing(longnames)) {stop("requires longnames")}
  }
  x <- as.vector(unlist(results_overall))
  x <- cbind(
    value = prettyNum(round(x, 3), big.mark = ","), 
    indicator = longnames
    )
  return(x)
}
