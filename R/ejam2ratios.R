

#' Quick view of summary stats by type of stat, but lacks rounding specific to each type, etc.
#'
#' @param ejamitout list as from ejamit() that includes results_overall
#' @param decimals optional number of decimal places to round to
#'
#' @return prints to console and returns a simple data.frame
#'
#' @export
#'
ejam2ratios <- function(ejamitout,
                          # vartypes = c("ratio", "pctile", "pct", "all")[1],
                          # stats = c("Average site", "Average person", "Median site", "Median person", "Min", "Max"),
                          decimals = 1) {

  ## quick view of summary stats by type of stat, but lacks rounding specific to each type, etc.
  ## table_ratios_from_ejamit(testoutput_ejamit_100pts_1miles)

  x <- round(
    data.frame(
    Ratio_to_US_avg    = unlist(out$results_overall[, ..names_ratio_to_avg_these]),
    Ratio_to_State_avg = unlist(out$results_overall[, ..names_ratio_to_state_avg_these])
    ), 1
    )

  rownames(x) <- fixcolnames(names_these, "r", "shortlabel")
  cat("\n\nAverage Resident in Places Analyzed vs US or State\n\n")
  return(x)

stop()

  # OLD WAY TRIED TO USE results_summarized that comes from batch.summarize()
  #### #
#
#   xx <- ejamitout$results_summarized$rows
#   # drop Percentile of Sites - not useful really and not sure these are correct
#   xx <- xx[!grepl("of sites", rownames(xx)), ]
#   # drop Percentiles of People ? - not sure these are correct !! they looked wrong so far
#     xx <- xx[!grepl("of people", rownames(xx)), ]
#   # Median.person is also incorrect so far
#     xx <- xx[!grepl("Median.person", rownames(xx)), ]
#
#   xx <-  t(xx)
#   xx <- xx[ , colnames(xx) %in% stats]
#
#   # make these colnames more compact if they had been kept:
#   colnames(xx) <- gsub("Percentile of people ", "pctile", colnames(xx))
#
#     if ("all" %in% vartypes) {
#
#   } else {
#     keep <- rep(FALSE, length(rownames(xx)))
#     if ("ratio" %in% vartypes) {
#       keep <- keep | grepl( "^ratio.to", rownames(xx))
#     }
#     if ('pct' %in% vartypes) {
#       keep <-  keep | (grepl( "^pct", rownames(xx)) & !grepl("^pctile|^state.pctile", rownames(xx)) )
#     }
#     if ("pctile" %in% vartypes) {
#       keep <-  keep | grepl( "^pctile|^state.pctile", rownames(xx))
#     }
#
#     xx <- xx[keep, ]
#   }
#      # better to sort before doing rounding
#   xx <- xx[order(xx[ , 'Average person'], decreasing = TRUE), ] # space not period in Average person colname since did not make it data.frame which replaces space with .
#
#   xx <- round(xx, decimals)
#   rownames(xx) <- fixcolnames(rownames(xx), 'r', 'long')
#   xx <- data.frame(Indicator = rownames(xx), xx)
#   rownames(xx) <- NULL # numbered in original order from ejamit() but return indicators sorted by avg persons score
#   # xx <- xx[order(xx$Average.person, decreasing = TRUE), ]
#
#   if ("Average.person" %in% names(xx)) {
#     xx <- data.frame(
#       Indicator = xx$Indicator,
#       Average.person = xx$Average.person,
#       xx[ , setdiff(names(xx), c('Indicator', 'Average.person'))]
#     )
#   }
#   return(xx)
}
####################################################################### #



#' @export
#'
table_ratios_from_ejamit <- ejam2ratios

####################################################################### #
