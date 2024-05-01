
#' Answers questions like What `%` of sites have at least 1 demog. indicator `>2x` state avg?
#' 
#' @description This function provides tables of summary stats but also text
#' that explains those findings in plain English. It relies on [colcounter_summary_all()]
#' 
#' @details Helps provide stat summaries such as: 
#' 
#'  (x%) of these (sites) have 
#'  
#'  at least (N) of these (YTYPE )indicators
#' 
#'  at least (R) times the (State/National average)
#'  
#' @return Returns a list with two named elements, "stats" and "text" where
#'   stats is a 3-dimensional array of numbers. See dimnames(output$stats).
#'   
#' 
#' @param scores scores in a table with one row per place and one column per indicator
#' @param thresholds thresholds vector of numbers as benchmarks. Assuming the indicators
#'   in the scores table are ratios to the average, then the thresholds could be
#'   for example, 1.50, 2, etc. which would represent ratios that are 1.5x or 2x etc.
#' @param xwide must be "statewide" or "nationwide" -- used only in the text output
#'   that describes the findings.
#' @param text_suffix If using ratios, use the default, which explains these
#'   thresholds as X times the average. 
#'   If using percentiles as thresholds, set text_suffix = "."
#'   or text_suffix = "th percentile in the state." for example
#' @param text_indicatortype can be "EJ Indexes" or "demographic indicators"
#'   for example
#' @param quiet whether to print findings to console
#'
#' @examples
#' # x <- ejamit(testpoints_100, radius = 1)
#' x <- testoutput_ejamit_1000pts_1miles 
#' out <- x$results_bysite
#' out <- setDF(copy(out))
#' ratio_data <- out[, names_d_ratio_to_state_avg]
#' ratio_benchmarks <- c(2, 3, 5, 10)
#'
#' findings <- count_sites_with_n_high_scores(ratio_data, ratio_benchmarks)
#'
#' names(findings)
#' dim(findings$text)
#' # see most striking stat only
#' tail(findings$text[findings$text != ""], 1) # the most extreme finding
#' findings$text[findings$text != ""]
#'
#' dimnames(findings$stats) # count, cut, stat
#' ## stat can be count, cum, pct, or cum_pct
#'
#' findings$stats[1,,] # any of the indicators (at least one indicator)
#' findings$stats[,,"count"]
#' findings$stats[ , , 1]
#' findings$stats[ , 1, ]
#' 
#' pctile_data <- testoutput_ejamit_1000pts_1miles$results_bysite
#' pctile_data <- pctile_data[, ..names_ej_state_pctile]
#' pctile_benchmarks <- c(80, 90)
#' x <- count_sites_with_n_high_scores(pctile_data, pctile_benchmarks,
#'   text_indicatortype = "EJ Indexes",
#'   text_suffix = "th percentile in the state.")
#' 
#' # see most striking stat only
#' mx <- count_sites_with_n_high_scores(pctile_data, 
#'   thresholds = 1:100, quiet = TRUE,
#'   text_indicatortype = "EJ Indexes",
#'   text_suffix = "th percentile in the state.")
#' tail(mx$text[mx$text != ""], 1) # the most extreme finding
#' 
#' @export
#'
count_sites_with_n_high_scores <- function(scores, thresholds = c(1.01, 2, 5, 10),
                                           text_indicatortype = "indicators", # can be "EJ Indexes" or "demographic indicators"
                                           xwide = c("statewide", "nationwide")[1],
                                           text_suffix = paste0(" times the ", xwide, " overall average."), # can be "th percentile in the state."
                                           quiet = !interactive()) {
  
  ratiodata <- as.data.frame(scores) # needs a data.frame
  ratio_benchmarks <- thresholds
  
  # (x%) of these (sites) have at least (N) of these (YTYPE )indicators at least (R) times the (State/National average)
  
  # e.g., What % of sites have at least 1 demog indicator >2x state avg? 
  
  # x <- ejamit(testpoints_100, radius = 1)
  # out <- x$results_bysite  # $ratio.to.state.avg.Demog.Index
  ## library... need to have data.table pkg
  # out <- setDF(copy(out))
  # 
  # ratio_benchmarks <- c(1.01, 1.50, 2, 3, 5, 10)
  # ratiodata <- out[, names_d_ratio_to_state_avg]
  
  ratiodata[is.na(ratiodata)] <- NA 
  for (ccc in 1:ncol(ratiodata))
  {
    ratiodata[is.infinite(ratiodata[,ccc]), ccc] <- NA
  }  
  
  sitestats <-  colcounter_summary_all(ratiodata, thresholdlist =  ratio_benchmarks, or.tied = TRUE)
  
  cumpcts <-  sitestats[, , "cum_pct"]  
  # sitestats[, , "count"]
  #                radius
  # count.of.cols      1 1.5  2  3  5 10
  #                0   2   2  2  2  2  2
  #                1   2  17 33 44 48 48
  #                2  11  15 10  3  0  0
  #                3   8   6  1  1  0  0
  #                4   3   3  1  0  0  0
  #                5   5   3  1  0  0  0
  #                6   7   2  0  0  0  0
  #                7   9   1  2  0  0  0
  #                8   3   1  0  0  0  0
  #                9   0   0  0  0  0  0
  #                10  0   0  0  0  0  0
  
  # cutnum = 1
  # benchmark = ratio_benchmarks[cutnum]
  # n = 1 # how many of the indicators?
  # coln = which(ratio_benchmarks == benchmark) # or same as cutnum
  # rown - 1 = n  
  # cumpcts
  textout <- matrix("", nrow = NROW(cumpcts), ncol = NCOL(cumpcts))
  for (rown in  (2:NROW(cumpcts))) {
    # cat('\n')
    for (coln in rev(1:length(ratio_benchmarks))) {
      # this assumes you provided ratio_benchmarks in increasing order !
      # but makes sense to report highest ratios first.
      n <- rown - 1
      pct <-  (cumpcts[rown, coln])
      sitetxt <- paste0(
        "At ", 
        ifelse(pct > 0, paste0(pct, "% of "), "none of "), "these sites, ",
        "at least ", n, " of the ", text_indicatortype, ifelse(n != 1, " are ", " is "), 
        "at least ", ratio_benchmarks[coln], text_suffix
      )
      if (pct > 0) {
        # cat(sitetxt, "\n")
        textout[rown, coln] <- sitetxt
      } else {
          textout[rown, coln] <- ""
        }
    }
  }
  if (!quiet) {print(findings$text[findings$text != ""])}
  return(list(stats = sitestats, text = textout))
}
