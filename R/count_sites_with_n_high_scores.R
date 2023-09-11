count_sites_with_n_high_scores <- function(scores, thresholds=c(1.01, 1.50, 2, 3, 5, 10), xwide=c("statewide", "nationwide")[1]) {
  ratiodata <- scores # data.frame
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
  
  sitestats <-  colcounter_summary_all(ratiodata, thresholdlist =  ratio_benchmarks, or.tied=TRUE)
  
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
    cat('\n')
    for (coln in rev(1:length(ratio_benchmarks))) {
      # this assumes you provided ratio_benchmarks in increasing order !
      # but makes sense to report highest ratios first.
      n <- rown - 1
      pct <-  (cumpcts[rown, coln])
      sitetxt <- paste0(
        "At ", 
        ifelse(pct > 0, paste0(pct, "% of "), "none of "), "these sites, ",
        "at least ", n, " indicator", ifelse(n != 1, "s are ", " is "), 
        "at least ", ratio_benchmarks[coln], " times the ", xwide," overall average."
      )
      if (pct > 0) {
        cat(sitetxt, "\n")
        textout[rown, coln] <- sitetxt
      } else {
          textout[rown, coln] <- ""
        }
    }
  }
  return(list(stats=sitestats, text=textout))
}
