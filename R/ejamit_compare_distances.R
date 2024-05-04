

#' compare ejamit results for more than one radius
#' run ejamit() once per radius, get a table with a row per radius
#' 
#' @param sitepoints like for [ejamit()]
#' @param radii vector of radius values like 1:3 for [ejamit()]
#' @param ... passed to [ejamit()]
#' @param quiet  passed to [ejamit()]
#' @param silentinteractive  passed to [ejamit()]
#' 
#' @examples 
#'   radii <- c(1,2,3,6,10)
#'   radii <- c(1, 10)
#'   pts <- testpoints_100
#'   pts <- testpoints_10
#'   x <- ejamit_compare_distances(pts, radii = radii)
#'   EJAM:::ejamit_compare_distances(x)
#'   
#' @seealso [plot_distance_by_pctd()], [distance_by_group()] and several other related functions
#' @return data.table like ejamit()$results_overall but with one row
#'   per radius.
#'   
#' @export
#'
ejamit_compare_distances <- function(sitepoints, radii = c(1,2,3), quiet = TRUE, silentinteractive = TRUE, plot = TRUE, ...) {

  # ejamit_compare_distances  could be a good name like jamit_compare_groups_of_places
    
  z = list()
  for (i in seq_along(radii)) {
    z[[i]] <- EJAM::ejamit(sitepoints = sitepoints, radius = radii[i], quiet = quiet, silentinteractive = silentinteractive, ...)$results_overall
  }
  z = rbindlist(z)
  shown = data.frame(round(t(z)[names_these_ratio_to_state_avg, ], 1))
  colnames(shown) <- radii
  rownames(shown) <- fixcolnames(rownames(shown), 'r', 'shortlabel')
  cat("\n")
  print(shown)
  
  if (plot) {
    cat("\n Indicators that most strongly get larger as you get closer: \n")
    print(distance_comparison_ejamit2plot(z))
  }
  return(z)
}
#################################################################### #



distance_trends = function(x, radii, n = 1) {
  
  # radii <- x$radius.miles
  
  # REPORT STRONGEST TREND
  # fit line to points and report which has the most negative slope, e.g.
  # cat("Indicators that most strongly get larger as you get closer: \n")
  slopes = coef(lm( as.matrix(x) ~ radii ))[2, ]
  topn = head(sort(slopes), n)
  topn = fixcolnames(names(topn), "r", "long")
  
  return(topn)
}
#################################################################### #



distance_comparison_ejamit2plot <- function(outall, myvars = names_d_subgroups_ratio_to_state_avg, n = 1) {
  
  outall <- data.frame(outall)
  radii <- outall$radius.miles
  
  x <- outall[, myvars]
  
  # REPORT STRONGEST TREND
  
  topn = distance_trends(x = x, radii = radii, n = n)
  
  # PLOT
  
  mycolors = palette.colors(NCOL(x))
  
  for (i in 1:(NCOL(x))) {
    if (i == 1) {
      plot(x = radii, y = x[, i], type = "b", col = mycolors[i],
           # x labels should be radii
           xlab = "Distance (radius) in miles",
           ylab = "Ratio of Avg. within X miles to Avg. Statewide or Nationwide",
           xlim = c(0, max(radii)),
           ylim = c(0, 5))
    } else {
      points(x = radii, y = x[, i], type = "b", col = mycolors[i])
    }
  }
  
  legend("topleft", legend = paste0("", fixcolnames(names(x), "r", "shortlabel"),""), fill = palette.colors(NCOL(x)))
  abline(h = 1, col = "lightgray")
  
  return(
    topn
  )
}
#################################################################### #
