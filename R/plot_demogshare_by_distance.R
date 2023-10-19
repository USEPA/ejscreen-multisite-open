#' plot_demogshare_by_distance - work in progress
#' @details Could also consider plotting something like boxplot(demogvar ~ round(distance, 1))
#' 
#'   See notes on plots at [plot_barplot_ratios()]
#'   
#' @param results_bybg_people 
#' @param demogvarname 
#' @param siteids 
#' @param show.lowess F
#' @param show.lm linefit
#' @param show.line linefit
#' @param ... passed to plot

#' @export

plot_demogshare_by_distance <- function(results_bybg_people, demogvarname=names_d[1], siteids=unique(results_bybg_people$siteid),
                                        show.lowess=F, show.lm=TRUE, show.line=TRUE, ...) {
  #  results_bybg_people <- data.table::copy(testoutput_ejamit_10pts_1miles$results_bybg_people)
  x <- data.table::copy(results_bybg_people)
 myrad = max(x$radius.miles) 
  # SHOULD IT USE distance_avg or distance_min_avgperson ?? ***
  colsneeded <- c("siteid", "distance_min_avgperson", demogvarname)
  x <- x[siteid %in% siteids, ..colsneeded]
  
  data.table::setorder(x, distance_min_avgperson)
  xvals <- x$distance_min_avgperson
  yvals <- unlist(x[ , ..demogvarname])
  
  plot(
    x = xvals,
    y = yvals, 
    main = "Demographic share at each distance, by block group",
    xlab = "Distance from site in miles",
    ylab = fixcolnames(demogvarname, "r", 'long'),
    xlim = c(0, myrad), ...
       )
  
  linefit <- function(x, y, type='b', cex=4, show.lowess=TRUE, show.lm=TRUE, show.line=TRUE) {
    if (show.lowess) { lines(lowess(x, y), type = type, col = "blue", pch = '.', cex = cex) } # lowess line (x, y)
    if (show.lm)     { abline(lm(y ~ x, na.action = na.exclude), col = "dark green") } # regression line (y ~ x)
    if (show.line)   { abline(coef(line(x,y)), col = 'light green') }
  }
  linefit(xvals, yvals, type = 'b', cex = 4, show.lowess = show.lowess, show.lm  = show.lm, show.line = show.line)
  # lines(lowess(xvals, yvals), type= 'b', col="blue", pch='.', cex=4)
  
}
