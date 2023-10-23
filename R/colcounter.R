#' colcounter - Count columns with Value (at or) above (or below) threshold
#' @param x Data.frame or matrix of numbers to be compared to threshold value.
#' @param threshold numeric threshold value to compare to
#' @param or.tied if TRUE, include ties (value in x equals threshold)
#' @param na.rm if TRUE, used by colcounter to count only the non-NA columns in given row
#' @param below if TRUE, count x below threshold not above threshold
#' @param one.cut.per.col if FALSE, compare 1 threshold to all of x.
#'   If TRUE, specify one threshold per column.
#' @return vector of counts as long as NROW(x)
#' @seealso colcounter_summary_all() colcounter_summary() colcounter_summary_cum() colcounter_summary_pct() colcounter_summary_cum_pct() tablefixed()
#' @export
#'
#' @examples \dontrun{
#'  pdata <- data.frame(a=rep(80,4),b=rep(93,4), col3=c(49,98,100,100))
#'   ### pdata <- EJAM::blockgroupstats[ , names_e_pctile]
#'   ## or ## pdata <- ejscreen package file bg22[ , ejscreen package file names.e.pctile]
#'  pcuts <-  5 * (0:20)  # <- as.vector(keystats_e['highcut', ])
#' colcounter_summary(        pdata, pcuts)
#' colcounter_summary_pct(    pdata, pcuts)
#' colcounter_summary_cum(    pdata, pcuts)
#' colcounter_summary_cum_pct(pdata, pcuts)
#' colcounter_summary_cum_pct(pdata, 5 * (10:20))
#'
#' x80 <- colcounter(pdata, threshold = 80, or.tied = T)
#' x95 <- colcounter(pdata, threshold = 95, or.tied = T)
#' table(x95)
#' tablefixed(x95, NCOL(pdata))
#' cbind(at80=tablefixed(x80, NCOL(pdata)), at95=tablefixed(x95, NCOL(pdata)))
#'   }
#'
colcounter <- function(x, threshold, or.tied=TRUE, na.rm=TRUE, below=FALSE, one.cut.per.col=FALSE) {
  # Function to count SCORES ABOVE BENCHMARK(S) at each place, returns list as long as NROW(x).
  #
 
  if (is.null(dim(x))) {numcols <- 1; stop('expected data.frame as x but has only 1 dimension')} else {numcols <- dim(x)[2]}
  if (missing(threshold)) {
    if (one.cut.per.col) {
      threshold <- colMeans(x, na.rm = na.rm)
    } else {
      threshold <- rowMeans(x, na.rm = na.rm)
    }
  }
  if (one.cut.per.col) {
    if (length(threshold) != NCOL(x)) {stop('length of threshold should be same as number of columns in x if one.cut.per.col=T')}
    x <- t(as.matrix(x)) # this allows it to compare vector of N thresholds to N columns
  } else {
    if ((length(threshold) != NROW(x)) & (length(threshold) != 1)) {stop('length of threshold should be 1 or same as number of columns in x, if one.cut.per.col=F')}
  }
  if (below) {
    if  (or.tied) { y <- ( x <= threshold) }
    if (!or.tied) { y <- ( x <  threshold) }
  } else {
    if  (or.tied) { y <- ( x >= threshold) }
    if (!or.tied) { y <- ( x >  threshold) }
  }
  if (one.cut.per.col) {y <- t(y)}
  count.per.row <- rowSums(y, na.rm = na.rm)
  return(count.per.row)
}
######################################## #




