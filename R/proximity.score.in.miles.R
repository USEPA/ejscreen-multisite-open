#' convert EJScreen proximity scores to miles per site instead of sites per kilometer
#' Shows US percentiles if no arguments used
#' @param scoresdf data.frame of simple proximity scores like for tsdf, rmp, npl 
#'   but not traffic.score or npdes one since those are weighted and not just count per km
#'
#' @export
#'
proximity.score.in.miles <- function(scoresdf=NULL) {
  if (is.null(scoresdf)) {
    scoresdf <- EJAM::usastats[ , c('proximity.tsdf', 'proximity.rmp', 'proximity.npl')]
    defaultused=TRUE} else { defaultused=FALSE}
  # grep("^proximity|^traffic",names(EJAM::blockgroupstats),value = TRUE)
  # [1] "traffic.score"   "proximity.npdes" "proximity.npl"   "proximity.rmp"   "proximity.tsdf" 
  # but .npdes is weighted by exposure so should not really be included
  # and traffic.score is weighted by volume of traffic and units or scope differ
  miles_per_km <- 0.6213712 # 1000 / EJAMejscreenapi::meters_per_mile
  scoresdf <- as.data.frame(scoresdf)
  
  x = 1 / scoresdf
  x = miles_per_km * x
  # x = sapply(scoresdf, function(z) miles_per_km / z)
  x = round(x, 3)
  x = sapply(x, function(z) ifelse(is.infinite(z), NA, z))
  if (missing(scoresdf)) print('missing')
  if(defaultused) {x = data.frame(PCTILE=EJAM::usastats$PCTILE, x, stringsAsFactors = FALSE)}
  return(x)
}

