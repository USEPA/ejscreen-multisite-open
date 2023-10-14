#' rrtable
#' Calculate risk ratios of avg E in group D / in everyone not in group D
#' @param x blockgroupstats
#'
#' @return table of ratios and max per row and max per col
#' @export
#'
rrtable <- function(x) {
  x <- as.data.frame(x)
  if (mean(colMeans(  x[ , names_d], na.rm = TRUE)) > 1) {
    x[ , names_d] <- x[ , names_d] / 100
  }
  if (mean(colMeans(  x[ , names_d_subgroups_nh], na.rm = TRUE)) > 1) {
    x[ , names_d_subgroups_nh] <- x[ , names_d_subgroups_nh] / 100
  }
  
  
  rt <- RR(
    e = x[ , names_e],  
    d = x[ , c(names_d, names_d_subgroups_nh)], 
    pop = x$pop)
  
  round(cbind(
    bbb <- rbind(
      rt, 
      MAX = matrixStats::colMaxs(rt) ), 
    MAX = matrixStats::rowMaxs(bbb)), 3)
  
}
