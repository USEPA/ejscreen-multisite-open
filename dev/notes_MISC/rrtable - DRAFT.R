#' rrtable - NOT USED/ DRAFT 
#' Calculate risk ratios of avg E in group D / in everyone not in group D
#' @param x blockgroupstats
#'
#' @return table of ratios and max per row and max per col
#'
rrtable <- function(x) {
  x <- as.data.frame(x)
  if (mean(colMeans(  x[ , names_d], na.rm = TRUE)) > 1) {
    x[ , names_d] <- x[ , names_d] / 100
  }
  if (mean(colMeans(  x[ , names_d_subgroups_nh], na.rm = TRUE)) > 1) {
    x[ , names_d_subgroups_nh] <- x[ , names_d_subgroups_nh] / 100
  }
  
  
  rt <- RR(  # THIS RR( )  FUNCTION IS FROM THE PACKAGE CALLED ejanalysis
    e = x[ , names_e],  
    d = x[ , c(names_d, names_d_subgroups_nh)], 
    pop = x$pop)
  
  rt <- addmargins(rt, FUN = max) ## ??? did not try it
  
  round(cbind(
    bbb <- rbind(
      rt, 
      MAX = colMaxs2(rt)  # from this pkg, or 
      # MAX = colMaxs(rt) # from matrixStats pkg, or
      # MAX = do.call(pmax, rt) # if rt is a list, it does max for each set in list of sets## ??? did not try it
      ),
      
      MAX =  rowMaxs2(bbb)  # from this pkg,
      # trying to reduce depending on matrixStats package etc., and rrtable is not critical currently anyway
    # MAX = apply(bbb, 1, FUN = pmax  )   ## ??? did not try it
      # MAX = collapse  package could do fmax() per row and/or column   # 
    
    ), 3)
  
}
