#' ratiostats - not used - depends on ejanalysis package
#' Comparison of Indicator scores for Average person in each Demog Group
#' @param results_bybg_people data.table from  doaggregate()[,"results_bybg_people"]
#' @param pts e.g.,  pts <-  frs[sample(1:nrow( frs), 1e3),]
#' @param dig digits to round to
ratiostats <- function(results_bybg_people, pts, dig=2 ) {
  
  # Comparison of Indicator scores for Average person in each Demog Group
  
  stinfo <- data.frame(siteid=1:nrow(pts),  EJAM::state_from_latlon(lat=pts$lat, lon=pts$lon))
  # x <- getblocksnearby(pts, radius = 3.1, quadtree = localtree)
  # y <- doaggregate(x, sites2states = stinfo)
  # z <- y$results_bybg_people
  z <- results_bybg_people # a data.table from  doaggregate()[,"results_bybg_people"]
  
  vars <- c("proximityscore", "sitecount_max", "sitecount_near_bg", names_e)
  vars2 <- c("distance_avg", "distance_min")
  vars <- c(vars, vars2)
  
  # other functions that do similar things
  # setDF(z)
  # r2 <- RR.table(z, vars, c(names_d, names_d_subgroups))
  # r2 <- RR(e=z[ , ..vars], d= z[ ,  c(..names_d, ..names_d_subgroups)] , pop=z$pop)
  #
  warning(
    "this depended on a package not on CRAN and is not used anyway"
  )
  # r2 <- ejanalysis::RR.means(e=z[ , ..vars], d= z[ ,  c(..names_d, ..names_d_subgroups)] , pop=z$pop) 
  
  r2 <- r2[,"ratio",]
  # essentially for each group it is just =  colSums(x * groupwts) / colSums(groupwts) with na.rm=T
   #  divided by the same formula for everyone else
 
  
  r2 <- addmargins(r2, FUN=list(max = function(x) 1.0001 * max(x)) ) # to help sort max row or col 1st before actual max
  r2 <- r2[order(r2[,"max"],decreasing = T), order(r2["max",],decreasing = T)]
  r2 <- round(r2,dig)
  return(r2)
  
  
  # plot()
  
  # library(EJAM)
  
  # library(EJAMblockdata
  #         
  # pts <-  frs[sample(1:nrow( frs), 1e3),]
  
  
  # > dimnames(r2) when 1st created:
  # $d
  # [1] "VSI.eo"          "pctlowinc"       "pctmin"          "pctlths"         "pctlingiso"      "pctunder5"      
  # [7] "pctover64"       "pctunemployed"   "pctnhwa"         "pcthisp"         "pctnhba"         "pctnhaa"        
  # [13] "pctnhaiana"      "pctnhnhpia"      "pctnhotheralone" "pctnhmulti"     
  # 
  # $group.or.not
  # [1] "group" "not"   "ratio"
  # 
  # $e
  # [1] "proximityscore"    "distance_avg"      "distance_min"  "sitecount_max"     "sitecount_near_bg"
  # [6] "pm"                "o3"                "cancer"            "resp"              "dpm"              
  # [11] "pctpre1960"        "traffic.score"     "proximity.npl"     "proximity.rmp"     "proximity.tsdf"   
  # [16] "proximity.npdes"   "ust"              
}



