#' Calculate a proximity score for every blockgroup 
#' Indicator of proximity of each blockgroups to some set of facilities or sites.
#' Proximity score is sum of (1/d) where each d is distance of a given site in km, 
#'   summed over all sites within 5km, as in EJScreen.
#'   
#'   *** Still need area of each block to fix this func proxistat2()
#'  
#' @param pts data.table of lat lon
#' @param cutoff distance max, in miles, default is 5km (8.04672 miles)
#'   which is the EJScreen max search range for proximity scores
#' @param quadtree must be localtree from EJAM:: 
#' @return data.table with proximityscore, bgfips, lat, lon, etc.
#' @import data.table
#' @export
#'
#' @examples 
#'  # pts <- testpoints_50
#'  # x <- proxistat2(pts = pts[1:1000,], quadtree = localtree) 
#'  #
#'  # summary(x$proximityscore)
#'  # # analyze.stuff::pctiles(x$proximityscore)
#'  # plot(x$lon, x$lat)
#'  # tops = x$proximityscore > 500 & !is.infinite(x$proximityscore) & !is.na(x$proximityscore)
#'  # points(x$lon[tops], x$lat[tops], col="red")
#'  
proxistat2 <- function(pts, cutoff=8.04672, quadtree) {
  
  warning("temporarily uses block areas from another dataset for most but not all blocks")
  warning("if none found within cutoff of 5km, this func does not yet create score based on single nearest")
  
  
  # 8.04672 miles is 5 km which is the EJScreen max search range for proximity scores
  # # FACILITY DENSITY INDICATOR 
  #  AS PROXIMITY SCORE FOR SITES IN FRS
  
  sites2blocks_dt <- getblocksnearby(sitepoints = pts, cutoff = cutoff, quadtree = quadtree)
  
  # if none found within cutoff of 5km, this func does not yet create score based on single nearest
  
  
  #     TO BE ADDED HERE 
  
  
  #  ADJUST DISTANCE USING A MINIMUM DISTANCE ####
  # BASED ON SIZE (AREA) OF BLOCK, as in EJScreen proximity scores
  # proxistat::blockpoints_area_pop  has area in square meters, not yet added to EJAMblockdata::blockpoints
  # > dim(EJAMblockdata::blockpoints)
  # [1] 8174955       3
  # > dim(blockpoints_area_pop)
  # [1] 8132968       6
  # miles_per_km <- EJAMejscreenapi::meters_per_mile / 1000
  km_a_mile = 1.609344
  # distance in miles needs min.dist in square miles; if distance in meters, square meters.
  # min.dist <- 0.9 * sqrt( area / pi )
  # min.dist <- (0.9 / sqrt(pi)) * sqrt(area)
  # (0.9 / sqrt(pi)) = 0.5077706  # so, min.dist := 0.5077706 * sqrt(area)
  sites2blocks_dt <- proxistat::blockpoints_area_pop[sites2blocks_dt, .(blockid, distance, siteid, area), on="blockid"]
  # area was in square meters, so convert   1609.344 meters per mile 
  sites2blocks_dt[ , min.dist.km := 0.0005077706 * sqrt(area)]
  sites2blocks_dt[ , distance.km := pmax(min.dist.meters, distance * km_a_mile, na.rm = TRUE)]
  
  # create score per block ####
  blockscores <- sites2blocks_dt[ , sum(1 / distance.km, na.rm = TRUE), by=blockid] # result is data.table with blockid, V1
   # blockscores[is.infinite(V1), V1 := 999]
  x <- data.table::merge.data.table(blockwts, blockscores, by="blockid", all.x = FALSE, all.y = TRUE)
  
  # create score per block group ####
  bgscore <- x[, sum(V1 * blockwt, na.rm=TRUE)/sum(blockwt, na.rm=TRUE), by=bgid]
  setnames(bgscore, 'V1', "proximityscore")
  bgscore = merge(bgscore, bgpts, by = "bgid", all.x = TRUE, all.y = FALSE)
  
  cat("proximity score is sum of (1/d) where each d is distance of a given site in km, summed over all sites within 5km \n")
  return(bgscore)
}
