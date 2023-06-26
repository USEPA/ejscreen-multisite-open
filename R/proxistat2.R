#' Calculate a proximity score for every blockgroup 
#' Indicator of proximity of each blockgroups to some set of facilities or sites.
#' @details  Proximity score is sum of (1/d) where each d is distance of a given site in km, 
#'   summed over all sites within 5km, as in EJScreen.
#'   
#'   doaggregate() has a bit of code in it to do this same thing that proxistat2() does.
#'   
#'   *** Still need area of each block to fix this func proxistat2()
#'  
#' @param pts data.table of lat lon
#' @param radius distance max, in miles, default is 5km (8.04672 miles)
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
#'  # # analyze.stuff   pctiles(x$proximityscore)
#'  # plot(x$lon, x$lat)
#'  # tops = x$proximityscore > 500 & !is.infinite(x$proximityscore) & !is.na(x$proximityscore)
#'  # points(x$lon[tops], x$lat[tops], col="red")
#'  
proxistat2 <- function(pts, radius=8.04672, quadtree) {
  stop("this does not work without proxistat package dataset ")
  warning("temporarily uses block areas from another dataset for most but not all blocks")
  warning("if none found within radius of 5km, this func does not yet create score based on single nearest - see source code for notes")
  ######################################## #
  # Sequence of steps in finding d value(s):
  ######################################## #
  #
  # 1) get distances that are <=radius using get.distances()
  # 2) where d < min.dist, set d <- min.dist to adjust it upwards
  # 3)     and for those, check again to see if new d is still <= radius. keep only if d<=radius now. *** 
  # 4) for each frompoints, if no distances were found, get nearest single d at any radius,
  #       originally thought perhaps by expanding outwards step by step until at least one is found (but not worth the overhead vs just finding ALL d and picking min)

  # steps 3 and 4 had not yet been implemented as of 1/29/23.  
  
  # 8.04672 miles is 5 km which is the EJScreen max search range for proximity scores
  # # FACILITY DENSITY INDICATOR 
  #  AS PROXIMITY SCORE FOR SITES IN FRS
  
  # EFFICIENCY QUESTION: 
  #  Obvious algorithm is to 
  #  STEP 1: loop through all 8 million US blocks, and for each block count all nearby facilities (sites),
  #   but vast majority of blocks will have zero sites nearby, so 
  #   STEP 2: for every block with zero sites nearby, expand search somehow until finding nearest 1 site. HOW?
  
  #  Maybe another approach to check is 
  # STEP 1: loop through just the sites (is it faster than step 1 above?), and for each site find all nearby blocks, maybe 1k each, say. 
  #   then do STEP 2 as above.
  
  sites2blocks_dt <- getblocksnearby(sitepoints = pts, radius = radius, quadtree = quadtree)
  
  # THE VAST MAJORITY OF BLOCKS WILL HAVE ZERO WITHIN THE 5 KM RADIUS, SO NEAREST 1 IS BASIS FOR THEIR SCORE, BUT
  #   SOME WILL EVEN HAVE ZERO WITHIN THE MAX RADIUS TO CHECK
  # if none found within radius of 5km, this func does not yet create score based on single nearest

  
  
  #     TO BE ADDED HERE 
  
  
  
  #  ADJUST DISTANCE USING A MINIMUM DISTANCE ####
  # BASED ON SIZE (AREA) OF BLOCK, as in EJScreen proximity scores
  # proxistat  package  blockpoints_area_pop  has area in square meters, not yet added to    blockpoints ?
  # > dim( blockpoints)
  # [1] 8174955       3
  # > dim(blockpoints_area_pop)
  # [1] 8132968       6
  # miles_per_km <- EJAMejscreenapi::meters_per_mile / 1000
  km_a_mile = 1.609344
  # distance in miles needs min.dist in square miles; if distance in meters, square meters.
  # We should precalculate effective radius of each block, 
  #    blockpoints[ , effectiveradius := sqrt(area/pi)]
  # effectiveradius might be a better name for min.dist but code here used min.dist
  # min.dist <- 0.9 * sqrt( area / pi )
  # min.dist <- (0.9 / sqrt(pi)) * sqrt(area)
  # (0.9 / sqrt(pi)) = 0.5077706  # so, min.dist := 0.5077706 * sqrt(area)
  
  # this dataset called blockpoints_area_pop  was in the proxistat package:
  sites2blocks_dt <- blockpoints_area_pop[sites2blocks_dt, .(blockid, distance, siteid, area), on="blockid"]
  
  # area was in square meters, so convert   1609.344 meters per mile 
  sites2blocks_dt[ , min.dist.km := 0.0005077706 * sqrt(area)]
  sites2blocks_dt[ , distance.km := pmax(min.dist.meters, distance * km_a_mile, na.rm = TRUE)] # collapse::fmin() is probably much faster
  
  
  
  
  # create score per block = sum of sites wtd by 1/d ####
  blockscores <- sites2blocks_dt[ , sum(1 / distance.km, na.rm = TRUE), by=blockid] # result is data.table with blockid, V1
   # blockscores[is.infinite(V1), V1 := 999]
  x <- data.table::merge.data.table(blockwts, blockscores, by="blockid", all.x = FALSE, all.y = TRUE)
  
  # create score per block group = popwtd mean of block scores ####
  bgscore <- x[, sum(V1 * blockwt, na.rm=TRUE)/sum(blockwt, na.rm=TRUE), by=bgid]
  setnames(bgscore, 'V1', "proximityscore")
  bgscore = merge(bgscore, bgpts, by = "bgid", all.x = TRUE, all.y = FALSE)
  
  cat("proximity score is sum of (1/d) where each d is distance of a given site in km, summed over all sites within 5km \n")
  return(bgscore)
}
