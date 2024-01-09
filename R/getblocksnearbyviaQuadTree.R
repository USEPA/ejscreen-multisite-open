#' getblocksnearbyviaQuadTree - Fast way to find nearby points (distance to each Census block centroid near each site)
#'
#' @description Given a set of points and a specified radius in miles, 
#'   this function quickly finds all the US Census blocks near each point.
#' @details  
#'   For each point, it uses the specified search radius and finds the distance to 
#'   every block within the circle defined by the radius. 
#'   Each block is defined by its Census-provided internal point, by latitude and longitude.
#'   
#'   Results are the sites2blocks table that would be used by doaggregate(), 
#'   with distance in miles as one output column of data.table.
#'   Adjusts distance to avg resident in block when it is very small relative to block size,
#'   the same way EJScreen adjusts distances in creating proximity scores.
#'   
#'   Each point can be the location of a regulated facility or other type of site, and 
#'   the blocks are a high-resolution source of information about where
#'   residents live. 
#'   
#'   Finding which blocks have their internal points in a circle provides 
#'   a way to quickly estimate what fraction of a block group is 
#'   inside the circular buffer more accurately and more quickly than
#'   areal apportionment of block groups would provide. 
#'   
#' @param sitepoints data.table with columns siteid, lat, lon giving point locations of sites or facilities around which are circular buffers
#' @param radius in miles, defining circular buffer around a site point 
#' @param maxradius miles distance (max distance to check if not even 1 block point is within radius)
#' @param avoidorphans logical If TRUE, then where not even 1 BLOCK internal point is within radius of a SITE, 
#'   it keeps looking past radius, up to maxradius, to find nearest 1 BLOCK. 
#'   What EJScreen does in that case is report NA, right? So, 
#'   does EJAM really need to report stats on residents presumed to be within radius,
#'    if no block centroid is within radius? 
#'   Best estimate might be to report indicators from nearest block centroid which is 
#'   probably almost always the one your site is sitting inside of,
#'   but ideally would adjust total count to be a fraction of blockwt based on 
#'   what is area of circular buffer as fraction of area of block it is apparently inside of.
#'   Setting this to TRUE can produce unexpected results, which will not match EJScreen numbers.
#'   
#'   Note that if creating a proximity score, by contrast, you instead want to find nearest 1 SITE if none within radius of this BLOCK.
#' @param quadtree (a pointer to the large quadtree object) 
#'    created using indexblocks() which uses the SearchTree package.
#'    Takes about 2-5 seconds to create this each time it is needed.
#'    It can be automatically created when the package is attached via the .onAttach() function
#' @param report_progress_every_n Reports progress to console after every n points,
#'   mostly for testing, but a progress bar feature might be useful unless this is super fast.
#' @param quiet Optional. set to TRUE to avoid message about using getblock_diagnostics(),
#'   which is relevant only if a user saved the output of this function.
#' @param retain_unadjusted_distance set to FALSE to drop it and save memory/storage. If TRUE, 
#'   the distance_unadjusted column will save the actual distance of site to block internal point
#'   -- the distance column always represents distance to average resident in the block, which is
#'   estimated by adjusting the site to block distance in cases where it is small relative to the 
#'   size of the block, to put a lower limit on it, which can result in a large estimate of distance
#'   if the block is very large. See EJScreen documentation.
#' @examples 
#'   # indexblocks() # if localtree not available yet, quadtree = localtree
#'   x = getblocksnearby(testpoints_1000, radius = 3)
#' @seealso [ejamit()] [getblocksnearby()] 
#' @export
#' @import data.table
#' @importFrom pdist "pdist"
#'   
getblocksnearbyviaQuadTree  <- function(sitepoints, radius = 3, maxradius = 31.07, avoidorphans = FALSE, 
                                        report_progress_every_n = 500, quiet = FALSE, retain_unadjusted_distance = TRUE,
                                        quadtree) {
  # indexgridsize was defined at start as say 10 miles in global? could be passed here as a parameter ####
  # and buffer_indexdistance defined here in code but is never used anywhere...  
  # buffer_indexdistance <- ceiling(radius / indexgridsize)
  
  if (class(quadtree) != "QuadTree") {
    stop('quadtree must be an object created with indexblocks(), from SearchTrees package with treeType = "quad" and dataType = "point"')  
  }
  if (!data.table::is.data.table(sitepoints)) {data.table::setDT(sitepoints)} # should we set a key or index here, like ? ***
  
  ejam_uniq_id_as_submitted_to_getblocks <- NULL
  if (!('ejam_uniq_id' %in% names(sitepoints))) {
    sitepoints$ejam_uniq_id <- seq.int(length.out = NROW(sitepoints))
  } else {
    if (!identical(sitepoints$ejam_uniq_id, seq.int(length.out = NROW(sitepoints)))) {
    warning('ejam_uniq_id was already a column in sitepoints, but was not the same as the rownum, which would cause problems for other code assuming it is 1 through N, 
            so ejam_uniq_id_as_submitted_to_getblocks now contains the original values and ejam_uniq_id was changed to 1:NROW(sitepoints)')
    ejam_uniq_id_as_submitted_to_getblocks <- sitepoints$ejam_uniq_id
    sitepoints[ , ejam_uniq_id := .I] # .I just means  1:NROW(sitepoints)
    }
  }
  
  # pass in a list of uniques and the surface radius distance
  
  # filter na values? or keep length of out same as input? ####
  # sitepoints <- sitepoints[!is.na(sitepoints$lat) & !is.na(sitepoints$lon), ] # perhaps could do this by reference to avoid making a copy
  
  # compute and add grid info ####
  earthRadius_miles <- 3959 # in case it is not already in global envt
  radians_per_degree <- pi / 180
  
  sitepoints <- data.table::copy(sitepoints) # make a copy?
  sitepoints[ , lat_RAD := lat * radians_per_degree]   # PROBLEM? - this alters sitepoints in the calling envt bc of how data.table works
  sitepoints[ , lon_RAD := lon * radians_per_degree]
  cos_lat <- cos(sitepoints[ , lat_RAD])    # or maybe # sitepoints[ , cos_lat := cos(lat_RAD)]
  sitepoints[ , FAC_X := earthRadius_miles * cos_lat * cos(lon_RAD)]
  sitepoints[ , FAC_Y := earthRadius_miles * cos_lat * sin(lon_RAD)]
  sitepoints[ , FAC_Z := earthRadius_miles *           sin(lat_RAD)]
  
  truedistance <- distance_via_surfacedistance(radius)   # simply 7918*sin(radius/7918) which is nearly identical to unadjusted distances, like 9.999997 vs. 10.000000 miles ! even 29.999928 vs 30 miles
  
  # **** getblocksnearbyviaQuadTree2.R is different here
  
  
  
  
  
  
  
  #---- Get ready for loop here ----
  
  # allocate memory for result list
  nRowsDf <- NROW(sitepoints)
  res <- vector('list', nRowsDf)  # list of data.tables   cols will be blockid, distance, siteid    
  
  if (!quiet) {
    cat("Finding Census blocks with internal point within ", radius," miles of the site (point), for each of", nRowsDf," sites (points)...\n")
  }
  
  ######################################################################################################################## # 
  ######################################################################################################################## # 
  #
  ### # LOOP OVER SITES HERE - can some be done faster via data.table ?? ***  ----
  
  for (i in 1:nRowsDf) {
    
    ########################################################################### ## ** SLOW STEPS TO OPTIMIZE   *** ** *** *** 
    
    coords <- sitepoints[i, .(FAC_X, FAC_Z)]  #           ** SLOW STEP TO OPTIMIZE - makes a copy of each row of sitepoints (the similar clustered function uses sitepoints2use not sitepoints)
    x_low  <- coords[,FAC_X] - truedistance   #            EXTREMELY SLOW LINE - see version 2 of this whole function
    x_hi   <- coords[,FAC_X] + truedistance
    z_low  <- coords[,FAC_Z] - truedistance
    # z_hi  <-  coords[,FAC_Z] + truedistance   #          ** THIS HAD BEEN THE SLOWEST LINE  OVERALL ***
    
    #### USE quadtree INDEX OF USA BLOCKS TO FIND BLOCKS NEAR THIS 1 SITE, and store blockids as vector in vec, and .(BLOCK_X , BLOCK_Z  , BLOCK_Y, blockid)  as "tmp"
    # find vector of the hundreds of block ids that are approximately near this site? (based on bounding box?)
    vec <- SearchTrees::rectLookup(
      tree = quadtree, 
      ptOne = unlist(c(x_low, z_low)), 
      ptTwo = unlist(c(x_hi, coords[,FAC_Z] + truedistance))
    ) 
    
    # mapfast(blockpoints[blockid %in% vec, .(lat,lon)], radius = 0.01)
    
    ### try this chunk out? and then  after the loop do this: sites2blocks <- sites2blocks[distance <= truedistance, ] 
    # {
    # res[[i]]  <-  quaddata[vec, ]  # all the blocks near this 1 site.
    # ###   ** SLOW STEP TO OPTIMIZE  #  1 OF SLOWEST LINES   *** *** ***
    # res[[i]][ , `:=`( distance = (pdist::pdist(res[[i]][ , .(BLOCK_X, BLOCK_Y, BLOCK_Z)], sitepoints[i, c('FAC_X', 'FAC_Y', 'FAC_Z')]))@dist,
    #                   BLOCK_X = NULL, BLOCK_Z = NULL, BLOCK_Y = NULL)]
    # ## pdist computes a n by p distance matrix using two separate matrices
    # res[[i]][ , siteid := sitepoints[i, .(siteid)]] 
    # }
    
    ## instead of  
    
    #{ 
    tmp <-  quaddata[vec, ]  # all the blocks near this 1 site.
    
    ########################################################################### ## ** SLOWSTEP TO OPTIMIZE: 
    
    distances <- as.matrix(pdist::pdist(
      tmp[ , .(BLOCK_X, BLOCK_Y, BLOCK_Z)], 
      sitepoints[i, c('FAC_X','FAC_Y','FAC_Z')])
    )   
    
    
    
    
    
    # distances is now just a 1 column data.table of hundreds of distance values. Some may be 5.08 miles even though specified radius of 3 miles even though distance to corner of bounding box should be 1.4142*r= 4.2426, not 5 ? 
    # pdist computes a n by p distance matrix using two separate matrices
    
    # add the distances and siteid to the table of nearby blocks
    tmp[ , distance := distances[ , c(1)]]      # converts distances dt into a vector that becomes a column of tmp
    #tmp[ , siteid := sitepoints[i, .(siteid)]]  # the similar clustered function differs, why?
    tmp[, ejam_uniq_id := sitepoints[i, .(ejam_uniq_id)]]
    #### LIMIT RESULTS SO FAR TO THE RADIUS REQUESTED  
    
    #filter actual distance, exclude blocks that are roughly nearby (according to index and bounding boxes) but are just beyond the radius you specified
    # e.g., 805 blocks roughly nearby, but only 457 truly within radius.
    
    #res[[i]] <- tmp[distance <= truedistance, .(blockid, distance, siteid)]  #   *** SLOW STEP TO OPTIMIZE  #  1 OF SLOWEST LINES *** - cant we do this outside the loop just once??
    res[[i]] <- tmp[distance <= truedistance, .(blockid, distance, ejam_uniq_id)]
    #
    # }
    
    # tmp
    #      BLOCK_X  BLOCK_Z   BLOCK_Y blockid  distance siteid
    # 1: -198.8586 1985.476 -3419.360 3041513 0.4734989      1
    # 2: -199.8715 1984.961 -3419.600 3041514 0.6885808      1
    
    ################################# #
    #
    #### If avoidorphans TRUE, and no blockpt within radius of site, look past radius to maxradius   ############## # 
    #
    # But note looking past radius is NOT how EJScreen works, for buffer reports - it just fails to provide any result if no blockpoint is inside circle. (For proximity scores, which are different than circular buffer reports, EJScreen does look beyond radius, but not for circular zone report). Also, you would rarely get here even if avoidorphans set TRUE.
    # cat('about to check avoidorphans\n')
    if ( 1 == 0 ) { 
      # if ( avoidorphans && (NROW(res[[i]])  == 0)) {
      if (!quiet) {cat("avoidorphans is TRUE, so avoiding reporting zero blocks nearby at site ", i, " by searching past radius of ", radius, " to maxradius of ", maxradius, "\n")}
      #search neighbors, allow for multiple at equal distance
      
      vec  <- SearchTrees::knnLookup(
        quadtree,
        unlist(c(coords[ , 'FAC_X'])), 
        unlist(c(coords[ , 'FAC_Z'])),
        k = 10   # why 10?
      )
      
      tmp <-  quaddata[vec[1, ], ]  # the first distance in the new vector of distances? is that the shortest?
      
      x <- tmp[, .(BLOCK_X, BLOCK_Y, BLOCK_Z)]
      y <- sitepoints[i, .(FAC_X, FAC_Y, FAC_Z)]
      distances <- as.matrix(pdist::pdist(x, y))
      
      tmp[ , distance := distances[ , c(1)]]
      #tmp[ , siteid := sitepoints[i, .(siteid)]]
      tmp[ , siteid := sitepoints[i, .(ejam_uniq_id)]]
      # keep only the 1 block that is closest to this site (that is > radius but < maxradius) -- NEED TO CONFIRM/TEST THIS !!
      truemaxdistance <- distance_via_surfacedistance(maxradius)
      data.table::setorder(tmp, distance) # ascending order short to long distance
      #res[[i]] <- tmp[distance <= truemaxdistance, .(blockid, distance, siteid)]   
      res[[i]] <- tmp[distance <= truemaxdistance, .(blockid, distance, ejam_uniq_id)]   
      
    }  
    ### end of if avoidorphans
    ################################# #
    
    if (((i %% report_progress_every_n) == 0) & interactive()) {cat(paste("Finished finding blocks near ",i ," of ", nRowsDf),"\n" ) }   # i %% report_progress_every_n indicates i mod report_progress_every_n (“i modulo report_progress_every_n”) 
    
  } # do next site in loop, etc., until end of this loop.
  # end loop over sites ################################################################################################ # 
  ###################################################################################################################### # 
  ###################################################################################################################### # 
  
  sites2blocks <- data.table::rbindlist(res)
  #data.table::setkey(sites2blocks, blockid, siteid, distance)
  data.table::setkey(sites2blocks, blockid, ejam_uniq_id, distance)
  
  ########################################################################### ## 
  if (!quiet) {
    cat('Stats via getblocks_diagnostics(), but BEFORE ADJUSTING FOR SHORT DISTANCES: \n')
    cat("min distance before adjustment: ", min(sites2blocks$distance, na.rm = TRUE), "\n")
    cat("max distance before adjustment: ", max(sites2blocks$distance, na.rm = TRUE), "\n\n")
    getblocks_diagnostics(sites2blocks) # returns NA if no blocks nearby
    cat("\n\nAdjusting for very short distances now...\n ")
  }
  
  # ADJUST THE VERY SHORT DISTANCES ####
  
    # distance gets adjusted to be the minimum possible value,  0.9 * effective radius of block_radius_miles (see EJScreen Technical Documentation discussion of proximity analysis for rationale)
  #
  # use block_radius_miles here, to correct the distances that are small relative to a block size.
  # This adjusts distance the way EJScreen does for proximity scores - so distance reflects distance of sitepoint to avg resident in block
  # (rather than sitepoint's distance to the block internal point),
  # including e.g., where distance to block internal point is so small the site is inside the block.
  # This also avoids infinitely small or zero distances.
  # 2 ways considered to do join here - may be able to optimize.
  # a) try to do join that updates sites2blocks by reference - not sure it works this way, but goal was to make join faster:
  # sites2blocks[blockwts, .(siteid,blockid,distance,blockwt,bgid, block_radius_miles), on = 'blockid']

  # b) try to do join that updates sites2blocks by making a copy?  

  if (retain_unadjusted_distance) {
    sites2blocks[ , distance_unadjusted := distance] # wastes space but for development/ debugging probably useful
    sites2blocks <-  blockwts[sites2blocks, .(ejam_uniq_id, blockid, distance, blockwt, bgid, block_radius_miles, distance_unadjusted), on = 'blockid'] 
  } else {
    sites2blocks <-  blockwts[sites2blocks, .(ejam_uniq_id, blockid, distance, blockwt, bgid, block_radius_miles), on = 'blockid'] 
  }

  # 2 ways considered here for how exactly to make the adjustment: 
  sites2blocks[distance < block_radius_miles, distance := 0.9 * block_radius_miles]  # assumes distance is in miles
  # or a more continuous but slower (and nonEJScreen way?) adjustment for when dist is between 0.9 and 1.0 times block_radius_miles: 
  # sites2blocks_dt[ , distance  := pmax(block_radius_miles, distance, na.rm = TRUE)] # assumes distance is in miles
  
  # drop that info about area or size of block to save memory. do not need it later in sites2blocks
  sites2blocks[ , block_radius_miles := NULL]
  
  if (!quiet) {
    cat('Stats via getblocks_diagnostics(), AFTER ADJUSTING FOR SHORT DISTANCES: \n')
    cat("min distance AFTER adjustment: ", min(sites2blocks$distance, na.rm = TRUE), "\n")
    cat("max distance AFTER adjustment: ", max(sites2blocks$distance, na.rm = TRUE), "\n\n")
    # getblocks_diagnostics(sites2blocks)  
   cat("\n")
  }
  ########################################################################### ## 
  
  ### and with above idea, cant we subset to keep only distance <=  radius here, instead of inside the loop ? Or do it even later, after adjusting short distances? What would make sense to report as distance to avg resident if the effective radius happends to be > radius specified, as with small radius circle in rural huge block? 
  # sites2blocks <- sites2blocks[distance <= truedistance, ] 
  
  
  # if (interactive() & !quiet) { 
  #   cat("You can use  getblocks_diagnostics(sites2blocks)  to see this info on distances found:\n\n")
  #   getblocks_diagnostics(sites2blocks)
  # }

  if (!is.null(ejam_uniq_id_as_submitted_to_getblocks)) {
    # disabled until fixed:  need to join or merge so 1 site matches many block-site pairs here: ***
    # sites2blocks$ejam_uniq_id_as_submitted_to_getblocks <- ejam_uniq_id_as_submitted_to_getblocks
  }
  
  return(sites2blocks)
}
