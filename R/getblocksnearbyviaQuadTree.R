#' Find nearby blocks using Quad Tree data structure for speed, NO PARALLEL PROCESSING
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
#'   
#'   Note that if creating a proximity score, by contrast, you instead want to find nearest 1 SITE if none within radius of this BLOCK.
#' @param quadtree (a pointer to the large quadtree object) 
#'    created from the SearchTree package example:
#'    SearchTrees::createTree( quaddata, treeType = "quad", dataType = "point")
#'    Takes about 2-5 seconds to create this each time it is needed.
#'    It can be automatically created when the package is attached via the .onAttach() function
#' @param report_progress_every_n Reports progress to console after every n points,
#'   mostly for testing, but a progress bar feature might be useful unless this is super fast.
#'   
#'   
#'   
#' @seealso [ejamit()] [getblocksnearby()] [getblocksnearbyviaQuadTree()] [getblocksnearbyviaQuadTree_Clustered()] [getblocksnearbyviaQuadTree2()]
#' 
#' @export
#' @import data.table
#' @importFrom pdist "pdist"
#'   
getblocksnearbyviaQuadTree  <- function(sitepoints, radius=3, maxradius=31.07, 
                                        avoidorphans=TRUE, report_progress_every_n=500, 
                                        quadtree) {
  if(class(quadtree) != "QuadTree"){
    stop('quadtree must be an object created with indexblocks(), from SearchTrees package with treeType = "quad" and dataType = "point"')  
  }
  if (!data.table::is.data.table(sitepoints)) {data.table::setDT(sitepoints)}
  
  if (!('siteid' %in% names(sitepoints))) {sitepoints$siteid <- seq.int(length.out = NROW(sitepoints))}
  
  #pass in a list of uniques and the surface radius distance
  
  #filter na values? or keep length of out same as input? ####
  # sitepoints <- sitepoints[!is.na(sitepoints$lat) & !is.na(sitepoints$lon), ] # perhaps could do this by reference to avoid making a copy
  
  #compute and add grid info ####
  earthRadius_miles <- 3959 # in case it is not already in global envt
  radians_per_degree <- pi / 180
  
  # f2 <- data.table::copy(sitepoints) # make a copy?
  sitepoints[ , lat_RAD := lat * radians_per_degree]   # PROBLEM? - this alters sitepoints in the calling envt bc of how data.table works
  sitepoints[ , lon_RAD := lon * radians_per_degree]
  cos_lat <- cos(sitepoints[ , lat_RAD])    # or maybe # sitepoints[ , cos_lat := cos(lat_RAD)]
  sitepoints[ , FAC_X := earthRadius_miles * cos_lat * cos(lon_RAD)]
  sitepoints[ , FAC_Y := earthRadius_miles * cos_lat * sin(lon_RAD)]
  sitepoints[ , FAC_Z := earthRadius_miles *           sin(lat_RAD)]
  
  # indexgridsize was defined at start as say 10 miles in global? could be passed here as a parameter ####
  # and buffer_indexdistance defined here in code but is never used anywhere...  
  # buffer_indexdistance <- ceiling(radius / indexgridsize) 
  truedistance <- distance_via_surfacedistance(radius)   # simply 7918*sin(radius/7918) 
  
  # main reason for using foreach::foreach() is that it supports parallel execution,
  # that is, it can execute those repeated operations on multiple processors/cores on your computer
  # (and there are other advantages as well)
  
  #---- Get ready for loop here ----
  
  # allocate memory for result list
  nRowsDf <- NROW(sitepoints)
  res <- vector('list', nRowsDf)  # list of data.tables   cols will be blockid, distance, siteid    
  
  # **** getblocksnearbyviaQuadTree2.R is different here
  
  
  
  
  cat("Finding nearby Census blocks...\n")
  
  for (i in 1:nRowsDf) {    # LOOP OVER SITES HERE - can some or all of this be vectorized and done faster via data.table ??  ----
    ########################################################################### ## ** SLOW STEP TO OPTIMIZE   *** ** ** ** 
    coords <- sitepoints[i, .(FAC_X, FAC_Z)]  # ** SLOW STEP TO OPTIMIZE  (the similar clustered function uses sitepoints2use not sitepoints)
    x_low  <- coords[,FAC_X]-truedistance;  #  EXTREMELY SLOW LINE
    x_hi  <-  coords[,FAC_X]+truedistance
    z_low  <- coords[,FAC_Z]-truedistance;
    # z_hi  <-  coords[,FAC_Z]+truedistance   # ** THIS HAD BEEN THE SLOWEST LINE  OVERALL ***
    
    # why not at least try  
    
    
    
    
  
    # USE THE INDEX OF ALL BLOCKS TO FIND BLOCKS NEAR THIS 1 SITE  AND STORE THEM AS tmp
    
    vec <- SearchTrees::rectLookup(
      quadtree, # quadtree or localtree is the index that enables fast lookup within quaddata (all US blocks)
      unlist(c(x_low, z_low  )),         
      unlist(c(x_hi, coords[,FAC_Z]+truedistance))) # x and z things are now vectorized
    # *** FIX/CHECK: 
    
    tmp <-  quaddata[vec, ]  # all the blocks near this 1 site.
    # x <- tmp[ , .(BLOCK_X, BLOCK_Y, BLOCK_Z)] # but not blockid ?? 
    # y <- sitepoints[i, c('FAC_X','FAC_Y','FAC_Z')]  # the similar clustered function uses something other than sitepoints here - why?
    ########################################################################### ## ** SLOW STEP TO OPTIMIZE 
    distances <- as.matrix(pdist::pdist(tmp[ , .(BLOCK_X, BLOCK_Y, BLOCK_Z)] , 
                                        sitepoints[i, c('FAC_X','FAC_Y','FAC_Z')] ))  # ** SLOW STEP TO OPTIMIZE  #  1 OF SLOWEST LINES  
    
    # pdist computes a n by p distance matrix using two separate matrices
    
    #clean up fields
    tmp[ , distance := distances[ , c(1)]]
    tmp[ , siteid := sitepoints[i, .(siteid)]]  # the similar clustered function differs, why?
    
    #filter actual distance
    ########################################################################### ## ** SLOW STEP TO OPTIMIZE 
    res[[i]] <- tmp[distance <= truedistance, .(blockid, distance, siteid)]  # ** SLOW STEP TO OPTIMIZE  #  1 OF SLOWEST LINES  - cant we do this outside the loop just once??
    
    # hold your horses, what if there are no blocks and you are supposed to avoid that - only relevant if creating a proximity score, not if you only want blocks truly within radius of this 1 site. 
    if ( avoidorphans && (nrow(res[[i]])) == 0) { # rarely get here so not critical to optimize
      #search neighbors, allow for multiple at equal distance
      
      vec  <- SearchTrees::knnLookup(quadtree, unlist(c(coords[ , 'FAC_X'])), unlist(c(coords[ , 'FAC_Z'])), k=10)  
      
      # *** FIX/CHECK: 
      tmp <-  quaddata[vec[1, ], ]
      
      x <- tmp[, .(BLOCK_X, BLOCK_Y, BLOCK_Z)]
      y <- sitepoints[i, .(FAC_X, FAC_Y, FAC_Z)]
      distances <- as.matrix(pdist::pdist(x, y))
      
      #clean up fields
      tmp[ , distance := distances[ , c(1)]]
      tmp[ , siteid := sitepoints[i, .(siteid)]]
      
      #filter to max distance
      truemaxdistance <- distance_via_surfacedistance(maxradius)
      res[[i]] <- tmp[distance <= truemaxdistance, .(blockid, distance, siteid)]
      # saving results as a list of tables to rbind after loop; old code did rbind for each table, inside loop 
    } else {
      #?
    }
    if ((i %% report_progress_every_n) == 0 & interactive()) {cat(paste("Finished finding blocks near ",i ," of ", nRowsDf),"\n" ) } # i %% report_progress_every_n indicates i mod report_progress_every_n (“i modulo report_progress_every_n”) 
  }
  sites2blocks <- data.table::rbindlist(res)  
  
  # data.table::setkey(sites2blocks, blockid, siteid, distance)
  data.table::setkey(sites2blocks, blockid, siteid, distance)
  
    
  # ADJUST THE VERY SHORT DISTANCES ####
  
  if (!("block_radius_miles" %in% names(blockwts))) {
    # if missing because not added to dataset yet then use placeholder of 100 / meters_per_mile, or 100 meters
    # not sure if this updates by reference blockwts for the remainder of this session and all users, or if this happens each time getblocksnearby... is called.
    message("using temporary approximation of block_radius_miles")
    blockwts[ , block_radius_miles := block_radius_miles_round_temp] # lazy load this and add it into blockwts
  }
  # Add block_radius_miles here, now to be able to correct the distances that are small relative to a block size.
  # This adjusts distance the way EJScreen does for proximity scores - so distance reflects distance to avg resident in block
  # even if distance to block internal point is so small the site is inside the block. This also avoids infinitely small or zero distances.
  # 2 ways considered to do join here - may be able to optimize.
  # a) try to do join that updates sites2blocks by reference - not sure it works this way, but goal was to make join faster:
  # sites2blocks[blockwts, .(siteid,blockid,distance,blockwt,bgid, block_radius_miles), on = 'blockid']
  # b) try to do join that updates sites2blocks by making a copy? This does work:
  sites2blocks <-  blockwts[sites2blocks, .(siteid,blockid,distance,blockwt,bgid, block_radius_miles), on='blockid'] 
  # 2 ways considered here for how exactly to make the adjustment: 
  sites2blocks[distance < block_radius_miles, distance := 0.9 * block_radius_miles]  # assumes distance is in miles
  # or a more continuous adjustment for when dist is between 0.9 and 1.0 times block_radius_miles: 
  # sites2blocks_dt[ , distance  := pmax(block_radius_miles, distance, na.rm = TRUE)] # assumes distance is in miles
  
  # drop that info about area or size of block to save memory. do not need it later in sites2blocks
  sites2blocks[ , block_radius_miles := NULL] 

  if (interactive()) { cat("You can use  getblocks_diagnostics(sites2blocks)  to see info on distances found.\n")  }
  return(sites2blocks)
}
