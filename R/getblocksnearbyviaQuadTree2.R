#' Find nearby blocks using Quad Tree data structure for speed, NO PARALLEL PROCESSING
#' 
#' @description 
#'   This should be almost identical to getblocksnearbyviaQuadTree(), 
#'   but it uses f2, a copy of sitepoints, and more importantly it 
#'   pulls some code out of the for loop and uses a vectorized approach.
#'   Given a set of points and a specified radius (in miles), 
#'   this function quickly finds all the US Census blocks near each point. 
#'   For each point, it uses the specified search radius and finds the distance to 
#'   every block within the circle defined by the radius. 
#'   Each block is defined by its Census-provided internal point, by latitude and longitude.
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
#' @param radius in miles, defining circular buffer around site point 
#' @param maxradius miles distance (max distance to check if not even 1 block point is within radius)
#' @param avoidorphans logical Whether to avoid case where no block points are within radius, 
#'   so if TRUE, it keeps looking past radius to find nearest one within maxradius.
#' @param quadtree (a pointer to the large quadtree object) 
#'    created as with indexblocks(), from the SearchTree package example:
#'    SearchTrees::createTree( quaddata, treeType = "quad", dataType = "point")
#'    Takes about 2-5 seconds to create this each time it is needed.
#'    It can be automatically created when the package is loaded via the [.onLoad()] function 
#' @param report_progress_every_n Reports progress to console after every n points,
#'   mostly for testing, but a progress bar feature might be useful unless this is super fast.
#' @examples 
#'   localtree_example = SearchTrees::createTree( quaddata, treeType = "quad", dataType = "point")
#'   x = getblocksnearby2(testpoints_1000_dt, quadtree = localtree_example)
#' @seealso [getblocksnearbyviaQuadTree_Clustered()]  [getblocksnearbyviaQuadTree()]  
#' @export
#' @import data.table
#' @importFrom pdist "pdist"
#'
getblocksnearbyviaQuadTree2 <- function(sitepoints, radius=3, maxradius=31.07, 
                                        avoidorphans=TRUE,  report_progress_every_n=500,
                                        quadtree) {
  if(class(quadtree) != "QuadTree"){
    stop('quadtree must be an object created from SearchTrees package with treeType = "quad" and dataType = "point"')  
  }
  if (!data.table::is.data.table(sitepoints)) {data.table::setDT(sitepoints)}

  if (!('siteid' %in% names(sitepoints))) {sitepoints$siteid <- seq.int(length.out = NROW(sitepoints))}
  
  #pass in a list of uniques and the surface radius distance
  
  #filter na values? or keep length of out same as input? ####
  sitepoints <- sitepoints[!is.na(sitepoints$lat) & !is.na(sitepoints$lon), ] # perhaps could do this by reference to avoid making a copy
  
  #compute and add grid info ####
  earthRadius_miles <- 3959 # in case it is not already in global envt
  radians_per_degree <- pi / 180
  
  f2   <- data.table::copy(sitepoints) # make a copy to avoid altering sitepoints in the calling envt when modifying by reference using data.table
  f2[         , lat_RAD := lat * radians_per_degree]   # data.table modifies it by reference
  f2[         , lon_RAD := lon * radians_per_degree]
  cos_lat <- cos(         f2[, lat_RAD])    # or maybe # sitepoints[ , cos_lat := cos(lat_RAD)]
  f2[         , FAC_X := earthRadius_miles * cos_lat * cos(lon_RAD)]
  f2[         , FAC_Y := earthRadius_miles * cos_lat * sin(lon_RAD)]
  f2[         , FAC_Z := earthRadius_miles *           sin(lat_RAD)]
  
  # indexgridsize was defined at start as say 10 miles in global? could be passed here as a parameter ####
  # and buffer_indexdistance defined here in code but is never used anywhere...  
  # buffer_indexdistance <- ceiling(radius / indexgridsize) 
  truedistance <- distance_via_surfacedistance(radius)   # simply 7918*sin(radius/7918) 
  
  # main reason for using foreach::foreach() is that it supports parallel execution,
  # that is, it can execute those repeated operations on multiple processors/cores on your computer
  # (and there are other advantages as well)
  
  #---- Get ready for loop here ----
  
  # allocate memory for result list
  nRowsDf <- NROW(f2)
  res <- vector('list', nRowsDf)  # list of data.tables   cols will be blockid, distance, siteid
  
  # **** these are now outside the loop, so they can be used in the vectorized way
  coords <- f2[ , .(FAC_X, FAC_Z)] 
  x_low <- coords[ , FAC_X] -truedistance;
  x_hi  <-  coords[ , FAC_X] +truedistance
  z_low <- coords[ , FAC_Z] -truedistance;
  z_hi  <-  coords[ , FAC_Z] +truedistance
  
  for (i in 1:nRowsDf) {    # LOOP OVER SITES HERE ----
    
    
    
    
    
    
    
    
    
    
    
    
    if ((i %% report_progress_every_n) == 0) {print(paste("Cells currently processing: ",i ," of ", nRowsDf) ) } # i %% report_progress_every_n indicates i mod report_progress_every_n (“i modulo report_progress_every_n”) 
    
    vec <- SearchTrees::rectLookup(
      quadtree, 
      unlist(c(x_low[i, ], z_low[i, ])), 
      
      
      unlist(c(x_hi[i, ], z_hi[i, ]))) 
    
    
    # *** FIX/CHECK: 
    tmp <-  quaddata[vec, ]
    # x <- tmp[ , .(BLOCK_X, BLOCK_Y, BLOCK_Z)] # but not blockid ??   # ** SLOW STEP TO OPTIMIZE 
    # y <-         f2[i, c('FAC_X','FAC_Y','FAC_Z')]  # the similar clustered function uses something other than f2 or sitepoints here - why?
    ########################################################################### ## ** SLOWSTEP TO OPTIMIZE: 
    distances <- as.matrix(pdist::pdist(tmp[ , .(BLOCK_X, BLOCK_Y, BLOCK_Z)] ,
                                        f2[i, c('FAC_X','FAC_Y','FAC_Z')] ))
    
    # pdist computes a n by p distance matrix using two separate matrices
    
    #clean up fields
    tmp[ , distance := distances[ , c(1)]]
    tmp[ , siteid :=         f2[i, .(siteid)]]  # the similar clustered function differs, why?
    
    #filter actual distance
    
    tmp      <- tmp[distance <= truedistance, .(blockid, distance, siteid)]  # ** SLOW STEP TO OPTIMIZE
    
    # hold your horses, what if there are no blocks and you are supposed to avoid that
    if ( avoidorphans && (nrow(tmp))      == 0) {
      #search neighbors, allow for multiple at equal distance
      vec  <- SearchTrees::knnLookup(quadtree, unlist(c(coords[i, 'FAC_X'])), unlist(c(coords[i, 'FAC_Z'])), k=10) 
      
      
      # *** FIX/CHECK: 
      tmp <-  quaddata[vec[1, ], ]
      
      x <- tmp[, .(BLOCK_X, BLOCK_Y, BLOCK_Z)]
      y <-         f2[i, .(FAC_X, FAC_Y, FAC_Z)]
      distances <- as.matrix(pdist::pdist(x, y))
      
      #clean up fields
      tmp[ , distance := distances[ , c(1)]]
      tmp[ , siteid :=         f2[i, .(siteid)]]
      
      #filter to max distance
      truemaxdistance <- distance_via_surfacedistance(maxradius)
      res[[i]] <- tmp[distance <= truemaxdistance, .(blockid, distance, siteid)]
      # saving results as a list of tables to rbind after loop; old code did rbind for each table, inside loop 
    } else {
      res[[i]] <- tmp
    }
  }
  result <- data.table::rbindlist(res) 
  
  data.table::setkey(result, blockid, siteid, distance)
  # print(summary_of_blockcount(result))
  
  return(result)
}
