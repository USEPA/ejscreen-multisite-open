#' Get complete EJ analysis numbers (demographic and environmental indicators) for a list of locations
#' @description This is the main function in EJAM for users who want to use EJAM from RStudio.
#'   See help("EJAM")  
#' @param sitepoints    see [getblocksnearbyviaQuadTree()] or other such functions
#' @param cutoff        see [getblocksnearbyviaQuadTree()] or other such functions
#' @param maxcutoff     see [getblocksnearbyviaQuadTree()] or other such functions
#' @param avoidorphans  see [getblocksnearbyviaQuadTree()] or other such functions
#' @param quadtree User does not need to provide this parameter. 
#'   Already created when EJAM package is loaded, this is 
#'   a large quadtree object created from the SearchTree package example:
#'   SearchTrees::createTree(EJAMblockdata::quaddata, treeType = "quad", dataType = "point")
#' @param ...  see [getblocksnearbyviaQuadTree_Clustered()] or other such functions
#' @examples \dontrun{
#' 
#'  # All in one step, using functions not shiny app:
#'  out <- ejamit(testpoints_100_dt, 2, quadtree=localtree)
#'  
#'  # do not specify sitepoints and it will prompt you for a file,
#'  # if in RStudio in interactive mode!
#'  out <- ejamit(cutoff = 3)
#'  
#'   # Specify facilities or sites as points for test data, 
#'   # use 1000 test facility points from the R package 
#'   testsites <- testpoints_1000_dt
#'   # use facility points in an excel or csv file
#'   testsites <- latlon_from_anything(
#'    "./inst/testdata/testpoints_207_sites_with_signif_violations_NAICS_326_ECHO.csv")
#'   # use facility points from a random sample of EPA-regulated facilities
#'   testsites <- EJAMfrsdata::frs[sample(1:nrow(EJAMfrsdata::frs), 1e3),] # this is slow
#'   
#'   # Specify max distance from sites to look at (residents within X miles of site point)
#'   radius <- 3.1 # miles
#'   
#'   # Get summaries of all indicators near a set of points 
#'   out <- ejamit(testsites, radius)  
#'   # out <- ejamit("myfile.xlsx", 3.1)  
#'   
#'   # out2 <- EJAMejscreenapi::ejscreenit(testpoints_1000[1:3,])
#'   
#'   # View results overall
#'   round(t(out$results_overall), 3.1)
#'   
#'   # View plots
#'   # plot_distance_avg_by_group(out)  
#'   # plot_distance_cdf_by_group(out)
#'   
#'   # View maps
#'   mapfast(out$results_bysite, radius = 3.1)
#'   
#'   # view results at a single site
#'   t(out$results_bysite[1, ])
#'   t(out$results_bysite[out$results_bysite$siteid == 2, ])
#'   
#'   # if doing just 1st step of ejamit() 
#'   #  get distance between each site and every nearby Census block
#'   s2b <- testdata_sites2blocks
#'   s2b <- getblocksnearby(testsites, cutoff = radius)
#'   s2b <- getblocksnearbyviaQuadTree(testsites, cutoff = radius)
#'   summarize_blockcount(s2b)
#'   
#'   # if doing just 2d step of ejamit()
#'   #  get summaries of all indicators based on table of distances
#'   out <- doaggregate(s2b, testsites) # this works now and is simpler
#'   
#'   # if localtree had to be built from block point data again
#'   localtree_example = SearchTrees::createTree(EJAMblockdata::quaddata, treeType = "quad", dataType = "point")
#'   
#' }
#' @seealso  [getblocksnearby()] [doaggregate()]
#' @export
ejamit <- function(sitepoints, 
                   cutoff=3, maxcutoff=31.07, 
                   avoidorphans=TRUE, 
                   quadtree,
                   ...
) {
  if (missing(cutoff)) cat("\nUsing default radius of", cutoff, "miles.\n")
  if (missing(quadtree)) {
    if (exists("localtree")) {
      quadtree <- localtree 
    } else {
      stop("Nationwide index of block locations is required but missing (quadtree parameter default is called localtree but was not found).")
    }
  }
  
  ################################################################################## #
  # note this overlaps or duplicates code in latlon_from_anything()
  # select file
  if (missing(sitepoints)) { 
    if (interactive()) {
      sitepoints <- rstudioapi::selectFile()
    } else {
      stop("sitepoints (locations to analyze) is missing but required.")
    }
  }
  # if user entered a table, path to a file (csv, xlsx), or whatever, then read it to get the lat lon values from there
  sitepoints <- latlon_from_anything(sitepoints)
  ################################################################################## #
  
  cat('Finding blocks nearby.\n')
  
  mysites2blocks <- getblocksnearby(
    sitepoints=sitepoints, 
    cutoff=cutoff, maxcutoff=maxcutoff, 
    avoidorphans=avoidorphans, 
    quadtree=quadtree,
    ...)
  
  # pass along lat and lon, and hopefully ST too, so State percentiles can be reported
  sites2states <- sitepoints
  
  cat('Aggregating at each buffer and overall.\n')
  
  out <- suppressWarnings (
    doaggregate(sites2blocks = mysites2blocks, sites2states = sites2states)
    )

  if (interactive()) {
    
    # already done by doaggregate()
    
    # Show as nicely named indicators in console of RStudio - Overall results (across all sites)
     # print to console
     
    # Show datatable view in RStudio - Site by Site (each site)
    
    # Plot some results 
  }
  return(out)
}

#' @export
getblocksnearby_and_doaggregate <- ejamit
