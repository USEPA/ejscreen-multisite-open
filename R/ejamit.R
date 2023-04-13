#' Get complete EJ analysis numbers (demographic and environmental indicators) near a list of locations
#' @description This is the main function in EJAM for users who want to use EJAM from RStudio.
#'   It does essentially what the webapp does to analyze/summarize near a set of points.
#'   See help("EJAM")  
#' @inheritParams getblocksnearby
#' @return A list of tables of results.
#' @examples \dontrun{
#' 
#'  # All in one step, using functions not shiny app:
#'  out <- ejamit(testpoints_100_dt, 2, quadtree=localtree)
#'  
#'  # Do not specify sitepoints and it will prompt you for a file,
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
#'   testsites <- testpoints_n(1e3)  
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
#' }
#' @seealso  [getblocksnearby()] [doaggregate()]
#' @export
ejamit <- function(sitepoints, 
                   cutoff=3, maxcutoff=31.07, 
                   avoidorphans=TRUE,
                   quadtree=NULL,
                   ...
) {
  if (missing(cutoff)) cat("\nUsing default radius of", cutoff, "miles.\n")
  if (!missing(quadtree)) {stop("quadtree should not be provided to ejamit() - that is handled by getblocksnearby() ")}

  ################################################################################## #
  # note this overlaps or duplicates code in app_server.R 
  #   for data_up_latlon() around lines 81-110 and data_up_frs() at 116-148
  
  # select file
  if (missing(sitepoints)) {
    if (interactive()) {
      sitepoints <- rstudioapi::selectFile(caption = "Select xlsx or csv with lat,lon values", path = '.' )
    } else {
      stop("sitepoints (locations to analyze) is missing but required.")
    }
  }
  # if user entered a table, path to a file (csv, xlsx), or whatever, then read it to get the lat lon values from there
  sitepoints <- latlon_from_anything(sitepoints)  
  ################################################################################## #
  # 1. getblocksnearby() ####
  cat('Finding blocks nearby.\n')
  
  mysites2blocks <- getblocksnearby(
    sitepoints=sitepoints, 
    cutoff=cutoff, maxcutoff=maxcutoff, 
    avoidorphans=avoidorphans, 
    ...)
  
  # 2. doaggregate() ####
  cat('Aggregating at each buffer and overall.\n')
  
  out <- suppressWarnings (
    doaggregate(
      sites2blocks = mysites2blocks, 
      sites2states = sitepoints
    )
  )
  # provide sitepoints table provided by user aka data_uploaded(), (or could pass only lat,lon and ST -if avail- not all cols?)
  # and doaggregate() decides where to pull ST info from - 
  # ideally from ST column, 
  # second from fips of block with smallest distance to site, 
  # third from lat,lon of sitepoints intersected with shapefile of state bounds
  
  ################################################################ # 
  
  # Hyperlinks added (to site by site table) ####
  ##  >>this should be a function, since used here and in ejamit() ####
  # duplicated almost exactly in app_server but uses reactives there
  browser()
  if ("REGISTRY_ID" %in% names(out$results_bysite)) {
    echolink = url_echo_facility_webpage(REGISTRY_ID, as_html = T)
  } else {
    echolink = rep(NA,nrow(out$results_bysite))
  }
  out$results_bysite[ , `:=`(
    `EJScreen Report` = url_ejscreen_report(    lat = out$results_bysite$lat, lon = out$results_bysite$lon, distance = cutoff, as_html = T), 
    `EJScreen Map`    = url_ejscreenmap(        lat = out$results_bysite$lat, lon = out$results_bysite$lon,                    as_html = T), 
    `ACS Report`      = url_ejscreen_acs_report(lat = out$results_bysite$lat, lon = out$results_bysite$lon, distance = cutoff, as_html = T),
    `ECHO report` = echolink
  )]
  out$results_overall[ , `:=`(
    `EJScreen Report` = NA,   #  rep(NA,nrow(out$results_bysite)), 
    `EJScreen Map`    = NA,    # rep(NA,nrow(out$results_bysite)),  
    `ACS Report`      = NA,   #  rep(NA,nrow(out$results_bysite)), 
    `ECHO report`     = NA     # rep(NA,nrow(out$results_bysite))
  )]
  newcolnames <- c(
    "EJScreen Report", 
    "EJScreen Map", 
    "ACS Report", 
    "ECHO report")
  setcolorder(out$results_bysite, neworder = newcolnames)
  setcolorder(out$results_overall, neworder = newcolnames)
  out$longnames <- c(newcolnames, out$longnames)
  
  ################################################################ # 
  # 3. batch.summarize()** on already processed data ####
  out$results_summarized <- EJAMbatch.summarizer::batch.summarize(
    sitestats = data.frame(out$results_bysite),
    popstats =  data.frame(out$results_bysite),
    ## user-selected quantiles to use
    #probs = as.numeric(input$an_list_pctiles),
    threshold = list(95) # compare variables to 95th %ile
  )
  
  ################################################################ # 
  if (interactive()) {  # would be nice to provide the 1pager summary report as html here too
    # already done by doaggregate()
    # Show as nicely named indicators in console of RStudio - Overall results (across all sites)
    # print to console
    # Show datatable view in RStudio - Site by Site (each site)
    print(
      DT::datatable(
      out$results_bysite[1:min(nrow(out$results_bysite), 2000) ], # >2k rows is too much for client-side DataTables
      colnames = out$longnames,
      escape = FALSE,
      caption = paste0(nrow(out$results_bysite), ' FACILITIES "', " ", '"'),
      filter = "top"
    )
    )
    # Map of facilities in an industry, plus popups with links to each facility in ECHO and EJScreen
    # mapfast(out$results_bysite)  
    # had some bugs/ problems with mapfast if using "ej" option and too many indicators otherwise
    cat("\nSome more key results: \n\n")
    somenames <- grep("ratio.to.state", names(out$results_summarized$rows), value = TRUE)
    print(  round(t(out$results_summarized$rows[ , somenames])[ ,c(1,2,6)],2)  )  # 1:70 
    # site counts and distance minima
    
    print(  round(tail(t(out$results_summarized$rows)[ ,1:7],7),1)  )
  }
  ################################################################ # 
  
  invisible(out)
}

#' @export
getblocksnearby_and_doaggregate <- ejamit
