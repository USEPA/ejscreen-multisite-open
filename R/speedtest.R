#' speedtest
#' Runs EJAM analysis for several radius values for many sitepoints, recording how long each step took. 
#' @param n optional, how many random points to test, or set to 0 to interactively pick file of points in RStudio (n is ignored if sitepoints provided)
#' @param sitepoints optional,  (use if you do not want random points) data.frame of points or path/file with points, where columns are lat and lon in decimal degrees
#' @param weighting optional, if using random points, how to weight them, such as facilities, people, or blockgroups. see [testpoints_n()]
#' @param radius optional, one or more radius values in miles to use in creating circular buffers when findings residents nearby each of sitepoints
#' @param do_batch.summarize  optional, if want to also do that function
#' @param logging logical optional, whether to save log file with timings of steps
#' @param logfolder optional, name of folder for log file
#' @param logfilename optional, name of log file to go in folder
#' @param honk_when_ready optional, self-explanatory
#'
#' @return EJAM results similar to as from the web app  [ejamit()]  
#' @export
#'
speedtest <- function(n=10, sitepoints=NULL, weighting='frs', 
                      radius=c(1, 3.106856, 5, 10, 20)[1], 
                      do_batch.summarize=TRUE,
                      logging=TRUE, logfolder=getwd(), logfilename="log_n_datetime.txt", honk_when_ready=TRUE) {
  
  cat('see profvis::profvis({}) for viewing where the bottlenecks are!\n')
  
  # Test script that times each step of EJAM for a large dataset
  
  # - pick a sample size (n) (or enter sitepoints, or set n=0 to interactively pick file of points in RStudio)
  # - pick n random points
  # - pick a few different radii for circular buffering
  # - analyze indicators in circular buffers and overall (find blocks nearby and then calc indicators)
  # - get stats that summarize those indicators
  # - compare times between steps and radii and other approaches or tools
  
  if (n == 0 & is.null(sitepoints)) {
    if (interactive()) {sitepoints <- rstudioapi::selectFile("Select xlsx or csv file with lat,lon coordinates", path=".", existing = FALSE)
    }
  }
  
  if (is.null(sitepoints)) {
    # PICK TEST DATASET OF FACILITY POINTS 
    cat("Picking random points for testing.\n")
    # sitepoints <- read.csv(file="./data/SamplePoints100k.csv",stringsAsFactors=FALSE)
    sitepoints <- testpoints_n(n=n, weighting=weighting)
    cat("Finished picking random points for testing.\n\n\n")
  } else {
    sitepoints <- latlon_from_anything(sitepoints)
    if (!missing(n)) {warning("n ignored because sitepoints provided")}
    if (!missing(weighting)) {warning("weighting ignored because sitepoints provided")}
    n <- NROW(sitepoints)
  }
  
  # PICK SOME OPTIONS FOR RADIUS 
  if (length(radius) > 10 | radius < 1) {stop("Did you intend to provide more than 10 radius values? Cannot try more than 10 radius values in one run.")}
  # radius <- c(1, 3.106856, 5, 10, 20)
  # 3.1 miles IS 5 KM ... 5*1000/ meters_per_mile
  
  
  ############################################################### # 
  
  
  # START RUNNING ANALYSIS 
  
  if (logging) {
    # PREP LOG FILE
    now <- function() {gsub("-","", gsub(':', '', gsub(' ', '_', Sys.time())))}
    logfilename <- paste0("logfile_", n, "_", now(), ".txt")  # file.path(logfolder, fname)
    logto = file.path( logfolder, logfilename)
    # START LOGGING
    sink(file = logto, split = TRUE)
  }

  cat("\n\nAnalyzing ", n, " facilities.\n\n")
  cat("Radius choices = ", paste(radius, collapse = ", "), '\n\n')
  cat("Started analysis "); print(Sys.time()) 
  
  overall_start_time <- Sys.time()
  
  # MAKE SURE localtree INDEX OF ALL US BLOCKS IS AVAILABLE FROM EJAM PACKAGE 
  if (!exists("localtree")) {
    step0 <- system.time({
      cat("Creating national index of block locations (localtree) since it was not found.\n")
      localtree <- SearchTrees::createTree(
        EJAMblockdata::quaddata, treeType = "quad", dataType = "point"
      )
      cat("Finished createTree()\n")
      print(step0)
      #time to create quadtree 1.116 seconds
    })
  }
  
  cat("--------------------------------------------------------------------\n")
  
  for (myradius in radius) {
    
    cat("----------------------------------\n")
    cat("Starting analysis for radius value number ", which(radius == myradius), " out of ", length(radius), '\n')
    cat("Radius = ", radius, '\n')
    cat(" Analyzing ", n, " facilities.\n")
    start_time <- Sys.time()
    
    elapsed <- system.time({
      cat(' Started getblocksnearby() to find Census blocks (by internal point) near each facility')
      step1 = system.time({
        mysites2blocks <- EJAM::getblocksnearby(
          sitepoints=sitepoints,
          cutoff=myradius, maxcutoff=31.07,
          avoidorphans=TRUE,
          quadtree=localtree)
      })
      cat(" Finished getblocksnearby()\n")
      print(step1)
      
      step2 = system.time({
        cat(' Started doaggregate() to calculate each indicator for each site, and overall.\n')
        out <- EJAM::doaggregate(sites2blocks = mysites2blocks)
      })
      cat(" Finished doaggregate()\n")
      print(step2)
      
      if (do_batch.summarize) {
      step3 = system.time({
        cat(' Started batch.summarize() to calculate stats that summarize those indicators.\n')
        out2 <- EJAMbatch.summarizer::batch.summarize(
          sitestats = data.frame(out$results_bysite),
          popstats =  data.frame(out$results_bysite),
          ## user-selected quantiles to use
          #probs = as.numeric(input$an_list_pctiles),
          threshold = list(95) # compare variables to 95th %ile
        )
      })
      cat(" Finished batch.summarize()\n")
      print(step3)
      }
      if (saveoutput) {
        save(out, file = "out.rda")
        save(out2, file= "out2.rda")
      }
      
    })
   if (honk_when_ready) {beepr::beep()}
    cat("Elapsed processing time for radius ", myradius, ' miles: \n\n')
    speedreport(start_time, Sys.time(), n)
    
    #write time elapsed to csv?
    # write.csv(t(data.matrix(elapsed)),file=paste0("./inst/time_radius_",myradius,"_100k.csv"))
    
  } # NEXT RADIUS 
  cat("Finished analyzing all radius values.\n")
  cat('\n\n')
  speedreport(overall_start_time, Sys.time(), n * length(radius))
  
  if (logging) {sink(NULL)} # stop logging to file.
  if (honk_when_ready) {beepr::beep(8)}
  invisible(out)
}

