#' speedtest
#' Runs EJAM analysis for several radius values for various numbers of sitepoints, recording how long each step took. 
#' @param n optional, vector of 1 or more counts of how many random points to test, or set to 0 to interactively pick file of points in RStudio (n is ignored if sitepoints provided)
#' @param sitepoints optional,  (use if you do not want random points) data.frame of points or path/file with points, where columns are lat and lon in decimal degrees
#' @param weighting optional, if using random points, how to weight them, such as facilities, people, or blockgroups. see [testpoints_n()]
#' @param radii optional, one or more radius values in miles 
#'   to use in creating circular buffers when findings residents nearby each of sitepoints.
#'   The default list includes one that is 5km (approx 3.1 miles)
#' @param do_batch.summarize  optional, if want to also do that function
#' @param logging logical optional, whether to save log file with timings of steps. NOTE this slows it down though.
#' @param logfolder optional, name of folder for log file
#' @param logfilename optional, name of log file to go in folder
#' @param honk_when_ready optional, self-explanatory
#' @param saveoutput but this slows it down if set to TRUE to save each run as .rda file
#' @examples #  speedtest(c(10,100),radii=c(1,3),logging=FALSE, honk=FALSE)
#' @return EJAM results similar to as from the web app  [ejamit()]  
#' @export
#'
speedtest <- function(n=10, sitepoints=NULL, weighting='frs', 
                      radii=c(1, 3.106856, 5, 10, 31.06856)[1:3], 
                      do_batch.summarize=FALSE, 
                      logging=TRUE, logfolder=getwd(), logfilename="log_n_datetime.txt", honk_when_ready=TRUE, saveoutput=FALSE) {
  
  warning("not tested yet ")
  
  cat('see profvis::profvis({}) for viewing where the bottlenecks are!\n')
  
  # Test script that times each step of EJAM for a large dataset
  
  # - pick a sample size (n) (or enter sitepoints, or set n=0 to interactively pick file of points in RStudio)
  # - pick n random points
  # - pick a few different radii for circular buffering
  # - analyze indicators in circular buffers and overall (find blocks nearby and then calc indicators)
  # - get stats that summarize those indicators
  # - compare times between steps and radii and other approaches or tools
  
  if (n[1] == 0 & is.null(sitepoints)) {
    if (interactive()) {sitepoints <- rstudioapi::selectFile("Select xlsx or csv file with lat,lon coordinates", path=".", existing = FALSE)
    }
  } 
  
  if (is.null(sitepoints)) {
    # PICK TEST DATASET(s) OF FACILITY POINTS 
    cat("Picking random points for testing.\n")
    # sitepoints <- read.csv(file="./data/SamplePoints100k.csv",stringsAsFactors=FALSE)
    sitepoints <- list()
    for (i in 1:length(n)) {
      sitepoints[[i]] <- testpoints_n(n=n[i], weighting=weighting)
    }
    cat("Finished picking random points for testing.\n\n\n")
  } else {
    sitepoints <- latlon_from_anything(sitepoints)
    if (!missing(n)) {warning("n ignored because sitepoints provided")}
    if (!missing(weighting)) {warning("weighting ignored because sitepoints provided")}
    n <- NROW(sitepoints)
    sitepoints <- list(sitepoints)
  }
  
  # PICK SOME OPTIONS FOR RADIUS 
  if (length(radii) > 10 | any(radii < 1)) {stop("Did you intend to provide more than 10 radius values? Cannot try more than 10 radius values in one run.")}
  # radii <- c(1, 3.106856, 5, 10, 20)
  # 3.1 miles IS 5 KM ... 5*1000/ meters_per_mile
  
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
  ############################################################### # 
  
  
  # START RUNNING ANALYSIS 
  
  cat("Started analysis "); print(Sys.time())
  
  rtextfile <- paste(radii,   collapse = "_or_")
  ntextfile <- paste(n, collapse = "_or_")
  
  if (logging) {
    # PREP LOG FILE
    now <- function() {gsub("-","", gsub(':', '', gsub(' ', '_', Sys.time())))}
    logfilename <- paste0("logfile_", ntextfile, "_points_", rtextfile, "_radii_", now(), ".txt")  # file.path(logfolder, fname)
    logto = file.path( logfolder, logfilename)
    # START LOGGING
    sink(file = logto, split = TRUE)
  }
  
  cat("Started analysis "); print(Sys.time()) 
  
  ntext <- paste(n, collapse = ", ")
  rtext <- paste(radii,   collapse = ", ")
  cat("Radius choice(s) = ", rtext, '\n')
  cat("Size(s) of list(s) of points = ", ntext, '\n\n')
  cat("do_batch.summarize = ", do_batch.summarize, "\n")
  cat("saveoutput = ", saveoutput, "\n")
  
  overall_start_time <- Sys.time() 
  nlist=n
  for (n in nlist) {
    i <- which(n == nlist)
    cat("--------------------------------------------------------------------\n")
    cat("\n\nAnalyzing ", n, " facilities.\n\n")
    
    for (radius in radii) {
      
      cat("----------------------------------\n")
      cat("\n\nAnalyzing radius of ", radius, " miles.\n\n")
      cat("  radius value number ", which(radius == radii), " out of ", length(radii), '\n')    
      
      start_time <- Sys.time()
      
      
      elapsed <- system.time({
        cat(' Started getblocksnearby() to find Census blocks (by internal point) near each facility')
        step1 = system.time({
          mysites2blocks <-  getblocksnearby(
            sitepoints=sitepoints[[i]],
            cutoff=radius, maxcutoff=31.07,
            avoidorphans=TRUE)
        })
        cat(" Finished getblocksnearby()\n")
        print(step1)
        
        step2 = system.time({
          cat(' Started doaggregate() to calculate each indicator for each site, and overall.\n')
          out <-  doaggregate(sites2blocks = mysites2blocks, silentinteractive=TRUE)
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
          save(out, file = "out n", n, "_rad", radius, ".rda")
          # save(out2, file= "out2.rda")
        }
        
      })

      cat(paste0("Elapsed processing time for ", n, " points at ", radius, " miles: \n\n"))
      speedreport(start_time, Sys.time(), n)
      
      #write time elapsed to csv?
      # write.csv(t(data.matrix(elapsed)),file=paste0("./inst/time_radius_",myradius,"_100k.csv"))
      
    } # NEXT RADIUS 
    cat("Finished analyzing all radius values for this one set of points or sites.\n")
    
  } # NEXT LIST OF POINTS (facility list) 
  
  cat('\n\n\n')
  cat("Finished with all sets of sites, ", length(radii)," radius values, each for a total of ", sum(nlist)," sites, or ", sum(nlist) * length(radii)," circles total. \n\n\n")
  EJAMejscreenapi::speedreport(overall_start_time, Sys.time(), sum(n) * length(radii))
  
  if (logging) {sink(NULL)} # stop logging to file.
  if (honk_when_ready) {beepr::beep(8)}
  invisible(out)
}

