#' ejamit - Get complete EJ analysis (demographic and environmental indicators) near a list of locations
#' @description This is the main function in EJAM for users who want to use EJAM from RStudio.
#'   It does essentially what the webapp does to analyze/summarize near a set of points.
#'   See help("EJAM")  
#' @inheritParams getblocksnearby
#' @param silentinteractive Set to FALSE to prevent long output showing in console in RStudio when in interactive mode,
#'   passed to doaggregate() also. app server sets this to TRUE when calling doaggregate() but 
#'   ejamit() default is to set this to FALSE when calling doaggregate(). 
#' @param fips FIPS code vector if using FIPS instead of sitepoints to specify places to analyze,
#'   such as a list of US Counties or tracts.
#' @param threshold1 percentile like 80 or 90 or 95 to compare percentiles to
#'   "alone" for groups like white alone (whether or not hispanic),
#'    "both" may try to include both,
#'     or possibly "original" or "default" might be added as options
#' @param subgroups_type Optional (uses default). Set this to 
#'   "nh" for non-hispanic race subgroups as in Non-Hispanic White Alone, nhwa and others in names_d_subgroups_nh; 
#'   "alone" for EJScreen v2.2 style race subgroups as in    White Alone, wa and others in names_d_subgroups_alone; 
#'   "both" for both versions. Possibly another option is "original" or "default" but work in progress.
#' @param calculate_ratios whether to calculate and return ratio of each indicator to US and State overall averages
#' @return A list of tables of results.
#' @examples \dontrun{
#'  # All in one step, using functions not shiny app:
#'  out <- ejamit(testpoints_100_dt, 2, quadtree=localtree)
#' 
#'  # Do not specify sitepoints and it will prompt you for a file,
#'  # if in RStudio in interactive mode!
#'  out <- ejamit(radius = 3)
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
#'   # out2 <- ejscreenit(testpoints_05)
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
#'   
#'   # if doing just 1st step of ejamit() 
#'   #  get distance between each site and every nearby Census block
#'   s2b <- testdata_sites2blocks
#'   s2b <- getblocksnearby(testsites, radius = radius)
#'   s2b <- getblocksnearbyviaQuadTree(testsites, radius = radius)
#'   getblocks_diagnostics(s2b)
#'   plotblocksnearby(s2b)
#'    
#'   # if doing just 2d step of ejamit()
#'   #  get summaries of all indicators based on table of distances
#'   out <- doaggregate(s2b, testsites) # this works now and is simpler
#'
#' }
#' 
#' @seealso  [getblocksnearby()] [doaggregate()]
#' @export
ejamit <- function(sitepoints, 
                   radius=3, maxradius=31.07, 
                   avoidorphans=FALSE,
                   quadtree=NULL,
                   silentinteractive=F,
                   fips=NULL,
                   subgroups_type = 'nh', calculate_ratios = TRUE,
                   threshold1 = 90, # threshold.default['comp1'],
                   ...
) {
  if (is.null(fips)) {
    
    if (missing(radius)) {warning(paste0("Using default radius of ", radius, " miles because not provided as parameter."))}
    if (!missing(quadtree)) {warning("quadtree should not be provided to ejamit() - that is handled by getblocksnearby() ")}
    
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
    
    if (!silentinteractive) {cat('Finding blocks nearby.\n')}
    
    mysites2blocks <- getblocksnearby(
      sitepoints = sitepoints, 
      radius = radius, maxradius = maxradius, 
      avoidorphans = avoidorphans, 
      quiet = TRUE,
      ...)
    ################################################################################## #
    
    # 2. doaggregate() ####
    
    if (!silentinteractive) {cat('Aggregating at each buffer and overall.\n')}
    
    out <- suppressWarnings(
      doaggregate(
        sites2blocks = mysites2blocks,  
        subgroups_type = subgroups_type, 
        sites2states_or_latlon = sitepoints, # sites2states_or_latlon = unique(x[ , .(siteid, lat, lon)]))
        silentinteractive = silentinteractive, called_by_ejamit = TRUE,
        calculate_ratios = calculate_ratios,
        radius = radius
      )
    )
    # provide sitepoints table provided by user aka data_uploaded(), (or could pass only lat,lon and ST -if avail- not all cols?)
    # and doaggregate() decides where to pull ST info from - 
    # ideally from ST column, 
    # second from fips of block with smallest distance to site, 
    # third from lat,lon of sitepoints intersected with shapefile of state bounds
    
  } else {
    # fips provided, not latlons
    mysites2blocks <- getblocksnearby_from_fips(fips) # this should have site = each FIPS code (such as each countyfips), and otherwise same outputs as getblocksnearby()
    out <- doaggregate(
      mysites2blocks, 
      subgroups_type = subgroups_type, 
      sites2states_or_latlon = unique(mysites2blocks[ , .(siteid, lat, lon)]),   #  
      radius = 999
    )
  }
  ################################################################ # 
  
  # 2b. add  HYPERLINKS  to output (to site by site table) ####
  
  # ( doaggregate does not provide this ? )
  
  #  >this should be a function  and is used by both server and ejamit() ####
  # duplicated almost exactly in app_server but uses reactives there.
  # #  Do maybe something like this:
  # links <- url_4table(lat=out$results_bysite$lat, lon=out$results_bysite$lon, radius = radius, regid=ifelse("REGISTRY_ID" %in% names(out$results_bysite), out$results_bysite$REGISTRY_ID, NULL))
  # out$results_bysite[ , `:=`(links$results_bysite)] # would that work??? how to avoid big cbind step to add the new columns?
  # out$results_overall <- cbind(out$results_overall, links$results_overall) # 
  # setcolorder(out$results_bysite, neworder = links$newcolnames)
  # setcolorder(out$results_overall, neworder = links$newcolnames)
  # out$longnames <- c(newcolnames, links$longnames)
  
  if ("REGISTRY_ID" %in% names(out$results_bysite)) {
    echolink = url_echo_facility_webpage(REGISTRY_ID, as_html = T)
  } else {
    echolink = rep(NA,nrow(out$results_bysite))
  }
  out$results_bysite[ , `:=`(
    `EJScreen Report` = url_ejscreen_report(    lat = out$results_bysite$lat, lon = out$results_bysite$lon, radius = radius, as_html = T),
    `EJScreen Map`    = url_ejscreenmap(        lat = out$results_bysite$lat, lon = out$results_bysite$lon,                  as_html = T),
    # `ACS Report`      = url_ejscreen_acs_report(lat = out$results_bysite$lat, lon = out$results_bysite$lon, radius = radius, as_html = T),
    `ECHO report` = echolink
  )]
  out$results_overall[ , `:=`(
    `EJScreen Report` = NA,   #  rep(NA,nrow(out$results_bysite)),
    `EJScreen Map`    = NA,    # rep(NA,nrow(out$results_bysite)),
    # `ACS Report`      = NA,   #  rep(NA,nrow(out$results_bysite)),
    `ECHO report`     = NA     # rep(NA,nrow(out$results_bysite))
  )]
  newcolnames <- c(
    "EJScreen Report",
    "EJScreen Map",
    # "ACS Report",
    "ECHO report")
  # put those up front as first columns
  data.table::setcolorder(out$results_bysite,  neworder = newcolnames)
  data.table::setcolorder(out$results_overall, neworder = newcolnames)
  out$longnames <- c(newcolnames, out$longnames)
  
  ################################################################ # 
  
  # 2c. add  RADIUS  to output (in server and in ejamit() ####
  
  # ( doaggregate does not provide this ? )
  
  out$results_bysite[      , radius.miles := radius]
  out$results_overall[     , radius.miles := radius]
  out$results_bybg_people[ , radius.miles := radius] # do not really need to export to excel though, and most users do not need this large table
  
  ################################################################ # 
  
  # 3. batch.summarize()**  ####
  
  # For each indicator, calc AVG and PCTILES, across all SITES and all PEOPLE 
  
  # (doaggregate does not provide this)
  
  # out$results_summarized <- EJAMbatch.summarizer   ::   batch.summarize(   # disabled in ejam lite package ***
  #   sitestats = data.frame(out$results_bysite),
  #   popstats =  data.frame(out$results_bysite),
  #   ## user-selected quantiles to use
  #   #probs = as.numeric(input$an_list_pctiles),
  #   threshold = list(threshold1) # compare variables to 90 or other  %ile
  # )
  
  #   The percentiles in these $rows seem wrong as of 10/2023 so far:
  #
  # rownames(out$results_summarized$rows)
  # [1] "Average site"             "Average person"           "Median site"              "Median person"            "Min"                     
  # [6]  "Max"                      "Sum"  
  #
  # "Percentile of sites 0"    "Percentile of sites 25"   "Percentile of sites 50"   "Percentile of sites 75"   "Percentile of sites 80"   "Percentile of sites 90"   "Percentile of sites 95"   "Percentile of sites 99"  "Percentile of sites 100"  
  # "Percentile of people 0"   "Percentile of people 25"  "Percentile of people 50"  "Percentile of people 75"  "Percentile of people 80"  "Percentile of people 90"  "Percentile of people 95"  "Percentile of people 99"  "Percentile of people 100"
  ################################################################ # 
  
  # 4. add a tall summary table ####
  
  # (doaggregate alone does not provide this)
  
  out$formatted <- table_tall_from_overall(out$results_overall, out$longnames)
  
  # 5. would be nice to provide the 1pager summary report as html here too
  
  ################################################################ # 
  if (interactive() & !silentinteractive) {  
    
    # Show summary info and tips in RStudio console  #### 
    # NOTE: SOME OF THIS BELOW SHOULD BE USED IN A VIGNETTE RATHER THAN BEING HERE ***
    
    # Show bysite in DT::datatable view in RStudio ####
    # - Site by Site (each site)
    if (nrow(out$results_bysite) > 1000) {message("> 1,000 rows may be too much for client-side DataTables - only showing some rows here")}
    print(
      DT::datatable(
        out$results_bysite[1:min(nrow(out$results_bysite), 1000) ], # >2k rows is too much for client-side DataTables
        colnames = out$longnames,
        rownames = FALSE,
        escape = FALSE,
        caption = paste0(nrow(out$results_bysite), ' FACILITIES "', " ", '"'),
        filter = "top"
      )
    )
    ###################################### # 
    #  *** THE results_summarized$rows percentiles info needs debugging - numbers may be wrong
    # cat("\nWhich Demog groups or Envt stressors are highest (relative to States overall): \n\n")
    # 
    # if (subgroups_type == 'nh')    { subratvarnames <- names_d_subgroups_nh_ratio_to_state_avg}
    # if (subgroups_type == 'alone') { subratvarnames <- names_d_subgroups_alone_ratio_to_state_avg}
    # if (subgroups_type == 'both')  { subratvarnames <- c(names_d_subgroups_nh_ratio_to_state_avg, names_d_subgroups_alone_ratio_to_state_avg)}
    # 
    # grps <- list(
    #   names_d_ratio_to_state_avg, 
    #   subratvarnames, #names_d_subgroups_ratio_to_state_avg,   #   edited to flexibly use nh, alone, or both types
    #   names_e_ratio_to_state_avg
    # )
    # for (somenames in grps) {
    #   # somenames <- grep("ratio.to.state", names(out$results_summarized$rows), value = TRUE)
    #   # cat("Score as Ratio to State Average:\n")
    #   someinfo <- t(out$results_summarized$rows[ , somenames])[ , c(1,2,6)]         # disabled in ejam lite package ***
    #   someinfo <- data.frame(someinfo)
    #   rownames(someinfo) <- fixcolnames(somenames, 'rname', 'long')      
    #   colnames(someinfo) <- c("Avg resident overall", "at site with max ratio", "Avg site")
    #   print(
    #     round(someinfo[order(someinfo[,"Avg resident overall"], decreasing = TRUE), ], 1)
    #   )
    #   cat("\n\n")
    # }
    ###################################### # 
    # site counts and distance minima
    # print(  round(tail(t(out$results_summarized$rows)[ ,1:7],7),1)  )   
    # cat("\n\n")
    
    ###################################### # 
    cat("Population Density:\n")
    cat("  ", popshare_p_lives_at_what_pct(out$results_bysite$pop, p = 0.50, astext = TRUE), "\n")
    cat("  ", popshare_at_top_n(out$results_bysite$pop, c(1, 5, 10), astext = TRUE), "\n\n")
    
    ###################################### #
    
    cat("For example, \n out <- ejamit(testpoints_1000, radius = 1) \n # or\n out <- testoutput_ejamit_1000pts_1miles \n\n") 
    
    cat("To see a histogram of population counts nearby: \n\n",
        '     hist(out$results_bysite$pop/1000, 100, 
        xlab = "Residents nearby (in thousands)", 
        ylab = "Number of sites", 
        main =  "Population Counts within', radius, 'miles of Various Sites")',
        "\n\n")
    
    cat("To see cumulative distribution of population nearby:\n\n", 
        '     plot(ecdf(out$results_bysite$pop/1000), 
        ylab="Fraction of total population living any one or more of these sites", 
        xlab="# of residents (in thousands) near a site, showing one dot for each site", 
        main="A fraction of these sites are where most of the residents are located")',
        "\n\n")
    
    cat("To see barplots of average proximity by demog group:\n\n",
        '     plot_distance_mean_by_group(out$results_bybg_people)',
        "\n\n")
    
    cat("To see bar or boxplots of ratios of %Demographics vs US averages:\n\n", 
        "     ?plot_barplot_ratios() in EJAM package # or ",
        "     ?boxplots_ratios() in EJAMejscreenapi package",
        "     boxplots_ratios(ratios_to_avg(as.data.frame(out$results_bysite))$ratios_d)",
        "\n\n")
    
    cat("To see a map in RStudio: \n\n",
        "     mapfast(out$results_bysite, radius = out$results_overall$radius.miles, column_names = 'ej')", 
        "\n\n")
    
    cat("To view or save as excel files, see ?table_xls_from_ejam e.g., table_xls_from_ejam(out, fname = 'out.xlsx')  \n\n")
  }
  ################################################################ # 
  
  cat('Output is a list with the following names:\n')
  print(structure.of.output.list(out) )
  
  invisible(out)
}

