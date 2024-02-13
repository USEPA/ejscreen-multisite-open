
#' Get an EJ analysis (demographic and environmental indicators) in or near a list of locations
#' 
#' @description This is the main function in EJAM that runs the analysis. 
#'   It does essentially what the web app does, to analyze/summarize near a set of points,
#'   or in a set of polygons from a shapefile, or in a list of Census Units like Counties.
#'   
#' @param sitepoints data.table with columns lat, lon giving point locations of sites or facilities around which are circular buffers
#' @param radius in miles, defining circular buffer around a site point
#' @param maxradius miles distance (max distance to check if not even 1 block point is within radius)
#' @param avoidorphans logical If TRUE, then where not even 1 BLOCK internal point is within radius of a SITE,
#'   it keeps looking past radius, up to maxradius, to find nearest 1 BLOCK. 
#'   What EJScreen does in that case is report NA, right? 
#'   So, does EJAM really need to report stats on residents presumed to be within radius,
#'    if no block centroid is within radius? 
#'    Best estimate might be to report indicators from nearest block centroid 
#'    which is probably almost always the one your site is sitting inside of, 
#'    but ideally would adjust total count to be a fraction of blockwt 
#'    based on what is area of circular buffer as fraction of area of block it is apparently inside of. 
#'    Setting this to TRUE can produce unexpected results, which will not match EJScreen numbers.
#'    Note that if creating a proximity score, by contrast, you 
#'    instead want to find nearest 1 SITE if none within radius of this BLOCK.
#' @param quadtree (a pointer to the large quadtree object) created using indexblocks() which uses the SearchTree package.
#'   Takes about 2-5 seconds to create this each time it is needed. 
#'   It can be automatically created when the package is attached via the .onAttach() function
#' @param fips optional FIPS code vector to provide if using FIPS instead of sitepoints to specify places to analyze,
#'   such as a list of US Counties or tracts. Passed to [getblocksnearby_from_fips()]
#' @param shapefile_folder optional path to folder that has shapefiles to analyze polygons
#' @param countcols character vector of names of variables to aggregate within a buffer using a sum of counts, 
#'   like, for example, the number of people for whom a poverty ratio is known, 
#'   the count of which is the exact denominator needed to correctly calculate percent low income.
#' @param popmeancols character vector of names of variables to aggregate within a buffer using population weighted mean.
#' @param calculatedcols character vector of names of variables to aggregate within a buffer using formulas that have to be specified.
#' @param subgroups_type Optional (uses default). Set this to "nh" for non-hispanic race subgroups 
#'   as in Non-Hispanic White Alone, nhwa and others in names_d_subgroups_nh; 
#'   "alone" for EJScreen v2.2 style race subgroups as in White Alone, wa and others in names_d_subgroups_alone; 
#'   "both" for both versions. Possibly another option is "original" or "default"
#' @param include_ejindexes whether to try to include EJ Indexes (assuming dataset is available) - passed to [doaggregate()]
#' @param calculate_ratios whether to calculate and return ratio of each indicator to US and State overall averages - passed to [doaggregate()]
#' @param extra_demog if should include more indicators from v2.2 report on language etc.
#' @param need_proximityscore whether to calculate proximity scores
#' @param infer_sitepoints set to TRUE to try to infer the lat,lon of each site around which the blocks in sites2blocks were found.
#'   lat,lon of each site will be approximated as average of nearby blocks, 
#'   although a more accurate slower way would be to use reported distance of each of 3 of the furthest block points and triangulate
#' @param need_blockwt if fips parameter is used, passed to [getblocksnearby_from_fips()]
#' @param threshold1 percentile like 80 or 90 or 95 to compare percentiles to
#'   "alone" for groups like white alone (whether or not hispanic),
#'   "both" may try to include both,
#'   or possibly "original" or "default" might be added as options - passed to batch.summarize()
#' @param updateProgress progress bar function passed to doaggregate in shiny app
#' @param updateProgress_getblocks progress bar function passed to getblocksnearby in shiny app
#' @param in_shiny if fips parameter is used, passed to [getblocksnearby_from_fips()]
#' @param quiet Optional. set to TRUE to avoid message about using getblock_diagnostics(),
#'   which is relevant only if a user saved the output of this function.
#' @param parallel whether to use parallel processing in getblocksnearby() but may not be implemented yet. 
#' @param silentinteractive   to prevent long output showing in console in RStudio when in interactive mode,
#'   passed to doaggregate() also. app server sets this to TRUE when calling doaggregate() but 
#'   ejamit() default is to set this to FALSE when calling doaggregate(). 
#' @param called_by_ejamit Set to TRUE by ejamit() to suppress some outputs even if ejamit(silentinteractive=F)
#' @param testing used while testing this function
#' 
#' @return A list of tables of results
#' 
#' @examples 
#'  # All in one step, using functions not shiny app:
#'  out <- ejamit(testpoints_100_dt, 2)
#'  
#'  \dontrun{
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
#'   # out2 <- ejscreenit(testpoints_5)
#'   
#'   # View results overall
#'   round(t(out$results_overall), 3.1)
#'   
#'   # View plots
#'   # plot_distance_avg_by_group(out)  
#'   # distance_by_group_plot(out)
#'   
#'   # View maps
#'   mapfast(out$results_bysite, radius = 3.1)
#'   
#'   # view results at a single site
#'   t(out$results_bysite[1, ])
#'   t(out$results_bysite[out$results_bysite$ejam_uniq_id == 2, ])
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
#' @seealso  [getblocksnearby()] [doaggregate()]
#' 
#' @export
#' 
ejamit <- function(sitepoints,
                   radius = 3,
                   maxradius = 31.07,
                   avoidorphans = FALSE,
                   quadtree = NULL,
                   fips = NULL,
                   shapefile_folder = NULL,                   
                   countcols = NULL,
                   popmeancols = NULL,
                   calculatedcols = NULL,
                   subgroups_type = "nh",   
                   include_ejindexes = FALSE,
                   calculate_ratios = TRUE,
                   extra_demog = TRUE,
                   need_proximityscore = FALSE,
                   infer_sitepoints = FALSE,
                   need_blockwt = TRUE,
                   threshold1 = 90, # threshold.default['comp1']
                   updateProgress = NULL,
                   updateProgress_getblocks = NULL,
                   in_shiny = FALSE,
                   quiet = TRUE,
                   parallel = FALSE,
                   silentinteractive = FALSE,
                   called_by_ejamit = TRUE,
                   testing = FALSE
) {
  
  #  1. getblocksnearby() ####
  
  ######################## #
  
  ## get blocks in POLYGONS / SHAPEFILES ####
  
  if (!is.null(shapefile_folder)) {
    shp <- shapefile_from_folder(folder = shapefile_folder, cleanit = TRUE)  
    if (!missing(radius)) {
      # add buffers around the polygons
      if (!silentinteractive) {cat('Adding buffer around each polygon.\n')}
      shp <- shape_buffered_from_shapefile(shapefile = shp, radius.miles = radius) # default crs
    }
    if (!silentinteractive) {cat('Finding blocks whose internal points are inside each polygon.\n')}
    mysites2blocks <- (get_blockpoints_in_shape(shp))$pts
    if (!silentinteractive) {cat('Aggregating at each polygon and overall.\n')}
    out <- suppressWarnings(
      doaggregate(
        sites2blocks = mysites2blocks,
        radius = radius,  #  
        countcols = countcols,
        popmeancols = popmeancols,
        calculatedcols = calculatedcols,
        subgroups_type = subgroups_type,
        include_ejindexes = include_ejindexes,
        calculate_ratios = calculate_ratios,
        extra_demog = extra_demog,
        need_proximityscore = need_proximityscore,
        infer_sitepoints = FALSE,
        called_by_ejamit = called_by_ejamit,
        updateProgress = updateProgress,
        silentinteractive = silentinteractive,
        testing = testing
      )
    )
    
  } else {
    
    if (!is.null(fips)) {
      ######################## #    
      
      ##  get blocks in FIPS  ####
      
      # getblocksnearby_from_fips() should include doing something like fips_lead_zero() ? 
      # but also want to know what type each fips is (probably all should be same like all are tracts or all are county fips)
      
      radius <- 999 # use this value when analyzing by fips not by circular buffers.
      if (!silentinteractive) {cat('Finding blocks in each FIPS Census unit.\n')}
      mysites2blocks <- getblocksnearby_from_fips(
        fips = fips,
        inshiny = inshiny,
        need_blockwt = need_blockwt
      )
      if (nrow(mysites2blocks) == 0) {
        return(NULL)
      }
      # this should have site = each FIPS code (such as each countyfips), and otherwise same outputs as getblocksnearby()
      if (!silentinteractive) {cat('Aggregating at each FIPS Census unit and overall.\n')}
      out <- suppressWarnings(
        doaggregate(
          sites2blocks = mysites2blocks,
          # sites2states_or_latlon = unique(mysites2blocks[ , .(ejam_uniq_id, lat, lon)]),
          radius = radius,  # use artificially large value when analyzing by fips 
          countcols = countcols,
          popmeancols = popmeancols,
          calculatedcols = calculatedcols,
          
          subgroups_type = subgroups_type,
          include_ejindexes = include_ejindexes,
          calculate_ratios = calculate_ratios,
          extra_demog = extra_demog,
          need_proximityscore = need_proximityscore,
          infer_sitepoints = FALSE,
          called_by_ejamit = called_by_ejamit,
          updateProgress = updateProgress,
          silentinteractive = silentinteractive,
          testing = testing
        )
      )
    }
    if (is.null(fips)) {
      ######################## #
      
      ## get blocks near LAT/LON POINTS  ####
      
      if (missing(radius)) {warning(paste0("Using default radius of ", radius, " miles because not provided as parameter."))}
      if (!missing(quadtree)) {warning("quadtree should not be provided to ejamit() - that is handled by getblocksnearby() ")}
      
      ################################################################################## #
      # note this overlaps or duplicates code in app_server.R 
      #   for data_up_latlon() around lines 81-110 and data_up_frs() at 116-148
      
      # select file
      if (missing(sitepoints)) {
        if (interactive() & !silentinteractive & !in_shiny) {
          sitepoints <- rstudioapi::selectFile(caption = "Select xlsx or csv with FIPS column of Census fips values", path = '.' )
          if (missing(radius)) {
            radius <- askradius(default = radius) # also see  askYesNo()
            # radius <- as.numeric(rstudioapi::showPrompt("Radius", "Within how many miles of each point?", 3))
          }
        } else {
          if (shiny::isRunning()) {
            warning("sitepoints (locations to analyze) is missing but required.")
            return(NULL)
            
          } else {
            stop("sitepoints (locations to analyze) is missing but required.")
          }
        }
      }
      # If user entered a table, path to a file (csv, xlsx), or whatever, then read it to get the lat lon values from there
      #  by using sitepoints <- latlon_from_anything(sitepoints) which gets done by getblocksnearby() 
      ################################################################################## #
      
      if (!silentinteractive) {cat('Finding blocks nearby.\n')}
      
      mysites2blocks <- getblocksnearby(
        sitepoints = sitepoints,
        radius = radius, 
        maxradius = maxradius,
        avoidorphans = avoidorphans,
        # quadtree = localtree,
        quiet = quiet,
        parallel = parallel,
        updateProgress = updateProgress_getblocks
        # report_progress_every_n = 500  # would be passed through to getblocksnearbyviaQuadTree()
      )
      ################################################################################## #
      
      # 2. doaggregate() ####
      
      if (!silentinteractive) {cat('Aggregating at each buffer and overall.\n')}
      
      out <- suppressWarnings(
        doaggregate(
          sites2blocks = mysites2blocks,
          sites2states_or_latlon = sitepoints, # sites2states_or_latlon = unique(x[ , .(ejam_uniq_id, lat, lon)]))
          radius = radius,
          countcols = countcols,
          popmeancols = popmeancols,
          calculatedcols = calculatedcols,
          subgroups_type = subgroups_type,
          include_ejindexes = include_ejindexes,
          calculate_ratios = calculate_ratios,
          extra_demog = extra_demog,
          need_proximityscore = need_proximityscore,
          infer_sitepoints = infer_sitepoints,
          called_by_ejamit = called_by_ejamit,
          updateProgress = updateProgress,
          silentinteractive = silentinteractive,
          testing = testing
        )
      )
      # provide sitepoints table provided by user aka data_uploaded(), (or could pass only lat,lon and ST -if avail- not all cols?)
      # and doaggregate() decides where to pull ST info from - 
      # ideally from ST column, 
      # second from fips of block with smallest distance to site, 
      # third from lat,lon of sitepoints intersected with shapefile of state bounds
      
      
    }
  }
  # end of lat lon vs FIPS vs shapefile
  
  ################################################################ # 
  
  # 2b. add  HYPERLINKS  to output (to site by site table) ####
  
  # ( doaggregate does not provide this   )
  
  #  >this should be a function  and is used by both server and ejamit() ####
  # duplicated almost exactly in app_server (near line 1217) but uses reactives there. *** except this has been updated here to handle FIPS not just latlon analysis.
  # #  Do maybe something like this:
  # links <- url_4table(lat=out$results_bysite$lat, lon=out$results_bysite$lon, radius = radius, 
  #    regid=ifelse("REGISTRY_ID" %in% names(out$results_bysite), out$results_bysite$REGISTRY_ID, NULL))
  # out$results_bysite[ , `:=`(links$results_bysite)] # would that work??? how to avoid big cbind step to add the new columns?
  # out$results_overall <- cbind(out$results_overall, links$results_overall) # 
  # setcolorder(out$results_bysite, neworder = links$newcolnames)
  # setcolorder(out$results_overall, neworder = links$newcolnames)
  # out$longnames <- c(newcolnames, links$longnames)
  
  if ("REGISTRY_ID" %in% names(out$results_bysite)) {
    echolink = url_echo_facility_webpage(REGISTRY_ID, as_html = T)
  } else {
    echolink = rep(NA, nrow(out$results_bysite))
  }
  
  if (!is.null(fips)) {
    # analyzing by FIPS not lat lon values
    areatype <- fipstype(fips)
    if (!(all(areatype %in% c("blockgroup", "tract", "city", "county")))) {warning("FIPS must be one of 'blockgroup', 'tract', 'city', 'county' for the EJScreen API")}
    out$results_bysite[ , `:=`(
      `EJScreen Report` = url_ejscreen_report(   areaid   = fips, areatype = areatype, as_html = T),
      `EJScreen Map`    = url_ejscreenmap(       wherestr = fips, as_html = T),
      `ECHO report` = echolink
    )]
  } else {
    out$results_bysite[ , `:=`(
      `EJScreen Report` = url_ejscreen_report(    lat = out$results_bysite$lat, lon = out$results_bysite$lon, radius = radius, as_html = T),
      `EJScreen Map`    = url_ejscreenmap(        lat = out$results_bysite$lat, lon = out$results_bysite$lon,                  as_html = T),
      `ECHO report` = echolink
    )]
  }
  if (NROW(out$results_bysite) == 1) {
    # If we analyzed only 1 place then overall is same as 1 site per row!
    out$results_overall[ , `:=`(
      `EJScreen Report` = out$results_bysite$`EJScreen Report`,   #  rep(NA,nrow(out$results_bysite)),
      `EJScreen Map`    = out$results_bysite$`EJScreen Map`,    # rep(NA,nrow(out$results_bysite)),
      `ECHO report`     = out$results_bysite$`ECHO report`     # rep(NA,nrow(out$results_bysite))
    )]
  } else {
    out$results_overall[ , `:=`(
      `EJScreen Report` = NA,   #  rep(NA,nrow(out$results_bysite)),
      `EJScreen Map`    = NA,    # rep(NA,nrow(out$results_bysite)),
      `ECHO report`     = NA     # rep(NA,nrow(out$results_bysite))
    )]
  }
  
  newcolnames <- c(
    "EJScreen Report",
    "EJScreen Map",
    "ECHO report")
  # put those up front as first columns
  data.table::setcolorder(out$results_bysite,  neworder = newcolnames)
  data.table::setcolorder(out$results_overall, neworder = newcolnames)
  out$longnames <- c(newcolnames, out$longnames)
  
  ################################################################ # 
  
  # 2c. add  RADIUS  to output (in server and in ejamit() ####
  
  # ( doaggregate does not provide this ? )
  if (!is.null(fips)) {
    # Analyzed by FIPS so reporting a radius does not make sense here.
    radius <- NA
  }
  out$results_bysite[      , radius.miles := radius]
  out$results_overall[     , radius.miles := radius]
  out$results_bybg_people[ , radius.miles := radius] # do not really need to export to excel though, and most users do not need this large table
  
  ################################################################ # 
  
  # 3. batch.summarize()**  ####
  
  # For each indicator, calc AVG and PCTILES, across all SITES and all PEOPLE 
  
  # (doaggregate does not provide this)
  
  out$results_summarized <- EJAMbatch.summarizer::batch.summarize(   # disabled only in ejam lite package ***
    sitestats = data.frame(out$results_bysite),
    popstats =  data.frame(out$results_bysite),
    ## user-selected quantiles to use
    #probs = as.numeric(input$an_list_pctiles),
    threshold = list(threshold1) # compare variables to 90 or other  %ile
  )
  
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
  if (interactive() & !silentinteractive & !in_shiny) {
    
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
    
    # cat("To see barplots of average proximity by demog group:\n\n",
    #     '     plot_distance_mean_by_group(out$results_bybg_people)',
    #     "\n\n")
    
    cat("To see bar or boxplots of ratios of %Demographics vs US averages:\n\n", 
        "     ?plot_barplot_ratios() in EJAM package # or \n",
        "     ?boxplots_ratios() in EJAMejscreenapi package\n",
        "     boxplots_ratios(ratios_to_avg(as.data.frame(out$results_bysite))$ratios_d)",
        "\n\n")
    
    cat("To see a map in RStudio: \n\n",
        "     mapfast(out$results_bysite, radius = out$results_overall$radius.miles, column_names = 'ej')", 
        "\n\n")
    
    cat("To view or save as excel files, see ?table_xls_from_ejam e.g., table_xls_from_ejam(out, fname = 'out.xlsx')  \n\n")
    cat('Output is a list with the following names:\n')
    print(EJAM:::structure.of.output.list(out) )
  }
  ################################################################ # 
  
  
  invisible(out)
}

