

#' Get an EJ analysis (demographic and environmental indicators) in or near a list of locations
#'
#' @description This is the main function in EJAM that runs the analysis.
#'   It does essentially what the web app does, to analyze/summarize near a set of points,
#'   or in a set of polygons from a shapefile, or in a list of Census Units like Counties.
#' @details See examples in vignettes/ articles at https://usepa.github.io/EJAM/index.html
#' 
#' @param sitepoints data.table with columns lat, lon giving point locations of sites or facilities around which are circular buffers
#' @param radius in miles, defining circular buffer around a site point
#' @param radius_donut_lower_edge radius of lower edge of donut ring if analyzing a ring not circle
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
#' @param shapefile optional. A sf shapefile object or path to .zip, .gdb, or folder that has a shapefiles, to analyze polygons.
#'   If in RStudio you want it to interactively prompt you to pick a file, 
#'   use shapefile=1 (otherwise it assumes you want to pick a latlon file).
#' @param countcols character vector of names of variables to aggregate within a buffer using a sum of counts,
#'   like, for example, the number of people for whom a poverty ratio is known,
#'   the count of which is the exact denominator needed to correctly calculate percent low income.
#' @param popmeancols character vector of names of variables to aggregate within a buffer using population weighted mean.
#' @param calculatedcols character vector of names of variables to aggregate within a buffer using formulas that have to be specified.
#' @param subgroups_type Optional (uses default). Set this to "nh" for non-hispanic race subgroups
#'   as in Non-Hispanic White Alone, nhwa and others in names_d_subgroups_nh;
#'   "alone" for race subgroups like White Alone, wa and others in names_d_subgroups_alone;
#'   "both" for both versions. Possibly another option is "original" or "default"
#'   Alone means single race.
#' @param include_ejindexes whether to try to include EJ Indexes (assuming dataset is available) - passed to [doaggregate()]
#' @param calculate_ratios whether to calculate and return ratio of each indicator to US and State overall averages - passed to [doaggregate()]
#' @param extra_demog if should include more indicators from v2.2 report on language etc.
#' @param need_proximityscore whether to calculate proximity scores
#' @param infer_sitepoints set to TRUE to try to infer the lat,lon of each site around which the blocks in sites2blocks were found.
#'   lat,lon of each site will be approximated as average of nearby blocks,
#'   although a more accurate slower way would be to use reported distance of each of 3 of the furthest block points and triangulate
#' @param need_blockwt if fips parameter is used, passed to [getblocksnearby_from_fips()]
#' 
#' @param thresholds list of percentiles like list(80,90) passed to 
#'   batch.summarize(), to be 
#'   counted to report how many of each set of indicators exceed thresholds
#'   at each site. (see default)
#' @param threshnames list of groups of variable names (see default)
#' @param threshgroups list of text names of the groups (see default)
#'   
#' @param updateProgress progress bar function passed to [doaggregate()] in shiny app
#' @param updateProgress_getblocks progress bar function passed to [getblocksnearby()] in shiny app
#' @param in_shiny if fips parameter is used, passed to [getblocksnearby_from_fips()]
#' @param quiet Optional. set to TRUE to avoid message about using [getblocks_diagnostics()],
#'   which is relevant only if a user saved the output of this function.
#' @param parallel whether to use parallel processing in [getblocksnearby()], but not implemented yet.
#' @param silentinteractive   to prevent long output showing in console in RStudio when in interactive mode,
#'   passed to [doaggregate()] also. app server sets this to TRUE when calling doaggregate() but
#'   [ejamit()] default is to set this to FALSE when calling [doaggregate()].
#' @param called_by_ejamit Set to TRUE by [ejamit()] to suppress some outputs even if ejamit(silentinteractive=F)
#' @param testing used while testing this function
#' @param ... passed to [getblocksnearby()] etc. such as  report_progress_every_n = 0
#' 
#' @return A list of tables of results
#'
#' @examples
#' 
#' # See examples in vignettes/ articles at https://usepa.github.io/EJAM/index.html
#' 
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
#'   testsites <- testpoints_1000
#'   # use facility points in an excel or csv file
#'    testsites <- latlon_from_anything(
#'      system.file(paste0("testdata/latlon/",
#'       "testpoints_10.xlsx"),
#'     package = "EJAM")
#'     )
#'    # head(testsites)
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
#'    plot_distance_by_group(results_bybg_people = out$results_bybg_people)
#'    distance_by_group(out$results_bybg_people)
#'
#'   # View maps
#'   mapfast(out$results_bysite, radius = 3.1)
#'
#'   # view results at a single site
#'   mapfast(out$results_bysite, radius = 3.1)
#'   # all the raw numbers at one site
#'   t(out$results_bysite[1, ])
#'
#'   # if doing just 1st step of ejamit()
#'   #  get distance between each site and every nearby Census block
#'
#'   s2b <- testoutput_getblocksnearby_100pts_1miles
#'   getblocks_diagnostics(s2b)
#'
#'   testsites <- testpoints_10[2,]
#'   s2b <- getblocksnearby(testsites, radius = 3.1)
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
                   radius_donut_lower_edge = 0,
                   maxradius = 31.07,
                   avoidorphans = FALSE,
                   quadtree = NULL,
                   fips = NULL,
                   shapefile = NULL,
                   countcols = NULL,
                   popmeancols = NULL,
                   calculatedcols = NULL,
                   subgroups_type = "nh",
                   include_ejindexes = TRUE,
                   calculate_ratios = TRUE,
                   extra_demog = TRUE,
                   need_proximityscore = FALSE,
                   infer_sitepoints = FALSE,
                   need_blockwt = TRUE,
                   
                   thresholds = list(90, 90),
                   threshnames = list(c(names_ej_pctile, names_ej_state_pctile), 
                                      c(names_ej_supp_pctile, names_ej_supp_state_pctile)),
                   threshgroups = list("EJ-US-or-ST", "Supp-US-or-ST"),
                   
                   updateProgress = NULL,
                   updateProgress_getblocks = NULL,
                   in_shiny = FALSE,
                   quiet = TRUE,
                   parallel = FALSE,
                   silentinteractive = FALSE,
                   called_by_ejamit = TRUE,
                   testing = FALSE,
                   ...
) {
  
  sitetype <- ejamit_sitetype_check(sitepoints = sitepoints, fips = fips, shapefile = shapefile)
  if (interactive() && !silentinteractive) {print(sitetype)}
  
  # * POLYGONS / SHAPEFILES ####
  
  if (sitetype == "shp") {
    
    ## . check shp ####
    
    shp <- shapefile_from_any(shapefile, cleanit = FALSE) # if user entered ejamit(shapefile=1), e.g., it will prompt for an actual file.
    
    # made this very much like code in server: ***
    
    if (is.null(shp)) {
      # disable_buttons[['SHP']] <- TRUE                                # a reactiveValues object
      # shiny::validate("Uploaded file should contain the following file extensions: shp,shx,dbf,prj or json or zip")
      stop("file should contain the following file extensions: shp,shx,dbf,prj or json or zip")
    }
    
    # if shp contains point features, present message in app
    if (any(sf::st_geometry_type(shp) == "POINT")) {
      # disable_buttons[['SHP']] <- TRUE                                # a reactiveValues object
      # shiny::validate("Shape file must be of polygon geometry.")
      stop("Shape file must be of polygon geometry.") 
    }
    
    # Drop Z and/or M dimensions from feature geometries, resetting classes appropriately
    shp <- sf::st_zm(shp)
    
    # standardize colname to "geometry" since standard name not always seen. Can be "Shape" for example: shp <- shapefile_from_any(system.file('testdata/shapes/portland.gdb.zip', package = "EJAM"))  
    if (any(grepl("sfc", lapply(shp, class)))) {
      colnames(shp)[grepl("sfc", lapply(shp, class))] <- "geometry"
      st_geometry(shp) <- "geometry"
    }
    
    ## check if shp is valid, and
    ## add "valid" and "invalid_msg" columns re invalid rows/polygons
    
    if (nrow(shp) > 0) {
      ## terra provides faster valid check than sf
      shp_valid_check <- terra::is.valid(terra::vect(shp), messages = T)
      shp_is_valid <- shp_valid_check$valid
      numna <- sum(!shp_is_valid)
      warning(paste0(numna, "invalid polygons"))
      # num_valid_pts_uploaded[['SHP']] <- length(shp_is_valid) - sum(!shp_is_valid)    # a reactiveValues object
      # invalid_alert[['SHP']] <- numna                                                 # a reactiveValues object 
      ##shp_valid <- shp[sf::st_is_valid(shp),] # old way to check which shapes valid 
      shp_valid <- dplyr::mutate(shp, siteid = dplyr::row_number())
      shp_proj <- sf::st_transform(shp_valid,crs = 4269)
    } else {
      ## zero polygons found
      # invalid_alert[['SHP']] <- 0  # hides invalid site warning                       # a reactiveValues object
      # an_map_text_shp(HTML(NULL))  # hides the count of uploaded sites/shapes         # a reactiveValues object
      # disable_buttons[['SHP']] <- TRUE                                                # a reactiveValues object
      ## if not matched, return this message
      # shiny::validate('No shapes found in file uploaded.')
      stop("No shapes found in file")
    }
    # disable_buttons[['SHP']] <- FALSE                                                 # a reactiveValues object
    shp_proj$valid <- shp_is_valid
    
    if (!"ejam_uniq_id" %in% names(shp)) { # this should allow ejamit_compare_types_of_places() to work when it passes only a subset of rows here
      shp_proj <- cbind(ejam_uniq_id = 1:nrow(shp_proj), shp_proj)    #  UNIQUE ID HERE
    }
    shp_proj$invalid_msg <- NA
    shp_proj$invalid_msg[shp_proj$valid == F] <- shp_valid_check$reason[shp_proj$valid == F]
    shp_proj$invalid_msg[is.na(shp_proj$geometry)] <- 'bad geometry'
    class(shp_proj) <- c(class(shp_proj), 'data.table')
    
    shp <- shp_proj
    rm(shp_proj)
    shp_valid <- shp[shp$valid, ]
    
    ##################################### #
    
    # . radius (buffer) for polygons ####
    if (missing(radius)) {
      if (interactive() && !silentinteractive && !in_shiny && rstudioapi::isAvailable()) {
        radius <- askradius(default = 0, message = "To add a buffer around each polygon, enter distance in miles. For none, use 0.")
      } else {
        radius = 0 # default is no buffer around polygon
      }
    }
    if (!is.null(radius[1]) && radius[1] > 0) {
      if (!silentinteractive) {cat('Adding buffer around each polygon.\n')}
      shp_valid <- shape_buffered_from_shapefile(shapefile = shp_valid, radius.miles = radius) # default crs ok here? ***
    }
    ##################################### #
    if (!silentinteractive) {cat('Finding blocks whose internal points are inside each polygon.\n')}

    # . get_blockpoints_in_shape() ####
    
    mysites2blocks <- get_blockpoints_in_shape(shp_valid)$pts
    
    # . doaggregate shp ####
    
    if (!silentinteractive) {cat('Aggregating at each polygon and overall.\n')}
    
    sites2states_or_latlon <- mysites2blocks # ??? # data.frame(ejam_uniq_id = shp_valid$ejam_uniq_id, ST = NA)  # check this
    
    out <- suppressWarnings(
      
      doaggregate(
        
        sites2blocks = mysites2blocks,
        sites2states_or_latlon = sites2states_or_latlon,  # in server, this seems to be same as mysites2blocks ???
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
    
    ################################################################ #
    
    ## Handle sites dropped during getblocksnearby or doaggregate steps
    dup <- shp  # all rows, including invalid ones
    dup$invalid_msg[is.na(dup$invalid_msg) & !(dup$ejam_uniq_id %in% mysites2blocks$ejam_uniq_id)] <- 'no blocks found nearby'
    dup$valid <- dup$ejam_uniq_id %in% mysites2blocks$ejam_uniq_id
    dup$invalid_msg[is.na(dup$invalid_msg) & !(dup$ejam_uniq_id %in% out$results_bysite$ejam_uniq_id)] <- 'unable to aggregate'
    dup$valid <- dup$ejam_uniq_id %in% out$results_bysite$ejam_uniq_id
    
    out$results_bysite <- merge(dup[, c('ejam_uniq_id','valid','invalid_msg')],
                                out$results_bysite,
                                by = 'ejam_uniq_id', all = T) %>%
      sf::st_drop_geometry()
    
  }
  ######################## #
  
  if (sitetype == "fips") {
    
    # * FIPS  ####
    
    ## . check fips ####  
    
    # getblocksnearby_from_fips() should include doing something like fips_lead_zero() ?
    # but also want to know what type each fips is (probably all should be same like all are tracts or all are county fips)
    
    ## . radius is ignored for fips ####
    radius <- 999 # use this value when analyzing by fips not by circular buffers, as input to doaggregate(),
    # then in output of doaggregate()$results_bysite$radius.miles is returned as 0 for every fips, as in _overall. 
    if (!missing(radius) && radius != 0 && radius != 999) {warning("radius was specified, but will be ignored as irrelevant when analyzing fips")}
    
    ## . getblocksnearby_from_fips() ####
    
    if (!silentinteractive) {cat('Finding blocks in each FIPS Census unit.\n')}
    
    mysites2blocks <- getblocksnearby_from_fips(
      
      fips = fips,  # these get retained as ejam_uniq_id for the fips case.
      inshiny = inshiny,
      need_blockwt = need_blockwt
    )
    if (nrow(mysites2blocks) == 0) {
      return(NULL)
    }
    # this should have site = each FIPS code (such as each countyfips), 
    # no lat,lon columns,
    # and otherwise same outputs as getblocksnearby()
    
    ## . doaggregate  fips ####
    
    if (!silentinteractive) {cat('Aggregating at each FIPS Census unit and overall.\n')}
    # fipshere <- fips # ?? in case any got dropped during getblocks..., but cant create state pctiles for those anyway.
    fipshere <- unique(mysites2blocks$ejam_uniq_id)
    sites2states_or_latlon <- data.table(ejam_uniq_id = fipshere, ST = fips2state_abbrev(fipshere))
    setkey(sites2states_or_latlon, ejam_uniq_id)
    
    out <- suppressWarnings(
      
      doaggregate(
        
        sites2blocks = mysites2blocks,
        sites2states_or_latlon = sites2states_or_latlon,  
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
  ######################## #
  
  if (sitetype == "latlon") {
    
    # * LAT/LON POINTS ####
    
    ## . getblocksnearby() ####

    ################################################################################## #
    # note this overlaps or duplicates code in app_server.R   for data_up_latlon()  and data_up_frs() 
    
    ## . check pts ####
    if (missing(sitepoints)) {
      if (interactive() && !silentinteractive && !in_shiny && rstudioapi::isAvailable()) {
        sitepoints <- rstudioapi::selectFile(caption = "Select xlsx or csv with columns lat, lon (or synonyms)", path = '.' )
        # that returns the path to the file
        sitepoints <- latlon_from_anything(sitepoints) # in rstudio interactively read file and infer colnames with lat lon
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
    # if user entered a table, path to a file (csv, xlsx), or whatever, then read it to get the lat lon values from there
    
    sitepoints <- sitepoints_from_anything(anything = sitepoints) # adds ejam_uniq_id column if it is missing. if present and not 1:N, warns but leaves it that way so ejamit_compare_types_of_places() can work.
    stopifnot(is.data.frame(sitepoints), "lat" %in% colnames(sitepoints), "lon" %in% colnames(sitepoints), NROW(sitepoints) >= 1, is.numeric(sitepoints$lat))
    
    ###   *** should we drop all columns other than lat,lon,ejam_uniq_id ? ST? user might have provided a huge number of columns/ waste of memory.
    
    ##################################### #
    ## . radius ####
    if (missing(radius)) {
      if (interactive() && !silentinteractive && !in_shiny && rstudioapi::isAvailable()) {
        radius <- askradius(default = radius)
      } else {
        if (shiny::isRunning()) {
          # this should never arise - should have been set by app_server.R and ejamit was not being used except in fips case by server anyway
          warning(paste0("Using default radius of ", radius, " miles because not provided as parameter."))
          radius <- radius
        } else {
          # notshiny.  noninteractive or silent so don't prompt.
          warning(paste0("Using default radius of ", radius, " miles because not provided as parameter."))
          radius <- radius
        }
      }
    }
    ##################################### #
    
    ## . getblocksnearby() ####
    
    if (!missing(quadtree)) {warning("quadtree should not be provided to ejamit() - that is handled by getblocksnearby() ")}
    
    ################################################################################## #
    if (!silentinteractive) {cat('Finding blocks nearby.\n')}
    
    mysites2blocks <- getblocksnearby(
      
      sitepoints = sitepoints,
      radius = radius,
      radius_donut_lower_edge = radius_donut_lower_edge,
      maxradius = maxradius,
      avoidorphans = avoidorphans,
      # quadtree = localtree,
      quiet = quiet,
      parallel = parallel,
      updateProgress = updateProgress_getblocks,
      ...  #  could provide report_progress_every_n = 500  # would be passed through to getblocksnearbyviaQuadTree()
    )
    ################################################################################## #
    
    ## . doaggregate pts ####
    
    if (!silentinteractive) {cat('Aggregating at each site and overall.\n')}
    
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
    
    ################################################################ #
    
    ## Handle sites dropped during getblocksnearby or doaggregate steps
    dup <- sitepoints
    dup$invalid_msg[is.na(dup$invalid_msg) & !(dup$ejam_uniq_id %in% sites2blocks$ejam_uniq_id)] <- 'no blocks found nearby'
    dup$valid <- dup$ejam_uniq_id %in% sites2blocks$ejam_uniq_id
    dup$invalid_msg[is.na(dup$invalid_msg) & !(dup$ejam_uniq_id %in% out$results_bysite$ejam_uniq_id)] <- 'unable to aggregate'
    dup$valid <- dup$ejam_uniq_id %in% out$results_bysite$ejam_uniq_id
    
    
    ## merge valid and invalid places (to add those back into results, as mostly empty rows; but already done above for FIPS case)
    
    out$results_bysite <- merge(dup[, .(ejam_uniq_id, valid, invalid_msg)],
                                out$results_bysite,
                                by = 'ejam_uniq_id', all = T)
  }
  # end of lat lon vs FIPS vs shapefile
  
  ################################################################ #
  # ~ ####
  
  # * Invalid sites? ####
  ### _>>> differs from server code ! *** ####  
  
  # Add rows with invalid sites & cols with valid flag/msg
  # Handles sites dropped during getblocksnearby or doaggregate steps or with no data (zero pop)
  # dup here means "data uploaded" or data user provided" 
  if (sitetype == "latlon") {
    dup <- data.frame(sitepoints) # latlon. already has ejam_uniq_id
  }
  if (sitetype == "fips") {
    dup <- data.frame(fips = fips, ejam_uniq_id = as.character(fips)) # for merge or join below to work, must match class (integer vs character) of output of doaggregate() and before that output of getblocksnearby_from_fips(fips_counties_from_state_abbrev('DE'))  #  1:length(fips)) 
  }
  if (sitetype == "shp") {
    dup <- sf::st_drop_geometry(shp)[, unique(c(1:2, which(names(shp) == "ejam_uniq_id")))] # any 1 or 2 plus id to make sure it isn't just 1 col and drop=T happens
    # # shp already had ejam_uniq_id
  }
  
  site_in_blocksfound  <- dup$ejam_uniq_id %in% mysites2blocks$ejam_uniq_id
  site_in_results      <- dup$ejam_uniq_id %in% out$results_bysite$ejam_uniq_id
  site_in_results_pop0 <- dup$ejam_uniq_id %in% out$results_bysite$ejam_uniq_id[out$results_bysite$pop == 0]
  
  dup$valid <- site_in_results & !site_in_results_pop0
  dup$invalid_msg <- ""
  dup$invalid_msg[!site_in_blocksfound] <- "no blocks found nearby"
  dup$invalid_msg[site_in_blocksfound & !site_in_results] <- "no results found" # never happens? how can doaggregate drop it entirely if it was in mysites2blocks ?
  dup$invalid_msg[site_in_results_pop0] <- "blocks found but zero residents - unable to aggregate" # msg differs from server version
  
  # Add invalid sites and invalid flag/msg to results, so output has ALL sites, even those dropped by getblocks (or somehow dropped by just doaggregate)
  setDT(dup)
  out$results_bysite <- merge(dup[, .(ejam_uniq_id, valid, invalid_msg)],
                              out$results_bysite,
                              by = 'ejam_uniq_id', all = T)
  setorder(out$results_bysite, ejam_uniq_id)
  ################################################################ #
  
  # * Hyperlinks ####
  
  ##  _>>> should use url_4table() ! *** in server & ejamit ####
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
    echolink = rep(NA, NROW(out$results_bysite))
  }
  
  if (sitetype == "fips") {
    
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
      `EJScreen Report` = out$results_bysite$`EJScreen Report`,
      `EJScreen Map`    = out$results_bysite$`EJScreen Map`,
      `ECHO report`     = out$results_bysite$`ECHO report`
    )]
  } else {
    out$results_overall[ , `:=`(
      `EJScreen Report` = NA,
      `EJScreen Map`    = NA,
      `ECHO report`     = NA
    )]
  }
  # placeholders just so colnames are more consistent with other tables
  #  (but still missing pctiles, ratios, and averages)
  out$results_bybg_people[ , `:=`(
    `EJScreen Report` = NA, 
    `EJScreen Map`    = NA, 
    `ECHO report`     = NA  
  )]
  newcolnames <- c(
    "EJScreen Report",
    "EJScreen Map",
    "ECHO report")
  # put those up front as first columns
  data.table::setcolorder(out$results_bysite,  neworder = newcolnames)
  data.table::setcolorder(out$results_overall, neworder = newcolnames)
  data.table::setcolorder(out$results_bybg_people, neworder = newcolnames)
  out$longnames <- c(newcolnames, out$longnames)
  ################################################################ #
  
  if (sitetype == "fips") {
    # Analyzed by FIPS so reporting a radius does not make sense here.
    radius <- NA
  }
  # ( doaggregate already provided this but ok to do again)
  out$results_bysite[      , radius.miles := radius]
  out$results_overall[     , radius.miles := radius]
  out$results_bybg_people[ , radius.miles := radius]
  ################################################################ #
  
  # * batch.summarize()   ####
  
  # For each indicator, calc AVG and PCTILES, across all SITES and all PEOPLE
  
  out$results_summarized <- batch.summarize(
    sitestats = data.frame(out$results_bysite),
    # popstats =  data.frame(out$results_bysite), # now does not have to get passed twice
    quiet = quiet,
    ## user-selected quantiles to use
    #probs = as.numeric(input$an_list_pctiles),
    thresholds = thresholds, # list(90, 90),
    threshnames = threshnames, # list(c(names_ej_pctile, names_ej_state_pctile), c(names_ej_supp_pctile, names_ej_supp_state_pctile)),
    threshgroups = threshgroups # list("EJ-US-or-ST", "Supp-US-or-ST")
  )
  ################################################################ #
  
  # * table_tall_from_overall() ####
  
  out$formatted <- table_tall_from_overall(out$results_overall, out$longnames)
  
  ###################################### #
  if (interactive() & !silentinteractive & !in_shiny) {
    
    #* show summary in RStudio ####
    # and sites table in viewer pane
    if (nrow(out$results_bysite) > 1000) {message("> 1,000 rows may be too much for client-side DataTables - only showing some rows here")}
    # ejam2tableviewer(out)
    ###################################### #
    print(EJAM:::structure.of.output.list(out, objectname = "Output of ejamit()"))
    cat("\nWays to view or save results of  'x <- ejamit()'   \n Overall results: ejam2barplot(x); ejam2ratios(x); ejam2table_tall(x); ejam2report(x)\n Results by site: ejam2map(x); ejam2barplot_sites(x); ejam2tableviewer(x); ejam2excel(x); ejam2shapefile(x) \n\n")
  }
  ################################################################ #
  
  invisible(out)
}


