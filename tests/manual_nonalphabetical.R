# system.time({
#   ##    ABOUT 25 MINUTES TO RUN TESTS (if large datasets had not yet been loaded)
#   source("./tests/manual_nonalphabetical.R") #####
#     ## answering Yes to running ALL tests
# })
######################################################## #


#    test_interactively()


test_interactively = function(ask = TRUE, 
                              noquestions = TRUE, # just for shapefile folder selections
                              useloadall = TRUE,
                              
                              y_basic = FALSE, y_latlon=TRUE, y_shp=TRUE, y_fips = TRUE,
                              
                              y_runsome = FALSE, # if T, need to also create partial_testlist
                              tname = NULL, 
                              # c("test_fips", "test_naics", "test_frs", "test_latlon", "test_maps", 
                              # "test_shape", "test_getblocks", "test_fixcolnames", "test_doag", 
                              # "test_ejamit", "test_ejscreenapi", "test_mod", "test_app", "test_test")
                              
                              y_runall = TRUE,
                              
                              y_seeresults = TRUE,
                              y_save = TRUE,
                              y_tempdir = TRUE,
                              mydir = NULL
) {
  
  ########################################## # ########################################## # 
  if (missing(y_basic) & ask) {
    if (missing(y_basic)) {
      y_basic = askYesNo("Do ONLY basic quick checks (no unit tests, then STOP) ?", default = y_basic)
    }}
  if (is.na(y_basic)) {stop("cancelled")}
  if (y_basic) {
    if (missing(y_latlon) & ask) {y_latlon = askYesNo("quick tests for latlon?", default = y_latlon)}
    if (is.na(y_latlon)) {stop("cancelled")}
    if (missing(y_shp)    & ask) {y_shp    = askYesNo("quick tests for shp?",    default = y_shp)}
    if (is.na(y_shp))    {stop("cancelled")}
    if (missing(y_fips)   & ask) {y_fips   = askYesNo("quick tests for fips?",   default = y_fips)}
    if (is.na(y_fips))   {stop("cancelled")}
  }
  # if only doing basic non-unit-testing then do not ask about other details and do not find groups of test files, etc. - 
  #  just skip way ahead to load/library and do those quick checks
  
  if (!y_basic) {
    
    ########################################## # ########################################## # 
    # Setup ####
    # !diagnostics off ## to disable diagnostics in this document
    #        thisfile = "./tests/manual_nonalphabetical.R"
    # source(thisfile) to test the local source pkg, by group of functions, quietly, summarized
    # test_local()     to test the local source pkg
    # test_package()   to test installed version of package
    # test_check()     to test installed version of package, the way used by R CMD check or check()
    library(testthat)
    library(data.table) # used in functions here
    library(magrittr)
    library(dplyr)
    consoleclear <- function() {if (interactive() & rstudioapi::isAvailable()) {rstudioapi::executeCommand("consoleClear")}}
    consoleclear()
    
    ########################################## #
    
    ## Find the tests ####
    update_list_of_tests <- TRUE
    if (update_list_of_tests) {
      sdir <- getwd()
      test_files_found <-  basename(list.files(path = file.path(sdir, "tests/testthat"), full.names = TRUE, pattern = "test-"))
      ########################################## # 
      
      ## GROUP the tests ####
      
      testlist = list( 
        
        test_fips = c(
          # "test-fips_lead_zero.R",   
          # "test-fips_bg_from_anyfips.R",    #   test_file("tests/testthat/test-fips_bg_from_anyfips.R")
          "test-FIPS_FUNCTIONS.R",
          "test-state_from_fips_bybg.R",  
          "test-state_from_latlon.R"   
        ),
        test_naics = c(
          "test-naics_categories.R",   
          "test-naics_findwebscrape.R", 
          "test-naics_from_any.R",   
          "test-naics_from_code.R",  
          "test-naics_from_name.R",   
          "test-naics_subcodes_from_code.R",
          "test-naics_validation.R",  
          "test-naics2children.R"    
        ),
        test_frs = c(
          "test-regid_from_naics.R", 
          "test-frs_from_naics.R",    
          "test-frs_from_programid.R",  
          "test-frs_from_regid.R",  
          "test-frs_from_sic.R",   
          "test-frs_is_valid.R"   
        ),
        test_latlon = c(
          "test-latlon_infer.R",        
          "test-latlon_as.numeric.R",
          "test-latlon_df_clean.R",
          "test-latlon_is.valid.R",   
          "test-latlon_from_anything.R",   
          "test-latlon_from_sic.R",
          "test-address_xyz.R", 
          "test-latlon_from_address.R",
          "test-latlon_from_vectorofcsvpairs.R",
          "test-state_from_sitetable.R"
        ),
        test_maps = c(
          "test-MAP_FUNCTIONS.R"
        ),
        test_shape = c(
          "test-shapefile_xyz.R",
          "test-ejam2shapefile.R",
          "test-shapes_from_fips.R"
        ),
        test_getblocks = c(
          "test-radius_inferred.R",              # this is SLOW THOUGH
          "test-getblocks_summarize_blocks_per_site.R",  
          "test-getblocksnearby.R",        
          "test-getblocksnearby_from_fips.R", 
          "test-getblocksnearbyviaQuadTree.R"    #    -------------- NEEDS MORE TESTS ***
        ),
        test_fixcolnames = c(
          "test-fixcolnames.R",
          "test-fixnames.R",
          "test-fixnames_to_type.R",
          "test-fixcolnames_infer.R",
          "test-varinfo.R",
          "test-utils_metadata_add.R"
        ),
        test_doag = c(
          "test-pctile_from_raw_lookup.R",  
          "test-doaggregate.R"     
        ),
        test_ejamit = c(
          "test-ejamit.R",
          "test-ejam2barplot_sites.R",
          "test-ejamit_compare_distances.R",
          "test-ejamit_compare_types_of_places.R",
          "test-ejamit_sitetype_from_input.R",
          "test-ejamit_sitetype_from_output.R"
        ),
        test_ejscreenapi = c(
          "test-ejscreenapi.R",
          "test-ejscreenapi_plus.R",
          "test-ejscreenapi1.R",
          "test-ejscreenit.R",
          "test-ejscreenRESTbroker-functions.R"
        ),
        test_mod = c(
          "test-mod_save_report.R",    
          "test-mod_specify_sites.R",  
          "test-mod_view_results.R"    
        ),
        test_app = c(
          "test-report_residents_within_xyz.R",  # maybe belongs in a separate group about reports/tables?
          "test-ui_and_server.R",
          "test-FIPS-shiny-functionality.R", "test-latlon-shiny-functionality.R", "test-NAICS-shiny-functionality.R", "test-shapefile-shiny-functionality.R"
        ),
        test_test = c(
          "test-test.R"   #   fast way to check this script via  biglist <- test_interactively(ask = FALSE, y_runsome = T, tname = 'test')
        ),
        test_golem = c(
          "test-golem_utils_server.R", # not really used
          "test-golem_utils_ui.R"      # not really used
        )
      )
      ########################################## # 
      # groupnames <- names(testlist)
      test_all <- as.vector(unlist(testlist))
      ########################################## # 
      ## Check if all Grouped ####
      {
        
        if (!all(TRUE == all.equal(sort(test_all), sort(test_files_found)))) {
          cat("\n\n   test files found in folder does not match test_files_found list  \n")
          print(all.equal(sort(test_all), sort(test_files_found))) 
          cat("\n\n")
        }
        
        if (length(setdiff(test_files_found, test_all)) > 0) {
          cat("These are in test folder as files but not in list of groups above: \n\n")
          print(setdiff(test_files_found, test_all))
          cat("\n")
          stop("fix list of files")
        }
        
        if (length(setdiff(test_all, test_files_found)) > 0) {
          cat("These are in list of groups above but not in test folder as files: \n\n")
          print(setdiff(test_all, test_files_found))
          cat("\n")
          stop("fix list of test files")
        }
        
        if (any(duplicated(test_all))) {
          cat("some are listed >1 group\n")
          stop("some are listed >1 group")
        }
        
        cat("\n\n")
        ########################################## # 
      }
    } # end if, update_list_of_tests
    ## Define Functions that run tests ####
    {
      ##     BY GROUP, WITH SUCCINCT SUMMARY
      
      test1group <- function(fnames = test_all, 
                             reporter = "minimal", # some of the code below now only works if using this setting
                             # reporter = default_compact_reporter(), # 
                             print = FALSE
      ) {
        xtable <- list()
        # tfile <- tempfile("junk", fileext = "txt")
        timing = system.time({
          for (i in 1:length(fnames)) {
            cat(".")
            suppressWarnings(suppressMessages({
              junk <- testthat::capture_output_lines({
                x <- testthat::test_file(
                  file.path("./tests/testthat/", fnames[i]), 
                  reporter = reporter)
              }, print = print)
            }))
            # testthat_print(junk)
            x <- as.data.frame(x)
            x$tests <- x$nb
            x$nb <- NULL
            x$flag <- x$tests - x$passed
            x$err  <- x$tests - x$passed - x$warning
            x$error_cant_test <- ifelse(x$error, 1, 0)
            x$error <- NULL
            x$skipped <- ifelse(x$skipped, 1, 0)
            x <- x[, c('file',  'test', 
                       'tests', 'passed', 'failed',  'err',
                       'warning', 'flag', 'skipped', 'error_cant_test'
            )]
            x$test <- substr(x$test, 1, 50) # some are long
            xtable[[i]] <- data.table::data.table(x)
          }
        })
        print(timing)
        cat("\n")
        xtable <- data.table::rbindlist(xtable)
        print(colSums(xtable[, .(tests, passed, failed, err,
                                 warning, flag, 
                                 skipped, error_cant_test)]))
        return(xtable)
      }
      ########################### #
      
      ##     ONE SPECIFIC GROUP OF TESTS AT A TIME
      
      testbygroup <- function(testlist, ...) {
        # the ... can be print=TRUE, and possibly reporter=default_compact_reporter()
        # testlist[[(names(testlist)[[1]])]] is the same as get(names(testlist)[[1]]), vector of filenames
        xtable <- list()
        i <- 0
        for (tgroupname in names(testlist)) {
          i <- i + 1
          fnames = unlist(testlist[[tgroupname]])
          cat("", tgroupname, "group has", length(fnames), "test files ")
          xtable[[i]] <- data.table::data.table(testgroup = tgroupname, 
                                                test1group(testlist[[tgroupname]], ...) )
        }
        xtable <- data.table::rbindlist(xtable)
        return(xtable)
      }
    }   #   done defining functions
    #################################################### # 
    ########################### #  ########################################## #
    
    # >>Ask what to do<< ####
    
    # *** THIS SECTION ASKS ABOUT tname SO IT USES THE LATEST LIST OF TESTS FOUND to ask which ones to use, to know what the options are,
    # WHICH IS WHY THESE QUESTIONS ARE ASKED ONLY AFTER FINDING AND GROUPING TESTS
    
    if (y_runsome) {y_runall =  FALSE} # in case you want to say y_runsome = T and not have to also remember to specify y_runall = F
    
    if (interactive() & ask) {
      
      if (missing(useloadall)) {
        useloadall <- askYesNo(msg = "Do you want to load and test the current source code files version of EJAM (via devtools::load_all() etc.,
                      rather than testing the installed version)?", default = TRUE)
      }
      if (missing(y_runsome)) {
        y_runsome = askYesNo("Run ONLY SOME OF THE tests ?", default = FALSE)}
      if (is.na(y_runsome))  {stop("cancelled")}
      if (y_runsome) {
        
        tlistinfo = data.frame(groupnames = names(testlist),
                               shortgroupnames = gsub("^test_(.*)","\\1", names((testlist))), 
                               filecount = sapply(testlist, length)
                               #, `filenames as test-___.R` = as.vector(unlist(lapply(testlist, function(z) paste0(gsub("^test-|.R$", "", unlist(z)), collapse = ", "))))
        )
        rownames(tlistinfo) = NULL
        print(testlist) # long list of vectors
        cat("\n\n")
        print(tlistinfo)
        #          groupnames shortgroupnames filecount
        # 1         test_fips            fips         3
        # 2        test_naics           naics         8
        # 3          test_frs             frs         6
        # 4       test_latlon          latlon        10
        # 5         test_maps            maps         1
        # 6        test_shape           shape         3
        # 7    test_getblocks       getblocks         5
        # 8  test_fixcolnames     fixcolnames         6
        # 9         test_doag            doag         2
        # 10      test_ejamit          ejamit         6
        # 11 test_ejscreenapi     ejscreenapi         5
        # 12         test_mod             mod         3
        # 13         test_app             app         5
        # 14        test_test            test         1
        # 15       test_golem           golem         2
        # fnames = unlist(testlist)
        
        shortgroupnames = gsub("^test_(.*)","\\1", names((testlist)))
        
        if (missing(tname)) { 
          tname = rstudioapi::showPrompt(
            "WHICH TEST OR GROUPS COMMA-SEP LIST",
            paste0(shortgroupnames, collapse = ","),
            #e.g., "fips,naics,frs,latlon,maps,shape,getblocks,fixcolnames,doag,ejamit,ejscreenapi,mod,app"
          )
        }
        
        y_runall <- FALSE
      } else {
        if (missing(y_runall)) {
          y_runall = askYesNo("RUN ALL TESTS NOW?")}
        if (is.na(y_runall)) {stop("cancelled")}
      }
      if (missing(y_seeresults)) {
        y_seeresults = askYesNo("View results of unit testing?")}
      if (is.na(y_seeresults))  {stop("cancelled")}
      if (missing(y_save)) {
        y_save = askYesNo("Save results of unit testing (and log file of printed summaries)?")}
      if (is.na(y_save)) {stop("cancelled")}
      if (y_save) {
        if (missing(y_tempdir) & missing(mydir)) {
          y_tempdir = askYesNo("OK to save in a temporary folder you can see later? (say No if you want to specify a folder)")}
        if (is.na(y_tempdir)) {stop("cancelled")}
        if (y_tempdir & missing(mydir)) {
          mydir <- tempdir()
        } else {
          if (missing(mydir)) {
            mydir <- rstudioapi::selectDirectory()}
        }
        
        
      }
    }
    if (missing(mydir) && (!exists('mydir') || is.null(mydir))) {
      if (y_tempdir) {
        mydir <- tempdir()
      } else {
        mydir = '.'
      }
    }
    mydir <- normalizePath(mydir)
    if (y_runsome) {
      tname <- unlist(strsplit(gsub(" ", "", tname), ","))
      tname = paste0("test_", tname)
      #    test_file("./tests/testthat/test-MAP_FUNCTIONS.R" )
      partial_testlist <-  testlist[names(testlist) %in% tname] 
    }
    
    logfilename_only = paste0("testresults-", 
                              gsub(" ", "_", gsub("\\.[0-9]{6}$", "", gsub(":", ".", as.character(Sys.time())))), 
                              ".txt")
    logfilename = (  file.path(mydir, logfilename_only) )
    cat("Saving in ", logfilename, ' etc. \n')
    
    ################################### #  ################################### #
    if (y_runall == FALSE && y_runsome == FALSE) {
      stop('no tests run')
    } else {
      if (interactive() & ask & (y_runall | ("test_shape" %in% names(testlist)))) {
        # ***  note if interactive it normally tries to prompt for shapefile folder in some cases  
        if (missing(noquestions)) {
          if (askYesNo("run tests where you have to interactively specify folders for shapefiles?")) {
            noquestions <- FALSE
          }  else {
            noquestions <- TRUE
          }
        } else {
          # noquestions  was given as a parameter
        }}
    }
  } # finished asking what to do
  
  ########################### #  ########################################## #  
  # load_all() or library(EJAM) ####
  
  if (useloadall) {
    devtools::load_all()
  } else {
    suppressPackageStartupMessages({   library(EJAM)   }) 
  }
  dataload_from_pins("all")
  if (file.exists("./tests/testthat/setup.R")) {
    # rstudioapi::navigateToFile("./tests/testthat/setup.R")
    source("./tests/testthat/setup.R") #   asks if need load_all or library
  } else {
    cat("Need to source the setup.R file first \n")    
  }
  ########################### #  ########################################## #
  
  # just do basic quick checks (not unit tests) ####
  # for easy/basic case, main functions, without actually running unit tests with testthat
  
  if (y_basic) {
    
    if (y_latlon) {
      # latlon
      x <- ejamit(testpoints_5[1:2,], radius = 1)
      # names(x)
      ejam2table_tall(x)
      ejam2barplot(x)
      ejam2barplot_sites(x)
      ejam2tableviewer(x)
      junk = ejam2excel(x, save_now = F, launchexcel = T) # map and report were missing if not in shiny app
      # ejam2report(x)   #  report not yet working if not in shiny app
      ejam2map(x)
      fname = ejam2shapefile(x, folder = tempdir())
      shpin = shapefile_from_any(fname)
      map_shapes_leaflet(shpin)
      cat("\n\n DONE WITH latlon CHECKS \n\n")
    }
    
    if (y_shp) {
      # shapefile
      
      shp <- shape_buffered_from_shapefile( shapefile_from_sitepoints(testpoints_5[1:2,]), radius.miles = 1)
      # or use test data  shp <- shapefile_from_any()
      shp <- shapefile_from_any(system.file("testdata/shapes/portland_folder_shp/Neighborhoods_regions.shp", package = "EJAM"))[1:3, ]  
      x <- ejamit( shapefile = shp )  
      names(x)
      ejam2table_tall(x)
      ejam2barplot(x)
      ejam2barplot_sites(x)
      ejam2tableviewer(x , fname = file.path(tempdir(), "ejam2tableviewer_test.html")) # should be able to pick name
      junk = ejam2excel(x, save_now = F, launchexcel = T) # map and report were missing if not in shiny app
      ejam2report(x,)   #  report not yet working if not in shiny app
      ejam2map(x) # no latlon or geometry is in output of ejamit() here so this is not working for FIPS or shapefile analysis cases yet, except see  mapfastej_counties()
      ejam2shapefile(x, folder = tempdir()) # no latlon or geometry is in output of ejamit() here so this is not working for FIPS or shapefile analysis cases yet, except see  mapfastej_counties()
      map_shapes_leaflet(x)
      cat("\n\n DONE WITH shp CHECKS \n\n")
    }
    
    if (y_fips) {
      # fips
      x <- ejamit(fips = fips_bg_from_anyfips(fips_counties_from_state_abbrev("DE")[1])[1:2]) # just 2 blockgroups
      names(x)
      ejam2table_tall(x)
      ejam2barplot(x)
      ejam2barplot_sites(x)
      ejam2tableviewer(x)
      junk = ejam2excel(x, save_now = F, launchexcel = T) # map and report were missing if not in shiny app
      ejam2report(x)   #  report not yet working if not in shiny app
      ejam2map(x) # no latlon or geometry is in output of ejamit() here so this is not working for FIPS or shapefile analysis cases yet, except see  mapfastej_counties()
      ejam2shapefile(x, folder = tempdir()) # no latlon or geometry is in output of ejamit() here so this is not working for FIPS or shapefile analysis cases yet, except see  mapfastej_counties()
      
      x <- ejamit(fips = fips_counties_from_state_abbrev("DE"))  #   3 Counties
      mapfastej_counties(x$results_bysite) # not (x)
      cat("\n\n DONE WITH fips CHECKS \n\n")
    }
    
    stop("Done with basic checks. halting.")
  } # halts if this gets done
  ######################################################## #
  ########################### #  ########################################## #
  
  # start log file ####
  
  cat("Started at", as.character(Sys.time()), '\n')
  cat("Running all tests may take >40 minutes\n\n")
  
  junk = loggable(file = logfilename, x = {
    cat(logfilename_only, '\n ---------------------------------------------------------------- \n\n')
    cat("Started at", as.character(Sys.time()), '\n')
    
    if (is.null(tname)) {tnameprint = NA} else {
      tnameprint = paste0(tname, collapse = ',')
    }
    
    cat(" 
        ask          = ", ask, " 
        noquestions  = ", noquestions, " 
        useloadall   = ", useloadall, "  
        
        y_basic      = ", y_basic, "
          y_latlon     = ", y_latlon, "
          y_shp        = ", y_shp, "
          y_fips       = ", y_fips, "
        
        y_runsome    = ", y_runsome, "  
          tname        = ", tnameprint, " 
        
        y_runall     = ", y_runall, "  
        
        y_seeresults = ", y_seeresults, "  
        y_save       = ", y_save, "  
        mydir        = ", "[not shown here]" , "
        "
    )
  })
  
  ########################### #  ########################################## #
  
  # RUN JUST 1 FILE OR GROUP? ####
  
  # y_runsome = askYesNo("Run only some tests now?")
  # if (is.na(y_runsome))  {stop("cancelled")}
  if (y_runsome) {
    
    # consoleclear()
    
    x = testbygroup(testlist = partial_testlist)
    
    junk = loggable(file = logfilename, x = {
      cat("\n\n                                         RESULTS THAT FAILED/ WARNED/ CANT RUN     \n\n")
      if (any(x$flag + x$error_cant_test > 0)) {
        print(x[x$flag + x$error_cant_test > 0,])
      } else {
        cat("All selected tests ran and passed.")
      }
      cat("\n\n")
    })
    
  }
  ########################### #  ########################################## #
  
  # RUN ALL TESTS (slow)  ####
  
  # y_runall = askYesNo("RUN ALL TESTS NOW?")
  if (is.na(y_runall)) {stop("cancelled")}
  if (y_runall) {
    
    # consoleclear()
    print(tlistinfo)
    
    z <- system.time({
      testall <- testbygroup(testlist = testlist)
    })
    junk = loggable(file = logfilename, x = {
      print(z)
    })
    
    #
    # 
    ########################### #  ########################################## #
    x <- testall
    ## save results of testing ####
    
    # y_save = askYesNo("Save results of unit testing?") 
    if (is.na(y_save)) {stop("cancelled")}
    if (y_save) {
      fname <- paste0("results_of_unit_testing_", as.character(Sys.Date()), ".rda")
      fname = (  file.path(mydir, fname) )
      save(testall, file = fname)
      junk = loggable(file = logfilename, x = {
        # cat("TEST RESULTS AS OF "); cat(as.character(Sys.Date()))
        # cat("\n\n")
        # print(cbind(`PERCENT OF ALL TESTS` = round(100 * colSums(testall[,4:11]) / 1125, 1)))
        # cat("\n\n")
        cat('\n  See', fname, ' for full results of unit testing.\n\n') 
      })
    } # end if - save
  }
  ########################### #  ########################################## #
  ########################### #  ########################################## #
  
  # SUMMARIZE WHAT PASSED / FAILED ####
  
  # y_seeresults = askYesNo("View results of unit testing?") 
  if (is.na(y_seeresults))  {stop("cancelled")}
  if (y_seeresults) {
    consoleclear()
    ########################### #  ########################### #
    junk <- loggable(file = logfilename, x = {
      cat("TEST RESULTS AS OF "); cat(as.character(Sys.Date()))
      
      # HOW MANY TOTAL PASS/FAIL?
      
      cat("\n\n\n")
      cat("COUNT PASS / FAIL \n\n")
      passcount = colSums(x[, .( err, warning, tests, passed, failed, warning, flag, skipped, error_cant_test)])
      print(passcount)
      cat("\n")
      cat("PERCENT PASS / FAIL ")
      cat("\n\n")
      passpercent = round(100 * colSums(x[, .( err, warning, tests, passed, failed, warning, flag, skipped, error_cant_test)])
                          / sum(x$tests), 1)
      print(passpercent)
      
      # about 1.6 % of unit tests have errors   as of 7/1/24   ***********
      # (and ~1% have warnings)   ***********
      ########################### #  ########################### #
      
      ## KEY GROUPS - WHICH TEST GROUPS or FILES HAVE THE MOST FAILING TESTS?
      
      x[ , flag_byfile := sum(flag), by = "file"]
      x[ , err_byfile  := sum(err),  by = "file"]
      x[ , flag_bygroup := sum(flag), by = "testgroup"]
      x[ , err_bygroup  := sum(err),  by = "testgroup"]
      setorder(x, -err_bygroup, testgroup, -flag, -failed, file)
      
      bygroup <- x[ , .(err = sum(err), warning = sum(warning), flag = sum(flag), 
                        passed = sum(passed), tests = sum(tests)), by = "testgroup"]
      cat("\n\n\n")
      cat("KEY GROUPS OF FILES")
      cat("\n\n\n")
      print(bygroup)
      ########################### #  ########################### #
      
      ## WHICH FILES HAVE THE MOST FAILING TESTS?
      
      byfile <- x[ , .(
        err_byfile = err_byfile[1], 
        err_bygroup = err_bygroup[1],
        testgroup = testgroup[1]
      ), 
      by = "file"]
      setorder(byfile, -err_bygroup, testgroup, -err_byfile, file)
      setcolorder(byfile, neworder = c("testgroup", "err_bygroup", "file", "err_byfile"))
      
      cat("\n\n")
      cat("KEY FILES")
      cat("\n\n")
      print(byfile)
      # cat("\nTEST RESULTS AS OF "); cat(as.character(Sys.Date()))  
      ########################### #
      
      # WHICH TESTS? 
      
      cat("\n\n\n")
      cat("PRIORITIZED TESTS")
      cat("\n\n\n")
      
      prioritize = x[order(-x$flag, -x$failed), 1:11]
      these = prioritize$tests != prioritize$passed | prioritize$error_cant_test > 0
      if (any(these)) {
        prioritize <- prioritize[these, ]
        print(prioritize)
        cat("\n\n")  
      } else {
        prioritize = NA
      }
      
      cat('
TO OPEN SOME KEY TEST FILES FOR EDITING, FOR EXAMPLE:
  
  rstudioapi::navigateToFile("./tests/testthat/test-FIPS_FUNCTIONS.R")
  rstudioapi::navigateToFile("./tests/testthat/test-state_from_fips_bybg.R")
  rstudioapi::navigateToFile("./tests/testthat/test-doaggregate.R")
  rstudioapi::navigateToFile("./tests/testthat/test-ejamit.R")
  rstudioapi::navigateToFile("./tests/testthat/test-latlon_df_clean.R")
  
      ')
      
      # cat("MORE KEY TESTS DETAILS")
      # cat("\n\n")
      # xx = x[x$error_cant_test > 0 | x$test != x$passed, ]
      # print(xx)
    }) # end loggable
  } # end of big if - viewing results
  ########################### #  ########################################## #
  if (!exists("testall")) {testall <- NA}
  
  params = list(ask =  ask,
                noquestions  =  noquestions,
                useloadall   =  useloadall,
                y_basic      =  y_basic,
                y_latlon     =  y_latlon,
                y_shp        =  y_shp,
                y_fips       =  y_fips,
                y_runsome    =  y_runsome,  
                tname        =  paste0(tname, collapse = ","),
                y_runall     =  y_runall,
                y_seeresults =  y_seeresults,  
                y_save       =  y_save,  
                mydir        =  mydir 
  )
  
  biglist =     list(
    passcount = passcount,
    passpercent = passpercent,
    prioritize = prioritize,
    bygroup = bygroup, 
    byfile = byfile,
    testall = testall, 
    mydir = mydir,
    params = params  
  )
  ## save Summaries of results of testing ####
  if (y_save) {
    fname <- paste0("results_SUMMARY_of_unit_testing_", as.character(Sys.Date()), ".rda")
    fname = (file.path(mydir, fname))
    save(biglist, file = fname)
    cat(
      '\n saved ', fname, ' \n\n'
    )
  }
  loggable(file = logfilename, x = {cat("Finished at", as.character(Sys.time()), '\n')})
  
  if (interactive()) {
    browseURL(mydir) # open folder in file explorer / finder
    if (rstudioapi::isAvailable()) {
      # view the file
      rstudioapi::navigateToFile(logfilename) 
    }
  }
  
  cat("Finished at", as.character(Sys.time()), '\n')
  if (interactive()) {beepr::beep(10)} # utils::alarm() may not work 
  invisible(
    biglist
  )
} # end of function
################################### #  ################################### #  ################################### #

loggable <- function(x, file = 'will be created using timestamp if not provided and !exists(logfilename)', 
                     append = TRUE, split = TRUE, 
                     y_save_param=NULL) {
  
  if (missing(y_save_param)) {
    if (!exists('y_save')) {
      if (is.null(file)) {
        y_save <- FALSE
      } else {
        y_save <- TRUE
      }
    }
  } else {
    y_save <- y_save_param
  }
  
  if (y_save) {
    if (missing(file)) {
      if (exists('logfilename')) {
        file = logfilename
      } else {
        mydir = tempdir()
        file = paste0("testresults-", 
                      gsub(" ", "_", gsub("\\.[0-9]{6}$", "", gsub(":", ".", as.character(Sys.time())))), 
                      ".txt")
        file = (  file.path(mydir, file) )
      }
    }
    if (is.null(file)) {
      warning("file got set to NULL so NOT saving even though y_save was TRUE.")
    }
  } else {
    if (missing(file)) {
      file = NULL
    } else {
      if (!is.null(file)) {
        warning('file got specified so WILL save even though y_save was FALSE.')
      }
    }
  }
  
  capture.output(x, file = file, append = append, split = split) # this is supposed to print to console and to log file, but...
  
  cat('\n  Adding to ', file, ' log of results of unit testing.\n\n')
  
  # use file = logfilename  or file = NULL  to override whatever the y_save value was when func was defined
  # file = NULL  will show only in console and not log it
  # split=T  will show output in console, and save to file simultaneously unless file=NULL
  
  ### how to use it    ## example 
  # ## y_save = F will prevent logging unless you also specify a file
  # junk = loggable(file = logfilename, x = {
  #   })
  
  # junk = loggable(file = logfilename, x = { 
  #   # comments do not get logged
  #   #  x  or  1 + 1  is not logged without print() or cat() ?
  #   print(cbind(a=1:3,b=2:4))
  #   cbind(c = 1:3, d = 2:4)
  #   x = 56
  #   print(x)
  #   cat(1234,'\n\n')
  #   
  #   }) 
  ## use file = logfilename  or file = NULL  to override whatever the y_save value is
  
}
################################### # 

# biglist <- test_interactively(ask = FALSE, mydir = "~/../Downloads/unit testing")
