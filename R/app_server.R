#' EJAM app server
#' 
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  # Note: how to avoid or still have a global.R file in the golem approach, 
  # https://github.com/ThinkR-open/golem/issues/6
  
  # This app_server() function will be broken out into and refer to 
  #   functions and modules 
  #   but for now it has all the code in this file.
  # SERVER MODULES WILL PROBABLY REPLACE MOST OF THE CODE BELOW ####
  # such as possibly these placeholders:
  #  mod_view_results_server("view_results_1")
  #  mod_save_report_server("save_report_1")
  #  mod_specify_sites_server("specify_sites_1")
  
  # [this is a button that helps while debugging (REMOVE BEFORE DEPLOYING)] ####
  print('NOTE: remove the debugging button before deploying')
  observeEvent(input$browser,{
    browser()
  })
  
  # Build localtree quad tree index for EVERY session?? ####
  ## *** DOES localtree HAVE TO BE RECREATED EACH TIME dataLocationListProcessed REACTIVE UPDATES??
  # SEEMS LIKE THAT WOULD BE EVERY TIME radius is updated, 
  # but this is slow and only needs to happen once per session, right?
  cat('\n BUILDING INDEX TO ALL BLOCKS IN USA - TAKES A FEW SECONDS...\n\n')
  localtree <- SearchTrees::createTree(EJAMblockdata::quaddata, treeType = "quad", dataType = "point")
  
  # . ####
  # ______________ User inputs _________________ ####
  # . ####
  # UPLOAD/INPUT QUERY 
  # names of input file  ##########################################
  myfile_uploaded_FRS_IDs <- reactive({input$file_uploaded_FRS_IDs})
  myfile_uploaded_latlons <- reactive({input$file_uploaded_latlons})
  
  # Input Radius, Maxcutoff, etc. parameters ##########################################
  
  # THESE DO NOT MAKE SENSE... input$whatever is already a reactive value... so why redefine those here just renaming them??
  getCutoff    <- reactive({return(input$cutoffRadius)})
  getMaxcutoff <- reactive({return(maxcutoff_default)})
  # avoidorphans  # THIS WOULD ALLOW ONE TO GET SOME RESULTS EVEN IF CIRCLE IS SO SMALL NO BLOCK HAS A CENTROID IN IT. 
  # Expand distance for facilities with no nearby block centroid ####
  doExpandradius <- reactive({if (input$expandRadius=="no"){return(FALSE)} else {return(TRUE)}})
  
  # . ####
  # ______________ Lat Lon _________________ ####
  # . ####
  ####################################################################################################################### #
  # *** LAT LON: dataLocationList DEFINES BUFFERS** ######
  # imported location data
  ####################################################################################################################### #
  dataLocationList <- reactive({
    in2File <- input$file_uploaded_latlons
    if (is.null(in2File))
      return(NULL)
    isolate({ # THIS ISOLATE SEEMS LIKE IT WOULD NOT DO ANYTHING HERE. NOT SURE WHY THEY PUT IT IN.
      # read.table(file=in2File$datapath, sep=',', header=TRUE, quote='"')
      mypoints <- EJAMbatch.summarizer::read_csv_or_xl(fname = in2File$datapath, show_col_types = FALSE)
      names(mypoints) <- latlon_infer(names(mypoints))
        mypoints <- latlon_df_clean(mypoints)
      # mypoints <- latlon_readclean(in2File$datapath)
    })
    mypoints
  })
  
  output$inLocationList <- renderTable({
    if(is.null(dataLocationList())) {return () }
    dataLocationList()
  })
  ####################################################################################### #
  # _ Find nearby blocks and aggregate for buffer ########
  ####################################################################################### #
  
  dataLocationListProcessed <- reactive({
    if(is.null(dataLocationList())) {return () }
    sitepoints <- dataLocationList() # this is a data.frame not tibble not data.table yet
    if (!('siteid' %in% names(sitepoints))) {sitepoints$siteid <- seq.int(length.out = NROW(sitepoints))}
    setDT(sitepoints) #, key = 'siteid') # make it a data.table, by reference.
    # id sitename       lon      lat
    # 1:  1   site A -73.95738 40.77560
    # 2:  2   site B -66.38670 18.43211
    cutoff <- getCutoff() # radius (units?)
    maxcutoff <- getMaxcutoff()  # reactive, max distance to search
    # get_unique <- setUnique()     # reactive, TRUE = stats are for dissolved single buffer to avoid doublecounting. FALSE = we want to count each person once for each site they are near.
    avoidorphans <- doExpandradius() # Expand distance searched, when a facility has no census block centroid within selected buffer distance
    
    # note this does require that EJAMblockdata be loaded
    
    # ___ getblocksnearby()  ################################
    
    elapsed <- system.time({
      sites2blocks <- EJAM::getblocksnearby(
        sitepoints =  sitepoints,
        cutoff = cutoff, # radius
        maxcutoff = maxcutoff,
        avoidorphans = avoidorphans,
        quadtree = localtree          # this is very large... do we need to pass it to the function, or can it be just in global?
      )
    }) # end of timed function
    print('Found nearby blocks'); print(elapsed)
    
    # ___ doaggregate()  ################################
    
    elapsed <- system.time({
      out <- doaggregate(sites2blocks = sites2blocks)
    }) # end of timed function
    print('Summarized indicators in each buffer and overall'); print(elapsed)
    
    return(out)
  })
  # End of functions used for the dataLocList option
  
  # . ####
  # ______________ Facility ID _________________ ####
  # . ####
  ####################################################################################################################### #
  # *** FACILITY ID: dataFacList DEFINES BUFFERS** #######
  # imported facility list  ...  uploaded_FRS_IDs
  ####################################################################################################################### #
  dataFacList <- reactive({
    inFile <- input$file_uploaded_FRS_IDs
    if (is.null(inFile))
      return(NULL)
    isolate(data.table::as.data.table(read.table(file=inFile$datapath, sep=',', header=TRUE, quote='"')))
  })
  output$inFacList <- renderTable({
    if(is.null(dataFacList())) {return () }
    dataFacList()
  })
  ####################################################################################### #
  # _ Find nearby blocks and aggregate for buffer ########
  ####################################################################################### #
  
  dataFacListProcessed <- reactive({
    kf <- copy(dataFacList())
    # very old code... probably can make this much faster:
    kimssamplefacilities <- merge(x = kf, y = EJAMfrsdata::frs, by.x='REGISTRY_ID', by.y='REGISTRY_ID', all.x=TRUE)
    kimsunique <- data.table::unique(kimssamplefacilities[,.(REGISTRY_ID, lat, lon)])
    # rm(kf)
    # rm(kimssamplefacilities)
    kimsunique[ , ID := .I]  
    kimsunique <- kimsunique[ , .(ID, lat, lon)]
    
    cutoff = getCutoff() # radius (units?)
    maxcutoff = getMaxcutoff()  # reactive, max distance to search
    # get_unique = setUnique()     # reactive, TRUE = stats are for dissolved single buffer to avoid doublecounting. FALSE = we want to count each person once for each site they are near.
    avoidorphans = doExpandradius() # Expand distance searched, when a facility has no census block centroid within selected buffer distance
    
    # ___ getblocksnearby()  ################################
    
    system.time(sites2blocks <- EJAM::getblocksnearby(
      sitepoints =  kimsunique, ##
      cutoff = cutoff, # radius
      maxcutoff = maxcutoff,
      avoidorphans = avoidorphans,
      quadtree = localtree
    ))
    
    # ___ doaggregate()  ################################
    
    system.time(out <- doaggregate(sites2blocks))
    
    return(out)
  })
  # End of functions used for the dataFacList option #######3
  ####################################################################################################################### #
  
  # . ####
  # ______________ NAICS _________________ ####
  # . ####
  ####################################################################################################################### #
  # *** NAICS: SECTOR(S) DEFINE BUFFERS**  #######
  ####################################################################################################################### #
  
  # function that runs buffering on facilities selected via the NAICS dataset,
  # is defined here, not easily in separate file because it uses several reactives and facilities? which is in global env
  # so the file with this function needs to be deleted if not used.
  
  datasetNAICS <- function() {
    
    #   All this complicated code on query by NAICS/program info at the same time...
    # probably could be greatly improved/ replaced...
    #     very old code, a mess - I don't understand it... probably could be greatly improved/ replaced:
    # The files frs, frs_by_naics, and frs_by_programid are not set up to facilitate queries that look 
    # at program and NAICS at the same time, but this code was trying to do that (with an older differently formatted set of FRS info)
    #  This code seemed very messy and could be simplified and replaced I think.
    # At a minimum it should be easier to simply query by NAICS in the data.table EJAMfrsdata::frs_by_naics
    # without specifying anything about which program it is in, etc.
    
    ################################################################## #
    # clean up users selections ####
    ################################################################## #
    
    if (nchar(input$naics_user_wrote_in_box)>0 & length(input$naics_user_picked_from_list)>0) {return()} # WHY? can't do both??
    
    naics_user_wrote_in_box <- input$naics_user_wrote_in_box  # e.g. '' (empty)            # IF USER PICKED NAICS FROM LIST 
    naics_user_wrote_in_box <- as.list(strsplit(naics_user_wrote_in_box, ",")[[1]])
    naics_user_picked_from_list <- input$naics_user_picked_from_list                      # IF USER TYPED IN NAICS
    
    if (nchar(naics_user_wrote_in_box)>0 | length(naics_user_picked_from_list)>0) {
      inputnaics <- c(naics_user_wrote_in_box, naics_user_picked_from_list)
      inputnaics <- unique(inputnaics[inputnaics != ""])
      inputnaics <- paste("^", inputnaics, collapse="|")   ### the NAICS specified by user
      inputnaics <- stringr::str_replace_all(string = inputnaics, pattern = " ", replacement = "")
      
      ################################################################## #
      # Match user NAICS to FRS NAICS, TO GET LAT/LON OF MATCHED SITES ####
      ################################################################## #
      
      #     very old code, a mess - I don't understand it... probably could be greatly improved/ replaced:
      
      ################################################################## #
      # use full FRS dataset that has NAICS of all sites and their lat lon ####
      # Dataset of FRS sites and NAICS in long format  
      ################################################################## #
      # Sites that match based on just NAICS codes:
      sitepoints <- EJAMfrsdata::frs_by_naics[NAICS %in% inputnaics ,  ] # lat        lon  REGISTRY_ID  NAICS
      #  REGISTRY_ID  NAICS      lat       lon 
      # Also see EJAMfrsdata::frs , EJAMfrsdata::frs_by_programid 
      
      ############################### #
      # Input that specifies which datasystems are we searching for NAICS
      ############################### #
      # facility_mustbe_that_naics_in_this_program <- c('OIL','AIRS/AFS') # for testing?
      # facility_mustbe_in_this_program <- c('RCRAINFO') # for testing?
      
      facility_mustbe_that_naics_in_this_program  <- input$facility_mustbe_that_naics_in_this_program # e.g. NULL # USER SPECIFIED PROGRAM LIKE AIRS/AFS
      facility_mustbe_in_this_program <- input$facility_mustbe_in_this_program # e.g. NULL # USER SPECIFIED PROGRAM LIKE AIRS/AFS
      
      if (length(facility_mustbe_that_naics_in_this_program)>0 & length(facility_mustbe_in_this_program)>0) {
        # User filtered it both ways: 
        # The found sites must be in specified program(s) (listed as any NAICS in that program, as long as the queried NAICS apply to the site under at least some other program)
        # and also the site must be listed as being that queried NAICS within specified program(s) 
        
        #xxxx        
        
        # temp <- frsfull[program %in% facility_mustbe_that_naics_in_this_program] # 
        # temp <- temp[char_naics %in% matches]
        # temp <- unique(temp[,.(REGISTRY_ID)])
        # sub1 <- data.table::as.data.table(merge(x = frsfull, y = temp, by.x='REGISTRY_ID', by.y='REGISTRY_ID'), all.y=TRUE)
        # sitepoints <- sub1[program %in% facility_mustbe_in_this_program]
      }
      else if (length(facility_mustbe_in_this_program)>0) {
        
      }
      else if (length(facility_mustbe_that_naics_in_this_program)>0) {
        
        # xxxx
        
        
        
        # sitepoints <- frsfull[ , ]
        # sitepoints <- frsfull[char_naics %in% matches, ]
      }
      sitepoints$ID <- c(seq.int(nrow(sitepoints)))
      print(paste("Number of matches = ", nrow(sitepoints)))
      
      ####################################################################################### #
      # _ Find nearby blocks and aggregate for buffer ########
      ####################################################################################### #
      
      if (nrow(sitepoints)>0) {
        
        # ___ getblocksnearby()  ################################
        
        cutoff=getCutoff()  # reactive  (e.g., 3) in miles
        maxcuttoff=getMaxcutoff()  # reactive, max distance to search e.g. 4000
        avoidorphans=doExpandradius()  # reactive # Expand distance searched, when a facility has no census block centroid within selected buffer distance
        
        system.time({
          sites2blocks <- EJAM::getblocksnearby(
            sitepoints =  sitepoints,
            cutoff = cutoff, # radius
            maxcutoff = maxcutoff,
            avoidorphans = avoidorphans,
            quadtree = localtree
          )
        })
        
        # ___ doaggregate()  ################################
        
        system.time({out <- doaggregate(sites2blocks = sites2blocks)})
        return(out)
      } else {
        print("No matches were found")
        out <- data.table::as.data.table(setNames(data.frame(matrix(ncol = 1, nrow = 0)), paste0("No matches were found" #, c(1:1)
        )))
        return(out)
      }
    } else {
      print("Please submit an industry to run this query")
      out <- data.table::as.data.table(setNames(data.frame(matrix(ncol = 1, nrow = 0)), paste0("Please submit an industry to run this query", c(1:1))))
      return(out)
    }
  }
  # _______________________________ ####
  
  # warnings and text outputs re selected Facilities, Industry, or Locations ##########################################
  numUniverseSource <- function() {
    selInd=0
    if (nchar(input$naics_user_wrote_in_box)>0 | length(input$naics_user_picked_from_list)>0) {
      selInd=1
    }
    numLoc=nrow(dataLocationList())  # reactive, from reading input$file_uploaded_latlons
    numFac=nrow(dataFacList())       # reactive, from reading input$file_uploaded_FRS_IDs
    if (!is.null(numFac)) {
      if (numFac>0) {numFac=1}
    }
    if (!is.null(numLoc)) {
      if (numLoc>0) {numLoc=1}
    }
    tot=sum(selInd,numLoc,numFac)
    return (tot)
  }
  
  getWarning1 <- function() {
    if (nchar(input$naics_user_wrote_in_box)>0 & length(input$naics_user_picked_from_list)>0) {
      print("Please use a single industry select option.")
    }
  }
  
  getWarning2 <- function() {
    tot=numUniverseSource()
    length_selectIndustry1=0
    length_selectIndustry2=0
    if (!is.null(input$naics_user_wrote_in_box)) {
      length_selectIndustry1=nchar(input$naics_user_wrote_in_box)
    }
    if (!is.null(input$naics_user_picked_from_list)) {
      length_selectIndustry2=nchar(input$naics_user_picked_from_list)
    }
    if (length_selectIndustry1>0 & length_selectIndustry2>0) {
      print("Please use a single industry select option.")
    }
    else if(tot>1) {
      print(paste("Please use only one method of specifying locations (ie, select by industry, lat/lon points, OR facility IDs)"))
      #print(paste("Please use only one method of selecting universe (ie, select by industry, location, OR facility)",tot,"; numLoc=",nrow(dataLocationList()),"; numFac=",nrow(dataFacList())))
    }
  }
  
  output$inputWarning <- renderPrint({
    getWarning1()
  })
  
  output$inputWarning2 <- renderPrint({
    getWarning2()
  })
  
  output$selectInd1_for_testing <- renderPrint({
    if (length(input$naics_user_wrote_in_box) > 1) {  # not really used except in testing tab
      x = paste(input$naics_user_wrote_in_box,collapse=", ")
      return(paste("Selected industries ", x))
    }  else {
      if (length(input$naics_user_wrote_in_box) == 1 & nchar(input$naics_user_wrote_in_box) > 0) {
        return(paste("Selected industry ", input$naics_user_wrote_in_box))
      }
    }
    return('')
  })
  
  output$selectInd2_for_testing <- renderPrint({  # not really used except in testing tab
    if (length(input$naics_user_picked_from_list) > 1) {
      x=paste(input$naics_user_picked_from_list,collapse=", ")
      return(paste("Selected industries ", x))
    } else {
      # & nchar(input$naics_user_picked_from_list) > 0
      if (length(input$naics_user_picked_from_list) == 1 ) {
        return(paste("Selected industry ", input$naics_user_picked_from_list))
      }
    } 
    return('')
  })
  
  output$selectScope1 <- renderPrint({
    # input$goButton1  # seems not to be used at all
    isolate(input$selectFrom1)
  })
  
  output$selectScope2 <- renderPrint({input$selectFrom2}) # used ONLY in debuggin/testing tab
  output$file_uploaded_FRS_IDs_df <- renderPrint({input$file_uploaded_FRS_IDs}) # used ONLY in debuggin/testing tab
  output$file_uploaded_latlons_df <- renderPrint({input$file_uploaded_latlons}) # used ONLY in debuggin/testing tab
  
  # ###########################################'
  #### Get user info as metadata on results #########
  # ###########################################'
  # Support function for grabbing the user input that will be output as meta data in the data download
  
  addUserInput <- function(file,userin,mystring,strdesc){
    length_string = 0
    if(!is.null(mystring)) {
      length_string=nchar(mystring)
      userin=paste(mystring, userin,sep = "\n")
      userin=paste(strdesc, userin, " ")
      cat(userin,  file=file, append=T)   # THIS USED TO SIMPLY APPEND THIS METADATA TO THE TABULAR OUTPUTS
    }
    return(userin)
  }
  ####################################################################################################################### #
  
  # . ####
  # ______________ WHICH RESULTS TO RETURN? _________________ ####
  # . ####
  # outputs based on latlon, ID, or NAICS _________________ ####
  
  datasetResults <- function() {
    if (length(getWarning1() > 1) | length(getWarning2() > 1)) {
      return()
    } 
    else if (nchar(input$naics_user_wrote_in_box) > 0 | length(input$naics_user_picked_from_list) > 0) {
      # e.g.,   "324110"  might be the value of input$naics_user_picked_from_list 
      return(datasetNAICS( )) # that is a separate function not a reactive
    }
    else if (length(myfile_uploaded_latlons()) > 1) {
      return(dataLocationListProcessed())
    }
    else if (length(myfile_uploaded_FRS_IDs()) > 1) {
      stop('dataFacListProcessed reactive and myfile_uploaded_FRS_IDs not working currently')
      return(dataFacListProcessed())
    }
  }
  
  ############################################################################################## #
  # . ####
  # ______________ DOWNLOAD RESULTS _________________ ####
  # . ####
  # Download the Results #######
  output$downloadData1 <- shiny::downloadHandler(
    filename = function() {
      cleandate <- gsub(' ', '_', gsub(':', '.', Sys.time()))
      fname <- paste0("EJAM-OUT-", input$analysis_shortname, "_", 
                      input$cutoffRadius, '_miles_', cleandate, ".xlsx", sep='')
      fname
    },
    
    content = function(file) {
      cat('\nTRYING TO DOWNLOAD ', 
          paste0("EJAM-OUT-", input$analysis_shortname, "-", gsub(':', '-', Sys.time()), ".xlsx", sep=''),
          '\n\n')
      
      # OUTPUT RESULTS TABLE HERE - ONE ROW IS FOR OVERALL UNIQUE RESIDENTS OR BLOCKS, THEN 1 ROW PER SITE:
      
      #write.csv(x = rbind(datasetResults()$results_overall, datasetResults()$results_bysite, fill = TRUE), file = file, row.names = FALSE)
      #  this  would be much faster than write.csv, and works on data.frame or data.table:
      # data.table::fwrite(   x = rbind(datasetResults()$results_overall, datasetResults()$results_bysite, fill = TRUE), file = file)
      # see "EJAM/inst/notes_MISC"
      #
      #  but now using Excel output:
      # library(openxlsx)
 
      wb <- workbook_output_styled(
        overall = datasetResults()$results_overall, 
        eachsite = datasetResults()$results_bysite
        )
      saveWorkbook(wb, file = file, overwrite = TRUE)
      
      # openxlsx::write.xlsx(rbind(
      #   datasetResults()$results_overall, 
      #   datasetResults()$results_bysite, 
      #   fill = TRUE
      # ), 
      # file = file)
      # openxlsx::createStyle()
      cat('\n\n Wrote to ', file)
      
      ############################################################################### #
      # OBSOLETE code about user metadata we could save ####
      
      if (1 == 'obsolete code- it was meant to output metadata appended
          to the tabular results but we want a clean table and any metadata separately if at all') {
        
        # save to file which NAICS filters used
        userin = ""
        facility_mustbe_that_naics_in_this_program  = paste(input$facility_mustbe_that_naics_in_this_program,     collapse = ", ")
        facility_mustbe_in_this_program = paste(input$facility_mustbe_in_this_program,    collapse = ", ")
        #industryList =               paste(input$naics_user_wrote_in_box,        collapse = ", ")
        industryList =  paste(industryList, input$naics_user_picked_from_list, collapse = ", ")
        
        # save to file list of Facility IDs specified
        f1=""
        if(!is.null(input$file_uploaded_FRS_IDs)) {
          f1=input$file_uploaded_FRS_IDs
          f1=f1[0]
        }
        
        # save to file these settings - This might not be an option at all - may just pick one approach
        userin=addUserInput(file,userin, input$expandRadius, "Expand distance for facilities with no census block centroid within selected buffer distance: ")
        # this used to output this metadata on what distance was used, but that is in the output table now?
        userin=addUserInput(file,userin, as.character(getCutoff()), "Define Buffer Distance (in miles?): ") #as.character(getCutoff())
        
        # This used to output metadata about what NAICS or facility IDs, etc. were specified by the user:
        userin=addUserInput(file,userin, facility_mustbe_in_this_program, "Include facilities with records in: ")
        userin=addUserInput(file,userin, facility_mustbe_that_naics_in_this_program, "Match your NAICS code selection with: ")
        userin=addUserInput(file,userin, industryList,   "Industry/Industries: ")
        userin=addUserInput(file,userin, f1,  "Upload list of FRS IDs. Filename: ") #input$file_uploaded_FRS_IDs
        # used to report the name of the lat/lon uploaded file:
        # userin=addUserInput(file,userin, f2,  "Upload list of locations with lat lon coordinates. Filename: ") #input$file2 was old name # file_uploaded_latlons
        
        # save to file output results overall  ?
          
        cat(userin,  file=file) # write all those settings.
 
        # cat('\n\n Wrote to ', file)
      }
      # session$reload()
    }
  )
  ############################################################################################## #
}
