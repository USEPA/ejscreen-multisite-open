if (1==0) {
  
  # ####################################################################################
# Interactive app for running a batch of buffer/proximity reports for a list of places,
# aggregating envt and demog indicators in each circular buffer around each facility or site
# ####################################################################################

shinyServer(function(input, output, session) {
  
  # build localtree quad tree index for this session ####
  ## *** DOES localtree HAVE TO BE RECREATED EACH TIME dataLocationListProcessed REACTIVE UPDATES??
  # SEEMS LIKE THAT WOULD BE EVERY TIME radius is updated, 
  # but this is slow and only needs to happen once per session, right?
  localtree <- SearchTrees::createTree(EJAMblockdata::quaddata, treeType = "quad", dataType = "point")
  
  # warnings and text outputs re selected Facilities, Industry, or Locations ##########################################
  numUniverseSource <- function() {
    selInd=0
    if (nchar(input$selectIndustry1_byNAICS)>0 | length(input$selectIndustry2_by_selectInput)>0) {
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
    if (nchar(input$selectIndustry1_byNAICS)>0 & length(input$selectIndustry2_by_selectInput)>0) {
      print("Please use a single industry select option.")
    }
  }
  
  getWarning2 <- function() {
    tot=numUniverseSource()
    length_selectIndustry1=0
    length_selectIndustry2=0
    if (!is.null(input$selectIndustry1_byNAICS)) {
      length_selectIndustry1=nchar(input$selectIndustry1_byNAICS)
    }
    if (!is.null(input$selectIndustry2_by_selectInput)) {
      length_selectIndustry2=nchar(input$selectIndustry2_by_selectInput)
    }
    if (length_selectIndustry1>0 & length_selectIndustry2>0) {
      print("Please use a single industry select option.")
    }
    else if(tot>1) {
      print(paste("Please use only one method of selecting universe (ie, select by industry, location, OR facility)"))
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
    if (length(input$selectIndustry1_byNAICS) > 1) {  # not really used except in testing tab
      x = paste(input$selectIndustry1_byNAICS,collapse=", ")
      return(paste("Selected industries ", x))
    }  else {
      if (length(input$selectIndustry1_byNAICS) == 1 & nchar(input$selectIndustry1_byNAICS) > 0) {
        return(paste("Selected industry ", input$selectIndustry1_byNAICS))
      }
    }
    return('')
  })
  
  output$selectInd2_for_testing <- renderPrint({  # not really used except in testing tab
    if (length(input$selectIndustry2_by_selectInput) > 1) {
      x=paste(input$selectIndustry2_by_selectInput,collapse=", ")
      return(paste("Selected industries ", x))
    } else {
      # & nchar(input$selectIndustry2_by_selectInput) > 0
      if (length(input$selectIndustry2_by_selectInput) == 1 ) {
        return(paste("Selected industry ", input$selectIndustry2_by_selectInput))
      }
    } 
    return('')
  })
  
  output$selectScope1 <- renderPrint({
    input$goButton1  # seems not to be used at all
    isolate(input$selectFrom1)
  })
  
  output$selectScope2 <- renderPrint({
    input$goButton1  # seems not to be used at all
    isolate(input$selectFrom2) })
  
  output$inputValue <- renderPrint(input$goButton3)  # seems not to be used at all
  output$file_uploaded_FRS_IDs_df <- renderPrint({input$file_uploaded_FRS_IDs}) # not really used except in testing tab
  output$file_uploaded_latlons_df <- renderPrint({input$file_uploaded_latlons}) # not really used except in testing tab
  
  # names of input file  ##########################################
  myfile_uploaded_FRS_IDs <- reactive({input$file_uploaded_FRS_IDs})
  myfile_uploaded_latlons <- reactive({input$file_uploaded_latlons})
  
  # Input Radius, Maxcutoff, etc. parameters ##########################################
  
  # THESE DO NOT MAKE SENSE... input$whatever is already a reactive value... so why redefine those here just renaming them??
  getCutoff <- reactive({
    return(input$cutoffRadius)
  })
  
  getMaxcutoff <- reactive({
    return(maxcutoff_default)
  })
  
  setUnique <- reactive({
    if (input$uniqueOutput=="no"){
      return(FALSE)
    }
    else {
      return(TRUE)
    }
  })
  
  #avoidorphans
  # Expand distance for facilities with no nearby block centroid ####
  doExpandradius <- reactive({
    if (input$expandRadius=="no"){
      return(FALSE)
    }
    else {
      return(TRUE)
    }
  })
  # _______________________________ ####
  ####################################################################################################################### #
  # *** LAT LON: dataLocationList DEFINES BUFFERS** ######
  # imported location data
  ####################################################################################################################### #
  dataLocationList <- reactive({
    in2File <- input$file_uploaded_latlons
    if (is.null(in2File))
      return(NULL)
    isolate(read.table(file=in2File$datapath, sep=',', header=TRUE, quote='"'))
    
  })
  
  output$inLocationList <- renderTable(
    {
      if(is.null(dataLocationList())) {return () }
      dataLocationList()
    }
  )
  ####################################################################################### #
  # _____Find nearby blocks and aggregate for buffer - via quadtree and doaggregate  # *********** ########
  # IT GETS NEARBY BLOCKS AND AGGREGATES EJSCREEN DATA FROM THE BLOCKS NEAR EACH POINT
  ####################################################################################### #
  dataLocationListProcessed <- reactive({
    if(is.null(dataLocationList())) {return () }
    sitepoints <- data.table::copy(dataLocationList())
    setDT(sitepoints)#, key = 'siteid')
    
    cutoff = getCutoff() # radius (units?)
    maxcutoff = getMaxcutoff()  # reactive, max distance to search
    get_unique = setUnique()     # reactive, TRUE = stats are for dissolved single buffer to avoid doublecounting. FALSE = we want to count each person once for each site they are near.
    avoidorphans = doExpandradius() # Expand distance searched, when a facility has no census block centroid within selected buffer distance
    
    # note this does require that blockquadtree be available as data
    # getblocksnearby  ####
    system.time({
      sites2blocks <- EJAM::getblocksnearby(
        sitepoints =  sitepoints,
        cutoff = cutoff, # radius
        maxcutoff = maxcutoff,
        # uniqueonly = uniqueonly,
        avoidorphans = avoidorphans,
        quadtree = localtree
      )
    }) # end of timed function
    system.time({
      out <- doaggregate(sites2blocks = sites2blocks)
    })
    return(out)
  })
  # End of functions used for the dataLocList option
  
  
  # _______________________________ ####
  ####################################################################################################################### #
  # *** FACILITY ID: dataFacList DEFINES BUFFERS** #######
  # imported facility list  ...  uploaded_FRS_IDs
  ####################################################################################################################### #
  dataFacList <- reactive({
    inFile <- input$file_uploaded_FRS_IDs
    if (is.null(inFile))
      return(NULL)
    input$goButton3  # seems not to be used at all
    isolate(data.table::as.data.table(read.table(file=inFile$datapath, sep=',', header=TRUE, quote='"')))
  })
  output$inFacList <- renderTable({
    if(is.null(dataFacList())) {return () }
    dataFacList()
  })
  ####################################################################################### #
  # _____Find nearby blocks and aggregate for buffer - via quadtree and doaggregate  # *********** ########
  # IT GETS NEARBY BLOCKS AND AGGREGATES EJSCREEN DATA FROM THE BLOCKS NEAR EACH POINT
  ####################################################################################### #
  dataFacListProcessed <- reactive({
    kimssampledata <- dataFacList()
    kimssamplefacilities <- data.table::as.data.table(merge(x = kimssampledata, y = EJAMfrsdata::frs, by.x='REGISTRY_ID', by.y='REGISTRY_ID', all.x=TRUE))
    kimsunique <- data.table::as.data.table(unique(kimssamplefacilities[,.(REGISTRY_ID,LAT,LONG)]))
    rm(kimssampledata)
    rm(kimssamplefacilities)
    kimsunique$ID <- c(seq.int(nrow(kimsunique)))
    kimsunique <-as.data.table(unique(kimsunique[,.(ID,LAT,LONG)]))
    
    
    cutoff = getCutoff() # radius (units?)
    maxcutoff = getMaxcutoff()  # reactive, max distance to search
    get_unique = setUnique()     # reactive, TRUE = stats are for dissolved single buffer to avoid doublecounting. FALSE = we want to count each person once for each site they are near.
    avoidorphans = doExpandradius() # Expand distance searched, when a facility has no census block centroid within selected buffer distance
    # getblocksnearby  ####
    system.time(sites2blocks <- EJAM::getblocksnearby(
      sitepoints =  kimsunique, ##
      cutoff = cutoff, # radius
      maxcutoff = maxcutoff,
      # uniqueonly = uniqueonly,  # get_unique
      avoidorphans = avoidorphans,
      quadtree = localtree
    ))
    system.time(out <- doaggregate(sites2blocks))
    
    return(out)
  })
  # End of functions used for the dataFacList option #######3
  ####################################################################################################################### #
  
  # _______________________________ ####
  
  ####################################################################################################################### #
  # *** NAICS: SECTOR(S) DEFINE BUFFERS**  #######
  ####################################################################################################################### #
  
  # function that runs buffering on facilities selected via the NAICS dataset,
  # is defined here, not easily in separate file because it uses several reactives and facilities? which is in global env
  
  datasetNAICS <- function() {
    #### to pass all the reactives as parameters, you would do this:
    # selectIndustry1_byNAICS=input$selectIndustry1_byNAICS,
    # selectIndustry2_by_selectInput=input$selectIndustry2_by_selectInput,
    # cutoff=getCutoff(),
    # maxcutoff=getMaxcutoff(),
    # get_unique=TRUE,
    # avoidorphans=TRUE,
    # doExpandradius=doExpandradius(),
    # selectNaics_in_Datasystem1= input$selectNaics_in_Datasystem1,
    # selectNaics_and_Datasystem2 =input$selectNaics_and_Datasystem2)
    
    
    ################################################################## #
    # prep full FRS that has NAICS of all sites and their lat lon ####
    # Dataset of FRS sites and NAICS in long format (used to be facdata.rda)
    ################################################################## #
    
    mytest <- EJAMfrsdata::frs_naics_2016 # EJAMfrsdata::facilities
    mytest$cnaics <- as.character(mytest$NAICS)
    
    sub2 <- data.table::data.table(a = numeric(0), b = character(0))
    
    ################################################################## #
    # clean up users selections ####
    ################################################################## #
    
    if (nchar(input$selectIndustry1_byNAICS)>0 & length(input$selectIndustry2_by_selectInput)>0) {
      return()
    }
    cutoff=getCutoff()  # reactive  (e.g., 3) in miles
    maxcuttoff=getMaxcutoff()  # reactive, max distance to search e.g. 4000
    get_unique=setUnique() # maybe no longer users?  reactive, TRUE = stats are for dissolved single buffer to avoid doublecounting. FALSE = we want to count each person once for each site they are near.
    avoidorphans=doExpandradius()  # reactive # Expand distance searched, when a facility has no census block centroid within selected buffer distance
    
    # which datasystems are we searching?
    selectNaics_in_Datasystem1 = input$selectNaics_in_Datasystem1 # e.g. NULL
    selectNaics_and_Datasystem2 = input$selectNaics_and_Datasystem2 # e.g. NULL
    inNAICS1 = input$selectIndustry1_byNAICS  # e.g. '' (empty)
    inputnaics1 <- as.list(strsplit(inNAICS1, ",")[[1]])
    inNAICS2=input$selectIndustry2_by_selectInput
    
    if (nchar(inNAICS1)>0 | length(inNAICS2)>0) {
      
      selectNaics_in_Datasystem1 = c('OIL','AIRS/AFS')
      selectNaics_and_Datasystem2 = c('RCRAINFO')
      nrow(selectNaics_in_Datasystem1)
      
      inputnaics1 <- as.list(strsplit(inNAICS1, ",")[[1]])
      inputnaics <- input$selectIndustry2_by_selectInput
      inputnaics=c(inputnaics1,inNAICS2)
      inputnaics=unique(inputnaics[inputnaics != ""])
      x <- paste("^",inputnaics,collapse="|")   ### the NAICS specified by user
      y <- stringr::str_replace_all(string=x, pattern=" ", repl="")
      
      ################################################################## #
      # Match user NAICS to FRS NAICS, TO GET LAT/LON OF MATCHED SITES ####
      ################################################################## #
      
      matches <- unique(grep(y, mytest$cnaics, value=TRUE))  #
      
      if (length(selectNaics_in_Datasystem1)>0 & length(selectNaics_and_Datasystem2)>0) {
        temp<-mytest[PROGRAM %in% selectNaics_in_Datasystem1] # PROGRAM  IS MISSING
        temp<-temp[cnaics %in% matches]
        temp<-unique(temp[,.(REGISTRY_ID)])
        sub1 <-data.table::as.data.table(merge(x = mytest, y = temp, by.x='REGISTRY_ID', by.y='REGISTRY_ID'), all.y=TRUE)
        sub2<-sub1[PROGRAM %in% selectNaics_and_Datasystem2]
        sub2$ID<- c(seq.int(nrow(sub2)))
      }
      else if (length(selectNaics_and_Datasystem2)>0) {
        sub2<-mytest[PROGRAM %in% selectNaics_and_Datasystem2]
        sub2$ID<- c(seq.int(nrow(sub2)))
      }
      else if (length(selectNaics_in_Datasystem1)>0) {
        sub2<-mytest[cnaics %in% matches]
        sub2$ID<- c(seq.int(nrow(sub2)))
        colnames(sub2)
      }
      print(paste("Number of matches = ", nrow(sub2)))
      
      ################################################################## #
      # CALL FUNCTIONS DOING DISTANCES (BUFFERS) AND AGGREGATION  ### ###
      ################################################################## #
      ####################################################################################### #
      # _____Find nearby blocks and aggregate for buffer - via quadtree and doaggregate  # *********** ########
      # IT GETS NEARBY BLOCKS AND AGGREGATES EJSCREEN DATA FROM THE BLOCKS NEAR EACH POINT
      ####################################################################################### #
      
      if (nrow(sub2)>0) {
        
        # older code:
        # system.time(res <- getblocksnearby(sitepoints=sub2,cutoff,maxcuttoff,get_unique,avoidorphans))
        # system.time(dat <- doaggregate(sub2,res))
        # return(dat)
        
        # getblocksnearby  ####
        system.time({
          sites2blocks <- EJAM::getblocksnearby(
            sitepoints =  sitepoints,
            cutoff = cutoff, # radius
            maxcutoff = maxcutoff,
            # uniqueonly = uniqueonly,
            avoidorphans = avoidorphans,
            quadtree = localtree
          )
        }) 
        system.time({out <- doaggregate(sites2blocks = sites2blocks)})
        return(out)        
      }
      else {
        print("No matches were found")
        out <- data.table::as.data.table(setNames(data.frame(matrix(ncol = 1, nrow = 0)),paste0("No matches were found"#, c(1:1)
        )))
        return(out)
      }
    }
    else {
      print("Please submit an industry to run this query")
      out <- data.table::as.data.table(setNames(data.frame(matrix(ncol = 1, nrow = 0)),paste0("Please submit an industry to run this query", c(1:1))))
      return(out)
    }
  }
  # _______________________________ ####
  
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
      cat(userin,  file=file,append=T)
    }
    #if(!is.null(mystring)) {
    
    #if (!is.null(mystring) & nchar(mystring)>0)
    #{
    #  userin=paste(mystring, userin,sep = "\n")
    #    userin=paste(strdesc, userin, " ")
    #    cat(userin,  file=file,append=T)
    #}
    return(userin)
  }
  
  # Download the Results #######
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste0("ej-", Sys.Date(), "-",Sys.time(), ".csv",sep='')
    },
    contentType = 'text/csv',
    content = function(file) {
      cat('\nTRYING TO DOWNLOAD downloadData1 as ', paste0("ej-", Sys.Date(), "-",Sys.time(), ".csv",sep=''), '\n\n')
      print(str(datasetResults()))
      
      if (input$uniqueOutput == 'no') {
        write.csv(datasetResults()$results_bysite, file, row.names = FALSE) # check this - why does it write here and again below??
      } 
      if (input$uniqueOutput == 'yes') {
        write.csv(datasetResults()$results_overall, file, row.names = FALSE)
      } 
      if (input$uniqueOutput == 'both') {
        write.csv(rbind(datasetResults()$results_overall, datasetResults()$results_bysite, fill=TRUE), file, row.names = FALSE)
      }
      
      # write.csv(datasetResults()[ , 'results_bysite'], file)
      #must go back to version control and add this back in   ????????????
      if (1 == 'have not fixed this code yet') {
        
        userin=""
        
        selectNaics_in_Datasystem1=paste(input$selectNaics_in_Datasystem1,collapse=", ")
        selectNaics_and_Datasystem2=paste(input$selectNaics_and_Datasystem2,collapse=", ")
        industryList=paste(input$selectIndustry1_byNAICS,collapse=", ")
        industryList=paste(industryList,input$selectIndustry2_by_selectInput,collapse=", ")
        
        #"Individual facility statistics"
        
        definedOutput1=""
        definedOutput2=""
        if(!is.null(input$uniqueOutput)) {
          if (nchar(input$uniqueOutput)>1){
            if(input$uniqueOutput=="yes"){
              definedOutput1="Output for aggregating population statistics"
            }
            else {
              definedOutput2="Individual facility statistics"
            }
          }
        }
        
        f1=""
        if(!is.null(input$file_uploaded_FRS_IDs)) {
          f1=input$file_uploaded_FRS_IDs
          f1=f1[0]
        }
        
        f2=""
        if(!is.null(input$file_uploaded_latlons)) {
          f2=input$file_uploaded_latlons
          f2=f2[0]
        }
        
        #addUserInput <- function(file,userin,mystring,strdesc){
        userin=addUserInput(file,userin, definedOutput1,   "Defined output: ")
        userin=addUserInput(file,userin, definedOutput2,   "Defined output: ")
        userin=addUserInput(file,userin, input$expandRadius, "Expand distance for facilities with no census block centroid within selected buffer distance: ")
        userin=addUserInput(file,userin, as.character(getCutoff()), "Define Buffer Distance (in miles?): ") #as.character(getCutoff())
        userin=addUserInput(file,userin, selectNaics_and_Datasystem2, "Include facilities with records in: ")
        userin=addUserInput(file,userin, selectNaics_in_Datasystem1, "Match your NAICS code selection with: ")
        userin=addUserInput(file,userin, industryList,   "Industry/Industries: ")
        userin=addUserInput(file,userin, f1,  "Upload list of FRS IDs. Filename: ") #input$file_uploaded_FRS_IDs
        userin=addUserInput(file,userin, f2,  "Upload list of locations with lat lon coordinates. Filename: ") #input$file2 was old name # file_uploaded_latlons
        
        cat(userin,  file=file)
        
        # append tabular data to the file of results ?? #### 
        write.table(datasetResults()$results_bysite, file, append = T, sep = ",")
        # write.table(datasetResults()[ , 'results_bysite'], file, append = T, sep=",")
      }
      
      cat('\n\n Wrote to ', file)
      
      #session$reload()
      
    }
  )
  
  datasetResults <- function(){
    if (length(getWarning1() > 1) | length(getWarning2() > 1)) {
      return()
    }
    else if (nchar(input$selectIndustry1_byNAICS) > 0 | length(input$selectIndustry2_by_selectInput) > 0) {
      # e.g.,   "324110"  might be the value of input$selectIndustry2_by_selectInput 
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
  
  output$inNAICSresult <- renderTable(
    {
      # note that datasetNAICS is a separate function, not a reactive
      if(is.null(datasetNAICS( ))) {return () }
      datasetNAICS( )
    }
  )
  
})
}
