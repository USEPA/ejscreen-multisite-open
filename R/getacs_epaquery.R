
get_any_rest_chunked_by_id <- function(objectIds, chunksize=200, ...) {
  #  to get ACS data or Block weights nearby from EPA server via API  ###   #
  
  ############################################################## #
  # generic function to break request into chunks ####
  ############################################################## #

    if (missing(objectIds)) {
    stop('this only works for objectIds so far, breaking up into groups of 200 or so objectIds.')
    # could write it to check if >1000 would be returned and then request in chunks in that case.
    }
  x <- list()
  n <- length(objectIds)
  extrachunk <- ifelse((n %/% chunksize) * chunksize == n, 0, 1) 
  
  for (chunk in 1:(extrachunk + (n %/% chunksize))) {
    istart <- 1 + (chunk - 1) * chunksize
    iend <- chunksize + istart - 1
    iend <- min(iend, n)
    idchunk <- objectIds[istart:iend]
     
    x[[chunk]] <- getacs_epaquery(objectIds = idchunk,  ...)
  }
  return(do.call(rbind, x))
}

getacs_epaquery_chunked <- function(servicenumber=7,
                                    objectIds=1:3, 
                                    outFields=NULL, 
                                    returnGeometry=FALSE, 
                                    justurl=FALSE, 
                                    chunksize=200, ...) {
  ############################################################## #
  # function to break request into chunks ####
  ############################################################## #
  
  if (missing(objectIds)) {
    stop('this only works for objectIds so far, breaking up into groups of 200 or so objectIds.')
    # could write it to check if >1000 would be returned and then request in chunks in that case.
  }
  
  # warning('check if still has a bug to fix where it duplicates the last row of each chunk')
  x <- list()
  n <- length(objectIds)
  extrachunk <- ifelse((n %/% chunksize) * chunksize == n, 0, 1) 
  
  for (chunk in 1:(extrachunk + (n %/% chunksize))) {
    istart <- 1 + (chunk - 1) * chunksize
    iend <- chunksize + istart - 1
    iend <- min(iend, n)
    idchunk <- objectIds[istart:iend]
    # # TESTING:
    #   x[[chunk]] <- data.frame(id=idchunk, dat=NA)
    #   print(idchunk); print(x) # cumulative so far
    
    x[[chunk]] <- getacs_epaquery(objectIds = idchunk, outFields=outFields, servicenumber=servicenumber, ...)
  }
  return(do.call(rbind, x))  
  
  # # TESTING:
  # x <- list() # chunked chunks
  # x[[1]] <- getacs_epaquery_chunked(    1:10000, chunksize = 100)
  # x[[2]] <- getacs_epaquery_chunked(10001:50000, chunksize = 100)
  # x[[3]] <- getacs_epaquery_chunked(50001:70000, chunksize = 100)
  # x[[4]] <- getacs_epaquery_chunked(70001:74000, chunksize = 100)
  # x[[5]] <- getacs_epaquery_chunked(74001:75000, chunksize = 100)
  # x[[6]] <- getacs_epaquery_chunked(75001:76000, chunksize = 100)
  # xall <- do.call(rbind, x)
}

############################################################## #
# function to request <200 rows from mapservice ####
############################################################## #

getacs_epaquery <- function(servicenumber=7,
                            objectIds=1:3, 
                            outFields=NULL, 
                            returnGeometry=FALSE, 
                            justurl=FALSE, 
                           ...) {
  # Documentation of format and examples of input parameters:
  # https://geopub.epa.gov/arcgis/sdk/rest/index.html#/Query_Map_Service_Layer/02ss0000000r000000/
  
  # if (length(objectIds) < 1 | !all(is.numeric(objectIds))) {stop('no objectIds specified or some are not numbers')}
  if (any(objectIds == '*')) {stop('Trying to specify all objectIds will not work')}
  if (length(objectIds) > 200) {
    warning('seems to crash if more than about 211 requested per query - chunked version not yet tested')
    
    # return(get_any_rest_chunked_by_id(servicenumber=servicenumber,
    #                                   objectIds=objectIds, 
    #                                   outFields=outFields, 
    #                                   returnGeometry=returnGeometry, 
    #                                   justurl=justurl, 
    #                                   ...))
    return(getacs_epaquery_chunked(servicenumber=servicenumber,
                                   objectIds=objectIds, 
                                   outFields=outFields, 
                                   returnGeometry=returnGeometry, 
                                   justurl=justurl, 
                                   ...))
    }
  
  # use bestvars default outFields if unspecified ####
  if (is.null(outFields)) {
    bestvars <- c( ## bestvars ### #
      # outFields
      "OBJECTID",  # unique id 1 onwards
      "STCNTRBG",  # blockgroup fips
      "AREALAND", "AREAWATER",
      
      "TOTALPOP",   # population count 
      
      "LOWINC", "POV_UNIVERSE_FRT", "PCT_LOWINC",
      
      "HH_BPOV", "HSHOLDS", "PCT_HH_BPOV",
      
      "EDU_LTHS", "EDU_UNIVERSE", "PCT_EDU_LTHS",
      
      "LINGISO", "PCT_LINGISO",
      
      "EMP_STAT_UNEMPLOYED",
      "EMP_STAT_UNIVERSE",
      "PCT_EMP_STAT_UNEMPLOYED",
      
      "AGE_LT5",      "PCT_AGE_LT5",
      "AGE_GT64",     "PCT_AGE_GT64",
      
      "NUM_MINORITY", "PCT_MINORITY",
      
      "WHITE",        "PCT_WHITE",
      "BLACK",        "PCT_BLACK",
      "HISP" ,        "PCT_HISP",
      "ASIAN",        "PCT_ASIAN",
      "AMERIND",      "PCT_AMERIND",
      "HAWPAC",       "PCT_HAWPAC",
      "OTHER_RACE",   "PCT_OTHER_RACE",
      "TWOMORE",      "PCT_TWOMORE",
      "NHWHITE",      "PCT_NHWHITE",
      "NHBLACK",      "PCT_NHBLACK",
      "NHASIAN",      "PCT_NHASIAN",
      "NHAMERIND",    "PCT_NHAMERIND",
      "NHHAWPAC",     "PCT_NHHAWPAC",
      "NHOTHER_RACE", "PCT_NHOTHER_RACE",
      "NHTWOMORE",    "PCT_NHTWOMORE",
      
      "HOME_PRE60", "HSUNITS", "PCT_HOME_PRE60"
    )
    outFields <- bestvars
  } # use default fields if none specified
  # reformat the parameters - now done by url_... function ####
  # outFields <- paste(outFields, collapse = ',') # if a vector of values was provided, collapse them into one comma-separated string
  # objectIds <- paste(objectIds, collapse = ',') # if a vector of values was provided, collapse them into one comma-separated string
  
  ################################################################### #
  # assemble URL ####
  # url_to_get_ACS2019_rest_services_ejscreen_ejquery_MapServer_7()
  url_to_use <- url_to_get_ACS2019_rest_services_ejscreen_ejquery_MapServer_7(
    # servicenumber=servicenumber, 
    objectIds=objectIds,
    returnGeometry=returnGeometry,
    outFields=outFields,         # make same name?
    ...)
  ################################################################### #
  
  # call GET function (submit the query) ####
  
  if (justurl) {return(url_to_use)}
  return(get_via_url(url_to_use))
}



 