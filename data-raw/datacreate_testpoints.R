
## code to prepare testpoints datasets -- 
# 
# - points data objects, 
# - points excel files, 
# - sites2blocks data objects, and 
# - documentation files for all those

if (basename(getwd()) != "EJAM") {stop('do this from EJAM source package folder')}

nvalues <- c(2, 10, 100, 1000, 10000) # numbers of point locations, picked from FRS points.
myrad <- 1 # radius in miles. Larger would create MUCH larger versions of sites2blocks example objects

for (n in nvalues) {
  
  # CREATE TEST DATA OF RIGHT SIZE #### 
  varname <- paste0("testpoints_", n)
  varvalue <- testpoints_n(n=n, weighting = "frs", dt = FALSE) 
  
  # WITH DUMMY VALUES FOR MOST COLUMNS ####
  varvalue$sitename = paste0("Example name for site ", 1:n)
  
  # Drop other columns
  # just use lat lon siteid sitename
  varvalue = varvalue[,c("lat", "lon", "siteid", "sitename")]   
  # varvalue$NAICS = NULL # 722410
  # varvalue$SIC = NULL # 5992
  # varvalue$REGISTRY_ID = NULL # c(EJAM::test_xtrac, rep(NA,n))[1:n] # 
  # varvalue$PGM_SYS_ACRNMS = NULL
  
  assign(varname, varvalue)
  
  
  # SAVE AN EXCEL VERSION OF THE TEST DATA POINTS ####
  # varvalue$EJScreenMap = url_ejscreenmap(varvalue$lon, varvalue$lat, as_html = FALSE) # NOT CLICKABLE IN EXCEL THIS WAY BUT OK
  # varvalue$REGISTRY_ID <- as.character(varvalue$REGISTRY_ID)  # for excel just save as character not number, or could write as special zip code format in excel
  writexl::write_xlsx(list(varname = varvalue), path = paste0("./inst/testdata/latlon/", varname, ".xlsx"))  
  
  #  these links work on popup map but not in excel, if as_html=T 
  # varvalue$EJScreenMap = url_ejscreenmap(varvalue$lon, varvalue$lat, as_html = T)
  
  # SAVE AS DATA IN PACKAGE ####
  text_to_do = paste0("usethis::use_data(", varname, ", overwrite=TRUE)")
  eval(parse(text = text_to_do))
  
  # SAVE DOCUMENTATION FILE ####
  filecontents <- paste0( 
    "#' @name testpoints_", n, " 
#' @docType data
#' @title test points data.frame with columns siteid lat lon 
NULL
"
  )
  writeChar(filecontents, con = paste0("./R/data_", varname, ".R"))
  
  
  
  ################################## #   
  
  ## create examples of getblocksnearby() outputs #### 
  
  if (n <= 1000) { # do not create huge sites2blocks files, as for larger sets of sites
    
    getoutvalue = getblocksnearby(varvalue, radius = myrad)
    getoutname = paste0("sites2blocks_example_", n, "pts_", myrad, "miles")
    assign(getoutname, getoutvalue)
    
    # SAVE AS DATA IN PACKAGE ####
    text_to_do = paste0("usethis::use_data(", getoutname, ", overwrite=TRUE)")
    eval(parse(text = text_to_do))
    
    # SAVE DOCUMENTATION FILE ####
    filecontents <- paste0( 
      "#' @name ", getoutname, " 
#' @docType data
#' @title test output of getblocksnearby() or input to doaggregate() 
NULL
"
    )
    writeChar(filecontents, con = paste0("./R/data_", getoutname, ".R"))
  }
  
} # end of loop

################################## #   

## create example of doaggregate() output #### 

testdata_doaggregate_output_100 <- doaggregate(sites2blocks_example_100pts_1miles) # , sites2states_or_latlon = testpoints_100, radius = 1)

# use in package
usethis::use_data(testdata_doaggregate_output_100, overwrite = T)

# create documentation
getoutvalue = testdata_doaggregate_output_100
getoutname = ("testdata_doaggregate_output_100")
# assign(getoutname, getoutvalue)

# SAVE AS DATA IN PACKAGE ####
text_to_do = paste0("usethis::use_data(", getoutname, ", overwrite=TRUE)")
eval(parse(text = text_to_do))

# SAVE DOCUMENTATION FILE ####
filecontents <- paste0( 
  "#' @name ", getoutname, " 
#' @docType data
#' @title test output of doaggregate(sites2blocks_example_100pts_1miles)
NULL
"
)
writeChar(filecontents, con = paste0("./R/data_", getoutname, ".R"))

################################## #   

## create example of ejamit() output #### 

testdata_ejamit_output_100pts_1mile <- ejamit(testpoints_100, radius = 1)
# use in package
usethis::use_data(testdata_ejamit_output_100pts_1mile, overwrite = T)

# create documentation
getoutvalue = testdata_ejamit_output_100pts_1mile
getoutname = ("testdata_ejamit_output_100pts_1mile")
# assign(getoutname, getoutvalue)

# SAVE AS DATA IN PACKAGE ####
text_to_do = paste0("usethis::use_data(", getoutname, ", overwrite=TRUE)")
eval(parse(text = text_to_do))

# SAVE DOCUMENTATION FILE ####
filecontents <- paste0( 
  "#' @name ", getoutname, " 
#' @docType data
#' @title test output of ejamit(testpoints_100, radius = 1)
NULL
"
)
writeChar(filecontents, con = paste0("./R/data_", getoutname, ".R"))
