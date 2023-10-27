####################################################### # 
#
# Create an API for access to EJAM functionality, using the plumber package.
# 
# also see    EJAM file "plumber/test_the_api.R"


####################################################### # 
#  DEFINE THE API ENDPOINTS ####
####################################################### # 


# echo ####
#
#* Echo the parameter that was sent in
#* @param msg The message to echo back.
#* @get /echo
function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}
####################################################### #


# ejamit ####
#
#* ejamit
#* @param lat decimal degrees (single point only, for now)
#* @param lon decimal degrees (single point only, for now)
#* @param radius in miles
#* @param attachment NOT ENABLED YET - "true" or "false", whether to return attachment or normal default format (csv-like json)
#* @param test "true" or "false" if return dummy results ignoring lat, lon, radius
#* @get /ejamit
function(lat = NULL, lon = NULL, radius = 1, attachment = "false", test = "false") {
  
  if (test == "true") {
    out <- as.data.frame(EJAM::testoutput_ejamit_10pts_1miles$results_overall)
  } else {
    
    out <- ejamit(
      sitepoints = data.frame(lat = as.numeric(lat), lon = as.numeric(lon)), 
      radius = as.numeric(radius)
    )$results_overall
  }
  
  attachment <- FALSE # UNTIL SET UP CORRECTLY
  if (attachment == "true") {
    as_attachment(
      value = as.data.frame(out),
      filename = "ejamit_results.csv"
    )
  } else {
    out      
  }
}
####################################################### #

# ejamit_csv ####
#
#* ejamit_csv
#* @param lat decimal degrees (single point only, for now)
#* @param lon decimal degrees (single point only, for now)
#* @param radius in miles
#* @param attachment "true" or "false", whether to return attachment or normal default format (csv-like json)
#* @param test "true" or "false" if return dummy results ignoring lat, lon, radius
#* @serializer csv
#* @get /ejamit_csv
function(lat = NULL, lon = NULL, radius = 1, attachment = "true", test = "false") {
  
  
  if (test == "true") {
    out <- as.data.frame(EJAM::testoutput_ejamit_10pts_1miles$results_overall)
  } else {
    
   out <- ejamit(
     sitepoints = data.frame(lat = as.numeric(lat), lon = as.numeric(lon)), 
     radius = as.numeric(radius)
     )$results_overall
   
  }
  
  if (attachment == "true") {
  as_attachment(
    value = as.data.frame(out),
    filename = "ejamit_results.csv"
  )
  } else {
    out      
    }
}
  
####################################################### #
  
   

# getblocksnearby ####
#
#* getblocksnearby
#* @param lat Latitude of the site, a single point
#* @param lon Longitude of the site, a single point
#* @param radius Radius of circular area in miles. Find all Census blocks whose internal point is within radius of site point.
#* @get /getblocksnearby
function(lat, lon, radius) {
  
  # require(EJAM)
  # if (!exists("blockwts"))  dataload_from_aws()
  # if (!exists("localtree")) indexblocks()
  
  out <- EJAM::getblocksnearby(data.frame(
    lat = as.numeric(lat), 
    lon = as.numeric(lon)
    ),
    radius = as.numeric(radius)
    # , quadtree = localtree
    )
  
  out
    
}
####################################################### # 

# 
# # doaggregate ####
# #
# #* return see doaggregate() -- list of tables and other info summarizing demog and envt based on sites2blocks table    # why does the line not just have name like ejamit ?
# #* @param sites2blocks see [doaggregate()]
# #* @param sites2states_or_latlon see [doaggregate()]
# #* @param countcols see [doaggregate()]
# #* @param popmeancols see [doaggregate()]
# #* @param calculatedcols see [doaggregate()]
# #* @param ... passed to [doaggregate()]
# #* @get /doaggregate
# function(sites2blocks, sites2states_or_latlon, countcols, popmeancols, calculatedcols, ...){
#   require(EJAM)
#   if (!exists("blockwts"))  dataload_from_aws()
#   if (!exists("localtree")) indexblocks()
#   EJAM::doaggregate(sites2blocks = sites2blocks, 
#                     sites2states_or_latlon = sites2states_or_latlon, 
#                     countcols = countcols, popmeancols = popmeancols, calculatedcols = calculatedcols, ... )
# }
# ####################################################### # 
 


# if (format == "excel") {
#   # NOT WORKING YET - THIS WOULD NOT RETURN A SPREADSHEET IF save_now=FALSE... IT JUST WOULD CREATE A WORKBOOK IN openxlsx::  format.
#   # out <- table_xls_from_ejam(ejamit(sitepoints = sitepoints, radius = radius), launchexcel = F, save_now = FALSE)
#   out <- as.data.frame(as.data.frame(EJAM::ejamit(sitepoints = sitepoints, radius = radius)[["results_overall"]]))
# }
# 



# MULTIPLE POINTS WILL NEED TO BE PASSED USING POST AND REQUEST BODY SO IT IS A BIT MORE COMPLICATED - NOT DONE YET

# if (!exists("blockwts"))  dataload_from_aws()
# if (!exists("localtree")) indexblocks() 


####################################################### #
####################################################### #

