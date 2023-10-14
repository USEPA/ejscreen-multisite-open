####################################################### # 
# plumber:: helps create an API for access to EJAM functionality.
#
# browseURL("https://www.rplumber.io/articles/routing-and-input.html")
# 
# browseURL("https://httr.r-lib.org/articles/api-packages.html")
# 
# The pr object encapsulates all the logic represented in your plumber.R file. 
# To bring the API to life use the pr_run() method.
# You should see a message about your API running on your computer on port 8000. 
# The API will continue running in your R session until you press the Esc key. 
# If you’re running this code locally on your personal machine, you should be able to use the API in a browser.

####################################################### # 
# START UP THE API ####
#
library(plumber)
getwd()
root <- pr("./plumber/plumber.R")
root
root %>% pr_run()

####################################################### # 
# TRY OUT API ON LOCAL SERVER ####
if (1==0) { 
##############  # 
# try ejamit
#
browseURL("http://localhost:8000/ejamit")
url2 = "http://127.0.0.1:8036/ejamit?lon=-101&lat=36&radius=1"
browseURL(url2)
req2 = httr2::request(url2)
out2 = httr2::req_perform(req2)
class(out2)
str(out2)
print(out2)
browseURL(url2)
rm(url2, req2, out2)

##############  # 
# try getblocksnearby  
#
browseURL("http://localhost:8000/getblocksnearby") 
url3 = "http://127.0.0.1:8036/getblocksnearby?lon=-101&lat=39&radius=1"
browseURL(url3)
req3 = httr2::request(url3)
out3 = httr2::req_perform(req3)
class(out3)
str(out3)
print(out3)
browseURL(url3)

##############  # 
# try doaggregate
#
browseURL("http://localhost:8000/doaggregate")

# *** One challenge is that several large files have to be loaded and block index has to be built
# which is time-consuming, so there is no response for many seconds unless those are already done.
# see .onAttach()
#  EJAM::
dataload_from_aws()
indexblocks()
dataload_from_package()
}

####################################################### # 
#  DEFINE THE API ENDPOINTS ####
####################################################### # 

####################################################### #
# ejamit ####
#* ejamit 
#* @param lat decimal degrees (single point only, for now)
#* @param lon decimal degrees (single point only, for now)
#* @param radius in miles 
#* @param ... passed to [ejamit()]
#* @get /ejamit
#* @serializer csv
function(lat, lon, radius=3){
  library(EJAM)
  out = as.data.frame(EJAM::ejamit(sitepoints = data.table::data.table(lat = lat, lon = lon), radius = radius)[["results_overall"]])
as_attachment(out)
  ####################################################### # 
  #                      errors
  # Code	Details
  # 500	Error: Internal Server Error
  # Response body
  # Download
  # {
  #   "error": "500 - Internal server error",
  #   "message": "Error in x/7918: non-numeric argument to binary operator\n"
  # }
  # Response headers
  # content-encoding: gzip 
  # content-type: application/json 
  # date: Fri01 Sep 2023 15:37:55 GMT 
  # transfer-encoding: chunked 
  ####################################################### # 
}

####################################################### #
# getblocksnearby ####
#* return see getblocksnearby() -- sites2blocks table given lat lon   # why does the line not just have name like ejamit ?
#* @param lat see [getblocksnearby()]
#* @param lon see [getblocksnearby()]
#* @param ... passed to [getblocksnearby()]
#* @get /getblocksnearby
function(lat, lon){
  EJAM::getblocksnearby(data.table::data.table(lat = lat, lon = lon))
}

####################################################### # 
# doaggregate ####
#* return see doaggregate() -- list of tables and other info summarizing demog and envt based on sites2blocks table    # why does the line not just have name like ejamit ?
#* @param sites2blocks see [doaggregate()]
#* @param sites2states_or_latlon see [doaggregate()]
#* @param countcols see [doaggregate()]
#* @param popmeancols see [doaggregate()]
#* @param calculatedcols see [doaggregate()]
#* @param ... passed to [doaggregate()]
#* @get /doaggregate
function(sites2blocks, sites2states_or_latlon, countcols, popmeancols, calculatedcols, ...){
  EJAM::doaggregate(sites2blocks = sites2blocks, 
                    sites2states_or_latlon = sites2states_or_latlon, 
                    countcols = countcols, popmeancols = popmeancols, calculatedcols = calculatedcols, ... )
}
####################################################### # 

