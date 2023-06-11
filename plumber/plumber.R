
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

# library(plumber)
# root <- pr("./plumber/plumber.R")
# root
# root %>% pr_run()
# browseURL("http://localhost:8000/ejam_overall")
# 
# #  browseURL("http://localhost:8000/getblocksnearby") 
# #  browseURL("http://localhost:8000/doaggregate") 


#* ejam_overall
#* @param lat decimal degrees
#* @param lon decimal degrees
#* @param radius in miles 
#* @param ... passed to ejamit()
#* @get /ejam_overall
function(lat,lon, radius=1){
  library(EJAM)
  as.data.frame(EJAM::ejamit(sitepoints=data.table::data.table(lat=lat, lon=lon), radius=radius)[["results_overall"]])
}



#* return see getblocksnearby() -- sites2blocks table given lat lon  
#* @param lat see [getblocksnearby()]
#* @param lon see [getblocksnearby()]
#* @param ... passed to [getblocksnearby()]
#* @get /getblocksnearby
function(lat,lon){
  EJAM::getblocksnearby(data.table::data.table(lat=lat,lon=lon))
}

#* return see doaggregate() -- list of tables and other info summarizing demog and envt based on sites2blocks table  
#* @param sites2blocks see [doaggregate()]
#* @param sites2states_or_latlon see [doaggregate()]
#* @param countcols see [doaggregate()]
#* @param popmeancols see [doaggregate()]
#* @param calculatedcols see [doaggregate()]
#* @param ... passed to [doaggregate()]
#* @get /doaggregate
function(sites2blocks,sites2states_or_latlon, countcols, popmeancols, calculatedcols, ...){
  EJAM::doaggregate(sites2blocks=sites2blocks, sites2states_or_latlon=sites2states_or_latlon, countcols=countcols, popmeancols=popmeancols, calculatedcols=calculatedcols, ... )  
}

