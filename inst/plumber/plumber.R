####################################################### #
#
# Create an API for access to EJAM functionality, using the plumber package.
#
# also see    EJAM file "plumber/test_the_api.R"


####################################################### #
#  DEFINE THE API ENDPOINTS ####
####################################################### #


# ejamit ####
#
#* ejamit
#* @apiDescription
#*   You can request EJAM analysis summary results for all residents within X
#*   miles of a single point defined by latitude and longitude.
#*   Like EJAM::ejamit()$results_overall (but with friendlier column names for indicators).
#*
#*   Results are in JSON format.
#*
#*   Calling from R for example:
#*   url2 <- "http://urlgoeshere/ejamit?lon=-101&lat=36&radius=1&test=true";
#*   results_overall <- httr2::request(url2) |> httr2::req_perform() |>
#*     httr2::resp_body_json() |> jsonlite::toJSON() |> jsonlite::fromJSON()
#*
#* @param lat decimal degrees (single point only, for now)
#* @param lon decimal degrees (single point only, for now)
#* @param radius Radius in miles
#* @param names "long" returns plain-English name of each indicator. Any other setting returns short variable names like "pctlowinc"
#* @param test "true" or "false" If true, returns a pre-calculated result (ignoring lat, lon, radius)
#* @get /ejamit
function(lat = 40.81417, lon = -96.69963, radius = 1, names = "long", test = "false") {

  lat <- as.numeric(lat); lon <- as.numeric(lon); radius <- as.numeric(radius)
  if (length(lat) != 1 | length(lon) != 1) {lat <- 40.81417; lon <- -96.69963}
  if (length(radius) != 1) {radius <- 1}

  if (test == "true") {
    out <- as.data.frame(EJAM::testoutput_ejamit_10pts_1miles$results_overall)
  } else {

    out <- ejamit(
      sitepoints = data.frame(lat = lat, lon = lon),
      radius = radius
    )$results_overall
  }

  if (names == "long") {
    names(out) <- fixcolnames(names(out), 'r', 'long')
  }

  # if (attachment == "true") {
  # plumber::as_attachment(
  #   value = as.data.frame(out),
  #   filename = "EJAM_results.csv"
  # )
  # } else {
  out
  # }
}
####################################################### #


# ejamit_csv ####
#
#* ejamit_csv
#* @apiDescription
#*   You can request EJAM analysis summary results for all residents within X
#*   miles of a single point defined by latitude and longitude.
#*   Like EJAM::ejamit()$results_overall (but with friendlier column names for indicators).
#*
#*   Results are in .csv format for download.
#*
#* @param lat decimal degrees (single point only, for now)
#* @param lon decimal degrees (single point only, for now)
#* @param radius Radius in miles
#* @param names "long" returns plain-English name of each indicator. Any other setting returns short variable names like "pctlowinc"
#* @param test "true" or "false" If true, returns a pre-calculated result (ignoring lat, lon, radius)
#* @serializer csv
#* @get /ejamit_csv
function(lat = 40.81417, lon = -96.69963, radius = 1, names = "long", test = "false") {

  lat <- as.numeric(lat); lon <- as.numeric(lon); radius <- as.numeric(radius)
  if (length(lat) != 1 | length(lon) != 1) {lat <- 40.81417; lon <- -96.69963}
  if (length(radius) != 1) {radius <- 1}

  if (test == "true") {
    out <- as.data.frame(EJAM::testoutput_ejamit_10pts_1miles$results_overall)
  } else {

    out <- ejamit(
      sitepoints = data.frame(lat = lat, lon = lon),
      radius = radius
    )$results_overall
  }

  if (names == "long") {
    names(out) <- fixcolnames(names(out), 'r', 'long')
  }

  # if (attachment == "true") {
  plumber::as_attachment(
    value = as.data.frame(out),
    filename = "EJAM_results.csv"
  )
  # } else {
  #   out
  #   }
}
####################################################### #


# getblocksnearby ####
#
#* getblocksnearby
#* @apiDescription like EJAM::getblocksnearby(), returns as json a table of distances to all Census blocks near given point.
#* @param lat decimal degrees (single point only, for now)
#* @param lon decimal degrees (single point only, for now)
#* @param radius Radius of circular area in miles. Finds all Census blocks whose internal point is within radius of site point.
#* @get /getblocksnearby
function(lat, lon, radius) {

  lat <- as.numeric(lat); lon <- as.numeric(lon); radius <- as.numeric(radius)
  if (length(lat) != 1 | length(lon) != 1) {lat <- 40.81417; lon <- -96.69963}
  if (length(radius) != 1) {radius <- 1}

  # require(EJAM)
  # if (!exists("blockwts"))  dataload_from_pins()
  # if (!exists("localtree")) indexblocks()

  out <- EJAM::getblocksnearby(
    data.frame(
      lat = lat,
      lon = lon
    ),
    radius = as.numeric(radius)  # , quadtree = localtree
  )
  out
}
####################################################### #


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
#   if (!exists("blockwts"))  dataload_from_pins()
#   if (!exists("localtree")) indexblocks()
#   EJAM::doaggregate(sites2blocks = sites2blocks,
#                     sites2states_or_latlon = sites2states_or_latlon,
#                     countcols = countcols, popmeancols = popmeancols, calculatedcols = calculatedcols, ... )
# }
# ####################################################### #


# echo ####
#
#* Echo the parameter that was sent in
#* @param msg The message to echo back.
#* @get /echo
function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}
####################################################### #


# if (format == "excel") {
#   # NOT WORKING YET - THIS WOULD NOT RETURN A SPREADSHEET IF save_now=FALSE... IT JUST WOULD CREATE A WORKBOOK IN openxlsx::  format.
#   # out <- table_xls_from_ejam(ejamit(sitepoints = sitepoints, radius = radius), launchexcel = F, save_now = FALSE)
#   out <- as.data.frame(as.data.frame(EJAM::ejamit(sitepoints = sitepoints, radius = radius)[["results_overall"]]))
# }
#


# MULTIPLE POINTS WILL NEED TO BE PASSED USING POST AND REQUEST BODY SO IT IS A BIT MORE COMPLICATED - NOT DONE YET

# if (!exists("blockwts"))  dataload_from_pins()
# if (!exists("localtree")) indexblocks()


####################################################### #
####################################################### #
