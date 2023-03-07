# 
# We want functions to do these separately:
# 
# -----------------------------------------------
# TO GET FACILITIES AND POINTS (LAT / LON):  - see EJAMejscreenapi::locate_by_id(type = 'program') 
# 

# **MAYBE WE SHOULD RENAME THESE RELATED FUNCTIONS TO BE CONSISTENTLY NAMED?**
# Several functions that query on y to find x,
# Maybe rename something like   x_from_y()   or   get_x_from_y()   or y2x() 
# e.g., 
# EJAM::naics_find  could be renamed maybe  get_naics_from_naicsindustryname()
#   or maybe naics_from_naicsindustryname() 

 # These functions are used to go between these x and y: 
# latlon
# naicsindustryname,   "pulp" or "Pulp, Paper, and Paperboard Mills"
# naics            3221
# sicindustryname
# sic
# siteid
# programid
# program
#   etc. etc. 
# 



# examples of using them right now:
# 
#   # See a data table of facilities in one industry
#   
#   DT::datatable(
#     EJAMfrsdata::frs[EJAMfrsdata::frs$REGISTRY_ID %chin% unlist(
#       EJAMfrsdata::get_siteid_from_naics(
#         EJAM::naics_find("pulp", add_children = FALSE))[,"REGISTRY_ID"]), 1:5], 
#     caption = "FACILITIES WITH NAICS CODE MATCHING QUERY TERM 'PULP' ", filter = "top")
# 
# # See a map of one industry
#  
# # Easy to get just lat,lon,site,naics:

#   mapfast( get_siteid_from_naics( naics_find("pulp")))  
# 
# # But to get site name and program and programid, it is a little more awkward right now:
#
# EJAMejscreenapi::mapfast(
#   EJAMfrsdata::frs[EJAMfrsdata::frs$REGISTRY_ID %chin% unlist(
#     EJAMfrsdata::get_siteid_from_naics(
#       EJAM::naics_find("pulp", add_children = TRUE))[,"REGISTRY_ID"]),  ])
# 
# # Map sites by facility ID
# 
# mapfast(get_latlon_from_siteid(test_query_input_registry_id$registry_id))

# read_and_clean_points() ??

# EJAMejscreenapi::latlon_any_format()   Very flexible function.
#    - Given lat and lon vectors, 
#      or a data.table or data.frame with columns that seem to be lat/lon, 
#      or a filename .csv or .xlsx with such a table
#    - Return table of points with columns lat and lon  

# **(done) EJAMfrsdata::get_siteid_from_naics()   
#      [and maybe  get_facility_info_via_ECHO() but that is slow]
#   -Given NAICS names or codes, 
#   -Return facility registry IDs (and lat lon also)

# get_siteid_from_sic() ? not urgent
# get_siteid_from_mact_subpart() ? not urgent
# 
# get_siteid_from_programid()  - see EJAMejscreenapi::locate_by_id(type = 'program') or get_facility_info_...
#   -given program system IDs, 
#   -return facility registry IDs (locate_by_id() does this and then also gets lat/lon)
# 
# ** get_latlon_from_siteid()  - see EJAMejscreenapi::locate_by_id(type = 'frs') etc.
#   -given facility registry IDs, 
#   -return facility lat/lon values (and other facility info, like name, NAICS, etc.? locate_by_id() does)
# 
# # get_latlon_from_naics() - EJAMfrsdata::get_latlon_from_naics() is an alias for EJAMfrsdata::get_siteid_from_naics()  
#   - Given NAICS code(s) numbers,
    # - Returns data.table with columns lat, lon, etc

# # get_latlon_from_sic  and  get_latlon_from_programid ? (wrappers for pairs of the above)
# # 
# # This had all been done by locate_by_id() which took either    get_facility_info_via_FRS <- locate_by_id # get_facility_info_via_FRS() alias
# 
# -----------------------------------------------
# TO GET SHAPEFILES / POLYGON DATA:
# 
# get_shape_from_buffered_latlon()   use  sf::st_buffer() to add a buffer around point or polygon
# (To get circular buffers but if needed as shapefiles/ polygons)
#   -Given lat/lon values and radius (or multiple ones; or max?),  
#   -Return circular buffers as shapefiles (or sp polygon data) (in case someone wants to use areal apportionment)
# 
# get_shape_from_upload() **    use   sf::st_read()
#   -Given filename, 
#   -Return uploaded shapefile    
# 
# get_shape_from_fips()    maybe use TIGER file of bounds, or EJScreen map services or file?
#   - Given Census FIPS of County/Tract/Block group
#   - Return shapefile (polygon)
# If someone wants to run a report for each CBSA??, MSA??, Other types of geos?
# (note: this is not useful if just want to know which blocks are inside, etc. - can go directly from FIPS to get_shape_from_fips()  or even just join directly pull in entire blockgroup indicator scores)
# 
# get_shape_from_drawing() 
#   -Given series of lat/lon points a user clicked on, to draw polygon on map, 
#   -Return shapefile (or sp polygon data)
# 
# get_shape_buffered()   ALREADY DRAFTED THIS FUNCTION.  uses  sf::st_buffer() to add a buffer around   polygon
#   -Given shapefiles/poly & buffer radius, 
#   -Return shapefiles that buffered from those edges (or sp polygon data)

# get_shape_from_siteid()   EJScreen is working on this for NPL sites at least.
#   -Given NPL ids, or some other IDs that refer to sites with shapefile info? 
#   -Return shapefile for each  (or sp polygon data) 

# -----------------------------------------------
  
# NEAR A POINT = INSIDE (CIRCULAR) BUFFER AROUND A POINT:  
#     sf::st_is_within_distance()   

# INSIDE A POLYGON LIKE HIGH-RISK ZONE: WHAT POINTS ARE INSIDE SOME POLYGON LIKE A BUFFER:  
#     sf::st_intersects() to find points inside polygon or buffered polygon.
# 
#   OW suggested this good idea: First use sf::st_intersects() or similar to
#    find which BLOCKGROUPS are at least partially inside each polygon (which might be roughly 1,000 for example)
#    then filter US blocks (8 million) down to just blocks that are children of those identified BLOCKGROUPS,
#   which may be something like 1k to 10k per site?

# NEAR A POLYGON (NONCIRCULAR) BUFFER AROUND A POLYGON = near a POLYGON OR ROAD LINE: 
#     CREATE A BUFFER THAT ADDS SOME DISTANCE FROM A SHAPE LIKE NPL SITE  with 
#     sf::st_buffer() to add buffer around polygon
#     sf::st_intersects() to then find points in that buffer, i.e., near the polygon.


# -----------------------------------------------
# TO GET RESIDENTS IN A CIRCLE/ NEARBY, OR GET DISTANCE TO EACH 
# 
# **EJAM::getblocksnearby() is currently a poorly named function.   see also   sf::st_is_within_distance()  
#  possible names:  get_block_distances() ? 
#                   get_blocks_nearby() ? that does not mention distances.
#                   get_distance2blocks() ? _from_sitepoints ?
# -Given site lat/lons & max radius (radii?), and table of all US blocks, 
# -Return DISTANCES for SITE-BLOCK pairs
# (for each site, and maybe an overall dissolved version that has for each unique block the minimum distance to any site, and average distance if near >1 site)
# 
#     get_blocks_in_circle() ?? do we really need this?  see also   sf::st_is_within_distance()  
#     -Given site lat/lons & max radius (radii?), and table of all US blocks 
#     -Return which blockids are nearby (but not distance information? - why not just get distances too?) (for each site, and overall dissolved) 
# 
# get_blockpoints_in_shape() - DRAFTED a start at THIS ALREADY 3/2023.  find points inside polygon or buffered polygon.
# -Given shapes as a polygon per site, and optionally extra distance to use to add buffering at each, 
# -Return site-block pairs (but not distance to each, just whether inside buffer) (using intersect like ejscreenbatch or others)


# -----------------------------------------------
# TO GET BG WEIGHTS (THE FRACTION OF EACH BLOCKGROUP TO COUNT AS INSIDE OR NEARBY)
# [This could be made even more generic to handle getting fraction of any Census unit (not just bg) at which there is indicator data, like tracts, blocks, raster cells]
# 
# ** EJAM::doaggregate() is current name for this (and more) roughly:
#  ** get_fraction_of_bg_nearby() - if using circular buffer (proximity)
#  or essentially get_bgwts_from_blockwts ?
#   ** This is already one part of doaggregate() but could be split off?
# - Given those blockids nearby each site (inside each buffer), and blockwts table,
# - Return BLOCKGROUP WEIGHTS: (for each site, and overall dissolved) list of BLOCKGROUP ids and the weight for each (what fraction of it to count)
# 
# get_fraction_of_block_in_shape() ( if areal apportion_blocks)? - see Ejscreenbatch?
# -Given buffer shapefiles by site, 
# -Return fraction of each BLOCK area that is inside buffer (for dissolved overall shape and/or each buffer). This could be always 0 or 100% if the method specified is to include the whole block population if the block is intersected at all by the buffer. Or it could be a fraction of the block area that falls inside the buffer. 
# - THEN NEED FUNCTION TO ROLL THAT UP to return the fraction of each blockgroup, since that is the resolution at which you have indicators typically.
#   
# get_fraction_of_bg_in_shape()  - or arealapportion_bg()? see ejscreenbatch
# -Given buffer shapefiles by site, 
# -Return BLOCKGROUP ids and the fraction of each BLOCKGROUP area that is inside buffer (for dissolved overall shape and/or each site or buffer). This could be always 0 or 100% if the method specified is to include the whole block population if the block is intersected at all by the buffer. Or it could be a fraction of the block area that falls inside the buffer.
# 
# get_fraction_of_rastercell_in_shape()  - see ejscreenbatch
# -Given buffer shapefiles by site, 
# -Return which RASTER CELLS, or what fraction of each, are inside buffer (for dissolved overall shape and/or each buffer). This could be always 0 or 100% if the method specified is to include the whole block population if the block is intersected at all by the buffer. Or it could be a fraction of the block area that falls inside the buffer. This presumes you already have indicators at the raster cell scale, and do not have to translate the raster cells or fractions into some other units like blockgroups.
#    

# -----------------------------------------------
# TO CALCULATE BG INDICATOR AGGREGATED FOR EACH SITE AND OVERALL- JOIN BLOCKGROUP INDICATOR DATA and BG WTS and use formulas like ratios of sums of counts to get %poor, etc.
# 
# ** EJAM::doaggregate() is current name for this (and more) roughly:
#  ** aggregate_indicators() -- takes BG scores and BG wts and use formulas to aggregate by site and overall.
# -Given blockgroup weights nearby or in buffers (by site and overall), blockgroupstats (any indicators, for each blockgroup in US), formulas or metadata on types of indicators (how to aggregate them), etc., 
# -Return each indicators (by site and overall) as raw values only  (percentiles lookup separately)
#  **this could be broken up into 3 helper functions
#    * rollup_counts(), 
#    * rollup_means(), (can be wtd by pop, or by area, or by census block?)
#    * rollup_formulas() or  calculate_by_formula(), 
# 
# ** EJAM::doaggregate() is current name for this (and more) roughly:
#  ** percentiles_from_raw() - this is maybe in lookup.pctile() already?
# -Given raw values of indicators, percentiles lookup tables,
# -Return percentiles (US and/or State)
# 

# -----------------------------------------------
#   OTHER
# 
# ** popup_from_df()  or more complicated/tailored version:
#   Get popups text from percentiles (see ejscreen::make.popups.api() or something like that)
# - given percentiles for indicators
# - return popup text spelling that out for map popup
# 
# mapfast()  or maybe map_sites()  
#  
# 
# prep for excel: excel-related formatting of outputs 
# 
# ** batch.summarize() ... break up #   **350** lines of code  
# batch.clean() .... should be broken up; 
# for summary rows and columns; functions for print tables?
#   
# ** write_report() - a bit of this is in the exec sum tab of EJAMbatch.summarizer::
#   
# ratios_to_reference() ? 
# -Given raw values of indicators, and US/State overall means
# -Return those as ratios to US (and State?) overall means. 
# 
# 
# plot_boxplots() ratios?
# plot_barplots_?? ratios?
# 
# 
# -----------------------
#  
