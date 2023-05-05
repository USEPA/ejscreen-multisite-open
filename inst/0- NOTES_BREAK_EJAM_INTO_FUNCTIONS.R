
# LIST OF KEY FUNCTIONS WRITTEN OR NEEDED FOR EJAM/RELATED

# -----------------------------------------------
# DATA INPUT / READ TABLES 
#
#  EJAM::latlon_from_anything()  Reads any format (does not do a query). Very flexible function.
#   aka  latlon_any_format(), 
#    uses read_csv_or_xl() and also
#    uses latlon_df_clean() which uses latlon_infer(), latlon_as.numeric(), latlon_is.valid()
#  - Given lat and lon vectors, 
#      or a data.table or data.frame with columns that seem to be lat/lon, or similar list,
#      or a filename .csv or .xlsx with such a table,
#      or nothing in interactive mode in which case RStudio helps you pick a file.
#   - Return table of points with columns lat and lon  
#
# EJAMejscreenapi::read_and_clean_points  - obsolete or overlaps with latlon_from_anything()

# TO upload SHAPEFILES / POLYGON DATA:
# get_shape_from_upload() ** NOT WRITTEN   use   sf::st_read() e.g.,  dep <- sf::read_sf("~/afile_000_2022.gpkg", layer = "mylayer")
#   -Given path\filename 
#   -Return uploaded shapefile

# -----------------------------------------------
# QUERY FRS, NAICS, ETC. TO GET LAT/LON, STATE, FACILITY INFO, ETC.

# unwritten:   latlon_from_sic   code or sic industryname

# drafted:   latlon_from_mactsubpart      code (and possibly mact name?) search

# EJAM::latlon_from_program()
# EJAM::latlon_from_programid()
# EJAM::latlon_from_siteid()  aka latlon_from_regid()  
# unwritten!   latlon_from_sitename(), but see frs_from_sitename()
# EJAM::latlon_from_naics()

#        EJAM::frs_from_program()
#        EJAM::frs_from_programid()
#        EJAM::frs_from_siteid()  aka  frs_from_regid()
#        EJAM::frs_from_sitename()
#        EJAM::frs_from_naics()      and    siteid_from_naics()

#        EJAM::naics_from_any()  Queries via NAICS codes or industry names or any regular expression or partial words
#        EJAM::naics_findwebscrape()  Searches website to get more hits than naics_from_any() does
#  naics_from_code()   e.g., 325
#  naics_from_name()   e.g.,   "pulp" or "Pulp, Paper, and Paperboard Mills"
#  naics_subcodes_from_code()  (vs EJAM::naics2children() that is not as robust)

#        EJAM::naics_url_of_code()  NEED TO COMPARE TO /MERGE WITH  EJAM::url_naics.com
# and check these probably obsolete related ones= EJAMejscreenapi::locate_by_id1() and alias is EJAMejscreenapi::get_facility_info_via_FRS()

# EJAM::states_infer()   Get cleaned table of US State etc. by siteid, from lat/lon, or from FIPS
# EJAM::state_from_blockid()
# EJAM::state_from_blocktable()
# EJAM::state_from_fips()
# EJAM::state_from_latlon()
# EJAM::ST_by_site_from_sites2blocks()  Get ST from blockids and distances (infer the State each site is located in, based on the state of the nearest block)

# EJAM::fipsbg_from_anyfips()    Get BG fips of all BG in a county or in a tract, e.g., to easily report EJScreen stats at county or tract level or comparing counties, etc.

# -----------------------------------------------
# GET URL FOR MORE INFO FROM EJSCREEN, ECHO, ENVIROFACTS, ETC.
# 
# EJAMejscreenapi::addlinks_clusters_and_sort_cols()  needs to be broken up
#
# EJAMejscreenapi::url_linkify()
# EJAMejscreenapi::url_by_id()  too slow
# EJAMejscreenapi::url_echo_facility_webpage()
# EJAMejscreenapi::url_ejscreen_acs_report()
# EJAMejscreenapi::url_ejscreen_report()
# EJAMejscreenapi::url_ejscreenmap()
# EJAMejscreenapi::url_envirofacts_report()
#            EJAM::url_naics.com        NEED TO COMPARE TO /MERGE WITH  naics_url_of_code
#            EJAM::naics_url_of_code()  NEED TO COMPARE TO /MERGE WITH  url_naics.com

# -----------------------------------------------# -----------------------------------------------# -----------------------------------------------
# MAPS AND POPUPS  - these functions are done:

#  EJAMejscreenapi::mapfast()  
#  EJAM::map_facilities() ?
#  ggplot2::map_data() ?
#
# *popup_from_uploadedpoints()
# *popup_from_df()  
# *popup_from_ejscreen()
# 
# # Map sites by facility ID
# mapfast(latlon_from_siteid(test_query_input_registry_id$registry_id))

# -----------------------------------------------
# PLOTTING

# EJAM::plot_distance_cdf_by_group()   drafted but not used yet
# EJAM::plot_distance_mean_by_group()  drafted but not used yet
# Should we write a barplot function?? there is code in server of EJAM, e.g.

# EJAMejscreenapi::boxplots_ratios()   drafted, but also there is ggplot2 code in EJAM server file.
# 
# ratios_to_reference() ?  This is done in server code but also done in doaggregate() now. 
# -Given raw values of indicators, and US/State overall means
# -Return those as ratios to US (and State?) overall means. 

# -----------------------------------------------
# TABLE FORMATTING TO SAVE AS EXCEL  .XLSX
# 
# need to reconcile these drafted pieces:
# xls_formatting()
# xls_formatting_api()
# xls_formatting2()

# -----------------------------------------------
# TABLE FORMATTING STATIC REPORTS
#
# EJAM::format_gt_table()  DRAFTED

# **Something like write_report_short() write_report_full() - no function yet for this, just code within server/ui/rmd files -  a bit of this code is in the exec sum tab of EJAMbatch.summarizer::, some in EJAM:: etc.

# TABLE FORMATTING FOR INTERACTIVE TABLES (DT) ?
# * no functions written for this 


# -----------------------------------------------
#  GET RESIDENTS IN A CIRCLE/ NEARBY, OR GET DISTANCE TO EACH 
# 
# **EJAM::getblocksnearby() is written (but currently a poorly named function)
#    see also   sf::st_is_within_distance()  
#    possible names:  get_block_distances() ? 
#                   get_blocks_nearby() ? that does not mention distances.
#                   get_distance2blocks() ? _from_sitepoints ?
#   -Given site lat/lons & max radius (radii?), and table of all US blocks, 
#   -Return DISTANCES for SITE-BLOCK pairs
#   (for each site, and maybe an overall dissolved version that has for each unique block the minimum distance to any site, and average distance if near >1 site)
# 
# get_blockpoints_in_shape() -  ALREADY DRAFTED roughly.  
#  Find points inside polygon or buffered polygon.
# -Given shapes as a point or polygon per site, and optionally extra distance to use to add buffering at each, 
# -Return site-block pairs (but not distance to each, just whether inside buffer) (using intersect like ejscreenbatch or others)

# -----------------------------------------------
# TO ANALYZE RESIDENTS OF COUNTIES (OR TRACTS?) AS IF THEY WERE "SITES" OR FACILITIES... 

# get_shape_buffered_from_shapefile_points()   ALREADY DRAFTED    
#   -Given shapefiles/points (or polygons?) & buffer radius, 
#   -Return shapefiles that buffered from those edges (or sp polygon data)

# fipsbg_from_anyfips()    ALREADY DRAFTED  
#  - Given siteid,FIPS table i.e., 1 or more lists of FIPS (as blockgroups/tract/county/state-FIPS/ or bg grouped into "sites" like metro areas, attainment areas, etc.)
#  - Return bgid,siteid table to be used in an aggregation function
#  like rollup_blockgroups2sites() 

# counties_as_sites()    ALREADY DRAFTED  
#   But, doaggregate() expects sites2blocks, not counties2blockgroups 
  # you do not want all the blocks since you can jump to the middle of doaggregate and start from all blockgroups in the county,
#    join the blockgroupstats to that, and analyze. Maybe just as easy and faster to cache all results for each of 3200 counties?? 
 
# get_shape_from_drawing()   NOT DRAFTED
#   -Given series of lat/lon points a user clicked on, to draw polygon on map, 
#   -Return shapefile (or sp polygon data)
#
# get_shape_from_siteid()  NOT DRAFTED  - EJScreen is working on this for NPL sites at least.
#   -Given NPL ids, or some other IDs that refer to sites with shapefile info? 
#   -Return shapefile for each  (or sp polygon data) 

# -----------------------------------------------
# USe POLYGONS for AREAL APPORTIONMENT

# areal apportionment - TO GET BG WEIGHTS (THE FRACTION OF EACH BLOCKGROUP TO COUNT AS INSIDE OR NEARBY)
# [This could be made even more generic to handle getting fraction of any Census unit (not just bg) at which there is indicator data, like tracts, blocks, raster cells]

#   OW suggested this good idea: First use my get_blockpoints_in_shape(), and get parent bgid values, 
#    find which BLOCKGROUPS are at least partially inside each polygon (which might be roughly 1,000 for example)
#    then filter US blocks (8 million) down to just blocks that are children of those identified BLOCKGROUPS,
#   which may be something like 1k to 10k per site? Then could do areal apportionment on just those, more quickly?

# get_fraction_of_bg_in_shape()    NOT DRAFTED - or arealapportion_bg()? see ejscreenbatch
# -Given buffer shapefiles by site, 
# -Return BLOCKGROUP ids and the fraction of each BLOCKGROUP area that is inside buffer (for dissolved overall shape and/or each site or buffer). This could be always 0 or 100% if the method specified is to include the whole block population if the block is intersected at all by the buffer. Or it could be a fraction of the block area that falls inside the buffer.
# 
# get_fraction_of_block_in_shape()   NOT DRAFTED ( if areal apportion_blocks)? - see EJSCREENbatch?
# HIGHER RESOLUTION, BUT probably would rarely be used -- too slow to do portion of each block that is in shape.
# -Given buffer shapefiles by site, 
# -Return fraction of each BLOCK area that is inside buffer (for dissolved overall shape and/or each buffer). This could be always 0 or 100% if the method specified is to include the whole block population if the block is intersected at all by the buffer. Or it could be a fraction of the block area that falls inside the buffer. 
# - THEN NEED FUNCTION TO ROLL THAT UP to return the fraction of each blockgroup, since that is the resolution at which you have indicators typically.
#   
# get_fraction_of_rastercell_in_shape()    NOT DRAFTED - see EJSCREENbatch
# HIGHEST RESOLUTION OF ALL, IF 30X30 METER GRID CELL SIZE, BUT IF LARGE CELLS LIKE 1KM THEN THIS IS WORSE THAN BLOCK-POINT-INCLUSION METHOD (EJAM/EJSCREEN)
# -Given buffer shapefiles by site, 
# -Return which RASTER CELLS, or what fraction of each, are inside buffer (for dissolved overall shape and/or each buffer). This could be always 0 or 100% if the method specified is to include the whole block population if the block is intersected at all by the buffer. Or it could be a fraction of the block area that falls inside the buffer. This presumes you already have indicators at the raster cell scale, and do not have to translate the raster cells or fractions into some other units like blockgroups.

# -----------------------------------------------
# CALCULATE INDICATORs AGGREGATED FOR EACH SITE AND OVERALL- JOIN BLOCKGROUP INDICATOR DATA and BG WTS and use formulas like ratios of sums of counts to get %poor, etc.
# 
# ** EJAM::doaggregate() is current name for this (and more) roughly:
#   ** aggregate_indicators() -- takes BG scores and BG wts and use formulas to aggregate by site and overall.
#  -Given blockgroup weights nearby or in buffers (by site and overall), blockgroupstats (any indicators, for each blockgroup in US), formulas or metadata on types of indicators (how to aggregate them), etc., 
#  -Return each indicators (by site and overall) as raw values only  (percentiles lookup separately)
#  **this could be broken up into 3 helper functions
#    * rollup_counts(), 
#    * rollup_means(), (can be wtd by pop, or by area, or by census block?)
#    * rollup_formulas() or  calculate_by_formula(), 
# 
#   ** pctile_from_raw_lookup()  DRAFTED
#  -Given raw values of indicators, percentiles lookup tables,
#  -Return percentiles (US and/or State)
# 
# -----------------------------------------------
# SUMMARIZING - CREATE NEW INDICATORS FOR EACH SITE AND ALSO TO SUMMARIZE ALL SITES
# 
# ** batch.summarize() ... break up #   MANY lines of code  
# batch.clean() .... should be broken up; 
# for summary rows and columns; functions for print tables?



#---# -----------------------------------------------# -----------------------------------------------
####################################

#  Break doaggregate() into 2 parts ?
##   We need to split doaggregate() into   
#
# 1. rollup_blocks2blockgroups()   or  aggregate_blocks_by_bg()    ** NOT WRITTEN  
#    - given  blockid,siteid,distance (or NA if no distance info) 
#    - return bgid,bgwt,siteid, plus stats on distance: eg, distancemin, distanceavg, proxiscoreavg (or NA if no distance info)
#
# 2. rollup_blockgroups2sites()  or    aggregate_blockgroups_by_site()    ** NOT WRITTEN  
#    - given  bgid,bgwt,siteid,  plus stats on distance and nearby sites, plus bg-scale E&D&EJ indicators
#    - return like doaggregate() did:  summary by siteid plus overall stats. 
# 
#  and  could even break out into smaller pieces: Not a priority.  
#  ** get_fraction_of_bg_nearby() could be created  
#  or essentially get_bgwts_from_blockwts ?
#   ** This is already one part of doaggregate() but could be split off?
# - Given those blockids nearby each site (inside each buffer), and blockwts table,
# - Return BLOCKGROUP WEIGHTS: (for each site, and overall dissolved) list of BLOCKGROUP ids and the weight for each (what fraction of it to count)

#---# -----------------------------------------------# -----------------------------------------------

#  THE VARIOUS METHODS TO DEFINE LOCATIONS AND ESTIMATE DEMOGRAPHICS THERE 
# 
#  i.e., Types of buffering or spatial joining to identify which residents are in zone of interest (or at what distance are they)
# 
# SITE/FACILITY ZONE IS DEFINED AS... 
#   - near the central sitepoint, i.e., circle (like a buffered point)
#   - near any part of a polygonal site like NPL site (buffered polygon)
#   - within a polygon (e.g., where modeling shows higher risk or ambient concentrations)
# 
# RESIDENTS ARE LOCATED AND COUNTED BY one of these ways...
#   inclusion of internal points of census units (e.g., is the block centroid in the circle?) 
#   areal apportionment of census units like tract, blockgroup, or even block (i.e., what fraction of the blockgroup is inside the circle or polygon?)
#   intersection with raster cells? (how? intersects/partly inside? otherwise: fully inside, or centroid is inside, or majority is inside?)
#   areal apportionment of raster cells? not sure but maybe for very large raster cells it makes sense, like 1km.

# -----------------------------------------------# -----------------------------------------------
