
We want functions to do these separately:

-----------------------------------------------
TO GET FACILITIES AND POINTS (LAT / LON):

**(done) EJAMfrsdata::get_siteid_from_naics()  
     [and maybe  get_facility_info_via_ECHO() ]
-Given NAICS names or codes, 
-Return facility registry IDs (or lat lon also)

get_siteid_from_sic() ? not urgent
get_siteid_from_mact_subpart() ?

get_siteid_from_programid()  - see EJAMejscreenapi::locate_by_id(type = 'program') or get_facility_info_...
given program system IDs, 
return facility registry IDs (locate_by_id() does this and then also gets lat/lon)

** get_latlon_from_siteid()  - see EJAMejscreenapi::locate_by_id(type = 'frs') etc.
given facility registry IDs, 
return facility lat/lon values (and other facility info, like name, NAICS, etc.? locate_by_id() does)

get_latlon_from_naics(),    -  see EJAMfrsdata::siteid_from_naics()  
get_latlon_from_sic  and  get_latlon_from_programid ? (wrappers for pairs of the above)

This had all been done by locate_by_id() which took either 
-----------------------------------------------
TO GET SHAPEFILES / POLYGON DATA:

get_shape_from_buffered_latlon()   use  sf::st_buffer() to add a buffer around point or polygon
(To get circular buffers but if needed as shapefiles/ polygons)
-Given lat/lon values and radius (or multiple ones; or max?), 
-Return circular buffers as shapefiles (or sp polygon data) (in case someone wants to use areal apportionment)

get_shape_from_upload() **    use   sf::st_read()
-Given filename, 
-Return uploaded shapefile    

get_shape_from_fips()    maybe use TIGER file of bounds, or EJScreen map services or file?
- Given Census FIPS of County/Tract/Block group
- Return shapefile (polygon)
If someone wants to run a report for each CBSA??, MSA??, Other types of geos?
(note: this is not useful if just want to know which blocks are inside, etc. - can go directly from FIPS to get_shape_from_fips()  or even just join directly pull in entire blockgroup indicator scores)

get_shape_from_drawing() 
-Given series of lat/lon points a user clicked on, to draw polygon on map, 
-Return shapefile (or sp polygon data)

get_shape_from_buffered_shape()   use  sf::st_buffer() to add a buffer around   polygon
-Given shapefiles/poly & buffer radius, 
-Return shapefiles that buffered from those edges (or sp polygon data) 

get_shape_from_siteid()   EJScreen is working on this for NPL sites at least.
-Given NPL ids, or some other IDs that refer to sites with shapefile info? 
-Return shapefile for each  (or sp polygon data) 

-----------------------------------------------
  
# NEAR A POINT = INSIDE (CIRCULAR) BUFFER AROUND A POINT:  
#     sf::st_is_within_distance()   

# INSIDE A POLYGON LIKE HIGH-RISK ZONE: WHAT POINTS ARE INSIDE SOME POLYGON LIKE A BUFFER:  
#     sf::st_intersects() to find points inside polygon or buffered polygon.

# NEAR A POLYGON (NONCIRCULAR) BUFFER AROUND A POLYGON = near a POLYGON OR ROAD LINE: 
#     CREATE A BUFFER THAT ADDS SOME DISTANCE FROM A SHAPE LIKE NPL SITE  with 
#     sf::st_buffer() to add buffer around polygon
#     sf::st_intersects() to then find points in that buffer, i.e., near the polygon.


-----------------------------------------------
TO GET RESIDENTS IN A CIRCLE/ NEARBY, OR GET DISTANCE TO EACH 

**EJAM::getblocksnearby() is currently a poorly named function.   see also   sf::st_is_within_distance()  
 possible names:  get_block_distances() ? 
                  get_blocks_nearby() ? that does not mention distances.
                  get_distance2blocks() ? _from_sitepoints ?
-Given site lat/lons & max radius (radii?), and table of all US blocks, 
-Return DISTANCES for SITE-BLOCK pairs
(for each site, and maybe an overall dissolved version that has for each unique block the minimum distance to any site, and average distance if near >1 site)

    get_blocks_in_circle() ?? do we really need this?  see also   sf::st_is_within_distance()  
    -Given site lat/lons & max radius (radii?), and table of all US blocks 
    -Return which blockids are nearby (but not distance information? - why not just get distances too?) (for each site, and overall dissolved) 

get_blockpoints_in_shape() - or nearby_blockpoints()  try  sf::st_intersects() to find points inside polygon or buffered polygon.
-Given shapes/buffers as a polygon per site, 
-Return site-block pairs (but not distance to each, just whether inside buffer) (using intersect like ejscreenbatch or others)

-----------------------------------------------
TO GET BG WEIGHTS (THE FRACTION OF EACH BLOCKGROUP TO COUNT AS INSIDE OR NEARBY)
[This could be made even more generic to handle getting fraction of any Census unit (not just bg) at which there is indicator data, like tracts, blocks, raster cells]

** EJAM::doaggregate() is current name for this (and more) roughly:
 ** get_fraction_of_bg_nearby() - if using circular buffer (proximity)
 or essentially get_bgwts_from_blockwts ?
  ** This is already one part of doaggregate() but could be split off?
- Given those blockids nearby each site (inside each buffer), and blockwts table,
- Return BLOCKGROUP WEIGHTS: (for each site, and overall dissolved) list of BLOCKGROUP ids and the weight for each (what fraction of it to count)

get_fraction_of_block_in_shape() ( if areal apportion_blocks)? - see Ejscreenbatch?
-Given buffer shapefiles by site, 
-Return fraction of each BLOCK area that is inside buffer (for dissolved overall shape and/or each buffer). This could be always 0 or 100% if the method specified is to include the whole block population if the block is intersected at all by the buffer. Or it could be a fraction of the block area that falls inside the buffer. 
- THEN NEED FUNCTION TO ROLL THAT UP to return the fraction of each blockgroup, since that is the resolution at which you have indicators typically.
  
get_fraction_of_bg_in_shape()  - or arealapportion_bg()? see ejscreenbatch
-Given buffer shapefiles by site, 
-Return BLOCKGROUP ids and the fraction of each BLOCKGROUP area that is inside buffer (for dissolved overall shape and/or each site or buffer). This could be always 0 or 100% if the method specified is to include the whole block population if the block is intersected at all by the buffer. Or it could be a fraction of the block area that falls inside the buffer.

get_fraction_of_rastercell_in_shape()  - see ejscreenbatch
-Given buffer shapefiles by site, 
-Return which RASTER CELLS, or what fraction of each, are inside buffer (for dissolved overall shape and/or each buffer). This could be always 0 or 100% if the method specified is to include the whole block population if the block is intersected at all by the buffer. Or it could be a fraction of the block area that falls inside the buffer. This presumes you already have indicators at the raster cell scale, and do not have to translate the raster cells or fractions into some other units like blockgroups.
   
-----------------------------------------------
TO CALCULATE BG INDICATOR AGGREGATED FOR EACH SITE AND OVERALL- JOIN BLOCKGROUP INDICATOR DATA and BG WTS and use formulas like ratios of sums of counts to get %poor, etc.

** EJAM::doaggregate() is current name for this (and more) roughly:
 ** aggregate_indicators() -- takes BG scores and BG wts and use formulas to aggregate by site and overall.
-Given blockgroup weights nearby or in buffers (by site and overall), blockgroupstats (any indicators, for each blockgroup in US), formulas or metadata on types of indicators (how to aggregate them), etc., 
-Return each indicators (by site and overall) as raw values only  (percentiles lookup separately)
 **this could be broken up into 3 helper functions
   * rollup_counts(), 
   * rollup_means(), (can be wtd by pop, or by area, or by census block?)
   * rollup_formulas() or  calculate_by_formula(), 

** EJAM::doaggregate() is current name for this (and more) roughly:
 ** percentiles_from_raw() - this is maybe in lookup.pctile() already?
-Given raw values of indicators, percentiles lookup tables,
-Return percentiles (US and/or State)

-----------------------------------------------
  OTHER


** popup_from_df()  or more complicated/tailored version:
  Get popups text from percentiles (see ejscreen::make.popups.api() or something like that)
- given percentiles for indicators
- return popup text spelling that out for map popup

mapfast()  or maybe map_sites()  
 

prep for excel: excel-related formatting of outputs 

** batch.summarize() ... break up #   **350** lines of code  
batch.clean() .... should be broken up; 
for summary rows and columns; functions for print tables?
  
** write_report() - a bit of this is in the exec sum tab of EJAMbatch.summarizer::
  
ratios_to_reference() ? 
-Given raw values of indicators, and US/State overall means
-Return those as ratios to US (and State?) overall means. 


plot_boxplots() ratios?
plot_barplots_?? ratios?


-----------------------


# server.R  had     **1700** lines of code in batch.summarizer package
# 
  z = analyze.stuff::linesofcode('..', packages = 'EJAM')
  z[order(z$lines, decreasing = T), 1:5]
    lines code comments package                                                               filename
    
48    775  761       14    EJAM                                                               app_ui.R
53    575  522       53    EJAM                                                          doaggregate.R *******
47    529  523        6    EJAM                                                           app_server.R *****

44    243  243        0    EJAM                                          ejscreenapi-make.popups.api.R  **??
56    207  158       49    EJAM                                                      getacs_epaquery.R 
11    202  186       16    EJAM                                                          cleanoutput.R ?

61    161  147       14    EJAM                                 getblocksnearbyviaQuadTree_Clustered.R
62    144  109       35    EJAM                                          getblocksnearbyviaQuadTree2.R
60    142  106       36    EJAM                                           getblocksnearbyviaQuadTree.R 


z = linesofcode('..', packages = 'EJAMejscreenapi')
z[order(z$code, decreasing = T), 1:5]
    lines code comments         package                                            filename
    
53   570  569        1 EJAMejscreenapi                                                ui.R
52   517  517        0 EJAMejscreenapi                                            server.R *****

23   398  355       43 EJAMejscreenapi                                  ejscreenapi_plus.R  
22   312  272       40 EJAMejscreenapi                                       ejscreenapi.R 
24   271  168      103 EJAMejscreenapi                                ejscreenapi_script.R 

41   317  291       26 EJAMejscreenapi                                   make.popups.api.R  ?? but also see popup_from_df()

33   215  163       52 EJAMejscreenapi                        get_facility_info_via_ECHO.R  **but rename?

4    271  130      141 EJAMejscreenapi                            usefulcolumns_ECHO_API.R
51   143  112       31 EJAMejscreenapi                                     varname2color.R
40   225  108      117 EJAMejscreenapi                                      locate_by_id.R **
25   139  107       32 EJAMejscreenapi                                      ejscreenapi1.R 

  
