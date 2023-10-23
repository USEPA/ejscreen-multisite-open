#' @docType package
#' @title EJAM - Environmental Justice (EJ) Analysis Multisite tool
#' @name EJAM
#' @aliases EJAM-package blockwts blockpoints blockid2fips  bgid2fips 
#'
#' @md
#' 
#' @description
#' This package provides functions and data for very fast proximity analysis
#' for a large number of locations, or "buffers", summarizing conditions at each location.
#' It summarizes conditions as the conditions for the average resident in the buffer at that location.
#' A buffer or location here is defined as the area within a specified distance of a specified site.
#' A site of facility in this package is defined as a single geographic point (by latitude and longitude).
#'
#' The functions here will be somewhat generalized, but the specific datasets included here enable an
#' environmental justice (EJ) proximity analysis of US EPA's EJScreen data, including
#' environmental indicators (e.g., local traffic score, or estimated PM2.5 concentration)
#' and demographic indicators (e.g., percent low-income),
#' for the populations estimated to live within a specified distance (e.g., 1 mile) from
#' one or more sites, typically EPA-regulated facilities.
#' This means the tools can provide the same information that an EJScreen standard report provides,
#' but for a large number of reports (one for each site).
#'
#' @details  # **Vignette** ####################################################################
#' 
#' Please see the vignette at <EJAM-vignette.html> and web format documentation of functions and data at <EJAM.html>
#' 
#' (development version of vignette can be knit from EJAM/www folder of locally installed package)
#' 
#' @details  # **Key Functions:** ####################################################################
#' 
#'   - **[run_app()]** Launch the web app (R Shiny interface)
#' 
#'   - **[ejamit()]** Get results (tables, maps, plots) without the web app interface, largely by using these key functions:
#'     
#'     0. Getting key datasets and indexing blocks, if not yet done, via [dataload_from_aws()] and [indexblocks()]
#'     
#'     1. **[getblocksnearby()]**  Very fast method to buffer, identifying which blocks are 
#'       within specified distance of point(s) like sites/facilities, and get distance to each.
#'     
#'     2. **[doaggregate()]** Summarize demographic and environmental indicators from **[blockgroupstats]**.rda (see below)
#'      within each place, weighted using blockwts (for average resident within specified distance of site (e.g., facility), 
#'      or in each shapefile or FIPS-defined location).
#'
#' @details  # **Data files available as examples:** ####################################################################
#'   
#'   * **Excel files to read into ejamit()** or getblocksnearby() are in the local source package files in EJAM/inst/testdata/latlon 
#'   * [testpoints_10].rda  and larger datasets each provide a random test points data.frame with columns lat lon siteid
#'   * [testpoints_n()] can generate random test points at places weighted by population, FRS facilities, blockgroup, area, block
#'   * [testoutput_getblocksnearby_10pts_1miles].rda and larger datasets are sample outputs of getblocksnearbyviaQuadTree or just getblocksnearby(), 
#'     to try as inputs to doaggregate()
#'
#' @details  # **Specifying buffer sites / facilities:** ####################################################################
#' 
#'   A user can specify locations, via an interface, and that shiny app returns 
#'    
#'  - *`sitepoints`*, a data.table with fields siteid, lat, lon. 
#'  A user-specified table with maybe 100, 1k, 10k+ points (centers of circular buffers).
#'  Examples of test data are 
#'   
#'  One can specify sitepoints, with lat/lon coordinates of the places to be analyzed (sites or facilities),
#'  or can specify areas to be analyzed in general, 
#'  in one of these ways:
#' 
#'    - **Point locations uploaded** (lat, lon coordinates) - these could be regulated facilities 
#'      for which you already have locations, or could be any other types of points uploaded in a spreadsheet.
#'    
#'    - **Shapefiles uploaded** to directly define the areas to include instead of using circular buffers around points
#'    
#'    - **FIPS** to specify a list of counties or tracts or blockgroups, for example, to be compared.
#'    
#'    - **NAICS or SIC code** Industry categories selected from a list of codes or names, or uploaded in a table.
#'         The NAICS are 2-digit to 6-digit codes that specify sectors or types of facilities, such as
#'         325 - Chemical Manufacturing, or 325211 - Plastics Material and Resin Manufacturing.
#'         
#'    - **Facility IDs** - EPA Facility Registry Service (FRS) ID numbers or FRS Program ID numbers
#'    
#'    - **Program System Types** - picking a whole category of regulated sites, such as all GHG reporters or all TRI reporters.
#'    
#'    - **MACT Subpart** to specify Clean Air Act NESHAP program Max. Achievable Control Tech. 
#'         source categories (types of air emissions sources) defined by subpart such as OOOO. 
#'    
#'    Some additional details on some of these:
#'    
#'  **- BY INDUSTRIAL SECTOR/ NAICS:**
#'  
#'         Interface lets user select NAICS from pulldown, or type in NAICS 
#'         Interface returns a vector of one or more naics codes,
#'         to be converted to sitepoints.
#' 
#'    - **[latlon_from_naics()]** takes NAICS codes and returns a data.table of site points.
#'      Relies on ** frs_by_naics.rda**  A data.table, needed to get lat lon by naics. 
#'      Need to update FRS data used here regularly, ideally frequently.
#'      2023 version has columns   REGISTRY_ID,  NAICS, lat, lon
#'      
#'      NAICS codes also can be selected by text search of industry names or by categories of codes, via [naics_from_any()]
#'       
#'       
#'  **- BY FACILITY ID or PROGRAM ID:**  
#'  
#'         Interface so user can upload FRS REGISTRY_ID or PROGRAM ID csv/xls file, 
#'         Interface returns a list of REGISTRY_ID values from a copy of the EPA facility registry service (FRS) data  
#'         to be converted to sitepoints.
#' 
#'    - **[frs_from_siteid()]** takes REGISTRY_ID values and returns a data.table of site points.
#'      Relies on **frs** data.table with columns REGISTRY_ID, lat, lon, etc.
#'      
#'    - **[frs_from_programid()]** takes EPA program-specific site ID values and returns a data.table of site points.
#'      Relies on **[frs_by_programid]** data.table with columns program, pgm_sys_id, REGISTRY_ID, lat, lon 
#'      
#' 
#'  **- BY LAT/LON POINT:**
#'  
#'         Interface so user can specify or upload latitude longitude siteid (and optionally others like sitename),
#'         when using [ejamit()] which in turn uses [latlon_from_anything()].
#'         
#'         Interface returns sitepoints data.table with siteid, lat, lon (here, siteid may be just the row number)
#' 
#' 
#' @details  # **Buffering to find site-block-distances:** ####################################################################
#' 
#'   Input: **`sitepoints`** data.table from user picking points
#'   
#'   Columns are siteid, lat, lon; maybe 100 to 10k points
#'      
#'   - **[getblocksnearby()]** which by default uses *[getblocksnearbyviaQuadTree()]*
#'        Returns `sites2blocks` 
#'        Requires index called localtree that is build from dataset [quaddata] 
#'        
#'   - **sites2blocks**   Created by [getblocksnearby()] to be passed to  [doaggregate()]  
#'      This is a data table with maybe 100k to 1m rows (assume 1k blocks within 3 miles of each site, or 100 blocks within 1 mile),
#'      `sites2blocks[ , .(siteid, blockid, distance or dist)]`
#'     - siteid    (site with circular buffer to group by)
#'     - blockid     for join to blockwts
#'     - distance  (in miles, from block to site) (0 or irrelevant for noncircular buffers, 
#'          since a block is only in this table if in one or more buffers, 
#'          unless analysis is for residents within x miles of the edges of some shapes, like facility boundaries)
#'          
#'          
#' @details  # **Data files used for distance calculation:** ####################################################################
#' 
#'   - ** quaddata.rda** data.table with point location of internal point for each of 8 million Census blocks
#'     is used prior to or during startup of EJAM to create an index stored in memory, called localtree.
#'   
#'     quaddata can be obtained using [data_load_from_aws()]
#'     
#'     localtree is the index made from quaddata via [indexblocks()] (using the SearchTrees package)
#' 
#'    Those are used in getblocksnearby with some sitepoints to create a temporary object called sites2blocks:
#'   
#'   
#' @details  # **Summarizing indicators in buffers:** ####################################################################
#' 
#'       INPUT IS  `sites2blocks`, 
#'       OUTPUT IS results_overall, results_bysite, and other summary stats  ####
#'   
#'   - **[doaggregate()]** = function(sites2blocks) This summarizes in each buffer and for all unique residents across all buffers.
#'     
#'   - **[blockgroupstats].rda** a data.table with 220k rows (blockgroups) and hundreds of indicator columns.
#'      Will need bgid not just bgfips, to join to blockwts$bgid
#'      Needs to be updated each time EJScreen is updated. 
#'      
#'   - **[bgej].rda** a data.table like blockgroupstats but for the EJ Index raw scores. 
#'
#'   - **[usastats].rda** and **[statestats].rda**  data.table lookup of 100 percentiles and means 
#'       (for each indicator in blockgroupstats, and perhaps bgej) in each zone (USA, or a state or DC, PR).
#'       Need to update each time blockgroupstats is updated. Taken from EJScreen data or ejscreen package file lookupUSA and lookupStates
#'  
#'   - **blockwts.rda** see [data_load_from_aws()]
#'    Required by [doaggregate()]. A data.table of 6-8m rows 
#'     - blockwt  The fraction of parent blockgroup decennial pop that is in this one block
#'     - block_radius_miles squared times pi would equal the land plus water area in square miles
#'     - blockid (integer key  for join to sites2blocks)
#'     - bgid   integer key instead of bgfips - For sum(blockwt), by=bgid, and for join to blockgroupstats$bgid. 
#'       bgid is more efficient than bgfips. bgfips is not here but is in a bgid2fips data.table
#' 
#'            
#' @details  # **Output results for user:** ####################################################################
#'  
#'  [ejamit()] (or [doaggregate()]) provides the following outputs: 
#'  
#'   * **results_overall**   one row data.table, like results_by_site, but just one row with 
#'     aggregated results for all unique residents. 
#' 
#'   * **results_by_site**   results for individual sites (buffers) - a data.table of results, 
#'     one row per siteid, one column per indicator
#' 
#'   * **results_bybg_people**  results for each block group, to allow for showing the distribution of each 
#'      indicator across everyone within each demographic group.
#'      
#'   * **longnames**  descriptive long names for the indicators in the above outputs
#'   
#'   * **count_of_blocks_near_multiple_sites**  additional detail 
#'   
#' @details  # **Identification of nearby residents -- methodology:** ####################################################################
#'
#' The identification of nearby residents is currently done in a way that includes all 2020 Census blocks whose
#' "internal point" (a lat/lon provided by the Census Bureau) is within the specified distance of the facility point.
#' This is taken from the EJScreen block weights file, but can also be independently calculated.  
#' 
#' The summary or aggregation or "rollup" within the buffer is done by calculating the
#' population-weighted average block group score among all the people residing in the buffer.
#'
#' Since the blockgroup population counts are from American Community Survey (ACS) estimates,
#' but the block population counts are from a decennial census, the totals for a blockgroup differ.
#' The amount each partial blockgroup contributes to the buffer's overall score is based on
#' the estimated number of residents from that blockgroup who are in the buffer.
#' This is based on the fraction of the blockgroup population that is estimated to be in the buffer,
#' and that fraction is calculated as the fraction of the blockgroup's decennial census block population
#' that is in the census blocks inside the buffer.
#'
#' A given block is considered entirely inside or entirely outside the buffer,
#' and those are used to more accurately estimate what fraction of a given block group's
#' population is inside the buffer. This is more accurate and faster than areal apportionment of block groups.
#' Census blocks are generally so small relative to typical buffers that this is very accurate -
#' it is least accurate if a very small buffer distance is specified
#' in an extremely low density rural area where a block can be geographically large.
#' Although it is rarely if ever a significant issue (for reasonable, useful buffer sizes),
#' an even more accurate approach in those cases might be either areal apportionment of blocks,
#' which is very slow and assumes residents are evenly spread out across the full block's area,
#' or else an approach that uses higher resolution estimates of residential locations than even
#' the Decennial census blocks can provide, such as a dasymetric map approach.
#'
#'
#' @keywords internal
"_PACKAGE"
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
