#' @docType package
#' @title Environmental Justice (EJ) Analysis Multisite tool
#' @name EJAM
#' @aliases EJAM-package
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
#' Please see the vignette (development version can be knit from EJAM/www folder of locally installed package)
#' 
#' @details  # **Key Functions:** ####################################################################
#' 
#'   - **[run_app()]** Launch the Shiny app (web interface)
#' 
#'   - **[ejamit()]** Use outside the shiny app, does in 1 function the two steps below:
#' 
#'     - **[getblocksnearby()]**  Very fast method to buffer, identifying which blocks are within specified distance of site or facility
#'     
#'     - **[doaggregate()]** Summarize the demographic and environmental indicators from **[blockgroupstats].rda** (see below)
#'      within each buffer weighted using blockwts (for average resident within specified distance of site or facility).
#'      NOTE: FUNCTION IS BEING MODIFIED, to be generic to use any indicators, and be faster, etc.  
#'
#' @details  # **Specifying buffer sites / facilities:** ####################################################################
#' 
#'   A user can specify locations, via an interface, and that shiny app returns 
#'    
#'  - *`sitepoints`*, a data.table with fields siteid, lat, lon. 
#'  A user-specified table with maybe 100, 1k, 10k+ points (centers of circular buffers)
#'   
#'  One can specify sitepoints, with lat/lon coordinates of the places to be analyzed (sites or facilities), 
#'  in one of three ways:
#' 
#'    1. **NAICS code** (selecting from a list, one or more types of facilities, as defined by NAICS). 
#'         The NAICS are 2-digit to 6-digit codes that specify sectors or types of facilities, such as
#'         325 - Chemical Manufacturing, or 325211 - Plastics Material and Resin Manufacturing.
#'         
#'    2. **Facility IDs** - EPA Facility Registry Service (FRS) ID numbers
#'    
#'    3. **uploaded locations** as lat/lon points.
#'    
#'    
#'  **1. BY INDUSTRIAL SECTOR/ NAICS:**
#'  
#'         Interface lets user select NAICS from pulldown, or type in NAICS 
#'         Interface returns a vector of one or more naics codes,
#'         to be converted to sitepoints.
#' 
#'    - **[EJAM::latlon_from_naics()]** takes NAICS codes and returns a data.table of site points.
#'      Relies on **[EJAMfrsdata::frs_by_naics].rda**  A data.table, needed to get lat lon by naics. 
#'      Need to update FRS data used here regularly, ideally frequently.
#'      2023 version has columns   REGISTRY_ID,  NAICS, lat, lon
#'      
#'      NAICS codes also can be selected by text search of industry names or by categories of codes, via [EJAM::naics_from_any()]
#'       
#'       
#'  **2. BY FACILITY ID or PROGRAM ID:**  
#'  
#'         Interface so user can upload FRS REGISTRY_ID or PROGRAM ID csv/xls file, 
#'         Interface returns a list of REGISTRY_ID values from a copy of the EPA facility registry service (FRS) data  
#'         to be converted to sitepoints.
#' 
#'    - **[EJAM::frs_from_siteid()]** takes REGISTRY_ID values and returns a data.table of site points.
#'      Relies on **frs** data.table with columns REGISTRY_ID, lat, lon, etc.
#'      
#'    - **[EJAM::frs_from_programid()]** takes EPA program-specific site ID values and returns a data.table of site points.
#'      Relies on **[EJAMfrsdata::frs_by_programid]** data.table with columns program, pgm_sys_id, REGISTRY_ID, lat, lon 
#'      
#' 
#'  **3. BY LAT/LON POINT:**
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
#'   - **[getblocksnearby()](sitepoints)** which by default uses *[getblocksnearbyviaQuadTree()]*
#'        Returns `sites2blocks` 
#'        Requires datasets [quaddata] and [blockquadtree] 
#'        
#'   - **sites2blocks**   Created by [getblocksnearby()] and passed to  [doaggregate()]  
#'      This is a data table with maybe 100k to 1m rows (assume 1k blocks within 3 miles of each site, or 100 blocks within 1 mile),
#'      `sites2blocks[ , .(siteid, blockid, distance or dist)]`
#'     - siteid    (site with circular buffer to group by)
#'     - blockid     for join to blockwts
#'     - distance or dist  (in miles, from block to site) (0 or irrelevant for noncircular buffers, 
#'          since a block is only in this table if in one or more buffers, 
#'          unless analysis is for residents within x miles of the edges of some shapes, like facility boundaries)
#'          
#'          
#' @details  # **Data files used for distance calculation:** ####################################################################
#' 
#'   - **[EJAMblockdata::quaddata].rda** dataset data.table
#'   
#'    8,174,955 rows when non-populated blocks are kept. 
#'    5,806,512 rows have Census 2020 population (and blockwt) > 0. 
#'    This is the largest file in the package, and is 168 MB as a file, for 2020 Census.
#'      - blockid 
#'      - BLOCK_X, BLOCK_Y, BLOCK_Z  (not lat, lon)
#'  
#'   - **[EJAMblockdata::blockquadtree].rda**  (may rename as blocktree) 
#'   
#'     Index to quaddata (QuadTree class, via SearchTrees pkg), not a data.table 
#' 
#'   -------then those are used in getblocksnearby with some sitepoints to create sites2blocks:
#'   
#'   
#' @details  # **Summarizing indicators in buffers:** ####################################################################
#' 
#'       INPUT IS  `sites2blocks`, 
#'       OUTPUT IS results_overall, results_bysite, and maybe other summary stats  ####
#'   
#'   - **[doaggregate()]** = function(sites2blocks) This summarizes in each buffer and for all unique residents across all buffers.
#'     
#'   - **[EJAMblockdata::blockwts].rda**
#'    Required by [doaggregate()]. A data.table of 6-8m rows 
#'     - blockwt  The fraction of parent blockgroup decennial pop that is in this one block
#'     - blockid (integer key  for join to sites2blocks)
#'     - bgfips no longer is here - moved to bgfips2id.  
#'     - bgid   integer key instead of bgfips - For sum(blockwt), by=bgid, and for join to blockgroupstats$bgid. 
#'       More efficient than bgfips but bgfips is easier
#' 
#'   - **[blockgroupstats].rda** a data.table with 220k rows (blockgroups), and about 200 cols.
#'      Will need bgid not just bgfips, to join to blockwts$bgid
#'      Needs to be updated each time EJScreen is updated. 
#'      (such as EJScreen demographic and environmental data
#'       EJScreen 2020 version was about 100MB as .rda)
#'
#'   - **[usastats].rda** and **[statestats].rda**  data.table lookup of 100 percentiles and means 
#'       (for each indicator in blockgroupstats) in each zone (us,   or a state).
#'       Need to update each time blockgroupstats is updated. Taken from EJScreen data or ejscreen::lookupUSA & lookupStates
#' 
#'   - **bg2sites?**  Intermediate result, not saved.  aggregated version of sites2blocks
#'            unless we want to preserve the full bg2sites info for other site groupings, or later detailed 
#'            analysis of distribution of distances in each demog group.
#'            
#' @details  # **Output results for user:** ####################################################################
#' 
#'   * **`results_overall`**   one row data.table, like results_by_site, but just one row with 
#'     aggregated results for all unique residents. 
#' 
#'   * **`results_by_site`**   results for individual sites (buffers) - a data.table of results, 
#'     one row per siteid, one column per indicator
#' 
#'    * maybe want some extra rows with summary stats across people and sites 
#'      (about the distribution), one column per indicator. 
#'      BUT MOST OF THE INTERESTING STATS LIKE MEDIAN PERSON'S SCORE, OR WORST BLOCKGROUP,
#'      HAVE TO BE CALCULATED BEFORE AGGREGATING/ SUMMARIZING BY SITE (BUFFER), FROM RAW BG DATA!
#'      Same for sites: worst site as measured by highest nearby blockgroup-level
#'      %poor needs raw bg data before summarized by siteid.
#' 
#'    * maybe some extra columns with summary stats across indicators, as 
#'      separate summary stats beyond what EJScreen report does?, one row per site and for overall. 
#'      
#' @details  # **Data files available as examples:** ####################################################################
#'   
#'   * [testpoints_n()] can generate random test points at places weighted by population, FRS facilities, blockgroup, area, block
#'   * [testpoints_100_dt].rda  Random test points data.table with columns lat lon site
#'   * [testpoints_1000_dt].rda Random test points data.table with columns lat lon site
#'   * [sites2blocks_example].rda sample output of getblocksnearbyviaQuadTree or just getblocksnearby
#'   * See list.files("./inst/testdata/")
#'
#' @details  # **Identification of nearby residents -- methodology:** ####################################################################
#'
#' The identification of nearby residents is currently done in a way that includes all 2020 Census blocks whose
#' "internal point" (a lat/lon provided by the Census Bureau) is within the specified distance of the facility point.
#' This is taken from the EJScreen block weights file, but can also be independently calculated. See [EJAMblockdata] package.
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
