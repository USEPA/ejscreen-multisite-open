#' ST_by_site_from_sites2blocks - Get State that each site is in, from a table of siteid, blockid, distance
#' @description  Find the 2-character State abbreviation for each site. This is for when you need to know
#'   the state each site is in, to be able to report state percentiles, but you do not have 
#'   the original list of siteid lat/lon or State info. This can infer the State
#'   each site is located in, based on the state of the nearest block (and its parent blockgroup).
#' @param sites2blocks data.table or data.frame, like [testoutput_getblocksnearby_10pts_1miles],
#'   from [getblocksnearby()] that has columns siteid and blockid and distance
#'
#' @return data.table with columns  siteid, ST 
#' @export
#'
#' @examples \dontrun{
#'   fname = './inst/testdata/testpoints_207_sites_with_signif_violations_NAICS_326_ECHO.csv'
#'   x = ST_by_site_from_sites2blocks(
#'     getblocksnearby( latlon_from_anything(fname), quadtree = localtree))
#'   y = read_csv_or_xl(fname)
#'   x$ST == y$FacState
#'   }
#'   ST_by_site_from_sites2blocks(testoutput_getblocksnearby_10pts_1miles) 
#'   
ST_by_site_from_sites2blocks <- function(sites2blocks) {
  setDT(sites2blocks)
  #if (!all(c('siteid', 'blockid', 'distance') %in% names(sites2blocks) )) {
  if (!all(c('ejam_uniq_id', 'blockid', 'distance') %in% names(sites2blocks) )) {
      stop("column names must include siteid, blockid, and distance, as in output of getblocksnearby() - see ?testoutput_getblocksnearby_10pts_1miles")}
  nearestbg <- blockwts[sites2blocks[ , .( blockid = blockid[which.min(distance)]) , by = "ejam_uniq_id"], .(ejam_uniq_id, bgid), on = "blockid" ]
  blockgroupstats[nearestbg, .(ejam_uniq_id, ST), on = "bgid"][order(ejam_uniq_id),]
 # nearestbg <- blockwts[sites2blocks[ , .( blockid = blockid[which.min(distance)]) , by = "siteid"], .(siteid, bgid), on = "blockid" ]
#  blockgroupstats[nearestbg, .(siteid, ST), on = "bgid"][order(siteid),]
}
