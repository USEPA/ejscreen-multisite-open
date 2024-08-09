#' Get State that each site is in, from a table of ejam_uniq_id, blockid, distance
#' 
#' @description  Find the 2-character State abbreviation for each site. This is for when you need to know
#'   the state each site is in, to be able to report state percentiles, but you do not have 
#'   the original list of ejam_uniq_id lat/lon or State info. This can infer the State
#'   each site is located in, based on the state of the nearest block (and its parent blockgroup).
#' @param sites2blocks data.table or data.frame, like [testoutput_getblocksnearby_10pts_1miles],
#'   from [getblocksnearby()] that has columns ejam_uniq_id and blockid and distance
#'
#' @return data.table with columns  ejam_uniq_id, ST 
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
  if (!all(c('ejam_uniq_id', 'blockid', 'distance') %in% names(sites2blocks) )) {
    warning("column names must include siteid, blockid, and distance, as in output of getblocksnearby() - see ?testoutput_getblocksnearby_10pts_1miles")
    return(NULL)
  }
  # sites2blocks <- getblocksnearby(testpoints_1000, radius = 3.1)
  s2st <- blockgroupstats[sites2blocks, .(ejam_uniq_id, ST), on = "bgid"][ , .(st1 = ST[1], in_how_many_states = length(unique(ST))), by = "ejam_uniq_id"]
  setorder(s2st, ejam_uniq_id)
  
  # s2st[, multistate := in_how_many_states > 1]
  # multistate_ids  <- s2st[in_how_many_states > 1, ejam_uniq_id]
  # s2st is like this:
  #    ejam_uniq_id    st1 in_how_many_states
  #           <int> <char>              <int>
  # 1:          365     AL                  1
  # 2:          806     AL                  1
  # 3:          890     AL                  1
  
  s2st[in_how_many_states == 1, ST := st1] # done. same state for 1st bgid as for all bgids
  s2st[in_how_many_states != 1, ST := NA]  # will need sitepoints lat lon and shapefile of states for these.
  s2st[, st1 := NULL]
  
  # table(gotST = !is.na(x$ST), states = x$in_how_many_states )
  ## a few cases where in only 1 state but still assigns NA as the ST? why?
  ## because Connecticut FIPS not yet fixed for v2.3 update!
  # mapfast(testpoints_1000[testpoints_1000$sitenumber %in% (s2st[in_how_many_states == 1 & is.na(ST),ejam_uniq_id]), ])
  
       # states_infer(sites2blocks[ ejam_uniq_id %in% multistate_ids, ]) ]  # slow accurate way if in multiple states
  
  return(s2st) 
}
################################################################### # 

## example of 2 ways to add statename columns using ST
# results_bysite = copy(testoutput_ejamit_100pts_1miles$results_bysite)[,4:9]
#   results_bysite[, statename2 := fips2statename(fips_state_from_state_abbrev(ST))] # like  
#   results_bysite[, statename3 := stateinfo$statename[match(ST, stateinfo$ST)]]

