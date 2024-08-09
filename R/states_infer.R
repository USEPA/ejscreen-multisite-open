
#' states_infer - Identify US State that each site is in (given ST, FIPS, or lat/lon)
#' 
#' Identify US State that each site is in (given ST, lat/lon, or FIPS)
#' @param x data.frame or data.table, with one row per site, and
#'   column(s) that are either "ST" (2-letter abbreviation of State),
#'   "lat" and "lon", or "fips" or "bgfips" 
#'   and optionally a column like  "ejam_uniq_id" or "n"
#' @seealso [state_from_latlon()] [state_from_fips()]
#' @return the input table as a data.frame, but with these 
#'   new columns if ST was not already a column:
#'   ejam_uniq_id, ST, statename, FIPS.ST, REGION, n
#' @examples
#'   states_infer(testpoints_10)
#'   states_infer(testoutput_ejamit_10pts_1miles$results_bysite[, .(ejam_uniq_id, ST, pop)])
#'   states_infer(testoutput_ejamit_10pts_1miles$results_bysite[, .(ST, pop)])
#'   states_infer(testoutput_ejamit_10pts_1miles$results_bysite[, .(ST, lat, lon, pop)])
#'   
#' @export
#'
states_infer <- function(x) {
  
  # NEED TO RECONCILE / MERGE THESE:  ***
  # states_infer() - - being redone - looks at 1row/site info like ST, and can look at latlon of each site.
  # ST_by_site_from_sites2blocks() - being redone
  # 
  # latlon_from_s2b() - gets sitepoints latlon estimates, not block latlons. was drafted just as last resort for someone not using ejamit not using shiny app.
  
  
  bad_sites2states <- FALSE
  if (missing(x)) {
    bad_sites2states <- TRUE
    stop("states_infer() requires x parameter")
  } else {
    
    sites2states <- x # sites2states_or_latlon # could rename param or variable in code to avoid making this copy
    
    if (data.table::is.data.table(sites2states)) data.table::setDF(sites2states)
    # error handling
    ### overly inflexible to require identical lists of ejam_uniq_id values -- 
    # might want to get state pctiles for all where possible even if 1 site lacks ST, 
    # and ignore extra info too not found in sites2blocks
    # if (!all(unique(sites2blocks$ejam_uniq_id) %in% sites2states$ejam_uniq_id)) {
    #   warning("cannot provide state percentiles unless all ejam_uniq_id values in sites2blocks are also in sites2stats")
    #   bad_sites2states <- TRUE
    # }
    # if (!all(unique(sites2states$ejam_uniq_id) %in% sites2blocks$ejam_uniq_id)) {
    #   warning("cannot provide state percentiles unless all ejam_uniq_id values in sites2stats are also in sites2blocks")
    #   bad_sites2states <- TRUE
    # }
    
    # create ejam_uniq_id column if cannot find one (are we sure we want to do that?) ***
    if (!("ejam_uniq_id" %in% names(sites2states))) {
      if ("n" %in% names(sites2states)) {  # use n or rownumber as ejam_uniq_id if not explicitly provided as ejam_uniq_id column
        sites2states$ejam_uniq_id <- sites2states$n
      } else {
        sites2states$ejam_uniq_id <- 1:NROW(sites2states)
      }
    }
    
    # is ST missing?
    if (!("ST"  %in% names(sites2states))) {
      
      ### this looked for latlon 1st, but actually it
      ### should first try to use any fips like bgfips of each site, before resorting to latlon/shapefile !
      ## so move the code below into different sequence: ***
      
      
      # is at least lat / lon there?
      if (("lat" %in% names(sites2states) ) && ("lon" %in% names(sites2states))) {
        # use lat lon to get ST, but it takes a few seconds to use the shapefile to do this:
        sites2states <- cbind(
          sites2states,  
          state_from_latlon(lat = sites2states$lat, lon = sites2states$lon)  ## VERY SLOW STEP ***
          ) 
        
      } else {
        
        
        # is blockgroup FIPS, or even any FIPS there?
        if ("fips" %in% names(sites2states)) {
          sites2states$ST <- fips2state_fips(sites2states$fips) # *** need to check this  # state_from_fips(sites2states$fips) # check if now returns 1 state abbrev for each element of supplied vector even if there are many duplicates.
        } else {
          if ("bgfips" %in% names(sites2states)) {
            sites2states$ST <- fips2state_fips(sites2states$bgfips) # *** check this ! # state_from_fips(sites2states$bgfips) # check if now returns 1 state abbrev for each element of supplied vector even if there are many duplicates.
            
          } else {
            
            
            # nothing was found that could provide the ST info
            bad_sites2states <- TRUE
          }
        }
      }
    } else {
      # ST was already available, so just leave table as-is
    }
  }
  # if nothing found to tell us the ST info, fill in NA values
  if (bad_sites2states) {
    sites2states <- data.frame(ejam_uniq_id = 1:length(unique(sites2states$ejam_uniq_id)), ST = NA)
  }
  # check quality of ST info found or looked up
  if (!all(sites2states$ST %in% EJAM::stateinfo$ST)) {
    sites2states$ST[(!(sites2states$ST %in% EJAM::stateinfo$ST))] <- NA
    if (all(is.na(sites2states$ST))) {
      warning("no valid ST provided to states_infer() so cannot lookup state percentiles")
      bad_sites2states <- TRUE
    } else {
      bad_sites2states <- FALSE # SOME ST info was found OK
    }
    warning("cannot provide all state percentiles unless all sites2states$ST values are valid 2-character State abbreviations")
    # 
  }
  return(sites2states)
}
