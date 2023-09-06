#' @title Find approx wtd percentiles in lookup table that is in memory
#' @description This is used with a lookup table to
#'   convert a raw indicator vector to percentiles in US or States.
#' @details
#'   This could be recoded to be more efficient - could use data.table
#'   The data.frame lookup table must have a field called "PCTILE" that has quantiles/percentiles
#'   and other column(s) with values that fall at those percentiles.
#'   EJAM::usastats, EJAM::statstats, EJAM::regionstats are such lookup tables.
#'   This function accepts lookup table (or uses one called us if that is in memory), and
#'   finds the number in the PCTILE column that corresponds to where a specified value
#'   (in myvector) appears in the column called varname.in.lookup.table.
#'   The function just looks for where the specified value fits between values in the lookup table
#'    and returns the approximate percentile as found in the PCTILE column.
#'   If the value is between the cutpoints listed as
#'   percentiles 89 and 90, it returns 89, for example.
#'   If the value is exactly equal to the cutpoint listed as percentile 90,
#'   it returns percentile 90.
#'   If the value is exactly the same as the minimum in the lookup table and multiple percentiles 
#'   in that lookup are listed as tied for having the same threshold value defining the percentile
#'    (i.e., a large % of places have the same score and it is the minimum score), 
#'    then the percentile gets reported as 0, 
#'   not the percent of places tied for that minimum score. Note this is true whether they are 
#'   tied at a value of 0 or are tied at some other minimum value than 0.
#'   If the value is less than the cutpoint listed as percentile 0,
#'   which should be the minimum value in the dataset,
#'   it still returns 0 as the percentile, but with a warning that
#'   the value checked was less than the minimum in the dataset.
#'
#' @param myvector Numeric vector, required. Values to look for in the lookup table.
#' @param varname.in.lookup.table Character element, required.
#'   Name of column in lookup table to look in
#'   to find interval where a given element of myvector values is.
#' @param lookup Either lookup must be specified,
#'   or a lookup table called us must already be in memory. This is the lookup table
#'   data.frame with a PCTILE column and column whose name is the value of varname.in.lookup.table
#' @param zone Character element (or vector as long as myvector), optional.
#'   If specified, must appear in a column called REGION within the lookup table,
#'    or NA returned for each item looked up and warning give.
#'   For example, it could be 'NY' for New York State.
#' @aliases lookup_pctile
#' @return By default, returns numeric vector length of myvector.
#' @examples \dontrun{
#'   # compare ejscreen API output percentiles to those from this function:
#'   for (vname in c(names_d[c(1,3:6,8:10)] )) {
#'      print(pctile_from_raw_lookup(testoutput_ejscreenapi_plus_50[,vname] / 100, vname, 
#'        lookup = usastats) 
#'        - testoutput_ejscreenapi_plus_50[,paste0("pctile.",vname)] )
#'   }
#'   for (vname in c(names_e )) {
#'      print(pctile_from_raw_lookup(testoutput_ejscreenapi_plus_50[,vname], vname, 
#'        lookup = usastats)
#'          - testoutput_ejscreenapi_plus_50[,paste0("pctile.",vname)] )
#'   }
#' }
#' @export
#' 
pctile_from_raw_lookup <- function(myvector, varname.in.lookup.table, lookup=usastats, zone) {
  if (length(varname.in.lookup.table) != 1) {stop("Must specify only one variable (column name) as varname.in.lookup.table")}
  # if (missing(lookup) & (exists('usastats'))) {lookup <- us}
  # if (missing(lookup) & !exists('us')) {stop('must specify lookup= or have it in memory named "us"')}
  if (missing(zone) & lookup$REGION[1] != 'USA') {stop('If lookup is not us, need to specify zone="NY" for example')}
  # lookup table must have PCTILE field (& this removes the row called 'mean')
  if (!('PCTILE' %in% names(lookup))) {stop(
    'lookup must have a field called "PCTILE" that contains quantiles/percentiles')}
  # lookup <- lookup[lookup$PCTILE != "std.dev", ]  # SLOW - now DROPPED BEFORE SAVING AS DATA FOR THE PACKAGE
  lookup <- lookup[lookup$PCTILE != "mean", ]
  
  if (all(is.na(myvector))) {
    warning("All values in myvector were NA, so returning NA values as percentiles.")
    return(rep(NA, length(myvector))) # and note any individual NA values input return NA values as the percentiles
  }
  
  if (!(varname.in.lookup.table %in% colnames(lookup))) {
    warning(paste0(varname.in.lookup.table, " must be a column in lookup table - returning NA values"))
    return(rep(NA, length(myvector)))
  }  
  
  if (max(myvector, na.rm = TRUE) > 1 & max(lookup[,varname.in.lookup.table], na.rm = TRUE) <= 1) {
    warning("Raw scores are > 1, but lookup table values are not. Check if percentages should be expressed as fractions (0 to 1.00) instead of as integers 0-100")
  }
  
  # fixed case where input myvector is just NA value(s) 
  
 
  
  if (missing(zone)) { # USA being used
    
    if (anyNA(lookup[ , varname.in.lookup.table])) {
      # findInterval() requires no NA values in vector looked in, so assume the
      # percentile cannot be looked up for this indicator in this zone, return NA values.
    
        warning("All values in myvector were NA for ", varname.in.lookup.table, ", so returning NA values as percentiles.")
        return(rep(NA, length(myvector)))
    }
    whichinterval <- findInterval(myvector, lookup[ , varname.in.lookup.table])
    
    # findInterval returns the gap (interval) number, 0 through length(myvector), and
    # 0 if < min of lookup, and counts it as in the gap if tied with lower edge and less than upper edge.
    # If lookup was for pctiles 1-100, the whichinterval would be the same as the percentile, including for findInterval = pctile = 0.
    # The lookup table has pctiles 0-100, where 0 is min and 0-1 is 1st 1%, so 
    # the pctile should be reported as lookup[whichinterval, varname.in.lookup.table] ....
    #  unless whichinterval = 0 (i.e., place has score smaller than min of all places used in creating the lookup table) in which case report 0 for pctile.
    
    # ***   fix case where multiple percentiles are tied in lookup table... as with pctlingiso 
    # findInterval will return the last interval, not the first, that it matches when there are duplicates,
    # which does not mesh with how the lookup table should be interpreted.
    # -- We want to report the first not last when the same value is shown as being at multiple percentiles, 
    # which happens if a large percent of places are tied at some given value, such as when 30% of places have a score of zero, e.g.
    # One workaround is to check the lookup tables for cases of ties at min value, and add a very tiny amount to each of those, 
    #  which will force it to report 0 percentile for that value that hasn't had the tiny amount added.
    # that seems like an awkward hack but would work. 
    # We could alter the statestats table itself, and usastats. but, 
    # solution: created   high_pctiles_tied_with_min  dataset 
    # Rule for using it here:  
    # if reported pctile per lookup function is <= these high_pctiles_tied_with_min, 
    # then report instead zero as the percentile.
    #
    # To see what variables in what states have a bunch of tied values at minimum that is NOT zero, and then ties at zero:
    # datacols <- setdiff(names(statestats), c('PCTILE', 'REGION')); states <- unique(statestats$REGION);  for (myvar in datacols) {for (mystate in states) {z = statestats[mystate == statestats$REGION, myvar]
    #   if ((z[1] == z[2]) & (z[1] != 0)) {cat("in ",mystate, " for ", myvar, " = ", z[1], '\n')}}}
    # datacols <- setdiff(names(statestats), c('PCTILE', 'REGION')); states <- unique(statestats$REGION);  for (myvar in datacols) {for (mystate in states) {z = statestats[mystate == statestats$REGION, myvar]
    #   if ((z[1] == z[2]) & (z[1] == 0)) {cat("in ",mystate, " for ", myvar, " = ", z[1], '\n')}}}
    # datacols <- setdiff(names(usastats), c('PCTILE', 'REGION')); states <- unique(usastats$REGION);  for (myvar in datacols) {for (mystate in states) {z = statestats[mystate == usastats$REGION, myvar]
    #   if ((z[1] == z[2]) & (z[1] != 0)) {cat("in ",mystate, " for ", myvar, " = ", z[1], '\n')}}}
    # datacols <- setdiff(names(usastats), c('PCTILE', 'REGION')); states <- unique(usastats$REGION);  for (myvar in datacols) {for (mystate in states) {z = statestats[mystate == usastats$REGION, myvar]
    #   if ((z[1] == z[2]) & (z[1] == 0)) {cat("in ",mystate, " for ", myvar, " = ", z[1], '\n')}}}
    
    ## also, this probably happens for any set of tied threshold values (not only ties at min value) - but would need to confirm EJScreen was coded that way.
    
    # ** also using data.table might make this whole function significantly faster if statestats is a data.frame with keys REGION and PCTILE 
     # pctile <- lookup[myvector >= ..varname.in.lookup.table, PCTILE[1]] # but also, if none where >= true, pctile <- lookup$PCTILE[1]


   # fix unusual cases
    # would be an error if zeroeth row were selected here,
    # so just say it is at the lowest percentile listed (which is 0)
    # even if it is below the minimum value that supposedly defines the lower edge
    # whichinterval = 0 (i.e., place has score smaller than min of all places used in creating the lookup table) in which case report 0 for pctile
    belowmin <- (whichinterval == 0)
    if (any(belowmin, na.rm = TRUE)) {
      whichinterval[!is.na(belowmin) & belowmin]  <- 1 # which means 0th percentile
      if (interactive()) {warning('One or more values were below the minimum, or zeroeth percentile, but are reported by this function as being at the 0 percentile.')}
    }
   
    whichinterval[is.na(belowmin)] <- NA  
    percentiles_reported <- as.numeric(lookup$PCTILE[whichinterval])
# or should it be     
    # whichinterval[is.na(belowmin)] <- 0
    # percentiles_reported[is.na(belowmin)] <- NA 
        
    
    
    # ----------------------------------------------------------------------------
    # if reported pctile per lookup function is <= these high_pctiles_tied_with_min, 
    # then report instead zero as the percentile.
    percentiles_reported[percentiles_reported == unlist(high_pctiles_tied_with_min[["USA"]][ , varname.in.lookup.table, with=FALSE]) ] <- 0
    # ----------------------------------------------------------------------------

    return(percentiles_reported)
    
    # finished USA only case 
  } else {
    
    ######################################################################### # 
    # multiple zones, not just USA
    ######################################################################### # 

    if (length(zone) != length(myvector)) {
      if (length(zone) == 1) {
        # Assume they meant the one zone (e.g. a State) to apply to all the indicator values provided as myvector
        zone <- rep(zone, length(myvector))
      } else {
        stop('number of raw score values and number of zone values provided must be the same (or if just one zone value, assumed to apply for all)')
      }
    }
    ######################################################################### # 
    # also see similar code in ejanalysis package file lookup.pctiles() !
    
    whichinterval <- vector(length = NROW(myvector))
    percentiles_reported <- vector(length = NROW(myvector))
    # browser()
    
    for (z in unique(zone)) {
      
      #  for each zone
      if (!(z %in% lookup$REGION)) {
        # this zone is not in lookup table so return NA values and warn
        warning(z, " not found in percentile lookup table column called REGION")
        percentiles_reported[zone == z] <- NA
        next # go to next zone (for this one indicator)
      }
      myvector_selection <- myvector[zone == z] # sort(myvector)
      myvector_lookup <-   lookup[lookup$REGION == z, varname.in.lookup.table] 
      
      # if all or just some values in myvector_selection are NA, 
      #  findInterval(x=myvector_selection, vec=myvector_lookup) returns NA for each is.na(x)
      if (all(is.na(myvector_selection))) {
        message("All values in myvector (scores to be converted to percentiles) were NA for ", varname.in.lookup.table, " in zone = ", z, ", so returning NA values as the percentiles.")
        percentiles_reported[zone == z] <- NA
        next # go to next zone (for this one indicator)
      }
      # if all or some in lookup are NA, findInterval crashes unless that case is handled
      # we will just assume the lookup is useless for this indicator in this zone, probably because no blockgroupstats data at all for that indicator in that zone,
      # rather than figuring out why some percentiles in lookup are NA in this zone.
      if (any(is.na(myvector_lookup))) {
        # whichinterval[zone == z] <- rep(NA, length(myvector_selection))
        message("No percentile info is available in the lookup table, for the indicator ", varname.in.lookup.table, " in ", z)
        percentiles_reported[zone == z] <- NA
        next  # go to next zone (for this one indicator)
      } else {
        whichinterval[zone == z] <- findInterval(myvector_selection, myvector_lookup)
      }
       
      belowmin <- (whichinterval[zone == z] == 0)
      if (any(belowmin, na.rm = TRUE)) {
        whichinterval[zone == z][!is.na(belowmin) & belowmin]  <- 1 # which means 0th percentile
        warning('One or more values were below the minimum, or zeroeth percentile, but are reported by this function as being at the 0 percentile.')
      }
      whichinterval[zone == z][is.na(belowmin)] <- 1 # will not be used but wont cause error. pctile reported will be NA in this case.
      
      percentiles_reported[zone == z] <- as.numeric(lookup$PCTILE[lookup$REGION == z][whichinterval[zone == z]]) # this is just in case each zone has a different number of or set of PCTILE values.
      # returns NA if belowmin is NA
      percentiles_reported[zone == z][is.na(belowmin)] <- NA 
      # ---------------------------------------------------------------------------- 
      # if reported pctile per lookup function is <= these high_pctiles_tied_with_min, 
      # then report instead zero as the percentile.
      percentiles_reported[zone == z][percentiles_reported[zone == z] == unlist(high_pctiles_tied_with_min[[z]][ , varname.in.lookup.table, with=FALSE]) ] <- 0
      # ----------------------------------------------------------------------------
      
    } # end of loop over zones ####
    
    return(percentiles_reported)
    
  } # end of multiple states case 
  
  # should never get here!?
}

lookup_pctile  <- pctile_from_raw_lookup
