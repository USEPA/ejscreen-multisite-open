#' @title Find approx wtd percentiles in lookup table that is in memory
#' @description This is used with a data.frame that is a lookup table used to
#'   convert a raw indicator value to a percentile - US, Region, or State percentile.
#' @details
#'   This could be recoded to be more efficient.
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
#'   If specified, must appear in a column called REGION within the lookup table.
#'   For example, it could be 'NY' for New York State.
#'
#' @return By default, returns numeric vector length of myvector.
#' @export
lookup_pctile <- function(myvector, varname.in.lookup.table, lookup=usastats, zone) {

  if (missing(lookup) & (exists('us'))) {lookup <- us}
  if (missing(lookup) & !exists('us')) {stop('must specify lookup= or have it in memory named "us"')}
  if (missing(zone) & lookup$REGION[1] != 'USA') {stop('If lookup is not us, need to specify zone="NY" for example')}
  # lookup table must have PCTILE field (& this removes the row called 'mean')
  if (!('PCTILE' %in% names(lookup))) {stop('lookup must have a field called "PCTILE" that contains quantiles/percentiles')}
  lookup <- lookup[lookup$PCTILE != "std.dev", ]
  lookup <- lookup[lookup$PCTILE != "mean", ]

  if (missing(zone)) {
    whichinterval <- findInterval(myvector, lookup[ , varname.in.lookup.table])
  } else {
    if (any(!(zone %in% lookup$REGION))) {stop('zone(s) not found in lookup')}

    # also see similar code in ejanalysis::assign.pctiles()
    whichinterval <- vector(length = NROW(myvector))

    for (z in unique(zone)) {
      #find sort vectors for each zone
      myvector_selection <- sort(myvector)
      myvector_lookup <- sort(lookup[lookup$REGION == z, varname.in.lookup.table])

      # should be OK if some or all those values in myvector are NA
      # should check what if some or all in lookup are NA, though

      whichinterval[zone == z] <- findInterval(myvector_selection, myvector_lookup)
    }
  }

  # would be an error if zeroeth row were selected here,
  # so just say it is at the lowest percentile listed (which is 0)
  # even if it is below the minimum value that supposedly defines the lower edge
  belowmin <- (whichinterval == 0)
  if (any(belowmin, na.rm = TRUE)) {
    whichinterval[!is.na(belowmin) & belowmin]  <- 1
    warning('One or more values were below the minimum, or zeroeth percentile, but are reported by this function as being at the 0 percentile.')
  }
  whichinterval[is.na(belowmin)] <- NA
  # returns NA if belowmin is NA
  return(as.numeric(lookup$PCTILE[whichinterval]))
}

