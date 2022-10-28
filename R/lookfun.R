#' @title Find approx percentiles in lookup table 
#'
#' @description This is used with lookup, a data.frame that is a lookup table
#'   The data.frame us or lookup must have a field called "PCTILE" that contains quantiles/percentiles
#'   and other column(s) with values that fall at those percentiles.
#' @details  This function accepts lookup table 
#'   and finds the number in the PCTILE column that corresponds to where a specified value (in myvector)
#'   appears in the column called varname.in.lookup.table.
#'   The function just looks for where the specified value fits between values in the lookup table 
#'   and returns the approximate percentile as found in the PCTILE column.
#'   If the value is between the cutpoints listed as percentiles 89 and 90, it returns 89, for example.
#'   If the value is exactly equal to the cutpoint listed as percentile 90, it returns percentile 90.
#'   If the value is less than the cutpoint listed as percentile 0, which should be the minimum value in the dataset,
#'   it still returns 0 as the percentile, but with a warning that the value checked was less than the minimum in the dataset.
#' @param myvector Numeric vector, required. Values to look for in the lookup table.
#' @param varname.in.lookup.table Character element, required. Name of column in lookup table to look in to find interval where a given element of myvector values is.
#' @param lookup   lookup must be specified - This is the lookup table data.frame with a PCTILE column 
#'   and column whose name is the value of varname.in.lookup.table.
#' @return By default, returns numeric vector length of myvector.
lookfun <- function(myvector, varname.in.lookup.table, lookup) {
  # CONVERT raw scores TO PERCENTILES by looking them up in a lookup table of percentiles 0-100
  # simplified version of  ejanalysis::lookup.pctile.US
  lookup <- lookup[lookup$PCTILE != "std.dev", ] # drop this row if it is there
  lookup <- lookup[lookup$PCTILE != "mean", ] # drop this row if it is there
  whichinterval <- findInterval(myvector, lookup[ , varname.in.lookup.table])
  belowmin <- (whichinterval == 0)
  if (any(belowmin, na.rm = TRUE)) {
    whichinterval[!is.na(belowmin) & belowmin]  <- 1
    warning('One or more values were below the minimum, or zeroeth percentile, but are reported by this function as being at the 0 percentile.')
  }
  whichinterval[is.na(belowmin)] <- NA
  # returns NA if belowmin is NA
  return(as.numeric(lookup$PCTILE[whichinterval]))
}

#'   lookup.pctile(myvector = c(-99, 0, 500, 801, 949.9, 1000, 99999),
#'     lookup = data.frame(PCTILE = 0:100,
#'     myvar = 10*(0:100),
#'     REGION = 'USA', stringsAsFactors = FALSE), varname.in.lookup.table = 'myvar')
#'  \dontrun{
#'    What is environmental score at given percentile?
#'  ejanalysis::lookup.pctile(40,'cancer',lookupUSA)
#'  #   84
#'  ejanalysis::lookup.pctile(40,'cancer',lookupStates,'WV')
#'  #   93
#'  #    What is percentile of given environmental score?
#'  ejscreen::lookupUSA[lookupUSA$PCTILE=='84' ,'cancer']
#'  #   39.83055
#'  ejscreen::lookupStates[lookupStates$PCTILE=='84' & lookupStates$REGION =='WV','cancer']
#'  #   33.36371
#'  # also see ejanalysis::assign.pctiles
#'  }
#'   \dontrun{
#'      library(ejscreen)
#'        evarname <- 'traffic.score'
#'        evalues <- bg21[ , evarname]
#'        emax <- max(evalues,na.rm = TRUE)
#'        plot(lookupUSA[,evarname], lookupUSA$PCTILE, xlab = evarname, main='contents of lookup table', type='b')
#'
#'        plot(x = (1:1000) * (emax / 1000),
#'         y = lookup.pctile(myvector = (1:1000) * emax / 1000, varname.in.lookup.table = evarname, lookup = lookupUSA),
#'         ylim = c(0,100),   type='b',
#'         xlab=evarname, ylab='percentile per lookup table' , main='Percentiles for various envt scores, as looked up in lookup table')
#'       abline(v = lookupUSA[ lookupUSA$PCTILE == 'mean', evarname])
#'
#'   }