
table_x100 <- function(df, cnames = c(names_pct_as_fraction_blockgroupstats, 
                                      names_pct_as_fraction_ejamit,
                                      names_pct_as_fraction_ejscreenit)[2]) {
  
  fix_pctcols_x100(df = df, cnames = cnames)
}
############################################################################# #  


#' utility to multiply certain percentage columns by 100 to convert 0-1.00 into 0-100
#' 
#' multiplies some data to rescale percentages stored as 0 to 1, into 0-100
#' 
#' @aliases table_x100
#' 
#' @param df data.frame but can be data.table
#' @param cnames colnames in df of indicators to multiply by 100, like those in
#'
#'   names_pct_as_fraction_ejamit,
#'
#'   names_pct_as_fraction_blockgroupstats, or
#'
#'   names_pct_as_fraction_ejscreenit
#'
#' @seealso [table_signif_round_x100()] [table_signif()] [table_round()] [table_x100()]
#' @return df with data in specified columns multiplied by 100
#'
#' @examples 
#'  y = data.frame(pctlowinc = 1:2, pctpre1960 = 1:2, avg.pctunemployed = 1:2, avg.pctpre1960 = 1:2)
#'  
#'  fix_pctcols_x100(y, names_pct_as_fraction_ejscreenit)
#'  fix_pctcols_x100(y, names_pct_as_fraction_blockgroupstats)
#'  fix_pctcols_x100(y, names_pct_as_fraction_ejamit)
#'  cat("\n\n")
#'  names_pct_as_fraction_ejscreenit
#'  names_pct_as_fraction_blockgroupstats
#'  names_pct_as_fraction_ejamit
#'  cat("\n\n")
#'  ytable = data.table(pctlowinc = 1:2, pctpre1960 = 1:2, avg.pctunemployed = 1:2, avg.pctpre1960 = 1:2)
#'  
#'  fix_pctcols_x100(ytable, names_pct_as_fraction_blockgroupstats) 
#'  fix_pctcols_x100(ytable, names_pct_as_fraction_ejamit)
#'  cat("\n\n")
#'  y
#'  ytable
#'  
#' @keywords internal
#'
fix_pctcols_x100 <- function(df, cnames = c(names_pct_as_fraction_blockgroupstats, 
                                            names_pct_as_fraction_ejamit,
                                            names_pct_as_fraction_ejscreenit)[2]
) {
  
  ## which percentage indicators are stored as 0-1.00 not 0-100 ?
  ## This will correct for different scaling in blockgroupstats and ejamit()$results_bysite, etc.
  
  # inefficient to pass the whole df here but should work
  
  if (missing(cnames)) {
    message("missing cnames parameter so assuming defaults should be used")
  }
  tofix <- names(df)[names(df) %in% cnames]
  if (length(tofix) != length(cnames)) {
    # message("note that not all of cnames were found in df") # drop this since it happens always
  }
  
  if (is.data.table(df)) {
    
    ## This way would be only slightly faster, using data.table approach, 
    ## saving about  0.01 seconds for 1,000 points dataset with cnames = names_pct_as_fraction_ejamit
    ## but would update the data.table in the calling envt by reference 
    ## rather than just returning an updated copy, which may be unexpected.
    #
    # df[ , (tofix) := lapply(.SD, function(z) z * 100), .SDcols = tofix] 
    
    setDF(df)
    df[ , tofix] <- df[ , tofix] * 100
    setDT(df)
    return(df)
    
  } else {
    df[ , tofix] <- df[ , tofix] * 100
    return(df)
  }
}
