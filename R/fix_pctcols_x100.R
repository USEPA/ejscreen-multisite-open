
#' utility to multiply certain percentage columns by 100 to convert 0-1.00 into 0-100
#' 
#' multiplies some data to rescale percentages stored as 0 to 1, into 0-100
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
#' @return df with data in specified columns multiplied by 100
#'
#' @examples 
#'  y = data.frame(pctlowinc = 1:2, pctpre1960 = 1:2, avg.pctunemployed = 1:2, avg.pctpre1960 = 1:2)
#'  fix_pctcols_x100(y)
#'  fix_pctcols_x100(y, names_pct_as_fraction_ejscreenit)
#'  fix_pctcols_x100(y, names_pct_as_fraction_blockgroupstats)
#'  fix_pctcols_x100(y, names_pct_as_fraction_ejamit)
#'  cat("\n\n")
#'  names_pct_as_fraction_ejscreenit
#'  names_pct_as_fraction_blockgroupstats
#'  names_pct_as_fraction_ejamit
#'  cat("\n\n")
#'  ytable = data.table(pctlowinc = 1:2, pctpre1960 = 1:2, avg.pctunemployed = 1:2, avg.pctpre1960 = 1:2)
#'  fix_pctcols_x100(ytable)
#'  fix_pctcols_x100(ytable, names_pct_as_fraction_blockgroupstats) 
#'  fix_pctcols_x100(ytable, names_pct_as_fraction_ejamit)
#'  cat("\n\n")
#'  y
#'  ytable
#'  
#' @keywords internal
#'
fix_pctcols_x100 <- function(df, cnames = NULL) {

  ## which percentage indicators are stored as 0-1.00 not 0-100 ?
  ## This will correct for different scaling in blockgroupstats and ejamit()$results_bysite

  # inefficient to pass the whole df here but should work

  if (is.null(cnames)) {cnames <- names_pct_as_fraction_ejscreenit} # EJAM package data includes names_pct_as_fraction_blockgroupstats,
  tofix <- names(df)[names(df) %in% cnames]
  if (is.data.table(df)) {
    # df[ , (tofix) := lapply(.SD, function(z) z * 100), .SDcols = tofix] #painful syntax and updates the data.table in the calling envt by reference which may be unexpected
    setDF(df)
    df[ , tofix] <- df[ , tofix] * 100
    setDT(df)
    return(df)
  } else {
    df[ , tofix] <- df[ , tofix] * 100
    return(df)
  }
}
