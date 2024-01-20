#' utility to multiply certain percentage columns by 100 to convert 0-1.00 into 0-100
#' multiple some data to rescale percentages stored as 0 to 1, into 0-100
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
#' @export
#'
#' @examples
#'     y <- testoutput_ejamit_10pts_1miles$results_bysite
#'     y1 <- fix_pctcols_x100(y)
#'     y2 <- fix_pctcols_x100(y,  names_pct_as_fraction_ejscreenit)
#'     
fix_pctcols_x100 <- function(df, cnames = NULL) {
  
  ## which percentage indicators are stored as 0-1.00 not 0-100 ?
  ## This will correct for different scaling in blockgroupstats and ejamit()$results_bysite

  # inefficient to pass the whole df here but should work 
  
  if (is.null(cnames)) {cnames <- names_pct_as_fraction_ejscreenit} # EJAM package data includes names_pct_as_fraction_blockgroupstats, 
  tofix <- which(names(df) %in% cnames)
  
  if (is.data.table(df)) {
    df[ , ..tofix] <- df[ , ..tofix] * tofix
  } else {
    df[ , tofix] <- df[ , tofix] * 100
    return(df)
  }
}

