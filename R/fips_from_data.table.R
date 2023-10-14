#' fips_from_table
#' Just read the codes in one column of a table obtained from something like read.csv, or excel, etc.
#' @param fips_table data.frame or data.table of FIPS codes for counties, states, or tracts, 
#'   for example, in a column whose name can be interpreted as FIPS 
#'   (is one of the aliases like fips, countyfips, etc.)
#' @param addleadzeroes whether to add leading zeroes where needed as for a State whose FIPS starts with "01"
#' @param inshiny used by server during shiny app
#'
#' @return a vector of fips codes
#' @export
#' @seealso [fipsbg_from_anyfips()] [fips_lead_zero()] [getblocksnearby_from_fips()] [fips_from_table()]
fips_from_table <- function(fips_table, addleadzeroes=TRUE, inshiny=FALSE) {
  # fips_table can be data.frame or data.table, as long as colnames has one alid fips alias
  ## create named vector of FIPS codes (names used as siteid)
  fips_alias <- c('FIPS','fips','fips_code','fipscode','Fips','statefips','countyfips', 
                  'ST_FIPS','st_fips','ST_FIPS','st_fips', 'FIPS.ST', 'FIPS.COUNTY', 'FIPS.TRACT')
  if (any(tolower(colnames(fips_table)) %in% fips_alias)) {
    firstmatch <- intersect(fips_alias, colnames(fips_table))[1]
    if (addleadzeroes) {
      fips_vec <- fips_lead_zero(as.character(fips_table[[firstmatch]]))
    }
    names(fips_vec) <- as.character(fips_vec)
    
  } else {
    if (inshiny) {  # IF IN A SHINY REACTIVE:
      validate(paste0('No FIPS column found. Please use one of the following names: ', paste0(fips_alias, collapse = ', ')))
    } else {
      # outside shiny:
      stop(    paste0('No FIPS column found. Please use one of the following names: ', paste0(fips_alias, collapse = ', ')))
    }
  }
  return(fips_vec)
}
####################################################### # 
