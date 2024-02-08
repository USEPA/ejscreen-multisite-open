
#' Validate NAICS uploads
#' 
#' @description Validates and prepares echo uploads
#'
#' @param naics_enter vector of naics
#' @param naics_select single value
#' @return boolean value 
#' 
#' @keywords internal   
#' @export
#' 
naics_validation <- function(naics_enter, naics_select) {
  
  if (all(nchar(naics_enter) > 0) | length(naics_select) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
