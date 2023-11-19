#' naics_validation - Validate NAICS uploads
#' @description Validates and prepares echo uploads
#'
#' @param naics_enter vector of naics
#' @param naics_select 
#' @return boolean value (valid or not valid) - TRUE if 
#'   length of at least one of the two input vectors is > 0
#' @export
naics_validation <- function(naics_enter, naics_select){
  
  if (nchar(naics_enter) > 0 | length(naics_select) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}
