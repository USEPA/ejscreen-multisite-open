#' naics_validation - Validate NAIC uploads
#' @description Validates and prepares echo uploads
#'
#' @param NAICS upload validate missing and/or improper inputs
#' @return boolean value (valid or not valid)
#' @export
naics_validation <- function(NAICS_enter,NAIC_select){
  
  if (nchar(NAICS_enter) > 0 | length(NAIC_select) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}
