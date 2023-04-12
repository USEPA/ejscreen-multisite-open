#' Validate FRS uploads
#' @description Check for proper FRS facility id in uploaded data
#'
#' @param frs_upload upload frs converted to data frame
#' @return boolean value (valid or not valid)
#' @export
#'
frs_is_valid <- function(frs_upload) {
  #checks for registry id in second column of FRS uploaded dataset
  if(any(colnames(frs_upload) %in% "REGISTRY_ID")){
   # for (i in frs_upload$REGISTRY_ID) {
      #if(nchar(i) != 12){ #is every registry ID the correct length
        #return(FALSE)
      #}
      
    #}
    return(TRUE)
  } else {
    return(FALSE)
  }
}
