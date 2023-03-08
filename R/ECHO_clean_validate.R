#' Validate FRS uploads
#' @description Validates and prepares echo uploads
#'
#' @param frs_upload upload frs converted to data frame
#' @return boolean value (valid or not valid)
#' @export
#'
ECHO_clean_validate <- function(echo_upload) {
  test <- read.csv('/media/gdrive/malekP/VCS/VCS/temp_EJAM/ejam_internal/data/Data_Download_ECHO_2_8_2023_63e40a2316631.csv')
  #checks for registry id in second column of FRS uploaded dataset
  if(colnames(frs_upload)[2] == "REGISTRY_ID"){
    for (i in frs_upload$REGISTRY_ID) {
      if(nchar(i) != 12){ #is every registry ID the correct length
        return(FALSE)
      }
      
    }
  }else{
    return(FALSE)
  }
}
