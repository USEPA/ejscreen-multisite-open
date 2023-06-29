#' Validate FRS Registry ID list uploads
#' @description Check for proper FRS facility id in uploaded data
#'
#' @param frs_upload upload frs registry IDs table converted to data frame
#' @return boolean value (valid or not valid)
#' @export
#'
frs_is_valid <- function(frs_upload) {
  #checks for registry id in second column of FRS uploaded dataset
  if(any(colnames(frs_upload) %in%  "REGISTRY_ID" )) {  # FRS uses this colname
    return(TRUE)
  } else {
    if ("regid" %in% colnames(frs_upload)) {
      colnames(frs_upload) <- gsub("regid", "REGISTRY_ID", colnames(frs_upload)) # ECHO uses this colname
      if(nrow(frs_from_regid(frs_upload$REGISTRY_ID)) != 0){
        return(TRUE)
      }else{
        return(FALSE)
      }
    } else if ("siteid" %in% colnames(frs_upload)){
        colnames(frs_upload) <- gsub("siteid", "REGISTRY_ID", colnames(frs_upload))
        warning("assuming siteid column has REGISTRY_ID values")
        if(nrow(frs_from_regid(frs_upload$REGISTRY_ID)) != 0){
          return(TRUE)
        }else{
          return(FALSE)
        }
       # return(TRUE)
      } else if ("RegistryID" %in% colnames(frs_upload)) {
          colnames(frs_upload) <- gsub("RegistryID", "REGISTRY_ID", colnames(frs_upload))
          warning("assuming siteid column has REGISTRY_ID values")
          if(nrow(frs_from_regid(frs_upload$REGISTRY_ID)) != 0){
            return(TRUE)
          }else{
            return(FALSE)
          }
      }
    return(FALSE)
    } 
  }
