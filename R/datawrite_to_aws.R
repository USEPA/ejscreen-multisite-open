#' datawrite_to_aws -  NOT YET WORKING - AccessDenied 
#' Write object(s) like a dataset to DMAP Data Commons, formatted as .arrow or .rda 
#' @details 
#'   mybucket <-  'dmap-data-commons-oa' #
#'   
#'   bucket_contents <- data.table::rbindlist(
#'   
#'     get_bucket(bucket = mybucket, prefix = "EJAM"),
#'     
#'     fill = TRUE
#'       
#'     )
#'       
#'  bucket_contents
#'       
#' @param varnames vector of object names to upload
#' @param ext file .extension appropriate to the format and fun, like ".rda" or ".arrow"
#' @param fun function to use, but as a character string, like "arrow::write_ipc_file"
#'   but fun is ignored if ext=".rda" since it then just uses s3save()
#' @param mybucket do not need to change
#' @param mybucketfolder do not need to change
#' @param justchecking set this to FALSE to actually upload instead of 
#'   just viewing in console the commands to be used, to test/check this
#'
#' @return the paths of the objects on server
#' @export
#'
datawrite_to_aws <- function(varnames= c('bgid2fips',   'blockid2fips', 'blockpoints', 'blockwts' , 'quaddata' ), 
                             ext=c(".arrow", ".rda")[2],
                             fun=c("arrow::write_ipc_file", "save")[2], # not sure save would work here. 
                             mybucket =  'dmap-data-commons-oa',
                             mybucketfolder = "EJAM",
                             justchecking = TRUE) {
  
  ## Get bucket contents if you want to explore the bucket ----
  # mybucket <-  'dmap-data-commons-oa' # 
  # bucket_contents <- data.table::rbindlist(
  #   get_bucket(bucket = mybucket, prefix = "EJAM"), 
  #   fill = TRUE
  # )
  # bucket_contents
  
  if (!is.character(fun)) {stop('must specify function in fun parameter as a quoted character string')}
  if (length(ext) > 1) {stop('must specify only one file extension for all the files')}
  if (ext == 'arrow') ext <- ".arrow"
  if (ext == 'rda')   ext <- '.rda'
  if ((ext == '.arrow') & missing(fun)) {fun <- "arrow::write_ipc_file"} 
  
  fnames <- paste0(varnames, ext)
  objectnames <- paste0(mybucketfolder, '/', fnames)
  
  for (i in 1:length(varnames)) {
    
    if (ext == ".rda") { 
      text_to_do <- paste0("aws.s3::s3save(", varnames[i], 
                           ", object = '", objectnames[i],"', ",
                           "bucket = '",mybucket,"', opts = list(show_progress = TRUE))")
      ## e.g.,  # access denied: 
      # aws.s3::s3save(bgid2fips, object = 'EJAM/bgid2fips.rda',  bucket = 'dmap-data-commons-oa', opts = list(show_progress = TRUE))
      
    } else {
      
      # Use s3write_using to upload something like .arrow files to AWS DMAP Data Commons ####
      text_to_do <- paste0("aws.s3::s3write_using(", varnames[i], ", ",
                           "object = '", objectnames[i],"', ",
                           "FUN = ", fun, ", ",
                           "bucket = '",mybucket,"', opts = list(show_progress = TRUE))")
    }
    
    if (justchecking) {
      print(text_to_do)  # TO SEE COMMAND / CHECK THIS IS WORKING
    } else {
      eval(parse(text = text_to_do)) # executes the command
    }
    
  }
  # end loop
  
  baseurl <- "https://dmap-data-commons-oa.s3.amazonaws.com/"
  paths <- paste0(baseurl, objectnames)
  if (justchecking) {cat("\nTo upload files, set parameter   justchecking=FALSE  \n\n")}
  
  return(paths)
}
##############################################################
