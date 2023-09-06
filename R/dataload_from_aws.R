#' utility to load datasets from AWS 
#' @details See source code for details. Requires credentials.
#'   
#'   Use dataload_from_aws(justchecking=TRUE), 
#'   
#'   or datapack("EJAM") to get info, 
#'   
#'   or tables(), 
#'   
#'   or object.size(quaddata)
#'   
#'   List 9/2023 was:
#'   
#'   - blockid2fips (20 MB on disk, approx 600 MB RAM)
#'   
#'   - quaddata (168 MB on disk, 218 MB RAM)
#'   
#'   - blockpoints (86 MB on disk, 156 MB RAM)
#'   
#'   - blockwts (31 MB on disk, 125 MB RAM)
#'   
#'   - bgid2fips (18 MB RAM)
#'   
#'            quaddata is >150 MB on disk and >200 MB RAM, while all others are smaller on disk.
#'            
#'            blockid2fips is roughly 600 MB in RAM because it stores 8 million block FIPS as text.

#' @param fnames vector of the names of the rda files
#' @param envir default is parent.frame()
#' @param mybucket where in AWS
#' @param mybucketfolder where in AWS
#' @param justchecking set to TRUE to get object size (and confirm file is accessible/exists)
#' @seealso [datapack()] [dataload_from_aws()] [dataload_from_package()] [indexblocks()] [.onAttach()] 
#' @return nothing - just loads data into environment (unless justchecking=T)
#' 
#' @export
#'
dataload_from_aws <- function(fnames=c('bgid2fips.rda', 'blockid2fips.rda', 'blockwts.rda', 'blockpoints.rda', 'quaddata.rda'), 
                              envir=globalenv(),  # should it be parent or global or package EJAM envt ??
                              mybucket =  'dmap-data-commons-oa',
                              mybucketfolder = "EJAM",
                              justchecking = FALSE) {
  
  ### note: 
  # could switch to another method, or switch to arrow which should be faster.
  # need to read documentation of aws.s3 pkg to get this syntax correct, though...
  # 
  #     s3write_using(bgid2fips, bucket = mybucket, object = "bgid2fips.arrow", FUN = arrow::write_feather)
  # x <- s3read_using(bgid2fips, bucket = mybucket, object = "bgid2fips.arrow", FUN = arrow::read_feather)
  # 
  # or   
  # bp =  paste0( mybucket, "/", mybucketfolder , "/") 
  # b = paste0("s2://", bp) # put_bucket() # ??  not sure about s2, s3 approach here...???
  # objectpath = paste0( b, "bgid2fips")
  #  s3save(bgid2fips, bucket = b, object = "bgid2fips")
  #  x = get_object(objectpath, show_progress = TRUE) # returns a complicated format object
  #      load(rawConnection(x[[1]]))
  
  
  pathnames <- paste0(mybucketfolder, '/', fnames) # 
  varnames <- gsub("\\.rda", "", fnames)
  mybucket <- 'dmap-data-commons-oa'
  
  if (justchecking) {
    for (i in 1:length(fnames)) {
      cat(
        fnames[i], " = ",
        aws.s3::object_size(object = pathnames[i], bucket = mybucket) / 10^6, "MB\n"
      )
    }
  } else {
    
    # if not already in memory/ global envt, get from AWS 
    for (i in 1:length(fnames)) {
      if (!exists(varnames[i])) {
        cat('loading', varnames[i], 'from', pathnames[i], '\n')
        
        # change to use arrow::open.... much faster
        
        aws.s3::s3load(object = pathnames[i], bucket = mybucket, envir = envir)
      }
    }
  }
  
  # BUCKET_CONTENTS <- data.table::rbindlist(aws.s3::get_bucket(bucket = mybucket), fill = TRUE)
  # baseurl <- "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/"
  # urls <- paste0(baseurl, fnames)
  ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/quaddata.rda"
  ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/bgid2fips.rda"
  ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/blockpoints.rda"
  ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/blockwts.rda"
  ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/blockid2fips.rda"
  #  #  obsolete: # "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/lookup_states.rda"  no longer on aws, but replaced with stateinfo2.rda in package data folder
  ################################################################## # 
  
  #   blockid2fips is HUGE and used only in state_from_blocktable() and state_from_blockid(), which are not necessarily used, 
  #   so maybe should not load this unless/until needed?
  # 
}

