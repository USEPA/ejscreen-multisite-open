#' utility to load datasets from AWS 
#'
#' @param fnames vector of the names of the rda files
#' @param envir default is parent.frame()
#' @param mybucket where in AWS
#' @param mybucketfolder where in AWS
#'
#' @return nothing - just loads data into environment
#' @export
#'
data_load_from_aws <- function(fnames=c('lookup_states.rda', 'bgid2fips.rda', 'blockid2fips.rda', 'blockwts.rda', 'blockpoints.rda', 'quaddata.rda'), 
                               envir=parent.frame(),
                               mybucket =  'dmap-data-commons-oa',
                               mybucketfolder = "EJAM"
) {
  # get block data etc if missing, from EPA AWS Data Commons. ####
  
  # fnames <- c(
  #   'lookup_states.rda', 
  #   'bgid2fips.rda', 
  #   'blockid2fips.rda', 
  #   'blockwts.rda', 
  #   'blockpoints.rda', 
  #   'quaddata.rda'       # this is needed to build localtree at the start of a session
  # )
  
  pathnames <- paste0(mybucketfolder, '/', fnames) # 
  varnames <- gsub("\\.rda", "", fnames)
  mybucket <- 'dmap-data-commons-oa'
  # if not already in memory/ global envt, get from AWS 
  for (i in 1:length(fnames)) {
    if (!exists(varnames[i])) {
      cat('loading', varnames[i], 'from', pathnames[i], '\n')
      
      # change to use arrow::open.... much faster
      
      aws.s3::s3load(object = pathnames[i], bucket = mybucket, envir = envir)
    }
  }
  # BUCKET_CONTENTS <- data.table::rbindlist(aws.s3::get_bucket(bucket = mybucket), fill = TRUE)
  # baseurl <- "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/"
  # urls <- paste0(baseurl, fnames)
  ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/quaddata.rda"
  ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/bgid2fips.rda"
  ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/blockpoints.rda"
  ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/lookup_states.rda"
  ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/blockwts.rda"
  ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/blockid2fips.rda"
  ################################################################## # 
  
    
  #   blockid2fips is used only in state_from_blocktable() and state_from_blockid(), which are not necessarily used, 
  #   so maybe should not load this unless/until needed?
  # 
}


#' utility to load a couple of datasets using data() if lazy loading not set up?
#' @details These two datasets, blockgroupstats and frs, are some of the ones stored in the package as data()
#' @return Nothing
#' @export
#'
data_load_from_package <- function() {
  
  # Note:
  #
  #   Use of data within a function without an envir argument has the almost always undesirable side-effect of putting an object in the user's workspace (and indeed, of replacing any object of that name already there). It would almost always be better to put the object in the current evaluation environment by data(..., envir = environment()). However, two alternatives are usually preferable, both described in the ‘Writing R Extensions’ manual.
  # 
  # For sets of data, set up a package to use lazy-loading of data.
  # 
  # For objects which are system data, for example lookup tables used in calculations within the function, use a file ‘R/sysdata.rda’ in the package sources or create the objects by R code at package installation time.
  # 
  # A sometimes important distinction is that the second approach places objects in the namespace but the first does not. So if it is important that the function sees mytable as an object from the package, it is system data and the second approach should be used
  
  data("blockgroupstats", package="EJAM") # one version was a  54 MB rda file
  data("frs", package="EJAM") # was about 80 MB rda file
   
  # f.rda <- list.files('./data', pattern = '\\.rda$' )
  # fname = gsub(".rda","",f.rda)
  
}

# data_indexblocks <- EJAM::indexblocks # alias cannot be created like this unless that function is exported by pkg and pkg is already installed
