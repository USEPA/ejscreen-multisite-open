#' Creates index to all US blocks (internal point lat lon) at package load
#' Note this duplicates some code in global.R
#' @param libname na
#' @param pkgname na
#'
.onLoad <- function(libname, pkgname) {
  ok_to_do_slow_load_early <- FALSE # MAYBE WANT FALSE WHILE TESTING/Building often
  if (  ok_to_do_slow_load_early) {
    ### Maybe load some datasets now that are needed, 
    ### instead of when user does a query that triggers slow lazyloading?
    
    # get block group data (EJScreen data)
    # but note this only loads a couple of the many data objects needed.
    # EJAM::data_load_from_package() # requires the package be installed already
    data("blockgroupstats", package="EJAM") # one version was a  54 MB rda file
    data("frs", package="EJAM") # was about 80 MB rda file
    # Other .rda files in package data folder are <50MB each, mostly <10MB each.
    # To manually reload block group percentiles and means (EJScreen data):
    #  data("statestats", package="EJAM")
    #  data("usastats",   package="EJAM")
    # statestats.rda is about 1.3 MB, usastats.rda is about 28 KB
    
    ########################### #
    
    # this duplicates code in global.R
    
    # get block (and some other) data from EPA AWS Data Commons ####
    # but this means the package cannot correctly load until this function is already in installed pkg
    EJAM::data_load_from_aws() # loads quaddata needed to make localtree index, etc.
    
    # cat("Loading data.tables of Census Blocks...\n")
    # fnames <- c('lookup_states.rda', 'bgid2fips.rda', 'blockid2fips.rda', 'blockwts.rda', 'blockpoints.rda', 'quaddata.rda')
    # pathnames <- paste0('EJAM/', fnames)
    # varnames <- gsub("\\.rda", "", fnames)
    # mybucket <- 'dmap-data-commons-oa'
    # for (i in 1:length(fnames)) {
    #   cat('loading', varnames[i], 'from', pathnames[i], '\n')
    #   aws.s3::s3load(object = pathnames[i], bucket = mybucket)
    # }
    
    
    #   blockid2fips is used only in state_from_blocktable() and state_from_blockid(), which are not necessarily used, 
    #   so maybe should not load this unless/until needed?
    #
    # BUCKET_CONTENTS <- data.table::rbindlist(aws.s3::get_bucket(bucket = mybucket), fill = TRUE)
    # baseurl <- "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/"
    # urls <- paste0(baseurl, fnames)
    ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/quaddata.rda"
    ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/bgid2fips.rda"
    ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/blockpoints.rda"
    ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/lookup_states.rda"
    ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/blockwts.rda"
    ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/blockid2fips.rda"
    
    
  } else {
    
    cat("You can use data(blockgroupstats, package='EJAM') \n")
    cat("You can use data_load_from_aws() to get the datasets (blocks, etc.) again from the AWS bucket.")
    cat("You can use EJAM::indexblocks() to rebuild the index of block point locations.\n")
    cat("\n")
    cat("Developers may want to modify the .onLoad() function, \n")
    cat("to have it immediately load less or more data \n")
    cat("(which takes several seconds each time you rebuild or just load the package)\n") 
    cat("instead of that happening only when you first run an analysis,\n") 
    cat("as with these large datasets (data.tables of Census blocks or block groups):\n ")
    cat("\n- quaddata (168 MB), \n- blockgroupstats  (54 MB), \n- blockpoints (86 MB), \n- blockwts (31 MB), \n- blockid2fips (20 MB)\n ")
    cat("\n")
  }
  ok_to_do_slow_indexing_early <- FALSE # MAYBE WANT FALSE WHILE TESTING/Building often
  if (ok_to_do_slow_indexing_early) {
    
    # this duplicates code in global.R and in indexblocks()
    
    # This should create the index of all US block points to enable fast queries 
    # This cannot be done during package build and saved, because of what this createTree function creates.
    # NOT TESTED in context of an app published on RStudio Server
    cat("Building index of Census Blocks (localtree)...\n")
    if (!exists("localtree")) {
      if (!exists("quaddata")) {stop("requires quaddata to be loaded - cannot build localtree without it.")}
      # It is obtained from AWS currently, via  data_load_from_aws()
      
      # This assign() below is the same as the function called  indexblocks() 
      # indexblocks() # this creates  localtree object
      assign(
        "localtree",
        SearchTrees::createTree(quaddata, treeType = "quad", dataType = "point"),
        envir = globalenv()
        # need to test, but seems to work.
        # But takes a couple seconds at every reload of pkg.
      )
      cat("  Done building index.\n")
    }
  }
  
  
  
}


