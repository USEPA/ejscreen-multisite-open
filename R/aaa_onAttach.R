#' .onAttach - Do slow initialization steps - Download data, load key data into RAM, create index to all US blocks
#' Note this duplicates some code in global.R, and see source code here to adjust settings.
#' @details Does this even happen if connect server runs app as a regular shiny app without loading all?
#'   In what order? see app.R too ***
#' @param libname na
#' @param pkgname na
#' 
.onAttach <- function(libname, pkgname) {
  
  # These instead could be set in the golem-config.yml file
  asap_aws   <- TRUE  # download large datasets now?           Set to FALSE while Testing/Building often
  asap_index <- TRUE  # build index those now?                 Set to FALSE while Testing/Building often 
  asap_bg    <- FALSE  # load now vs lazyload blockgroup data?  Set to FALSE while Testing/Building often
  
  # startup message shown at library(EJAM) or when reinstalling from source ####
  packageStartupMessage("Now running .onAttach(), as part of attaching the EJAM package.")
  # packageStartupMessage('The leaflet package is what requires the sp package\n')
  
  packageStartupMessage(
    
    # cat(
    "\n
      Developers may want to modify the .onAttach() or even .onLoad() function,
      to control timing of the slow steps needed to load or create EJAM data, 
      to have the delays occur at preferred times automatically
      (or only manually when a coder initiates them interactively or via their code).
      
      These are the slow steps as EJAM starts:
      
      1- Download the large datasets stored online (quaddata, blockpoints, frs, etc.) 
         EJAM uses dataload_from_pins() for this. Also see dataload_from_local() or dataload_from_aws()
         quaddata is >150 MB on disk and >200 MB RAM, while all others are smaller on disk.
         blockid2fips is roughly 600 MB in RAM because it stores 8 million block FIPS as text.
      
      2- Build the national index of all block point locations
         EJAM function indexblocks() does this, using quaddata - see ?indexblocks
      
      3- Load into memory some datasets installed with EJAM (blockgroupstats, usastats, etc.)
         EJAM function dataload_from_package() can do this - see ?dataload_from_package and ?datapack()  
         Otherwise these are only lazyloaded at the moment they are needed, making a user wait.
         blockgroupstats (>60 MB on disk, >200 MB in RAM) and usastats, statestats are essential.
         frs-related tables are huge and not always required - needed to look up regulated sites by ID. 

      These are the times at which you may want them to happen:
      
      - when the EJAM package is loaded and/or attached 
        i.e., every time the source package is rebuilt it is loaded; but it is attached less often,
        as when a coder uses require( ) in RStudio or script
      
      - when the shiny app launches and runs the global.R script 
         i.e., only once a new user opens the app and their session starts,
         and when a coder uses run_app(), either after library( ), or by using EJAM function run_app() 
      
      - once the app or coder actually needs a given dataset that is available for lazyLoad, which 
        works only for data in EJAM/data/ like frs.rda, frs_by_programid.rda, frs_by_sic.rda, etc.
        See utils::data( package = 'EJAM' )
      
      - only if manually obtained by coder via functions noted above.
      \n\n"
    # )
  )
  
  # download BLOCK (not blockgroup) data, etc, from EPA AWS Data Commons ####
  
  if (asap_aws) {
    
    # Note this duplicates code in global.R too
    
    if (length(try(find.package("EJAM", quiet = T))) == 1) { # if it has been installed. but that function has to have already been added to package namespace once 
      
      dataload_from_pins(varnames = c("blockpoints", "blockwts", "quaddata"), 
                         folder_local_source = app_sys('data')) # use default local folder when trying dataload_from_local()
      # EJAM function ... but does it have to say EJAM :: here? trying to avoid having packrat see that and presume EJAM pkg must be installed for app to work. ***
    }
    
    #################### # 
    # see ?dataload_from_aws  
    # loads quaddata needed to make localtree index, etc.   
    # 
    #   blockid2fips is used only in state_from_blocktable() and state_from_blockid(), which are not necessarily used, 
    #   so maybe should not load this unless/until needed?
    #     but, then we would need to be sure to track when it is used and load from aws on the fly
    
    # BUCKET_CONTENTS <- data.table::rbindlist(aws.s3::get_bucket(bucket = mybucket), fill = TRUE)
    # baseurl <- "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/"
    # urls <- paste0(baseurl, fnames)
    ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/quaddata.rda"
    ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/bgid2fips.rda"
    ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/blockpoints.rda"
    ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/blockwts.rda"
    ## "https://dmap-data-commons-oa.s3.amazonaws.com/EJAM/blockid2fips.rda"
    ######################### # 
    
  }
  
  # create index of all US block points, to enable fast queries ####
  
  if (asap_index) {
    
    # this duplicates code from  global.R 
    
    if (length(try(find.package("EJAM", quiet = T))) == 1) { # if it has been installed. but that function has to have already been added to package namespace once 
      
      indexblocks()   # EJAM function works only AFTER shiny does load all/source .R files or package attached
      
    }
    
    # This cannot be saved with the pkg to be installed as data, because of what this createTree function creates (memory pointers?).
    # NOT TESTED in context of an app published on RStudio Server
    
    # cat("Building index of Census Blocks (localtree)...\n")
    # if (!exists("localtree")) {
    #   if (!exists("quaddata")) {stop("requires quaddata to be loaded - cannot build localtree without it.")}
    #   # It was obtained from AWS at one time, via  dataload_from_aws()
    #   
    #   # This assign() below is the same as the function called  indexblocks() 
    #   # indexblocks() # this creates  localtree object
    #   assign(
    #     "localtree",
    #     SearchTrees::createTree(quaddata, treeType = "quad", dataType = "point"),
    #     envir = globalenv()
    #     # need to test, but seems to work.
    #     # But takes a couple seconds at every reload of pkg.
    #   )
    #   cat("  Done building index.\n")
    # }
  }
  
  # load blockgroupstats etc. from package ####
  
  if (asap_bg) {
    
    # this duplicates code from  global.R 
    
    if (length(try(find.package("EJAM", quiet = T))) == 1) { # The first time you try to install the package, it will not have access to EJAM :: etc. !
      
      dataload_from_package() # EJAM function works only AFTER shiny does load all/source .R files or package attached
      
    }
    
    # load to memory  BLOCKGROUP (not block) data (EJScreen indicators), etc. from the installed package
    # see ?dataload_from_package()
    # This loads some key data, while others get lazy loaded if/when needed.
    # data(list=c("blockgroupstats", "usastats", "statestats"), package="EJAM") # would work after package is installed
    # data(list=c("frs", "frs_by_programid ", "frs_by_naics"),  package="EJAM") # would be to preload some very large ones not always needed. 
    
  } 
}
