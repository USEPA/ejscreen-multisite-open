# script to read lookup tables from csv, add metadata, save as datasets in R package

# Below is obsolete old function to read percentile lookup tables, ####
#  and process and   save the percentile lookup tables 

# for the 2023 version 2.2 approach, see update_to_ejscreenv2.2.R scripts








# Below is obsolete old function to read percentile lookup tables, ####
#  and process and   save the percentile lookup tables 
if ("now" == 2022) {
# usastats2.2 <- read.csv('~/../EJ 2021/EJScreen 2023/ejscreen2023_07/statestats2023.csv', stringsAsFactors = F)


read_to_save_as_data <- function(myname, mydir=getwd(), justread=TRUE, rename=FALSE, 
                                 mynewname=NULL, attributelist_to_add) {
  x <- as.data.frame(readr::read_csv(paste0(file.path(mydir, 'inst', myname), '.csv')))
  if (!is.null(mynewname)) {myname <- mynewname}
  if (rename) {
    names(x) <- EJAMejscreenapi::fixcolnames(names(x), towhichnames = "friendly", fromwhichnames = "original", mapping_for_names = EJAMejscreenapi::map_headernames)
    # names(x) <- change.fieldnames.ejscreen.csv(names(x)) # requires the ejscreen package, so should change to using EJAMejscreenapi::map_headernames or fixnames() etc. 
    cat('renamed columns \n')
  }
  
  if (!missing(attributelist_to_add)) {warning("ignoring attributelist_to_add - please use EJAM::metadata_add() ")}
  #attributes(x) <- c(attributes(x), attributelist_to_add)
  # BUT THAT APPENDS EVEN IF WANT TO REPLACE WITH UPDATED INFO
  if (!justread) {
    assign(myname, x)  # lookupUSA <- x
    save(list = myname, file = paste0(file.path(mydir, 'data', myname), '.rda'))
    # usethis::use_data(dynGet(myname)) # this does not work, actually, since get or dynGet dont work for this
  } else {
    cat('\n If you have read it as such, you can enter this: \n\n usethis::use_data(', myname, ')\n')
    invisible(x)
  }
} 

# specify folder ####

  mydir <- '....../ejscreen'
 

stop('here')


   
######################## #
# add metadata to pctile lookups ####
# LOOKUP TABLES OF PERCENTILES
  # for 2023 v2.2, 
  # x <- EJAM::metadata_add( x )
  # ejscreen_version     "2.2"       
  # acs_version          "2017-2021" 
  # census_version       2020        
  # ejscreen_releasedate "2023-06-23"
  # acs_releasedate      "2022-12-08"
  # ejscreen_pkg_data    NA      
  
# "2022 version" (EJScreen version 2.1, bg22, ACS2020, after 7/2022)
# metadata <- list(
#   census_version = 2020,
#   acs_version = '2016-2020',
#   acs_releasedate = '3/17/2022',
#   ejscreen_version = '2.1',
#   ejscreen_releasedate = 'October 2022',
#   ejscreen_pkg_data = 'bg22'
# )
   
 
    
USA_2022_LOOKUP <- NULL; States_2022_LOOKUP <- NULL
USA_2022_LOOKUP <-    read_to_save_as_data(   'USA_2022_LOOKUP', mydir=mydir, rename = FALSE, attributelist_to_add = metadata)
States_2022_LOOKUP <- read_to_save_as_data('States_2022_LOOKUP', mydir=mydir, rename = FALSE, attributelist_to_add = metadata)
# save these with original column names as used on FTP site and in GDB
usethis::use_data(   USA_2022_LOOKUP, overwrite = TRUE)
usethis::use_data(States_2022_LOOKUP, overwrite = TRUE)

# now save versions with friendly names (but note file name might be confusing if two version kept with just renamed columns)
lookupUSA <- USA_2022_LOOKUP
lookupStates <- States_2022_LOOKUP
stop('need ejscreen pkg for change.fieldnames.ejscreen.csv or use EJAM equivalent')
names(lookupStates) <-  change.fieldnames.ejscreen.csv(names(lookupStates))
names(lookupUSA) <- change.fieldnames.ejscreen.csv(names(lookupUSA))
usethis::use_data(   lookupUSA, overwrite = TRUE)
usethis::use_data(lookupStates, overwrite = TRUE)

# delete older versions

file.remove(file.path(mydir, 'data', 'lookupUSA2022.rdata'))
file.remove(file.path(mydir, 'data', 'lookupStates2022.rdata'))

file.remove(file.path(mydir, 'data', 'lookupUSA2021.rdata'))
file.remove(file.path(mydir, 'data', 'lookupStates2021.rdata'))

file.remove(file.path(mydir, 'data', 'lookupUSA20.rdata'))
file.remove(file.path(mydir, 'data', 'lookupStates20.rdata'))
file.remove(file.path(mydir, 'data', 'lookupRegions20.rdata'))

# optional:

# "2021 version" (EJScreen version 2.0, bg21, ACS2019, through 6/2022)
metadata <- list(releasedate = 'early 2022', ejscreen_version = '2.0', ACS_version = '2015-2019')
USA_2021_LOOKUP =    read_to_save_as_data(   'USA_2021_LOOKUP', mydir=mydir, rename = FALSE, attributelist_to_add = metadata)
States_2021_LOOKUP = read_to_save_as_data('States_2021_LOOKUP', mydir=mydir, rename = FALSE, attributelist_to_add = metadata)
usethis::use_data(   USA_2021_LOOKUP, overwrite = TRUE)
usethis::use_data(States_2021_LOOKUP, overwrite = TRUE)

# the name had been this:
# lookupStates2021

# older notes:
# lookupUSA2022 <- as.data.frame(readr::read_csv(paste0(mydir, 'ejscreen/inst/USA_2022_LOOKUP.csv')))
# # save(lookupUSA2022, file = paste0(mydir, 'ejscreen/data/lookupUSA2022.rdata'))
# # or
# usethis::use_data(lookupUSA2022)
#
# lookupStates2022 <- as.data.frame(readr::read_csv(paste0(mydir, 'ejscreen/inst/States_2022_LOOKUP.csv')))
# save(lookupStates2022, file = paste0(mydir, 'ejscreen/data/lookupStates2022.rdata'))
# # or
# usethis::use_data(lookupStates2022)
  }
  
