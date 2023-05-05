library(devtools)

devtools::install_github('USEPA/EJAMejscreenapi',      force=TRUE)

devtools::install_github('USEPA/EJAMbatch.summarizer', force=TRUE)

devtools::install_github('USEPA/EJAMblockdata',        force=TRUE)

devtools::install_github('USEPA/EJAMejscreendata',     force=TRUE)

devtools::install_github('USEPA/EJAMfrsdata',          force=TRUE)

devtools::install_github('USEPA/EJAM',                 force=TRUE)


# Warning: file 'frs.rda' has magic number 'versi'
# Use of save versions prior to 2 is deprecated
# Error in load(zfile, envir = tmp_env) : 
#   bad restore file magic number (file may be corrupted) -- no data loaded
# ERROR: lazydata failed for package 'EJAMfrsdata'



# try to recreate it/resave it
library(data.table)
remove.packages("EJAMfrsdata")
frs <- NULL # to stop warning
load(file.path(Sys.getenv("R_USER"), "EJAMfrsdata/data/frs.rda"))
x <- copy(frs)
rm(frs)
file.remove(file.path(Sys.getenv("R_USER"), "EJAMfrsdata/data/frs.rda"))
frs <- copy(x); rm(x)

# try using version 3 format, though that requires newer R versions
#usethis::use_data(frs, version = 3, overwrite = TRUE)
#or maybe
# save(frs, file = file.path(Sys.getenv("R_USER"), "EJAMfrsdata/data/frs.rda"))
# or maybe
#frs <- frs_update_datasets() # which does use_data()
# or maybe
# save(frs, file = "./EJAMfrsdata/data/frs.rda", version = 3)

# NONE OF THOSE WORKED SO I FINALLY MOVED frs.rda and frs.R to the EJAM package,
# and updated all packages where they said EJAMfrsdata::frs to just say frs 
# and will rebuild EJAM and EJAMfrsdata pacakges.

rm(list=ls())

# then push to github repo 

# ** YOU NEED TO CLOSE the project/not be in the EJAMfrsdata project folder 
#  before trying to install that pkg from repo

# Finally try again to install from there (after PAT personal access token already set up)
devtools::install_github("USEPA/EJAMfrsdata") # and said "None" when asked which pkgs to update.

# again got these error messages:
# 
# * installing *source* package 'EJAMfrsdata' ...
# ** using staged installation
# ** R
# ** data
# *** moving datasets to lazyload DB
# Warning: file 'frs.rda' has magic number 'versi'
# Use of save versions prior to 2 is deprecated
# Error in load(zfile, envir = tmp_env) : 
#   bad restore file magic number (file may be corrupted) -- no data loaded
# ERROR: lazydata failed for package 'EJAMfrsdata'
# * removing 'C:/Users/...../R/myinstalled/EJAMfrsdata'
# Warning message:
#   In i.p(...) :
#   installation of package ‘C:/Users/....../AppData/Local/Temp/RtmpQ...B/file5...51c2f7f4af2/EJAMfrsdata_2.1.0.tar.gz’ had non-zero exit status



# Warning: file 'bgid2fips.rda' has magic number 'versi'
# Use of save versions prior to 2 is deprecated
# Error in load(zfile, envir = tmp_env) : 
#   bad restore file magic number (file may be corrupted) -- no data loaded
# ERROR: lazydata failed for package 'EJAMblockdata'
# * removing 'C:/Users/mcorrale/R/myinstalled/EJAMblockdata'
 
