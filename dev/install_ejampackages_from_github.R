# ?usethis::use_data_table()
################################################# #

rm(list=ls())



################################################# #

# make sure PAT is set up right. 
library(credentials)
credentials::set_github_pat()

library(devtools)
# make any changes to pkgs, devtools::document(), push changes to github, then...

devtools::install_github('USEPA/EJAMejscreenapi',      force=TRUE, upgrade=F) # works fine.
devtools::install_github('USEPA/EJAMbatch.summarizer', force=TRUE, upgrade=F) # works fine.


devtools::install_github('USEPA/EJAM',                 force=TRUE, upgrade=F) 
# works/worked fine, now includes frs.rda, etc. but needs EJAMblockdata

devtools::install_github('USEPA/EJAMblockdata',        force=TRUE, upgrade=F) 
# failed

# # errors most recently, 
# > library(credentials)
# Found git version 2.37.3.windows.1
# Supported HTTPS credential helpers: cache, store, helper-selector
# , manager-core
# Found OpenSSH_for_Windows_8.1p1, LibreSSL 3.0.2
# No key found. Use ssh_keygen() to generate one!
# > credentials::set_github_pat()
# If prompted for GitHub credentials, enter your PAT in the password field
# Using GITHUB_PAT from ejanalysis (credential helper: manager-core)
# > library(devtools)
# Loading required package: usethis
# > # make any changes to pkgs, devtools::document(), push changes to github, then...
# > devtools::install_github('USEPA/EJAM',                 force=TRUE, upgrade=F) 
# #works/worked fine, now includes frs.rda, etc. but needs EJAMblockdata
# Using github PAT from envvar GITHUB_PAT
# Downloading GitHub repo USEPA/EJAM@HEAD
# Error in utils::download.file(url, path, method = method, quiet = quiet,  : 
#       download from 'https://api.github.com/repos/USEPA/EJAM/tarball/HEAD' failed
#    > devtools::install_github('USEPA/EJAMblockdata',        force=TRUE, upgrade=F)  # failed
#       Using github PAT from envvar GITHUB_PAT
#           Downloading GitHub repo USEPA/EJAMblockdata@HEAD
#            Error in utils::download.file(url, path, method = method, quiet = quiet,  : 
#           download from 'https://api.github.com/repos/USEPA/EJAMblockdata/tarball/HEAD' failed
#                                                               

# earlier problems, when lazy loading data:

# Warning: file 'bgid2fips.rda' has magic number 'versi'
# Use of save versions prior to 2 is deprecated
# Error in load(zfile, envir = tmp_env) : 
#   bad restore file magic number (file may be corrupted) -- no data loaded
# ERROR: lazydata failed for package  

# and got rid of dependency on USEPA/EJAMfrsdata

##################################################################### #

# try to   resave the files
 

fnames = list.files("./data")
for (fname in fnames) {
   load(paste0("./data/", fname))
}
# "bgid2fips.rda" "blockid2fips.rda" "blockpoints.rda"   "blockwts.rda" "lookup_states.rda" "quaddata.rda"    
# bgid2fips=0
# blockid2fips=0
# blockpoints=0 
# blockwts=0
# lookup_states=0
# quaddata=0

# could try using newer version, but other files work OK with version 2
usethis::use_data(bgid2fips, version = 3, overwrite = TRUE, compress = "gzip")
usethis::use_data(blockid2fips, version = 3, overwrite = TRUE, compress = "gzip")
usethis::use_data(blockpoints, version = 3, overwrite = TRUE, compress = "gzip")
usethis::use_data(blockwts, version = 3, overwrite = TRUE, compress = "gzip")
usethis::use_data(lookup_states, version = 3, overwrite = TRUE, compress = "gzip")
usethis::use_data(quaddata, version = 3, overwrite = TRUE ) # the big file

 # compress = "gzip")
rm(list=ls())

# Tried various ways to completely recreate it/resave it .
# NONE OF THOSE WORKED SO I FINALLY MOVED frs.rda and frs.R to the EJAM package,
# and updated all packages where they said EJAM frsdata :: frs to just say frs 
# and will rebuild EJAM and EJAM frsdata pacakges.
##################################################################### #

# detach("package:EJAM", unload=TRUE)
# detach("package:EJAMejscreenapi", unload=TRUE)
# detach("package:EJAMbatch.summarizer", unload=TRUE)
# detach("package:EJAMblockdata", unload=TRUE)
# detach("package:EJAMfrsdata", unload=TRUE)
# detach("package:EJAMejscreendata", unload=TRUE)

# then push to github repo 

# ** YOU NEED TO CLOSE the project/not be in the EJAMfrsdata project folder 
#  before trying to install that pkg from repo 

# Finally try again to install from there (after PAT personal access token already set up)
# devtools::install_github("USEPA/EJAMfrsdata") # and said "None" when asked which pkgs to update.

# again got these error messages:
# # 
# Warning: file 'bgid2fips.rda' has magic number 'versi'
# Use of save versions prior to 2 is deprecated
# Error in load(zfile, envir = tmp_env) : 
#   bad restore file magic number (file may be corrupted) -- no data loaded
# ERROR: lazydata failed for package 'EJAMblockdata'

# or 
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


# check the compression type and version of each file in each package

checkdatafiles <- function(pkg, basefolder=Sys.getenv("R_USER")) {
  pkgfolder <- file.path(basefolder, pkg)
  datafolder <- file.path(pkgfolder, "data")
  these = list.files(datafolder)
  x = list() # print(these)
  i=0
  for (fname in  these) {
    i=i+1
    # print(
    x[[i]] <- tools::checkRdaFiles(file.path(datafolder, fname))
    # )
    # load(paste0("./data/", fname))
  }
  x = do.call(rbind, x)
  return(x)
}

# checkdatafiles("EJAM")
i=0
x=list()
for (z in EJAM::ejampackages) { i=i+1; x[[i]] =  checkdatafiles(z)}
x=do.call(rbind,x)
# print(x)

tail( x[order(x$size), ], 10)
#                                                                                                      size ASCII compress version
# C://Users//mcorrale//R//mysource/EJAM/data/states_shapefile.rda                                   8042649 FALSE    bzip2       2
# C://Users//mcorrale//R//mysource/EJAMblockdata/data/blockid2fips.rda                             19964681 FALSE    bzip2       2
# C://Users//mcorrale//R//mysource/EJAMblockdata/data/blockwts.rda                                 32508190 FALSE    bzip2       2
# C://Users//mcorrale//R//mysource/EJAM/data/frs_by_programid.rda                                  47138712 FALSE    bzip2       2
# C://Users//mcorrale//R//mysource/EJAM/data/blockgroupstats.rda                                   56910405 FALSE    bzip2       2
# C://Users//mcorrale//R//mysource/EJAM/data/frs.rda                                               61196637 FALSE    bzip2       2
# C://Users//mcorrale//R//mysource/EJAMejscreendata/data/EJSCREEN_StatePct_with_AS_CNMI_GU_VI.rda  61808425 FALSE    bzip2       2
# C://Users//mcorrale//R//mysource/EJAMejscreendata/data/EJSCREEN_Full_with_AS_CNMI_GU_VI.rda      66477049 FALSE    bzip2       2
# C://Users//mcorrale//R//mysource/EJAMblockdata/data/blockpoints.rda                              90120018 FALSE    bzip2       2
# C://Users//mcorrale//R//mysource/EJAMblockdata/data/quaddata.rda                                175979148 FALSE    bzip2       2

# Problematic files are in EJAMblockdata but the formats and sizes are not different than other pkgs, which do work.
