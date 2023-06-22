# ?usethis::use_data_table()
################################################# #

rm(list=ls())


detach("package:EJAM", unload=TRUE)
detach("package:EJAMbatch.summarizer", unload=TRUE)
detach("package:EJAMejscreenapi", unload=TRUE)

################################################# #

# make sure PAT is set up right. 
# library(credentials)
credentials::set_github_pat()

# library(devtools)
# make any changes to pkgs, devtools::document(), push changes to github, then...

devtools::install_github('USEPA/EJAMejscreenapi',      force=TRUE, upgrade=F) # works fine.
# Using github PAT from envvar GITHUB_PAT
# Downloading GitHub repo USEPA/EJAMejscreenapi@HEAD
# ── R CMD build ─────────────────────────────────────────────────────────────────────────────────────────────────
# ✔  checking for file 'C:\Users\xyz\AppData\Local\Temp\Rtmpis2NKX\remotes3b2451ef3276\USEPA-EJAMejscreenapi-70f29f3ddf4483d01631c12b9e2ec7314e94a7b4/DESCRIPTION' (514ms)
# ─  preparing 'EJAMejscreenapi': (3.4s)
# ✔  checking DESCRIPTION meta-information ... 
# ─  excluding invalid files
# Subdirectory 'R' contains invalid file names:   * * ************ * * 
#   '_disable_autoload.R'
# ─  checking for LF line-endings in source and make files and shell scripts
# ─  checking for empty or unneeded directories
# ─  building 'EJAMejscreenapi_2.1.2.tar.gz'   * * ************ * * 
# Installing package into ‘R/myinstalled’
# (as ‘lib’ is unspecified)
#   * installing *source* package 'EJAMejscreenapi' ...
# ** using staged installation
# ** R
# ** data
# *** moving datasets to lazyload DB   * * ************ * * 
# ** inst
# ** byte-compile and prepare package for lazy loading
# Greetings!
#   ** help
# *** installing help indices
# ** building package indices
#   ** testing if installed package can be loaded from temporary location
#   ** testing if installed package can be loaded from final location
#   ** testing if installed package keeps a record of temporary installation path
# * DONE (EJAMejscreenapi)
# > 
 


devtools::install_github('USEPA/EJAMbatch.summarizer', force=TRUE, upgrade=F) # works fine.

devtools::install_github('USEPA/EJAM',                 force=TRUE, upgrade=F) # DO I HAVE TO DO THIS??

stop()










##################################################################### #
# check the compression type and version of each file in each package

checkdatafiles <- function(pkg, basefolder=Sys.getenv("R_USER")) {
  pkgfolder  <- file.path(basefolder, pkg)
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
#  EJAM/data/states_shapefile.rda                  8042649 FALSE    bzip2       2
#     blockid2fips.rda                            19964681 FALSE    bzip2       2
#     blockwts.rda                                32508190 FALSE    bzip2       2
#  EJAM/data/frs_by_programid.rda                 47138712 FALSE    bzip2       2
#  EJAM/data/blockgroupstats.rda                  56910405 FALSE    bzip2       2
#  EJAM/data/frs.rda                              61196637 FALSE    bzip2       2
#     blockpoints.rda                             90120018 FALSE    bzip2       2
#     quaddata.rda                               175979148 FALSE    bzip2       2

#  # not used:
#  EJAMejscreendata/data/EJSCREEN_StatePct_with_AS_CNMI_GU_VI.rda  61808425 FALSE    bzip2       2
#  EJAMejscreendata/data/EJSCREEN_Full_with_AS_CNMI_GU_VI.rda      66477049 FALSE    bzip2       2
 
