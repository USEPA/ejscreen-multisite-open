# ?usethis::use_data_table()
################################################# #

# To deploy we do what is in this file:
### EJAM/dev/install_ejampackages_from_github.R 
# 
# to install from source packages on EPA github 
#    for EJAMejscreenapi and EJAMbatch.summarizer (and EJAMfrsdata for the functions not data anymore)
#    and for EJAM itself
# and then deploy by using RStudio's built in publish button that appears when editing the app.R file,
# and not deploy the dev folder but yes to everything else. 

# configuring server to allow installing a package from a private repository
# see   https://docs.posit.co/connect/admin/r/package-management/#from-private-git-repositories 

# how to have content like an entire app, etc. deployed from a github repo instead of from 1 users pc
#  https://docs.posit.co/connect/user/git-backed/#git-backed


### commented out to avoid creating dependencies 


# library(
# credentials
# )
# library(
# rsconnect
# )
# library(
# devtools
# )
# library(
# rsconnect
# )


rm(list=ls())

### make any changes to pkgs, then...
 # document() # in devtools pkg, 
#  push changes to github, then...

if (is.loaded("EJAM")) {  detach("package:EJAM", unload=TRUE)}
# if (is.loaded(
#  "EJAMfrsdata")) {  detach("package:
# 
# EJAMfrsdata", unload=TRUE)}
if (is.loaded("EJAMbatch.summarizer")) {  detach("package:EJAMbatch.summarizer", unload=TRUE)}
if (is.loaded("EJAMejscreenapi")) {  detach("package:EJAMejscreenapi", unload=TRUE)}

################################################# #

 
 # writeManifest()  # not sure  

### make sure PAT is set up right. 
set_github_pat() # in credentials pkg

install_github('USEPA/EJAMejscreenapi',      force=TRUE, upgrade="never") # works fine. in devtools pkg.

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
# ...etc
 

install_github('USEPA/EJAMbatch.summarizer', force=TRUE, upgrade="never") # got these warnings/msgs:

# Using github PAT from envvar GITHUB_PAT
# Downloading GitHub repo USEPA/EJAMbatch.summarizer@HEAD
# ── R CMD build ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# ✔  checking for file 'C:\Users\mcorrale\AppData\Local\Temp\RtmpIxpe7O\remotes62c057ae1498\USEPA-EJAMbatch.summarizer-1df55b1a861d37f40b859b3be74a7a2f80bdbaf5/DESCRIPTION' (887ms)
# ─  preparing 'EJAMbatch.summarizer':
#   ✔  checking DESCRIPTION meta-information ... 
# ─  excluding invalid files
# Subdirectory 'R' contains invalid file names:
#   '_disable_autoload.R'
# ─  checking for LF line-endings in source and make files and shell scripts
# ─  checking for empty or unneeded directories
# ─  building 'EJAMbatch.summarizer_2.1.0.tar.gz'
# 
# Installing package into ‘C:/Users/mcorrale/R/myinstalled’
# (as ‘lib’ is unspecified)
# Greetings!
#   * installing *source* package 'EJAMbatch.summarizer' ...
# ** using staged installation
# ** R
# ** data
# *** moving datasets to lazyload DB
# ** inst
# ** byte-compile and prepare package for lazy loading
# Greetings
#   ** help
# *** installing help indices
# *** copying figures
# ** building package indices
# Greetings!
#   ** testing if installed package can be loaded from temporary location
# Greetings!
#   ** testing if installed package can be loaded from final location
# Greetings!
#   ** testing if installed package keeps a record of temporary installation path
# * DONE (EJAMbatch.summarizer)
#
# Warning messages:
#   1: In untar2(tarfile, files, list, exdir, restore_times) :
#   skipping pax global extended headers
# 2: In untar2(tarfile, files, list, exdir, restore_times) :
#   skipping pax global extended headers


install_github('USEPA/EJAM',                 force=TRUE, upgrade= "never")  


# stop() 

deployApp(
  appDir = "C:/Users/mcorrale/R/mysource/EJAM",     
  #appFileManifest = "C:/Users/mcorrale/AppData/Local/Temp/5f29-617c-01d5-c927",      
  account = "Mark", server = "rstudio-connect.dmap-stage.aws.epa.gov",      
  appName = "ejam", #appId = 153, 
  launch.browser = function(url) { message("Deployment completed: ", url)  }, 
  lint = FALSE, 
  metadata = list(
    asMultiple = FALSE, asStatic = FALSE, 
    ignoredFiles = "NEWS.md|plumber/plumber.R|README.md|README.Rmd|vignettes/EJAM-vignette.html|vignettes/EJAM-vignette.R|vignettes/EJAM-vignette.Rmd|data-raw|dev|man|tests"
  ), 
  logLevel = "verbose"
)

# 
# Installing EJAMbatch.summarizer (2.1.0) ... 
# curl: HTTP 404 https://api.github.com/repos/USEPA/EJAMbatch.summarizer/tarball/1df55b1a861d37f40b859b3be74a7a2f80bdbaf5
# curl: (22) The requested URL returned error: 404 
# GET /__api__/tasks/PuxH2iOFGw0rDeg8?first_status=820 19ms
# curl: HTTP 404 https://api.github.com/repos/USEPA/EJAMbatch.summarizer/tarball/1df55b1a861d37f40b859b3be74a7a2f80bdbaf5
# curl: (22) The requested URL returned error: 404 
# GET /__api__/tasks/PuxH2iOFGw0rDeg8?first_status=822 29ms
# curl: HTTP 404 https://api.github.com/repos/USEPA/EJAMbatch.summarizer/tarball/1df55b1a861d37f40b859b3be74a7a2f80bdbaf5
# curl: (22) The requested URL returned error: 404 
# GET /__api__/tasks/PuxH2iOFGw0rDeg8?first_status=824 10ms
# curl: HTTP 404 https://api.github.com/repos/USEPA/EJAMbatch.summarizer/tarball/1df55b1a861d37f40b859b3be74a7a2f80bdbaf5
# curl: (22) The requested URL returned error: 404 
# GET /__api__/tasks/PuxH2iOFGw0rDeg8?first_status=826 10ms
# curl: HTTP 404 https://api.github.com/repos/USEPA/EJAMbatch.summarizer/tarball/1df55b1a861d37f40b859b3be74a7a2f80bdbaf5
# curl: (22) The requested URL returned error: 404 
# GET /__api__/tasks/PuxH2iOFGw0rDeg8?first_status=828 19ms
# FAILED
# Error in value[[3L]](cond): Failed to download package from URL:
#   - 'https://api.github.com/repos/USEPA/EJAMbatch.summarizer/tarball/1df55b1a861d37f40b859b3be74a7a2f80bdbaf5'
# - Reason: Error in doTryCatch(return(expr), name, parentenv, handler): Download failure.
# 
# 
# Unable to fully restore the R packages associated with this deployment.
# Please review the preceding messages to determine which package
# encountered installation difficulty and the cause of the failure.
# Warning message:
#   In packrat::restore(overwrite.dirty = TRUE, prompt = FALSE, restart = FALSE) :
#   The most recent snapshot was generated using R version 4.2.2
# Build error: exit status 1
# ── Deployment complete ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# ✖ Deployment failed with error: exit status 1
# GET /__api__/applications/153/config 40ms
# [2023-06-22 17:44:30] Deployment log finished






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

