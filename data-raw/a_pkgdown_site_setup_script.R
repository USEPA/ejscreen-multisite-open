############################################################# # 
# For documentation on set up of a pkgdown website,
# see https://pkgdown.r-lib.org/articles/pkgdown.html#configuration
# see https://usethis.r-lib.org/reference/use_pkgdown.html
############################################################# # 
## Initial setup: 
## 1st confirm personal access token PAT exists and not expired
##  (to allow use of github API to create new branch gh-pages, create github action, etc.)
# git_sitrep() # git situation report
##    To make a PAT:
# usethis::create_github_token()
##    To register a PAT, one way is as part of this
# usethis::use_github_pages(branch = "main", path = "/docs")
##   which does  usethis::use_pkgdown() and other steps,
##   but note this replaces/deletes any existing _pkgdown.yml file !
# Then modify the _pkgdown.yml file
# and write/edit the .Rmd vignettes in the vignettes folder
##############################################

# SCRIPT TO REBUILD THE vignettes (articles) 
# Probably does not require all these steps, though.

# Note: In RStudio menu, check on and maybe adjust the settings here:
#   Build, Configure Build Options, Build Tools, Generate Docs with Roxygen
  # look at the checkboxes for  build/install & vignettes 

library(devtools)
library(usethis)
library(pkgdown)
library(EJAM)
EJAM::rmost()

# may want to run tests and/or check here.

# devtools::build_readme() # takes a couple minutes! as it installs the package in a temporary library
# build_rmd() is a wrapper around rmarkdown::render() that first installs a temporary copy of the package, and then renders each .Rmd in a clean R session.
rmarkdown::render("README.Rmd")  # renders .Rmd to create a  .md file that works in github as a webpage

EJAM::dataload_from_pins("all") # not sure this helps with building vignettes though, which need access to frs file etc. in whatever environment they are built in

#################### # 

# just build/install using RStudio buttons, or can try this:
Sys.time()
devtools::install(quick = TRUE, 
                  upgrade = FALSE, 
                  build_vignettes = TRUE,  # can use build_vignettes=TRUE if you want to re-render them without doing full build caused by quick=FALSE.
                  build = FALSE,
                  quiet = FALSE
                  ) 
# # quick=T is MUCH faster but skips docs, vignettes, etc., building 'EJAM_2.2.2.tar.gz' or the .zip binary, etc.
# # quick=F is SLOW!  takes a few minutes! 
## build = TRUE means it converts a package source directory into a single bundled file. If binary = FALSE this creates a tar.gz package that can be installed on any platform, provided they have a full development environment (although packages without source code can typically be installed out of the box). If binary = TRUE, the package will have a platform specific extension (e.g. .zip for windows), and will only be installable on the current platform, but no development environment is needed.

### errors may include In fun(libname, pkgname) : internal error -3 in R_decompress1
# myinstalled/EJAM/R/EJAM.rdb' is corrupt
Sys.time() # SLOW - recreates all .html files, etc. (could perhaps do as bkgd job)

#################### # 

# then make the pkgdown documentation webpages:

pkgdown::build_site_github_pages(
  dest_dir = "docs",
  clean = FALSE,        # faster if FALSE. TRUE would delete objects already attached? 
  examples = FALSE,     # should only set TRUE if you want to include outputs of examples along with the function documentation!
  new_process = FALSE,  # faster if FALSE (and HAD PROBLEMS IF TRUE... if FALSE then it can rely on having frs and other files available in current environment, for building vignettes?)
  devel = FALSE,    # faster if TRUE
  lazy = TRUE       # faster if TRUE   (can force a build despite no change in source vs destination copy)
) 
# that does  build_site()  and then    
# that does  build_github_pages() ?
# usethis::use_github_pages(branch = "main", path = "/docs") # FAST - just defines source and URL. already done earlier, prob do not need to repeat.


Sys.time() # 40 minutes for all of this to run with slowest options above
#################### # 
stop( ' then COMMIT AND PUSH THE NEW FILES ')
browseURL("https://github.com/USEPA/EJAM/actions/") # to see automatic deployment happen
stop()

 ################################################################## #
