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

# devtools::build_readme() # takes a couple minutes! as it installs the package in a temporary library
# build_rmd() is a wrapper around rmarkdown::render() that first installs a temporary copy of the package, and then renders each .Rmd in a clean R session.
rmarkdown::render("README.Rmd")  # renders .Rmd to create a  .md file that works in github as a webpage

#################### # 

# in case this matters here:
EJAM::dataload_from_pins("all") # not sure this helps with building vignettes though, which need access to frs file etc. in whatever environment they are built in


# if  not et built by devtools::install ?... 

devtools::document()


# just build/install using RStudio buttons? but check if build options include vignettes
# or can try this:
Sys.time()   # about 4 minutes  
devtools::install(quick = TRUE,
                  upgrade = FALSE, 
                  build_vignettes = TRUE,  # does it do that? it says "installing vignettes" but does not update the .html files in the vignette folder.
                  # build_vignettes=TRUE if you want to re-render them without doing full build caused by quick=FALSE.
                  build = FALSE,
                  quiet = FALSE
                  ) 
Sys.time()
# # quick=T is MUCH faster but skips docs, vignettes, etc., building 'EJAM_2.2.2.tar.gz' or the .zip binary, etc.
# # quick=F is SLOW!  takes a few minutes! 
## build = TRUE means it converts a package source directory into a single bundled file. If binary = FALSE this creates a tar.gz package that can be installed on any platform, provided they have a full development environment (although packages without source code can typically be installed out of the box). If binary = TRUE, the package will have a platform specific extension (e.g. .zip for windows), and will only be installable on the current platform, but no development environment is needed.

#################### # 

#################### # 

library(EJAM)  #
EJAM::rmost()
EJAM::dataload_from_pins("all") # not sure this helps with building vignettes though, which need access to frs file etc. in whatever environment they are built in

# may want to run tests and/or check here.

 #   devtools::test()


#################### # 
# Build regular R package vignettes ? in /doc/ ?

 ## THIS TAKES SOME TIME: 
 devtools::build_vignettes(quiet = FALSE, upgrade = "never", install = FALSE)

 #################### # 
# Build Articles (web based vignettes) for pkgdown website.  in /docs/ ? not /doc/ 
# knit button might not work in some cases?

# build_articles()
  # # This works (at least for one article) ... new_process = FALSE seemed to help:
# build_article("6_future_plans", new_process = FALSE)
# build_article("3_analyzing",    new_process = FALSE)
# build_article("1_installing",   new_process = FALSE)

# This does ALL the pages over again:
Sys.time() # next part can be SLOW 
# reads the vignettes/xyz.Rmd and uses those as it
#  recreates all .html files, etc. (could perhaps do as bkgd job)

pkgdown::build_site_github_pages(
  dest_dir = "docs",
  clean = FALSE,        # faster if FALSE. TRUE would delete objects already attached? 
  examples = FALSE,     # should only set TRUE if you want to include outputs of examples along with the function documentation!
  new_process = FALSE,  # faster if FALSE (and HAD PROBLEMS IF TRUE... if FALSE then it can rely on having frs and other files available in current environment, for building vignettes?)
 
  devel = TRUE, # faster if TRUE - If FALSE, will first install the package to a temporary library, and will run all examples and vignettes in a new process.
                # build_site() defaults to devel = FALSE so that you get high fidelity outputs when you building the complete site; 
                # build_reference(), build_home() and friends default to devel = TRUE so that you can rapidly iterate during development.
  
  lazy = TRUE       # faster if TRUE   (can force a build despite no change in source vs destination copy)
) 
# that does  build_site()  and then    
# that does  build_github_pages() ?
# usethis::use_github_pages(branch = "main", path = "/docs") # FAST - just defines source and URL. already done earlier, prob do not need to repeat.


# now these steps fail: 
 #     build_search('.')   and   build_sitemap('.') 
#    which is a step in 
#   pkgdown:::build_site_local() which is part of 
# pkgdown::build_site_github_pages()
# ── Building search index ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# Error in UseMethod("xml_find_first") : 
#   no applicable method for 'xml_find_first' applied to an object of class "xml_document"
# this step would work     pkgdown:::build_redirects(".")



Sys.time() # 40 minutes for all of this to run with slowest options above
#################### # 
stop( ' then COMMIT AND PUSH THE NEW FILES ')
browseURL("https://github.com/USEPA/EJAM/actions/") # to see automatic deployment happen
stop()

# use  rstudio  menu, build ...
# only use binary=TRUE if package has C that needs to be compiled. otherwise source package is better.
## to (reinstall and) rebuild source package that makes it easier for users to install from github like this
# remotes::install_github(c(
#   'USEPA/EJAMejscreenapi',
#   'USEPA/EJAMbatch.summarizer',
#   'USEPA/EJAM'
# ), build_vignettes = TRUE) # if you want their installed package 
# to have vignettes available via vignette() or browseVignettes() in addition to the html versions available in the github pages at 
# #  
# 
# build(".")  # to build single file source package that could be shared with those who want to install without needing PAT. 
# installation from github via remotes::
# 
# build("../EJAMejscreenapi")
#  build("../EJAMbatch.summarizer")

 ################################################################## #
