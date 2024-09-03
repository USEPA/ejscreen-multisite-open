############################################################# # 
# For documentation on set up of a pkgdown website,
# see https://pkgdown.r-lib.org/articles/pkgdown.html#configuration
# see https://usethis.r-lib.org/reference/use_pkgdown.html
############################################################# # 
#
##   Initial setup of authorization tokens:
#
## 1st confirm personal access token PAT exists and not expired
##  (to allow use of github API to create new branch gh-pages, create github action, etc.)
#     To check on PATs:
# usethis::gh_token_help() and 
# usethis::git_sitrep() # git situation report
##    To make a PAT:
# usethis::create_github_token()
##    To register a PAT, see
# credentials::set_github_pat()
##    https://usethis.r-lib.org/articles/git-credentials.html#git-credential-helpers-and-the-credential-store
##    Windows more or less takes care of this for you now, in conjunction with github.
############################################# #
#
##   Initial setup of github pages website using pkgdown:
#
# usethis::use_github_pages(branch = "main", path = "/docs")
##   does
# usethis::use_pkgdown()   and does other steps,
##   but note it replaces/deletes any existing _pkgdown.yml file 
#
#   Traditional (not pkgdown) vignettes are no longer recommended by roxygen2 pkg docs:
# see   help(vignette_roclet, package = "roxygen2")
# Can turn them off in RStudio "Build" - "Configure Build Options" - "Build Tools" - "Generate Docs with Roxygen"
#   and by adding   --no-build-vignettes to the "Build Source Package" field in your project options. 
############################################# #

#    SCRIPT  TO  REBUILD  vignettes  (articles) using pkgdown
#
# Probably does not require all these steps, though
# setup ####
require(devtools)
require(pkgdown)

#################### # 
# README ####
# build_rmd() would take a couple minutes as it installs the package in a temporary library
# build_rmd() would just be a wrapper around rmarkdown::render() that 1st installs a temp copy of pkg, then renders each .Rmd in a clean R session.
rmarkdown::render("README.Rmd")  # renders .Rmd to create a  .md file that works in github as a webpage

# Usually just using devtools::load_all()  during development, not reinstalling every time you edit source.
#   You could build/install using RStudio buttons, but
#   1st confirm you already turned off traditional vignette-building...  see   help(vignette_roclet, package = "roxygen2")
#   That button includes a step that is the same as   devtools::document()

system.time({
  # 4+ minutes for steps below
  
  ############################################# #
  # pins available? (to build vignettes) ####
  dataload_pin_available <- function(boardfolder = "Mark",
                                     auth = "auto",
                                     server = "https://rstudio-connect.dmap-stage.aws.epa.gov", 
                                     silent = FALSE) {
    board <- tryCatch(pins::board_connect(server = server, auth = auth),
                      error = function(e) e)
    if (inherits(board, "error")) {
      board_available <- FALSE
      if (!silent) {cat("Failed trying to connect to pins board server.\n\n")}
    } else {
      board_available <- TRUE
      if (!silent) {cat("Successfully connected to Posit Connect pins board.\n\n")}
    }
    return(board_available)
  }
  ############################################# #
  # dataload_from_pins("all") ####
  # just in case 
  if (!dataload_pin_available()) {stop("cannot build vignettes correctly without access to pins board")}
  EJAM::dataload_from_pins("all") #
  ############################################# #
  
  # RUN TESTS OR CHECK ?
  
  # ? check() ? ####
  # devtools::check() 
  ##   automatically builds and checks a source package, using all known best practices.
  # devtools::check_man()
  # devtools::check_built() checks an already-built package.
  
  # ? devtools::test() ####
  
  ## [ FAIL 7 | WARN 7 | SKIP 1 | PASS 617 ] as of 5/13/24
  ############################################# #
  # install() ####
  
  devtools::install(
    
    quick = TRUE,   # USUALLY LEAVE IT AS TRUE
    # # quick=T is MUCH faster but skips docs, vignettes, etc., building 'EJAM_2.2.2.tar.gz' or the .zip binary, etc.
    # # quick=F is SLOW!  takes a few minutes! 
    
    upgrade = FALSE, 
    
    # build_vignettes = FALSE,  
    ## old-style vignetters were in  doc folder, but pkgdown-style are in   docs folder, 
    
    build = FALSE,
    ## build = TRUE means it converts a package source directory into a single bundled file...
    ##   If binary = FALSE this creates a tar.gz package that can be installed on any platform, provided they have a full development environment (although packages without source code can typically be installed out of the box). 
    ##   If binary = TRUE, the package will have a platform specific extension (e.g. .zip for windows), and will only be installable on the current platform, but no development environment is needed.
    
    quiet = FALSE
  )
  
  #################### # 
  # library(EJAM) ####
  
  library(EJAM)  #
  # that had been causing an error that quitting / restarting RStudio seemed tfix(Error in `poll(list(self), ms)`:
  # ! Native call to `processx_poll` failed
  # Caused by error in `chain_call(c_processx_poll, pollables, type, as.integer(ms))`:
  #   ! lazy-load database 'C:/Users/... .. . ./EJAM/R/EJAM.rdb' is corrupt
  
  # rmost() ####
  ########### but rstudio build button makes it try to load data and it connects to pins but does not use those yet-
  # tries to use local copies and fails to get .arrow files from local path supposed to be ~/../Downloads/......
  # so it loads the .rda from aws that are older and not all files are there. 
  ## why did it not use the pins versions since it did connect? and why not found in that local path???
  ## so did rm(list=ls()) and tried to continue from library( ) above .
  EJAM:::rmost(notremove = "dataload_pin_available")
  # dataload again ####
  if (!dataload_pin_available()) {stop("cannot build vignettes correctly without access to pins board")}
  EJAM::dataload_from_pins("all") # not sure this helps with building vignettes though, which need access to frs file etc. in whatever environment they are built in
  
})

#################### # #################### # 
# BUILD ARTICLES #### 
# (web based vignettes) for pkgdown website.  in /docs/ ? not /doc/ 
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
  
  # but this function is meant to be used as part of github actions
  # https://pkgdown.r-lib.org/reference/build_site_github_pages.html
  
  dest_dir = "docs",
  clean = FALSE,        # faster if FALSE. TRUE would delete objects already attached? 
  examples = FALSE,     # *** should only set TRUE if you want to include outputs of examples along with the function documentation!
  new_process = FALSE,  # faster if FALSE (and HAD PROBLEMS IF TRUE... if FALSE then it can rely on having frs and other files available in current environment, for building vignettes?)
  
  devel = TRUE, # faster if TRUE - If FALSE, will first install the package to a temporary library, and will run all examples and vignettes in a new process.
  # build_site() defaults to devel = FALSE so that you get high fidelity outputs when you building the complete site; 
  # build_reference(), build_home() and friends default to devel = TRUE so that you can rapidly iterate during development.
  
  lazy = TRUE       # faster if TRUE   (can force a build despite no change in source vs destination copy)
) 
# that does clean_site(),   build_site(), and  build_github_pages() 

Sys.time() # 40 minutes for all of this to run with slowest options above

# But within that, it stops with error on this step: 
#
#     build_search('.')   # **** PROBLEM IN pkgdown ******
# ── Building search index ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# Error in UseMethod("xml_find_first") : no applicable method for 'xml_find_first' applied to an object of class "xml_document"
#  and trying it alone afterwards fails with the same error.
#
# so it never gets to 
#   pkgdown:::build_sitemap('.') etc. etc.
#    which are steps in    
# pkgdown:::build_site_local() 
#    which is part of 
# pkgdown::build_site_github_pages()
# 
# But then trying these two steps alone in command line works:

# to finish that ? ####
pkgdown:::build_sitemap('.')
pkgdown:::build_redirects(".")

#################### # 

# stop and push new files now ####

stop( ' then COMMIT AND PUSH THE NEW FILES ')

browseURL("https://github.com/USEPA/EJAM/actions/") # to see automatic deployment happen

stop()

# use  rstudio  menu, build ...
# only use binary=TRUE if package has C that needs to be compiled. otherwise source package is better.
## to (reinstall and) rebuild source package that makes it easier for users to install from github like this
# remotes::install_github(c(
#   'USEPA/EJAM'
# ), build_vignettes = FALSE) 
#
# build(".")  # to build single file source package that could be shared with those who want to install without needing PAT. 
# installation from github via remotes

################################################################## #
