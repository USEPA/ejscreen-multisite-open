############################################################# # ############################################################# # 
############################################################# # ############################################################# # 

# SOURCE-ABLE SCRIPT  TO  REBUILD PACKAGE DOCUMENTATION AND vignettes  (articles) using pkgdown

# setup ####

#doask = TRUE   # or #  
doask = FALSE 
if (!interactive()) {doask <- FALSE}
offline_cat()

# defaults
dotests       = FALSE
dodocument    = FALSE # in case we just edited help or exports or func names !
doinstall     = FALSE # but skips document and vignettes
doloadall_not_library = TRUE  # (happens after install if that is being done here)
dopreviewonly = TRUE  # always.  do  pkgdown::build_site()  not  pkgdown::build_site_github_pages

golem::detach_all_attached()

require(devtools)
require(pkgdown)
############################################################# # ############################################################# # 
############################################################# # ############################################################# # 





############################################################# # 
# For documentation on set up of a pkgdown website,
# see https://pkgdown.r-lib.org/articles/pkgdown.html#configuration
# see https://usethis.r-lib.org/reference/use_pkgdown.html

### also possibly of interest:
# devtools::build_manual()          # for a  pdf manual
# postdoc::render_package_manual()  # for an html manual
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

if (doask & interactive() & rstudioapi::isAvailable()) {dotests <- utils::askYesNo("Do you want to run tests 1st?")}
if (is.na(dotests)) {stop('stopped')}

if (doask & interactive()  & rstudioapi::isAvailable() ) {dodocument <- utils::askYesNo("Do document() now since just installing via this script wont do document() ?")}
if (is.na(dodocument)) {stop('stopped')}

if (doask & interactive()  & rstudioapi::isAvailable() ) {doinstall <- utils::askYesNo("Do you want to re-install the package? does not redo document()")}
if (is.na(doinstall)) {stop('stopped')}

if (doask & interactive()  & rstudioapi::isAvailable() ) {doloadall_not_library  <- utils::askYesNo("do load_all() instead of library(EJAM) ?")}
if (is.na(doloadall_not_library)) {stop('stopped')}

# if (doask & interactive()  & rstudioapi::isAvailable() ) {dopreviewonly  <- utils::askYesNo("Just build site preview locally (not for github yet)?")}
# if (is.na(dopreviewonly)) {stop('stopped')}

#################### #

# TEST ? ####

if (dotests) {
  source("./tests/manual_nonalphabetical.R")
  # test_interactively(ask = doask & interactive() )
  test_interactively(ask = TRUE) # we would want to do this interactively even if the rest of docs updating is not asking
  }

# ? check() ? 
# devtools::check() 
##   automatically builds and checks a source package, using all known best practices.
# devtools::check_man()
# devtools::check_built() checks an already-built package.

# ? devtools::test() 

## [ FAIL 7 | WARN 7 | SKIP 1 | PASS 617 ] as of 5/13/24
#################### # #################### # #################### # #################### # 
if (dodocument) {
  
  # README ####
  
  rmarkdown::render("README.Rmd")  # renders .Rmd to create a  .md file that works in github as a webpage
  
  # build_rmd() would take a couple minutes as it installs the package in a temporary library
  # build_rmd() would just be a wrapper around rmarkdown::render() that 1st installs a temp copy of pkg, then renders each .Rmd in a clean R session.
  #################### # #################### # #################### # #################### # 
  
  # DOCUMENT ####
  
  document()
}
#################### # #################### # #################### # #################### # 

# INSTALL? ####

if (doinstall) {
  system.time({
    
    # 4+ minutes for install() 
    
    # Usually just use devtools::load_all()  during development, not re-install every time you edit source.
    
    # BUT, using devtools::install() will ensure anything that uses the INSTALLED version will work!
    
    # note, If you want to build/install using RStudio buttons, not the function install(), need to
    #   1st confirm you already turned off traditional vignette-building...  see   help(vignette_roclet, package = "roxygen2")
    #   That button includes a step that is the same as   devtools::document()
    
    devtools::install(
      
      quick = TRUE,   # USUALLY LEAVE IT AS TRUE
      # # quick=T is MUCH faster but skips docs, vignettes, etc., building 'EJAM_2.2.2.tar.gz' or the .zip binary, etc.
      # # quick=F is SLOW!  takes a few minutes! 
      
      upgrade = FALSE, 
      
      build_vignettes = FALSE,  
      ## old-style vignetters were in  doc folder, but pkgdown-style are in   docs folder, 
      
      build = FALSE,
      ## build = TRUE means it converts a package source directory into a single bundled file...
      ##   If binary = FALSE this creates a tar.gz package that can be installed on any platform, provided they have a full development environment (although packages without source code can typically be installed out of the box). 
      ##   If binary = TRUE, the package will have a platform specific extension (e.g. .zip for windows), and will only be installable on the current platform, but no development environment is needed.
      
      quiet = FALSE
    )
    #################### # 
    
    golem::detach_all_attached()
    library(devtools)
    library(pkgdown)
  })  
}
#################### # #################### # #################### # #################### # 
# rmost() ####
########### but rstudio build button makes it try to load data and it connects to pins but does not use those yet-
# tries to use local copies and fails to get .arrow files from local path supposed to be ~/../Downloads/......
# so it loads the .rda from aws that are older and not all files are there. 
## why did it not use the pins versions since it did connect? and why not found in that local path???
## so did rm(list=ls()) and tried to continue from library( ) above .

EJAM:::rmost(notremove = c('dotests', "dataload_pin_available", 'dopreviewonly', 'dodocument', 'doask', 'doinstall', 'doloadall_not_library'))

#################### # #################### # #################### # #################### # 

# LOAD ALL FROM SOURCE  ####
if (doloadall_not_library) {
load_all()
  } else {
    x = try( require(EJAM) )
    if (inherits(x, "try-error")) {stop("try restarting R")}
    rm(x)
}
#################### # #################### # #################### # #################### # 

# DATASETS FROM PINS  ####

if (!EJAM:::dataload_pin_available()) {stop("cannot build vignettes correctly without access to pins board")}
EJAM::dataload_from_pins("all") #  # just in case 

#################### # #################### # #################### # #################### # 
# ~ ####

## 2 options for how to keep site updated, from pkgdown intro https://pkgdown.r-lib.org/articles/pkgdown.html
# 
# A) If you’re using GitHub, we recommend setting up pkgdown and GitHub actions 
# (e.g., https://github.com/r-lib/actions/tree/v2-branch/examples#build-pkgdown-site ) 
# to automatically build and publish your site:
#  Run this ONCE EVER to have github actions re-publish your site regularly:
# 
#   usethis::use_pkgdown_github_pages()  # only ONCE
#
# B) But, if not using GitHub (or if GitHub Actions have trouble rendering vignettes to html 
#   due to lacking access to pins board etc.)
#   then you'll have to run this manually EVERY TIME you want to update the site:

#   pkgdown::build_site() 


# ** BUILD SITE  before committing #### 

if (dopreviewonly) {
 
  
  pkgdown::build_site(
    examples = FALSE, lazy = TRUE, 
    devel = FALSE, 
    install = FALSE, new_process = FALSE
    )
  
  # https://pkgdown.r-lib.org/reference/build_site.html
  # build_site() is a convenient wrapper around six functions:
  #   
  # init_site()
  # build_home() 
  # build_reference() & index - ** THIS TAKES FOREVER **   (could perhaps do as bkgd job)
  # build_articles()  - THIS TRIES TO RUN THE CODE IN VIGNETTES/ARTICLES AND RENDER THEM  
  # build_tutorials() - NA
  # build_news()
  # build_redirects()
  
  
  # build_site_github_pages() is for use in a github action, and would do more steps:
  #  build_site(), but then also gets metadata about package and does
  #  clean_site() and
  #  build_github_pages()
} else {
  #################### # #################### # #################### # #################### # 
  # ~ ####
  # Sys.time() # next part can be SLOW # 40 minutes for all of this to run with slowest options
  # 
  # # ** BUILD SITE GITHUB PAGES #### 
  # 
  # # (web based vignettes) for pkgdown website.  in /docs/ ? not /doc/ 
  # # knit button might not work in some cases?
  # # This does ALL the pages over again
  # 
  # # build_articles()
  # # >>>Note that when you run build_articles() directly (outside of build_site()) vignettes 
  # #  will use the currently installed version of the package, not the current source version. 
  # #  This makes iteration quicker when you are primarily working on the text of an article.
  # # # This works (at least for one article) ... new_process = FALSE seemed to help:
  # # build_article("0_whatis",   new_process = FALSE)
  # # build_article("0_webapp",   new_process = FALSE)
  # # build_article("1_installing",   new_process = FALSE)
  # # build_article("2_quickstart",   new_process = FALSE)
  # # build_article("3_analyzing",    new_process = FALSE)
  # # build_article("4_advanced",     new_process = FALSE)
  # 
  # # build_article("5_ejscreenapi",  new_process = FALSE)
  # # build_article("6_future_plans", new_process = FALSE)
  # 
  # # reads the vignettes/xyz.Rmd and uses those as it
  # #  recreates all .html files, etc. (could perhaps do as bkgd job)
  # 
  # #** build_site_github_pages 
  # 
  # pkgdown::build_site_github_pages(  ## ?? or just build_site() ?
  #   
  #   # but this function is meant to be used as part of github actions
  #   # https://pkgdown.r-lib.org/reference/build_site_github_pages.html
  #   
  #   dest_dir = "docs",
  #   clean = FALSE,        # faster if FALSE. TRUE would delete objects already attached? 
  #   examples = FALSE,     # *** should only set TRUE if you want to include outputs of examples along with the function documentation!
  #   new_process = FALSE,  # faster if FALSE (and HAD PROBLEMS IF TRUE... if FALSE then it can rely on having frs and other files available in current environment, for building vignettes?)
  #   
  #   devel = FALSE,
  #   # devel = TRUE, # faster if TRUE - If FALSE, will first install the package to a temporary library, and will run all examples and vignettes in a new process.
  #   # build_site() defaults to devel = FALSE so that you get high fidelity outputs when you building the complete site; 
  #   # build_reference(), build_home() and friends default to devel = TRUE so that you can rapidly iterate during development.
  #   
  #   lazy = TRUE       # faster if TRUE   (can force a build despite no change in source vs destination copy)
  # )
  # # that does clean_site(),   build_site(), and  build_github_pages() 
  # 
  # Sys.time() # 40 minutes for all of this to run with slowest options above
  # 
  # #################### # #################### # #################### # #################### # 
  # # ~ ####
  # # did it finish ? ####
  # 
  # # But within that, it stops with error on this step: 
  # #
  # #     build_search('.')   # **** PROBLEM IN pkgdown ******
  # # ── Building search index ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  # # Error in UseMethod("xml_find_first") : no applicable method for 'xml_find_first' applied to an object of class "xml_document"
  # #  and trying it alone afterwards fails with the same error.
  # # so it never gets to 
  # # #  some  steps in    
  # # pkgdown:::build_site_local() ?
  # #    which is part of 
  # # pkgdown::build_site_github_pages()
  # # 
  # # But then trying this in command line works:
  # 
  # cat("If there was a problem, you might need to restart and finish by doing these :  \n\n")
  # cat("
  # pkgdown:::build_search('.')
  #     \n")
  # # pkgdown:::build_search('.')
  }
#################### # 

# commit, push ####

cat( '\n\n NOW COMMIT AND PUSH THE NEW FILES \n\n')
cat("Try this to see github action happen: \n
browseURL('https://github.com/USEPA/EJAM/actions/') # to see automatic deployment happen

")
################################################################## #

# build single file source pkg? ####
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
