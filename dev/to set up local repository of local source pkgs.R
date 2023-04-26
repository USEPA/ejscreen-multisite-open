
# work in progress as of 4/24/23 



#   NOTE RStudio Connect EP Aserver uses packrat package but renv seems to be a better newer replacement for Packrat. 
# https://rstudio.github.io/renv/articles/renv.html
# vignette("renv", package="renv" )
# https://rstudio.github.io/packrat/walkthrough.html



# these are the same on one local machine, but not necessarily for everyone:
# dir("~/../../R/mysource")
# dir(Sys.getenv("R_USER"))

############### done ################
# Edited DESCRIPTION file in each EJAM pkg to not have Remotes: section since it indicated the packages are available from EPA's private github repos.
# Removed this section from each:
#
# Remotes: 
#   github::USEPA/EJAMblockdata,
#   github::USEPA/EJAMfrsdata,
#   github::USEPA/EJAMejscreenapi,
#   github::USEPA/EJAMbatch.summarizer,
#   github::USEPA/EJAMejscreendata
rstudioapi::openProject(file.path(Sys.getenv("R_USER"),"EJAM"))
for (ppp in file.path(Sys.getenv("R_USER"), EJAM::ejampackages, "DESCRIPTION")) {
  rstudioapi::documentOpen(ppp)
}
# just manually edit them, then
rstudioapi::documentSaveAll()
############### done ################

########################################################################################### # 
# try using renv instead of packrat? 

# renv::init() 










########################################################################################### # 
#  tried to do this with packrat   
 

  # packrat::disable() # to restart the whole process, including restarting R
rstudioapi::openProject(file.path(Sys.getenv("R_USER"),"EJAM"))

require(packrat)
# set up a local repository for the packages that get built from local source files instead of being on a public github repo or CRAN:
sourcepath <- normalizePath(Sys.getenv("R_USER"))
localrepopath <- paste0(sourcepath,"\\localrepo")


packrat::repos_create(path = localrepopath)  # not sure how/when to use this properly but seemed to work
# Local CRAN repository 'localrepo' created at: 
#    ....... /mysource/localrepo 
packrat::repos_set_local(localrepo = localrepopath)
############### done ################
 

# now rebuild each pkg from source and save tarball of it in the local repo:
  # normalizePath(file.path(sourcepath, "EJAMejscreenapi")), localrepopath)
mypkgs1 <- c( "EJAMfrsdata", "EJAMblockdata" ) # repos_upload can handle these OK
for (pkg in mypkgs1) {
  packrat::repos_upload(package = paste0(sourcepath,"\\", pkg), "localrepo")  
}
# for now, will build each app/package using devtools, since repos_upload does not like certain filenames: 
# Subdirectory 'R' contains invalid file names:
#   '_disable_autoload.R'
## but devtools::build() works, and just excludes those files:
# excluding invalid files (8.8s)
# Subdirectory 'R' contains invalid file names:
#   '_disable_autoload.R'
mypkgs2 <- c("EJAMejscreenapi", "EJAMbatch.summarizer", "EJAM") 
for (pkg in mypkgs2) {
  devtools::build(paste0(sourcepath,"\\",  pkg), sourcepath)
}
# building 'EJAMejscreenapi_2.1.1.tar.gz'
# building 'EJAMbatch.summarizer_2.1.0.tar.gz'
# building 'EJAM_2.1.1.tar.gz'
tarnames <- c('EJAMejscreenapi_2.1.1.tar.gz', 'EJAMbatch.summarizer_2.1.0.tar.gz', 'EJAM_2.1.1.tar.gz')
for (pkg in tarnames) {
  packrat::repos_upload(package = paste0(sourcepath,"\\", pkg  ), "localrepo")  
} 

# packrat::repos_upload(package = paste0(sourcepath,"\\","EJAMejscreenapi_2.1.1.tar.gz"))
# packrat::repos_upload(package = paste0(sourcepath,"\\","EJAMejscreenapi_2.1.1.tar.gz"))
# packrat::repos_upload(package = paste0(sourcepath,"\\","EJAMejscreenapi_2.1.1.tar.gz"))


#  seemed to work:
# external.packages: Packages which should be loaded from the user library. This can be useful for very large packages which you don't want duplicated across multiple projects, e.g. BioConductor annotation packages, or for package development scenarios wherein you want to use e.g. devtools and roxygen2 for package development, but do not want your package to depend on these packages. (character; defaults to Sys.getenv("R_PACKRAT_EXTERNAL_PACKAGES"))
# not sure if these are duplicative:
## use 'devtools' and 'knitr' from the user library
# packrat::set_opts(external.packages = c("devtools", "knitr"))
# ## get the set of 'external packages'
# packrat::opts$external.packages()
# ## set the external packages
# packrat::opts$external.packages(c("devtools", "knitr"))

 
# then (while in the EJAM project folder) 
# have packrat make a snapshot of all the packages that will be needed by EJAM, 
# including those in the local repo: 
setwd(file.path(Sys.getenv("R_USER"), "EJAM"))
# maybe do again?
packrat::repos_set_local(localrepo = localrepopath) 


#    This ***takes a very, very long while!! ***

packrat::init() # it now knows to look in local repo for those built from source locally




# Adding these packages to packrat:
#                 _        
#
# Adding these packages to packrat:
#   _          
# DBI                    1.1.3    
# DT                     0.27     
# EJAMbatch.summarizer   2.1.0    
# EJAMblockdata          2.1.2    
# EJAMejscreenapi        2.1.1    
# EJAMfrsdata            2.1.0    
# Formula                1.2-5    
# Hmisc                  5.0-1    
# R6                     2.5.1    
# RColorBrewer           1.1-3    
# RMySQL                 0.10.25  
# Rcpp                   1.0.10   
# SearchTrees            0.5.5    
# XML                    3.99-0.14
# askpass                1.1      
# attempt                0.3.1    
# backports              1.4.1    
# base64enc              0.1-3    
# bit                    4.0.5    
# bit64                  4.0.5    
# blob                   1.2.4    
# brio                   1.1.3    
# broom                  1.0.4    
# bslib                  0.4.2    
# cachem                 1.0.7    
# callr                  3.7.3    
# cellranger             1.1.0    
# checkmate              2.1.0    
# classInt               0.4-9    
# cli                    3.6.1    
# clipr                  0.8.0    
# collapse               1.9.5    
# colorspace             2.1-0    
# commonmark             1.9.0    
# config                 0.3.1    
# conflicted             1.2.0    
# cpp11                  0.4.3    
# crayon                 1.5.2    
# crosstalk              1.2.0    
# curl                   5.0.0    
# data.table             1.14.8   
# dbplyr                 2.3.2    
# desc                   1.4.2    
# diffobj                0.3.5    
# digest                 0.6.31   
# doSNOW                 1.0.20   
# dplyr                  1.1.1    
# dtplyr                 1.3.1    
# e1071                  1.7-13   
# ellipsis               0.3.2    
# evaluate               0.20     
# fansi                  1.0.4    
# farver                 2.1.1    
# fastmap                1.1.1    
# fontawesome            0.5.1    
# forcats                1.0.0    
# foreach                1.5.2    
# fs                     1.6.1    
# gargle                 1.4.0    
# generics               0.1.3    
# ggplot2                3.4.2    
# glue                   1.6.2    
# golem                  0.4.0    
# googledrive            2.1.0    
# googlesheets4          1.1.0    
# gridExtra              2.3      
# gtable                 0.3.3    
# haven                  2.5.2    
# here                   1.0.1    
# highr                  0.10     
# hms                    1.1.3    
# htmlTable              2.4.1    
# htmltools              0.5.5    
# htmlwidgets            1.6.2    
# httpuv                 1.6.9    
# httr                   1.4.5    
# hunspell               3.0.2    
# ids                    1.0.1    
# isoband                0.2.7    
# iterators              1.0.14   
# jquerylib              0.1.4    
# jsonlite               1.8.4    
# labeling               0.4.2    
# later                  1.3.0    
# lazyeval               0.2.2    
# leaflet                2.1.2    
# leaflet.extras2        1.2.0    
# leaflet.providers      1.9.0    
# lifecycle              1.0.3    
# lubridate              1.9.2    
# magrittr               2.0.3    
# mapproj                1.2.11   
# maps                   3.4.1    
# markdown               1.6      
# matrixStats            0.63.0   
# memoise                2.0.1    
# mime                   0.12     
# modelr                 0.1.11   
# munsell                0.5.0    
# openssl                2.0.6    
# openxlsx               4.2.5.2  
# packrat                0.9.1    
# pdist                  1.2.1    
# pillar                 1.9.0    
# pkgconfig              2.0.3    
# pkgload                1.3.2    
# png                    0.1-8    
# praise                 1.0.0    
# prettyunits            1.1.1    
# processx               3.8.0    
# progress               1.2.2    
# promises               1.2.0.1  
# proxy                  0.4-27   
# ps                     1.7.4    
# purrr                  1.0.1    
# ragg                   1.2.5    
# rappdirs               0.3.3    
# raster                 3.6-20   
# readr                  2.1.4    
# readxl                 1.4.2    
# rematch                1.0.1    
# rematch2               2.1.2    
# reprex                 2.0.2    
# rlang                  1.1.0    
# rmarkdown              2.21     
# rprojroot              2.0.3    
# rstudioapi             0.14     
# rvest                  1.0.3    
# s2                     1.1.2    
# sass                   0.4.5    
# scales                 1.2.1    
# selectr                0.4-2    
# sf                     1.0-12   
# shiny                  1.7.4    
# shinyBS                0.61.1   
# shinycssloaders        1.0.0    
# shinyjs                2.1.0    
# snow                   0.4-4    
# sourcetools            0.1.7-1  
# sp                     1.6-0    
# spelling               2.2.1    
# stringi                1.7.12   
# stringr                1.5.0    
# sys                    3.4.1    
# systemfonts            1.0.4    
# terra                  1.7-23   
# testthat               3.1.7    
# textshaping            0.3.6    
# tibble                 3.2.1    
# tidyr                  1.3.0    
# tidyselect             1.2.0    
# tidyverse              2.0.0    
# timechange             0.2.0    
# tinytex                0.45     
# triebeard              0.4.1    
# tzdb                   0.3.0    
# units                  0.8-1    
# urltools               1.7.3    
# utf8                   1.2.3    
# uuid                   1.1-0    
# vctrs                  0.6.1    
# viridis                0.6.2    
# viridisLite            0.4.1    
# vroom                  1.6.1    
# waldo                  0.4.0    
# withr                  2.5.0    
# wk                     0.7.2    
# xfun                   0.38     
# xml2                   1.3.3    
# xtable                 1.8-4    
# yaml                   2.3.7    
# zip                    2.3.0    
#
############################################################################# # 
# Now open the EJAM project and open the EJAM/app.R file,
# and try to publish the app/package EJAM to RStudio Connect server

# setwd(file.path(Sys.getenv("R_USER"), "EJAM"))
rstudioapi::openProject(file.path(Sys.getenv("R_USER"), "EJAM"))
rstudioapi::navigateToFile("app.R")



