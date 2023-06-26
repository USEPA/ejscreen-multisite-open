# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()
rhub::check_for_cran()


# To deploy we do what is in this file:

### EJAM/dev/install_ejampackages_from_github.R 
 # to install from source packages on EPA github 
 #    for EJAMejscreenapi and EJAMbatch.summarizer and EJAMfrsdata 
 #    and for EJAM itself
 # and then deploy by using RStudio's built in publish button that appears when editing the app.R file,
 # and not deploy the dev folder but yes to everything else. 

 
 
 
 
 
 
 
 
 ############ this is obsolete....  
 
# Deploy

## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
devtools::build()

## RStudio ----
## If you want to deploy on RStudio related platforms
golem::add_rstudioconnect_file()
# golem::add_shinyappsio_file()
# golem::add_shinyserver_file()

## Docker ----
## If you want to deploy via a generic Dockerfile
# golem::add_dockerfile_with_renv()

## If you want to deploy to ShinyProxy
# golem::add_dockerfile_with_renv_shinyproxy()

