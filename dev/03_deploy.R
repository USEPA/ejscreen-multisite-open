
 
# TEST  ## Check the package before sending to prod

testthat::test_local()

devtools::check()

# devtools::check()  automatically builds and checks a source package, using all known best practices. 
 # check_built() checks an already-built package.
# Passing ⁠R CMD check⁠ is essential if you want to submit your package to CRAN: you must not have any ERRORs or WARNINGs
#  and you want to ensure that there are as few NOTEs as possible.
# If you are not submitting to CRAN, at least ensure that there are no ERRORs or WARNINGs: these typically represent serious problems.
# Under-the-hood, check() and check_built() rely on pkgbuild::build() and rcmdcheck::rcmdcheck().

rhub::check_for_cran()

# usethis::use_release_issue()
cat('also see  usethis::use_release_issue()  \n')
# When preparing to release a package to CRAN there are quite a few steps that need to be performed, and some of the steps can take multiple hours. This function creates a checklist in a GitHub issue to:
# Help you keep track of where you are in the process
# Feel a sense of satisfaction as you progress towards final submission
# Help watchers of your package stay informed.


# DEPLOY
#
### see  EJAM/dev/install_ejampackages_from_github.R 
 #
 # to install from source packages on EPA github 
 #    for EJAMejscreenapi and EJAMbatch.summarizer and EJAMfrsdata 
 #    and for EJAM itself
 # and then deploy by using RStudio's built in publish button that appears when editing the app.R file,
 # and not deploy the dev folder but yes to everything else. 

 
 
 
 
 
 
 
 
 ############  also, to share as a  tar.gz  file 

## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally, sent to CRAN, or to a package manager

devtools::build()


## RStudio ----
## If you want to deploy on RStudio related platforms
#
# golem::add_rstudioconnect_file() 
