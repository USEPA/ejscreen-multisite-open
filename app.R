# Launch the ShinyApp (Do not remove this comment)
# This app.R file is used by the RStudio Connect server to launch the app since the 
#                 EJAM
#  app is a package unlike a typical shiny app,
#  and run_app() is loaded as an exported function that actually runs the app,
#  and while shiny normally sources all files in the /R folder, 
#  here _disable_autoload.R is used to avoid that 
#  since they are already loaded below via pkgload::load_all() which sources R files and loads data files. 
#
#  This was set up using  golem::add_rstudioconnect_file()
#  But note that this add_rstudioconnect_file() function says 
#  one needs to say pkgload::myfunction() to refer to the package functions ??
#  Need to clarify when/why/if... Within server code but not when used as non-shiny functions??
#
# To deploy, run:   rsconnect::deployApp()
# Or use the blue button on top of this file

# rm(list = ls())
# golem::detach_all_attached()

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)

EJAM::run_app() # add parameters here (if any)
