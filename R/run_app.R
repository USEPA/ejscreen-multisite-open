#' Launch the Shiny Application in RStudio
#' @description launch Shiny web app from RStudio
#' @details 
#' app_run_EJAM()                 is like [EJAM::run_app()]
#' app_run_EJAMejscreenapi()      is like [EJAMejscreenapi::run_app()]
#' @param ... arguments to pass to golem_opts. Maybe could be something like sitepoints="latlondata.xlsx" or sitepoints=[testpoints_100]
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app      <- function( ###################################################### #
    onStart = NULL,
    options = list(),
    
    enableBookmarking = 'server',    ################################ #
    # this and the bookmarkButton() in ui let user save any uploaded files plus state of all  input$  settings, saved on server.
    # also see onBookmark() onBookmarked() onRestore() onRestored() 
    # see https://mastering-shiny.org/action-bookmark.html or https://rdrr.io/cran/shiny/man/bookmarkButton.html
    
    uiPattern = "/",
    ...
) {
  # temporary workaround, see https://github.com/ThinkR-open/golem/issues/6
  source(system.file("global.R", package = "EJAM")) # source('./inst/global1.R') 

  golem::with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
  # Normally R Shiny apps are not R packages -
  # The server just sources all .R files found in the /R/ folder,
  # and then runs what is found in app.R (if that is found / it is a one-file Shiny app).
  # This R Shiny app, however, is shared as an R package,
  # via the golem package approach,
  # which provides the useful features of a package and
  # useful features that the golem package enables.
  # 
  # There is still an app.R script in the package root
  # -- note there is no function called app()  --
  # which lets RStudio Connect source the app.R script
  # to launch this shiny app.
  # 
  # The way this works is that there is a file called
  # 
  #  _disable_autoload.R in the /R/ folder
  # 
  # to tell the server to not source all the source .R files,
  # since they are already in the installed package.
  # Then they get loaded from the package because
  # the app.R script here says this:
  # 
  #   pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
  # 
  # with the shinyApp() call wrapped in shiny::runApp() rather than in app()
  # 
  # Also, app_runYYYY() is the same as YYYY::run_app() in case that is useful.
  # 
  # See https://thinkr-open.github.io/golem/
}
