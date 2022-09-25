#' Run the Shiny Application
#' 
#' @description EJAMM::run_app() is used here to run the app instead of shiny::runApp()
#'   to allow this app to be shared as an R package, and to provide other 
#'   useful features that the golem package enables. 
#' @details See https://thinkr-open.github.io/golem/ 
#'   
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  # temporary workaround, see https://github.com/ThinkR-open/golem/issues/6
  source(system.file("global.R", package = "EJAM"))
  
  with_golem_options(
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
}
