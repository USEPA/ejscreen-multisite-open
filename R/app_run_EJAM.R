#' Run the Shiny Application
#' app_run_EJAM()                 is like EJAM::run_app()
#' app_run_EJAMejscreenapi()      is like EJAMejscreenapi::run_app()
#' app_run_EJAMbatch.summarizer() is like EJAMbatch.summarizer::run_app()
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
app_run_EJAM <- function(
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
