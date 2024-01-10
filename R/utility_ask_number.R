#' utility - interactive prompt in RStudio to ask user to specify number like radius
#' same as askradius()
#' @aliases askradius
#' @param default default value
#' @param title question
#' @param message title of popup dialog box
#' @seealso askYesNo()
#' @return a number
#'
ask_number <- function(default = 3, title = "Radius", message = "Within how many miles of each point?") {
  radius <- NA
  while (is.na(radius)) {
    radius <- as.numeric(rstudioapi::showPrompt(title = title, message = message, default = default))
    if (length(radius) == 0) {radius <- NA}
  }
  cat("Specified value is", radius, '\n')
  return(radius)
}

askradius <- ask_number
