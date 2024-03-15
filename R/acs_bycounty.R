
### Adding these functions would require the tidycensus pkg and scales and stringr
# ## not already required by EJAM:
# library(tidycensus)
# library(scales)
# library(stringr)
#   and tidycensus imports these:
# httr, tigris, stringr, jsonlite,
# purrr, rvest,  rappdirs, readr, xml2,
# units, utils, rlang, crayon, tidyselect
################################# #

# #  a census api key would be needed here if large number queries needed
################################# #



#' download ACS 5year data from Census API,
#'
#' @param myvars  optional .extracted from x, one or more ACS5 variables like "B03002_001"
#' @param myst abbreviation of one state, like "DE"
#' @param yr like 2021 or 2022 end of 5 year ACS
#'
#' @return tibble table from output of acs_bycounty() i.e., output of get_acs()
#'   from tidycensus pkg
#' @examples
#'   x     <- acs_bycounty(myvars = "B03002_003", myst = "NY", yr = 2022) # nhwa
#'   denom <- acs_bycounty(myvars = "B03002_001", myst = "NY", yr = 2022) # pop
#'   z = x
#'   z$estimate = x$estimate / denom$estimate
#'   z$moe = 0  # x$moe / denom$estimate # need to calculate using census guidance if at all
#'   # z$variable = "pctnhwa" # not used if myvarnames is given
#'   plot_bycounty(z, myvarnames = "Percent NonHispanic White Alone (i.e., Not People of Color)",
#'                 labeltype = scales::label_percent())
#'
#' @export
#'
acs_bycounty <- function(myvars="B03002_001", myst="DE", yr=2021) {

stop('requires tidycensus and other packages not yet added to EJAM pkg')
## uncomment below if will add all those to DESCRIPTION Imports
### ## not already required by EJAM:
  ## library(tidycensus)
  ## library(scales)
  ## library(stringr)
  ##   and tidycensus imports these:
  ## httr, tigris, stringr, jsonlite,
  ## purrr, rvest,  rappdirs, readr, xml2,
  ## units, utils, rlang, crayon, tidyselect


  # data_bycounty <-  tidycensus::get_acs(
  #   geography = "county",
  #   variables = unlist(myvars),
  #   state = myst,
  #   year = yr
  # )
  # return(data_bycounty)
}
################################# ################################## #


#' plot comparison of counties in 1 state, for 1 indicator (variable)
#'
#' @param x table that is output of acs_bycounty() i.e., output of get_acs()
#'   from tidycensus pkg
#' @param myvars optional .extracted from x, one or more ACS5 variables like "B03002_001"
#' @param myvarnames optional friendlier names of myvars
#' @param mystate name of state
#' @param labeltype as from scales package.  for continuous scales:
#'   label_bytes(), label_number_auto(), label_number_si(), label_ordinal(),
#'   label_parse(), label_percent(), label_pvalue(), label_scientific()
#'
#' @param varinfo large table of metadata as from load_variables function
#'   from tidycensus pkg
#' @param yr like 2021 or 2022 end of 5 year ACS
#'
#' @return plot
#'
#' @export
#'
plot_bycounty <- function(x, myvars=x$variable[1], myvarnames=NULL, mystate=NULL,
                          labeltype = NULL, varinfo=NULL, yr=2021) {

  stop('requires tidycensus and other packages not yet added to EJAM pkg')
# see acs_bycounty() notes

  # if (is.null(labeltype)) {labeltype <- scales::label_number_auto()}
  # if (missing(mystate) || is.null(mystate)) {
  #   mystate <- gsub("^.*, ", "", x$NAME[1])
  # }
  #
  # ### get long name of variable and of state
  # if (missing(myvarnames) || is.null(myvarnames)) {
  #   if (missing(varinfo)) {
  #     varinfo <- tidycensus::load_variables(yr, "acs5")
  #   }
  #   myvarnames <- gsub("!!", " ", varinfo$label[match(myvars, varinfo$name)])
  # }
  #
  # plot_errorbar <- ggplot(x,
  #                         aes(x = estimate,
  #                             y = reorder(NAME, estimate))) +
  #   ggplot2::geom_errorbar(aes(xmin = estimate - moe, xmax = estimate + moe), #<<
  #                          width = 0.5, linewidth = 0.5) + #<<
  #   ggplot2::geom_point(color = "darkblue", size = 2) +
  #
  #   ggplot2::scale_x_continuous(labels = labeltype) +
  #
  #   ggplot2::scale_y_discrete(labels = function(x) stringr::str_remove(x, paste0(" County, ", mystate))) +
  #   ggplot2::labs(title = paste0(myvarnames, ", ", yr - 4, "-", yr," ACS"),
  #                 subtitle = paste0("Counties in ", mystate),
  #                 caption = "Data acquired with R and tidycensus. Error bars represent margin of error around estimates.",
  #                 x = "ACS estimate",
  #                 y = "") +
  #   ggplot2::theme_minimal(base_size = 12)
  #
  # plot_errorbar

}
################################# ################################## #

# yr <- 2022
# myvars <- "B03002_003" # c(income = "B19013_001") # works wrong if named vector now
# myst <- "UT"
# labeltype <- scales::label_currency()
# x <- acs_bycounty(myvars = myvars, myst = myst, yr = yr)
# plot_bycounty(x)

################################# #

# varinfo <- tidycensus::load_variables(2022, "acs5")
# varinfo[substr(varinfo$name, 1,6) == "B03002", ]
# B03002_003
################################# #
