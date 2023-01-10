#' read csv or xlsx and then clean it
#' This function just wraps [EJAMbatch.summarizer::read_csv_or_xl()]
#'   and [latlon_df_clean()]
#'
#' @param x character string, full path to a .csv or .xlsx file
#'
#' @return see [latlon_df_clean()]
#' @export
#'
latlon_readclean <- function(x) {
  x <- EJAMbatch.summarizer::read_csv_or_xl(x)
  x <- latlon_df_clean(x)
  return(x)
}
