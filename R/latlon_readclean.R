latlon_readclean <- function(x) {
  x <- EJAMbatch.summarizer::read_csv_or_xl(x)
  x <- latlon_df_clean(x)
  return(x)
}
