

#' Utility to download / load datasets from pin board in pkgdown github pages of public repo
#'
#' @param varnames vector of names of data objects stored in the pins board
#' @param server do not change
#' @param envir do not change default of globalenv()
#' @param justchecking set to TRUE if you just need to see what is stored in the pins board
#'
#' @return subset of varnames that is a vector of the ones successfully assigned to specified environment
#'
#' @export
#'
dataload_from_urlpins <- function(varnames = .arrow_ds_names[1:3],
                                  server = "https://ejanalysis.github.io/ejscreendata/pins/",
                                  envir = globalenv(),
                                  justchecking = FALSE) {
  
  board <- pins::board_url(server)
  
  if (all(varnames == 'all')) {varnames <- .arrow_ds_names}
  if (justchecking) {
    x <- board  %>% pins::pin_search() # like  pin_list()
    return(x)
    
  } else {
    
    got = NULL
    for (i in seq_along(varnames)) {
      pname = varnames[i]
      if (pins::pin_exists(board, pname)) {
        assign(pname, pins::pin_read(board, pname), envir = envir)
        if (!exists(pname, envir = envir)) {
          warning(pname, "was found on pins board but could not be assigned to specified environment")
        } else {
          got = c(got, pname)
        }
      } else {
        warning(pname, "not found on pins board")
      }
    }
    
    return(got)
  }
  # to delete files:
  # localboard %>% pin_delete("blockwts")
  #   localboard %>% pin_delete("bgid2fips")
  # localboard %>% write_board_manifest()
}
