#' See ejamit()$results_bysite in interactive table in RStudio viewer pane
#'
#' @param x output of ejamit(), or one table like ejamit()$results_overall,
#'   or subset like ejamit()$results_bysite[7,]
#' @param fname path and filename of the html file to save the table to, 
#'   or it uses tempdir() if not specified. Set it to NULL to not save to file.
#' @param maxrows only load/ try to show this many rows max.
#' @return a datatable object using [DT::datatable()] 
#'   that can be printed to the console or shown in the RStudio viewer pane
#' @examples ejam2tableviewer(testoutput_ejamit_10pts_1miles)
#'   
#' @export
#'
ejam2tableviewer = function(out, fname = file.path(tempdir(), "ejamtable.html"), maxrows = 1000) {
  
  if ("results_bysite" %in% names(out)) {
    x <- out$results_bysite  
  } else {
    x <- out
  }
  if (!is.data.frame(x)) { # data.table is ok too
    stop("Input must be a data frame")
  }
  
  x <- x[1:min(nrow(x), maxrows), ]
  
  x <- fix_pctcols_x100(x, cnames = names_pct_as_fraction_ejamit)
  x <- table_round(x)
  
  dt <-    DT::datatable(x,
                         # colnames = out$longnames, 
                         colnames = fixcolnames(names(x), 'r', 'long'),
                         rownames = FALSE,  
                         escape = FALSE,  # related to showing URLs correctly but note security implication
                         caption = paste0(nrow(x),  ' SITES \\"' , ' ', '\\"'), 
                         filter = "top")
  
  if (!is.null(fname)) { 
  # save file, launch external browser to 
  htmlwidgets::saveWidget(dt, fname)
  browseURL(fname)
  cat(fname, '\n')
  }
  
  return(dt)
}
################################################# #
