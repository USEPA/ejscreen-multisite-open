
#' Read table of data from .csv or .xlsx Excel file
#' 
#' If in RStudio, interactively can select file from local drive. Used by app etc 
#' 
#' @description Read simple table from csv or xls or xlsx.
#' For excel format, must be simple table on first tab, 
#' one row for header (column names),
#' data itself starting in first cell of second row, like A2, and
#' all other rows and columns must be empty.
#' 
#' @param fname full path to folder and filename  
#' @param show_col_types FALSE makes it print less to console as it reads using readr::read_csv()
#' @param rowsize_warn Give warning if there are more than this many rows in the table 
#' @return data.frame with contents of table it read
#' 
#' @keywords internal
#' @export
#'
read_csv_or_xl <- function(fname=NULL, show_col_types=FALSE, rowsize_warn= 30 * 1000) {
  
  if (is.null(fname)) {
    if (interactive()) {
      if (rstudioapi::isAvailable()) {
        fname <- rstudioapi::selectFile()
      }
      else {
        fname <- file.choose()
      }
    }
    else {
      stop("fname (file path/name) needed but not provided")
    }
  }
  
  if (is.data.frame(fname)) {
    # assume they accidentally provided the data not the file with the data
    ## We maybe should warn or even disable if this is a crazy number of points (rows) ***
    return(fname)
  }
  
  ## We should disable upload of a crazy number of points - maybe check file size or do quick pre-read check of NROW() ? ***
  
  if (grepl('\\.csv$', fname, ignore.case = TRUE)) {
    filecontents <- try(as.data.frame(readr::read_csv(file = fname, show_col_types = show_col_types)))
    if(inherits(filecontents, "try-error")){
      message("Error reading CSV file, returning NULL")
      return(NULL)
    }
    # error handling could go here - to ensure it is a solid block of data, only 1 header row, no extra rows or cols 
  }
  if (grepl('\\.xls$|\\.xlsx$', fname, ignore.case = TRUE)) {
    filecontents <- try(as.data.frame(readxl::read_excel(path = fname, sheet = 1)))
    if(inherits(filecontents, "try-error")){
      message("Error reading excel file, returning NULL")
      return(NULL)
    }
    # error handling could go here- to ensure it is a solid block of data, only 1 header row, no extra rows or cols or merged cells
  } 
  if (NROW(filecontents) > rowsize_warn) {warning("There appear to be ", NROW(filecontents), " rows in this dataset!")}
  return(filecontents)
}
