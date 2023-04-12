#' xls_formatting2
#' Format EJAM tabular outputs for saving as Excel spreadsheet
#' @details  NEED TO MERGE THIS WITH EJAMejscreenapi::xls_formatting_api() 
#' @param overall table to save in one tab, from EJAM analysis of indicators overall (one row)
#' @param eachsite table to save in another tab, from EJAM analysis site by site (one row per site)
#' @param graycolnums which columns to deemphasize
#' @param graycolor color used to deemphasize some columns
#' @param narrowcolnums which columns to make narrow
#' @param narrow6 how narrow
#' @param ... other params passed along to [openxlsx::writeData()]
#'
#' @return a workbook, ready to be saved in spreadsheet format, with tabs like "Overall" and "Each Site"
#' @export
#'
#' @examples \dontrun{
#'   wb <- xls_formatting2(datasetResults()$results_overall, datasetResults()$results_bysite)
#'   openxlsx::saveWorkbook(wb, "results.xlsx", overwrite = TRUE)
#' }
xls_formatting2 <- function(overall, eachsite, graycolnums=NULL, narrowcolnums=NULL, graycolor='gray', narrow6=6, ...) {

  if (!is.null(graycolnums))   {graycolnums <- which(grepl('avg', names(overall)) | grepl('state', names(overall)))}   # only works for the short names
  if (!is.null(narrowcolnums)) {narrowcolnums <- graycolnums}
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, 'Overall')
  openxlsx::addWorksheet(wb, 'Each Site')
  
  openxlsx::addWorksheet(wb, sheetName = "plot", gridLines = FALSE)
  openxlsx::addWorksheet(wb, sheetName = "notes", gridLines = FALSE)
  # showGridLines(wb, sheet = "plot", showGridLines = FALSE)
  
  headstyle_basic <- openxlsx::createStyle(
    wrapText = TRUE, halign = "CENTER", valign = 'center',
    fgFill = "#4F81BD",  textDecoration = "Bold" #,
    # border = "Bottom", fontColour = "white"
  )
  
  #  also  can use  tableStyle = "TableStyleLight2" for example
  
  # keepNA	
  #  If TRUE, NA values are converted to #N/A (or na.string, if not NULL) in Excel -- else NA cells will be empty.
  #  If FALSE, NA values shown as blank/empty cells.
  
  # na.string	
  #   If not NULL, and if keepNA is TRUE, NA values are converted to this string in Excel.
  
  openxlsx::writeData(wb, 
                      sheet = 'Overall', x = overall, 
                      xy = c(1,1), colNames = TRUE, 
                      withFilter = FALSE, 

                      headerStyle = headstyle_basic ,
                      ...
                      # borders = "rows", borderStyle = "medium",
  )
  openxlsx::writeData(wb, 
                      sheet = 'Each Site', x = eachsite, 
                      xy = c(1,1), colNames = TRUE, 
                      withFilter = TRUE, 
                      keepNA = FALSE,  # NA converted to blank or to #N/A
                      headerStyle = headstyle_basic ,
                      ...
  )
  
  # freeze panes
  openxlsx::freezePane(wb, sheet = 'Each Site', firstRow = TRUE) #, firstCol = TRUE)  ## freeze first row and column
  # freezePane(wb, sheet = 'Overall',   firstRow = TRUE)   ## freeze   column ?
  
  # make some cols NARROWER
  openxlsx::setColWidths(wb, 'Overall',   cols = narrowcolnums, widths = narrow6)
  openxlsx::setColWidths(wb, 'Each Site', cols = narrowcolnums, widths = narrow6)
  
  # make some cols GRAY/DIMMED
  style_gray <- openxlsx::createStyle(bgFill = graycolor)
  openxlsx::addStyle(wb, 'Overall',   cols = graycolnums, rows = 1, style = style_gray)
  openxlsx::addStyle(wb, 'Each Site', cols = graycolnums, rows = 1, style = style_gray)
  
  return(wb)
  
  #                         NOTES ON OTHER IDEAS: 

  #   PERCENTAGE STYLE
  #
  # style1 <- createStyle(   numFmt = "percentage")
  # addStyle(wb, 1, style = style1, 
  #          rows = 2:(nrow(df) + 1), cols = cols1, gridExpand = TRUE)
  # 
  
  # 
  # class(df$Cash2) <- "accounting"
  # class(df$hLink) <- "hyperlink"
  # class(df$Percentage) <- "percentage"
  # 
  ## To stop auto-formatting numerics set
  # options(openxlsx.numFmt = NULL)
  
  
  #   HEATMAP COLORING (conditional formatting) - or use xls_varname2color() etc. and see xls_formatting(), xls_formatting2(), xls_formatting_api(), etc.
  #
  # ## colourscale colours cells based on cell value
  # df <- read.xlsx(system.file("readTest.xlsx", package = "openxlsx"), sheet = 4)
  # writeData(wb, "colourScale", df, colNames = FALSE)  ## write data.frame
  
  #   INSERT A PLOT 
  # 
  # qplot(data = iris, x = Sepal.Length, y = Sepal.Width, colour = Species)
  # insertPlot(wb, sheet = 'plot', xy = c("B", 3))  ## insert plot at cell B3
  
}




