workbook_output_styled <- function(overall, eachsite, graycolnums=NULL, narrowcolnums=NULL, graycolor='gray', narrow6=6, ...) {
  
  # NEED TO MERGE THIS WITH EJAMejscreenapi's  prep_for_excel() 
  
  # example
  # wb <- workbook_output_styled(datasetResults()$results_overall, datasetResults()$results_bysite)
  # openxlsx::saveWorkbook(wb, "results.xlsx", overwrite = TRUE)
  
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
  
  openxlsx::writeData(wb, 
                      sheet = 'Overall', x = overall, 
                      xy = c(1,1), colNames = TRUE, 
                      withFilter = FALSE, 
                      keepNA = TRUE, na.string = "NA",  #  NA converted to blank or to #N/A or to na.string
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
  # freezePane(wb, sheet = 'Overall',   firstRow = TRUE) #, firstCol = TRUE)  ## freeze first row and column
  openxlsx::freezePane(wb, sheet = 'Each Site', firstRow = TRUE) #, firstCol = TRUE)  ## freeze first row and column
  
  openxlsx::setColWidths(wb, 'Overall',   cols = narrowcolnums, widths = narrow6)
  openxlsx::setColWidths(wb, 'Each Site', cols = narrowcolnums, widths = narrow6)
  style_gray <- openxlsx::createStyle(bgFill = graycolor)
  openxlsx::addStyle(wb, 'Overall',   cols = graycolnums, rows = 1, style = style_gray)
  openxlsx:: addStyle(wb, 'Each Site', cols = graycolnums, rows = 1, style = style_gray)
  
  return(wb)
  
  # style1 <- createStyle(   numFmt = "percentage")
  # addStyle(wb, 1, style = style1, 
  #          rows = 2:(nrow(df) + 1), cols = cols1, gridExpand = TRUE)
  
  # ## colourscale colours cells based on cell value
  # df <- read.xlsx(system.file("readTest.xlsx", package = "openxlsx"), sheet = 4)
  # writeData(wb, "colourScale", df, colNames = FALSE)  ## write data.frame
   
  
  # qplot(data = iris, x = Sepal.Length, y = Sepal.Width, colour = Species)
  # insertPlot(wb, sheet = 'plot', xy = c("B", 3))  ## insert plot at cell B3
  ## To stop auto-formatting numerics set
  # options(openxlsx.numFmt = NULL)
  # means <- aggregate(x = iris[, -5], by = list(iris$Species), FUN = mean)
  
  # writeDataTable() # can use  tableStyle = "TableStyleLight2" and
  # class(df$Cash2) <- "accounting"
  # class(df$hLink) <- "hyperlink"
  # class(df$Percentage) <- "percentage"
}




