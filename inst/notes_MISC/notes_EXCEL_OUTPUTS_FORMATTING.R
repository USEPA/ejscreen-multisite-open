workbook_output_styled <- function(overall, eachsite, graycolnums=NULL, narrowcolnums=NULL) {
  if (!is.null(graycolnums)) {graycolnums <- which(grepl('avg', names(overall)) | grepl('state', names(overall)))}   # only works for the short names
  if (!is.null(narrowcolnums)) {narrowcolnums <- graycolnums}
  
  # library(openxlsx)
  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, 'Overall')
  addWorksheet(wb, 'Each Site')
  
  addWorksheet(wb, sheetName = "plot", gridLines = FALSE)
  addWorksheet(wb, sheetName = "notes", gridLines = FALSE)
  # showGridLines(wb, sheet = "plot", showGridLines = FALSE)
  
  
  
  
  
  
  
  
  
  
  # qplot(data = iris, x = Sepal.Length, y = Sepal.Width, colour = Species)
  # insertPlot(wb, sheet = 'plot', xy = c("B", 3))  ## insert plot at cell B3
  
}

wb <- workbook_output_styled(datasetResults()$results_overall, datasetResults()$results_bysite)

 



## To stop auto-formatting numerics set
# options(openxlsx.numFmt = NULL)

 
# means <- aggregate(x = iris[, -5], by = list(iris$Species), FUN = mean)






headstyle_basic <- createStyle(
  wrapText = TRUE, halign = "CENTER", valign = 'center',
  fgFill = "#4F81BD",  textDecoration = "Bold" #,
  # border = "Bottom", fontColour = "white"
)

# writeDataTable() # can use  tableStyle = "TableStyleLight2" and
# class(df$Cash2) <- "accounting"
# class(df$hLink) <- "hyperlink"
# class(df$Percentage) <- "percentage"

openxlsx::writeData(wb, 
                    sheet = 'Overall', x = df, xy = c(1,1), colNames = TRUE, 
                    withFilter = FALSE, 
                    keepNA = TRUE, na.string = "NA",  #  NA converted to blank or to #N/A or to na.string
                    headerStyle = headstyle_basic 
                    # borders = "rows", borderStyle = "medium",
)
openxlsx::writeData(wb, 
                    sheet = 'Each Site', x = df, xy = c(1,1), colNames = TRUE, 
                    withFilter = TRUE, 
                    keepNA = FALSE,  # NA converted to blank or to #N/A
                    headerStyle = headstyle_basic 
                    # borders = "rows", borderStyle = "medium",
)
# freezePane(wb, sheet = 'Overall',   firstRow = TRUE) #, firstCol = TRUE)  ## freeze first row and column
freezePane(wb, sheet = 'Each Site', firstRow = TRUE) #, firstCol = TRUE)  ## freeze first row and column

setColWidths(wb, 'Overall',   cols = narrowcolnums, widths = 6)
setColWidths(wb, 'Each Site', cols = narrowcolnums, widths = 6)

style_gray <- createStyle(bgFill = 'gray')
addStyle(wb, 'Overall',   cols = graycolnums, style = style_gray)
addStyle(wb, 'Each Site', cols = graycolnums, style = style_gray)

# style1 <- createStyle(   numFmt = "percentage")
# addStyle(wb, 1, style = style1, 
#          rows = 2:(nrow(df) + 1), cols = cols1, gridExpand = TRUE)

saveWorkbook(wb, "results.xlsx", overwrite = TRUE)





# ## colourscale colours cells based on cell value
# df <- read.xlsx(system.file("readTest.xlsx", package = "openxlsx"), sheet = 4)
# writeData(wb, "colourScale", df, colNames = FALSE)  ## write data.frame

