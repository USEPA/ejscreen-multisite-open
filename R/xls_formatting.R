#' Format batch results for excel - OBSOLETE - NOW IN xml_formatting2()
#' @param df data.frame, table of batch buffer results
#' @param hyperlink_cols vector of names of columns in df to get treated as hyperlinks in excel
#' @param heatmap_colnames vector of names of columns in df to apply conditional formatting to,
#'   by coloring like a heatmap. 
#' @param heatmap_colors vector of colors corresponding to cuts
#' @param heatmap_cuts vector of cut points in values of heatmap_colnames data for 
#'   heatmap colorscheme
#' @param heatmap_cuts vector of color names for heatmap bins, same length as 
#'   heatmap_cuts, where first color is for those >= 1st cutpoint, but <2d,
#'   second color is for those >=2d cutpoint but <3d, etc.
#'
#' @return A workbook via openxlsx::writeData() ready to be saved via openxlsx::saveWorkbook()
#' @export
#'
xls_formatting  <- function(df, hyperlink_cols=NULL, 
                           heatmap_colnames=NULL,
                           heatmap_cuts=c(80, 90, 95),
                           heatmap_colors=c('yellow', 'orange', 'red')) {
  if (length(heatmap_cuts) != length(heatmap_colors)) stop('heatmap_cuts and heatmap_colors must be same length')
  if (!all(heatmap_colnames %in% names(df))) {stop('all column names in heatmap_colnames must be found in df')}
  if (!all(hyperlink_cols %in% names(df))) {stop('all column names in hyperlink_cols must be found in df')}
  
  ######################################################################## #
  # CREATE WORKBOOK ####
  ######################################################################## #
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "EJ stats") # names the tab (worksheet)
  
  #openxlsx::writeData(wb, sheet = 1, x = df, startRow = 1, startCol = 1)
  
  ######################################################################## #
  # HYPERLINKS ####
  # special names for the pdf and map links ####
  ######################################################################## #
  
  if(!is.null(hyperlink_cols)){
    hyperlink_text <- gsub('EJScreenPDF', 'EJScreen Report', hyperlink_cols)
    hyperlink_text <- gsub('EJScreenMAP', 'EJScreen Map',    hyperlink_cols)
    for (i in 1:length(hyperlink_cols)) {
      class(df[ , hyperlink_cols[i]]) <- 'hyperlink'
      names(df[ , hyperlink_cols[i]]) <- paste(hyperlink_text[i], rownames(df)) # to use e.g., "EJScreen Report 1" not "EJScreenPDF 1" 
    }
    # note this does not work if just use write.xlsx and df already had "=hyperlink(...)" 
    # class(df$EJScreenPDF )  <- "hyperlink" '   # # class(df$EJScreenMAP)   <- "hyperlink"
    # names(df$EJScreenPDF) <- paste("EJScreen Report", rownames(df)) # EJScreen Report 1, EJScreen Report 2, etc.
    # names(df$EJScreenMAP) <- paste("EJScreen Map",    rownames(df)) # EJScreen Map 1,    EJScreen Map 2,    etc.
    
    hypercolnums <- match(hyperlink_cols, names(df))
    for (i in 1:length(hyperlink_cols)) {
      openxlsx::writeData(wb, sheet = 1, x = names(df[ , hyperlink_cols[i]]), startRow = 2, startCol = hypercolnums[i])
    }
  }
  
  ######################################################################## #
  # HEADER ROW & freeze panes    ####
  ######################################################################## #
  row1style <- openxlsx::createStyle(wrapText = TRUE,  textDecoration = "bold", border = "bottom", borderStyle = "medium")
  openxlsx::addStyle(wb, sheet = 1, row1style, rows = 1, cols = 1:NCOL(df), gridExpand = TRUE)
  openxlsx::setRowHeights(wb, 1, rows = 1, heights = 115)
  openxlsx::freezePane(wb, sheet = 1, firstActiveRow = 2, firstActiveCol = 4)
  
  
  ######################################################################## #
  # COLOR CODING / CONDITIONAL FORMATTING HEATMAP  ####
  # to highlight large percentiles in Excel
  ######################################################################## #
  
  
  ## *** **ANOTHER WAY TO HANDLE THIS IS TO USE THE FUNCTIONS LIKE xls_varname2color()  xls_varname2type() etc. ####
  
  if(!is.null(heatmap_colnames)){
    pctilecolnums <- which(names(df) %in% heatmap_colnames)
    for (i in 1:length(heatmap_cuts)) {
      mystyle  <- openxlsx::createStyle(bgFill = heatmap_colors[i]) # "#C6EFCE")
      openxlsx::conditionalFormatting(wb, 1, cols = pctilecolnums, rows=2:(NROW(df)+1),
                                      rule = paste0(">=", heatmap_cuts[i]), style = mystyle)
    }
    #  'yellow')# "#C6EFCE")   #  'orange') #"#C6EFCE")   #  'red') # "#C6EFCE")
    
  }
 
  
  ######################################################################## #
  # OUTPUT #### 
  ######################################################################## #
  
  openxlsx::writeData(wb, sheet = 1, x = df, startRow = 1, startCol = 1)
  return(wb)
}
