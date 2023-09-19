#' xls_formatting2
#' Format EJAM tabular outputs for saving as Excel spreadsheet
#' @details  Already took and put here most or all of code from xls_formatting() or xls_formatting_api() 
#' @param overall table to save in one tab, from EJAM analysis of indicators overall (one row)
#' @param eachsite table to save in another tab, from EJAM analysis site by site (one row per site)
#' @param longnames vector of indicator names to display in Excel table
#' @param summary_plot optional plot passed from EJAM shiny app to save in 'Plot' sheet of Excel table
#' @param plot2 another plot for another tab
#' @param analysis_title optional title passed from Shiny app to 'Notes' sheet
#' @param buffer_desc optional description of buffer used in analysis, passed to 'Notes' sheet
#' @param heatmap_colnames optional vector of colnames to apply heatmap colors
#' @param heatmap_cuts vector of values to separate heatmap colors, between 0-100
#' @param heatmap_colors vector of color names for heatmap bins, same length as 
#'   heatmap_cuts, where first color is for those >= 1st cutpoint, but <2d,
#'   second color is for those >=2d cutpoint but <3d, etc.
#' @param hyperlink_cols which to treat as URLs that should be hyperlinks
#' @param graycolnums which columns to deemphasize
#' @param graycolor color used to deemphasize some columns
#' @param narrowcolnums which columns to make narrow
#' @param narrow6 how narrow
#' @param testing optional for testing only
#' @param ... other params passed along to [openxlsx::writeData()]
#' @import graphics
#' @return a workbook, ready to be saved in spreadsheet format, with tabs like "Overall" and "Each Site"
#' @export
#'
#' @examples \dontrun{
#'   wb <- xls_formatting2(datasetResults()$results_overall, datasetResults()$results_bysite)
#'   openxlsx::saveWorkbook(wb, "results.xlsx", overwrite = TRUE)
#' }
xls_formatting2 <- function(overall, eachsite, longnames, 
                            summary_plot = NULL, plot2 = NULL, 
                            analysis_title = "EJAM analysis",
                            buffer_desc = "Selected Locations", 
                            hyperlink_cols = NULL,
                            testing=FALSE,
                            heatmap_colnames=NULL, heatmap_cuts = c(80, 90, 95), heatmap_colors = c("yellow", "orange", "red"),
                            graycolnums=NULL, narrowcolnums=NULL, graycolor='gray', narrow6=6, ...) {
  if (testing) {
    cat('in xls_formatting2, names of each site: \n\n'); print(names(eachsite))
    cat('\n  and names of hyperlink_cols \n\n')
    print(hyperlink_cols)
  }
  
  if (length(heatmap_cuts) != length(heatmap_colors)) {
    warning("heatmap_cuts and heatmap_colors should be same length")
    if (length(heatmap_colors) == 2) {
      heatmap_colors <- colorRampPalette(c(heatmap_colors[1], heatmap_colors[2]) )(length(heatmap_cuts)) } else {
        heatmap_colors <-   heat.colors(length(heatmap_cuts), alpha = 0.5) # problem if length is 0? 1?
        # heatmap_colors <- colorRampPalette(c("white", "red" ))(length(heatmap_cuts))
      }
  }
  if (!is.null(heatmap_colnames) & !all(heatmap_colnames %in% names(eachsite))) {
    warning('all column names in heatmap_colnames should be found in eachsite table')
    heatmap_colnames <- intersect(heatmap_colnames, names(eachsite))
  }
  if (!is.null(hyperlink_cols) & !all(hyperlink_cols %in% names(eachsite)))   {
    warning('all column names in hyperlink_cols should be found in eachsite table')
    hyperlink_cols <- intersect(hyperlink_cols, names(eachsite))
  }
  
  # column names check/ cleanup?? #### 
  
  ## if no longnames provided, use existing column names ####
  if(is.null(longnames)){
    longnames <- names(overall)
  }
  ## replace missing column headers with friendly names ...but this only works if colnames are rname style *** ####
  longnames[longnames == ""] <- EJAMejscreenapi::map_headernames$names_friendly[match(names(overall)[longnames == ""],
                                                                                      EJAMejscreenapi::map_headernames$newnames_ejscreenapi)]
  # # or 
  # fixcolnames(......) ***
  
  headers_overall  <- names(overall)
  headers_eachsite <- names(eachsite)
  
  # replace Inf with NA to remove #NUM! errors in Excel
  overall   <- overall %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), function(x) ifelse(!is.finite(x), NA, x)))
  eachsite <- eachsite %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), function(x) ifelse(!is.finite(x), NA, x)))
  
  
  ############################################## # 
  # CREATE WORKBOOK with SHEETS ####
  
  wb <- openxlsx::createWorkbook()
  
  ## Overall sheet and Each Site sheet ####
  
  openxlsx::addWorksheet(wb, sheetName = 'Overall'  )
  openxlsx::addWorksheet(wb, sheetName = 'Each Site')
  
  ## PLOT  sheet ####
  
  #add v1_summary_plot() to 'plot' sheet of Excel download 
  openxlsx::addWorksheet(wb, sheetName = "plot",  gridLines = FALSE)
  ggplot2::ggsave(filename = paste0(tempdir(), '/', 'summary_plot.png'), plot = summary_plot,
                  width = 9, height = 7,units = 'in')
  openxlsx::insertImage(wb, sheet = 'plot', 
                        file = paste0(tempdir(), '/', 'summary_plot.png'),
                        width = 9, height = 7)
  
  if (!is.null(plot2)) {
    openxlsx::addWorksheet(wb, sheetName = "plot2",  gridLines = FALSE)
    png(filename = paste0(tempdir(), '/', 'plot2.png'), 
        width = 9 * 200, height = 7 * 200, units = "px")
    print(plot2)
    dev.off()
    openxlsx::insertImage(wb, sheet = 'plot2', file = paste0(tempdir(), '/', 'plot2.png'),
                          width = 9, height = 7)
  }
  
  ## NOTES  sheet  ####
  
  openxlsx::addWorksheet(wb, sheetName = "notes", gridLines = FALSE)
  notes_df <- data.frame(
    'Analysis Title' = analysis_title,
    'Number of Points Analyzed' = NROW(eachsite),
    ## to be generalized later when shapefile buffers are enabled
    'Radius of Circular Buffer (miles)' = buffer_desc,
    check.names = FALSE
  ) 
  notes_df <- t(notes_df)
  openxlsx::writeData(wb = wb, sheet = 'notes', x = notes_df, rowNames = TRUE, colNames = FALSE)
  
  
  ######################################################################## #
  # WRITE DATA ####
  ######################################################################## #
  
  openxlsx::writeData(wb, 
                      sheet = 'Overall', x = overall, 
                      xy = c(1,1), colNames = TRUE, 
                      withFilter = FALSE, 
                      keepNA = FALSE, # NA converted to blank or to #N/A
                      ...
  )
  openxlsx::writeData(wb, 
                      sheet = 'Each Site', x = eachsite, 
                      xy = c(1,1), colNames = TRUE, 
                      withFilter = TRUE, 
                      keepNA = FALSE,   # NA converted to blank or to #N/A
                      ...
  )
  
  ######################################################################## #
  # HYPERLINKS ####
  # special names for the pdf and map links #
  ######################################################################## #
  # 
  # output of ejamit()$results_bysite have a  EJScreen Report  column that has values like this:   
  # <a href="https://ejscreen.epa.gov/mapper/EJscreen_SOE_report.aspx?namestr=&geometry=%7B%22spatialReference%22:%7B%22wkid%22:4326%7D,%22x%22:-122.64108,%22y%22:45.55001%7D&distance=1&unit=9035&areatype=&areaid=&f=report", target="_blank">EJScreen Report</a>
  
  # output from app_server code, ready to get sent to xls_formatting2(), is like this:
  #   url_ejscreen_report(    lat = d_upload$lat, lon =  d_upload$lon, radius = input$bt_rad_buff, as_html = TRUE)
  # "<a href=\"https://ejscreen.epa.gov/mapper/EJscreen_SOE_report.aspx?namestr=&geometry=%7B%22spatialReference%22:%7B%22wkid%22:4326%7D,%22x%22:-103,%22y%22:42%7D&distance=3&unit=9035&areatype=&areaid=&f=report\", target=\"_blank\">EJScreen Report</a>"
  
  
  # ### code from ejscreenapi that was to make these columns work, somewhat generic naming possible
  
  if(!is.null(hyperlink_cols)){
    
    # hyperlink_text <- gsub('EJScreenPDF', 'EJScreen Report', hyperlink_cols)
    # hyperlink_text <- gsub('EJScreenMAP', 'EJScreen Map',    hyperlink_cols)
    hyperlink_text <- hyperlink_cols
    
    if (is.data.table(eachsite)) {
      data.table::setDF(eachsite) # to make syntax below work since it was written assuming data.frame only not data.table
    }
    
    for (i in 1:length(hyperlink_cols)) {
      
      class(eachsite[ , hyperlink_cols[i]]) <- 'hyperlink'
      names(eachsite[ , hyperlink_cols[i]]) <- paste(hyperlink_text[i], 1:NROW(eachsite))  # to use e.g., "EJScreen Report 1" not "EJScreenPDF 1"
      
    }
    
    hypercolnums <- match(hyperlink_cols, names(eachsite)) # returns the position of each, or NA if not found. is that what we want here??
    
    ## write to the worksheet the revised URL??
    for (i in 1:length(hyperlink_cols)) {
      openxlsx::writeData(wb, sheet = 'Each Site',
                          x = names(eachsite[ , hyperlink_cols[i]]),   # is that right ??? why names()  ???
                          startRow = 2, startCol = hypercolnums[i])
    }
  }
  
  ### to  fix html links to be simple URLs to work in csv pulled into Excel or in Excel files (as opposed to datatable shown in browser)
  #
  # eachsite$EJScreenPDF <- gsub('.*(http.*)\", target=.*', '\\1', eachsite$EJScreenPDF)  # converts as_html version back to simple URL 
  # eachsite$EJScreenMAP <- gsub('.*(http.*)\", target=.*', '\\1', eachsite$EJScreenMAP)
  
  # or
  ## code from EJAM xls_formatting function:
  
  ## format URL columns as hyperlinks ##
  
  # if('EJScreen Report' %in% names(eachsite)){
  #   eachsite$`EJScreen Report` <- gsub('<a href=\"', '',gsub('\", target=\"_blank\".*','',eachsite$`EJScreen Report`)) 
  #   class(eachsite$`EJScreen Report`) <- "hyperlink"   
  # } else {warning("Column not found with the name", )}
  # 
  # if('EJScreen Map' %in% names(eachsite)){
  #   eachsite$`EJScreen Map` <- gsub('<a href=\"', '',gsub('\", target=\"_blank\".*','',eachsite$`EJScreen Map`)) 
  #   class(eachsite$`EJScreen Map`) <- "hyperlink"
  # }
  # 
  # if('ECHO report' %in% names(eachsite)){
  #   eachsite$`ECHO report` <-ifelse(eachsite$`ECHO report` == 'N/A', 'N/A',  
  #                                   gsub('<a href=\"', '',gsub('\", target=\"_blank\".*','',eachsite$`ECHO report`)) )
  #   class(eachsite$`ECHO report`) <- ifelse(eachsite$`ECHO report` == 'N/A', "character", "hyperlink") 
  # }
  # 
  
  
  
  ######################################################################## #
  # end of hyperlink code
  ######################################################################## #
  
  ## check variable type of each column ####

  vartypes_overall  <- varname2vartype_ejam(headers_overall,  EJAMejscreenapi::map_headernames)
  vartypes_eachsite <- varname2vartype_ejam(headers_eachsite, EJAMejscreenapi::map_headernames)
  
  ## define percentile columns
  
  pctile_colnums_overall  <- which(vartypes_overall  == 'percentile')
  pctile_colnums_eachsite <- which(vartypes_eachsite == 'percentile')
  
  ## define columns getting heatmap colors for percentiles 
  ## note: there seems to be a small bug that also colors traffic indicator values... ?? ***
  # note FUNCTIONS LIKE xls_varname2color()  xls_varname2type() etc. ?
  
  if(!is.null(heatmap_colnames)){
    pctilecolnums <- which(names(eachsite) %in% heatmap_colnames)
    for (i in 1:length(heatmap_cuts)) {
      mystyle <- openxlsx::createStyle(bgFill = heatmap_colors[i])
      
      openxlsx::conditionalFormatting(wb, 'Overall', cols = pctile_colnums_overall,
                                      rows = 2:(NROW(overall) + 1), rule = paste0(">=", heatmap_cuts[i]),
                                      style = mystyle)
      openxlsx::conditionalFormatting(wb, 'Each Site', cols = pctile_colnums_eachsite,
                                      rows = 2:(NROW(eachsite) + 1), rule = paste0(">=", heatmap_cuts[i]),
                                      style = mystyle)
    }
  }
  
  
  ######################################################################## #
  
  # ROW 1 STYLE ####
  
  headstyle_basic <- openxlsx::createStyle(
    wrapText = TRUE, halign = "CENTER", valign = 'center',
    #fgFill = "#4F81BD",  
    textDecoration = "Bold"  , border = "bottom", borderStyle = "medium"
  )
  openxlsx::addStyle(wb, 'Overall',   style = headstyle_basic, stack = TRUE, rows = 1, cols = 1:NCOL(eachsite), gridExpand = TRUE)
  openxlsx::addStyle(wb, 'Each Site', style = headstyle_basic, stack = TRUE, rows = 1, cols = 1:NCOL(eachsite), gridExpand = TRUE)
  
  # FREEZE PANES: ROW 1 AND LEFTMOST COLUMNS ####
  
  # openxlsx::freezePane(wb, sheet = 'Each Site', firstRow = TRUE) #, firstCol = TRUE)  ## freeze first row and column
  openxlsx::freezePane(wb, sheet = 'Each Site', firstActiveCol = 4, firstActiveRow = 2)
  openxlsx::freezePane(wb, sheet = 'Overall',   firstActiveCol = 4) 
  
  # COLUMN WIDTHS   ####
  if (!is.null(narrowcolnums)) {
    openxlsx::setColWidths(wb, 'Overall',   cols = narrowcolnums, widths = narrow6)
    openxlsx::setColWidths(wb, 'Each Site', cols = narrowcolnums, widths = narrow6)
  }
  # HEADER ROW HEIGHT   ####
  
  openxlsx::setRowHeights(wb, sheet = 'Each Site', rows = 1, heights = 115)
  openxlsx::setRowHeights(wb, sheet = 'Overall',   rows = 1, heights = 115)
  
  # HEADER ROW COLOR ####
  
  header_colors_overall  <- varname2color_ejam(headers_overall,  EJAMejscreenapi::map_headernames)
  header_colors_eachsite <- varname2color_ejam(headers_eachsite, EJAMejscreenapi::map_headernames)
  header_colors_overall[ is.na(header_colors_overall )] <- ("gray")
  header_colors_eachsite[is.na(header_colors_eachsite)] <- ("gray")
  new_colors <- c(unique(header_colors_overall), unique(header_colors_eachsite))
  
  for(i in new_colors){
    style_cur <- openxlsx::createStyle(fgFill = i)
    openxlsx::addStyle(wb, 'Overall',   cols = which(header_colors_overall  == i), rows = 1, style = style_cur, stack = TRUE)
    openxlsx::addStyle(wb, 'Each Site', cols = which(header_colors_eachsite == i), rows = 1, style = style_cur, stack = TRUE)
  }
  
  # NUMBER FORMATS ####
  
  ### apply a default general number format to all misc columns ??? ***  ####
  
  
  
  
  ### Number format %ile columns   ####
  
  pctile_var_style <- openxlsx::createStyle(numFmt = '0')
  openxlsx::addStyle(wb, sheet = 'Overall',   rows = 2,                      cols = pctile_colnums_overall,  style=pctile_var_style)
  openxlsx::addStyle(wb, sheet = 'Each Site', rows = 2:(NROW(eachsite) + 1), cols = pctile_colnums_eachsite, style=pctile_var_style, gridExpand = TRUE)
  
  ### Number format raw indicator columns  ####
  
  raw_colnums_overall    <- which(vartypes_overall == 'raw data for indicator')
  raw_colnums_eachsite <-  which(vartypes_eachsite == 'raw data for indicator') 
  raw_var_style <- openxlsx::createStyle(numFmt = '0.00')
  openxlsx::addStyle(wb, sheet = 'Overall',   rows = 2,                      cols = raw_colnums_overall,  style=raw_var_style)
  openxlsx::addStyle(wb, sheet = 'Each Site', rows = 2:(NROW(eachsite) + 1), cols = raw_colnums_eachsite, style=raw_var_style, gridExpand = TRUE)
  
  ### Number format total count columns  ####
  
  count_colnums_overall  <- c(which(headers_overall == 'pop'), which(vartypes_overall  == 'count demog'))
  count_colnums_eachsite <- c(which(headers_overall == 'pop'), which(vartypes_eachsite == 'count demog'))
  count_var_style <- openxlsx::createStyle(numFmt = "#,###,###" )
  openxlsx::addStyle(wb, sheet = 'Overall',   rows = 2,                      cols = count_colnums_overall,  style=count_var_style)
  openxlsx::addStyle(wb, sheet = 'Each Site', rows = 2:(NROW(eachsite) + 1), cols = count_colnums_eachsite, style=count_var_style, gridExpand = TRUE)
  
  
  # Numbers HEATMAP / CONDITIONAL FORMATTING  ####
  
  # to highlight large percentiles in Excel
  other_colnums_overall  <- setdiff(1:length(vartypes_overall),  c(pctile_colnums_overall,  raw_colnums_overall,  count_colnums_overall))
  other_colnums_eachsite <- setdiff(1:length(vartypes_eachsite), c(pctile_colnums_eachsite, raw_colnums_eachsite, count_colnums_eachsite))
  other_var_style <- openxlsx::createStyle(numFmt = '##,##0.00')
  openxlsx::addStyle(wb, sheet = 'Overall',   rows = 2,                      cols = other_colnums_overall,  style=other_var_style)
  openxlsx::addStyle(wb, sheet = 'Each Site', rows = 2:(NROW(eachsite) + 1), cols = count_colnums_eachsite, style=other_var_style, gridExpand = TRUE)
  
  # done ###################### 
  
  return(wb)
}
################################################################################# # 


#' vartype2color_ejam
#' helper function - assign fill color to shade excel cells by indicator type
#' Use color shading to make spreadsheet easier to use, grouping the indicators
#'
#' @param vartype must be one found in varnameinfo$jsondoc_vartype, 
#'   ie "percentile", "average", or "raw data for indicator"
#'   NA if not found.
#'
#' @return vector of colors like c('lightblue', 'gray') matching length of vartype
#' @seealso varname2vartype_ejam() varname2color_ejam()
#' @export
#'
vartype2color_ejam <- function(vartype) {
  # for shading headers in Excel of results
  coloring <- matrix(
    c(
      # jsondoc_vartype 
      'percentile',              '#ADD8E6', #  "lightblue"
      "uspctile",                '#ADD8E6', #  "lightblue"
      "statepctile",             '#ADD8E6', #  "lightblue"
      
      'raw data for indicator' , '#FFA500', # "orange"
      "raw",                     '#FFA500', # "orange"
      "usraw",                   '#FFA500', # "orange"
      "stateraw",                '#FFA500', # "orange"
      
      'average',                 '#90EE90', # "lightgreen"
      "usavg",                   '#90EE90', # "lightgreen"
      "stateavg",                '#90EE90', # "lightgreen"
      
      'ratio',                   '#FFD700', # "gold"
      "usratio",                 '#FFD700', # "gold"
      "stateratio",              '#FFD700', # "gold" 
      
      'count demog',             '#FFBBFF', # "plum1"
      'misc',                    '#BEBEBE'  # "gray"
    ), 
    ncol=2, byrow=TRUE
  )
  colnames(coloring) <- c('vartype', 'color')
  coloring[match(vartype, coloring[, 'vartype'], nomatch = NA) , 'color']
}
################################################################################# # 


#' helper function - for color coding excel sheet columns
#' Wrapper for varname2vartype_ejam() and vartype2color_ejam() to simplify use case
#' @param varname things like us.avg.pctlowinc 
#'
#' @return vector of colors
#' @seealso varname2vartype_ejam() vartype2color_ejam()
#' @export
#'
varname2color_ejam <- function(varname, varnameinfo) {
  if (missing(varnameinfo)) {
    if (exists('map_headernames'))  {varnameinfo <- map_headernames} else {
      warning('missing varnameinfo and map_headernames')
      return(rep('black',length(varname)))
    }
  }
  vartype2color_ejam( varname2vartype_ejam(varname = varname, varnameinfo=varnameinfo))
}
################################################################################# # 


#' varname2vartype_ejam
#' helper function - given indicator names, look up what type each is
#' @details  
#'   The types are things like raw data count for indicator, average, percentile, etc.
#'   Variable names are stored in column of varnameinfo called newnames_ejscreenapi
#'   Types are stored in column of varnameinfo called jsondoc_vartype
#' @param varname vector of 1 or more names
#' @param varnameinfo data.frame with info on type of each variable
#'
#' @return vector same size as varname
#' @seealso vartype2color_ejam() varname2color_ejam()
#' @export
#'
varname2vartype_ejam <- function(varname, varnameinfo) {
  
  if (missing(varnameinfo)) {
    if (exists('map_headernames'))  {varnameinfo <- map_headernames} else {
      warning('missing varnameinfo and map_headernames')
      return(rep(NA,length(varname)))
    }
  }
  cur_matches <- varnameinfo[match(varname, varnameinfo[ , 'rname'], nomatch = NA) , 'vartype']
  
  ## hardcode Total Population column to 'misc'/gray since it has two rows in map_headernames table
  cur_matches[grep('pop', varname)] <- 'misc'
  #cur_matches[grep('Total Population', varname)] <- 'misc'
  
  cur_matches[grep('^avg', varname)] <- 'average'
  cur_matches[grep('^ratio.to.', varname)] <- 'ratio'
  cur_matches[cur_matches == 'n'] <- NA
  cur_matches[cur_matches == ""] <- NA
  return(cur_matches)
}
################################################################################# # 

