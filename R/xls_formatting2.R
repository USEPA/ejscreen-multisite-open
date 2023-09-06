#' xls_formatting2
#' Format EJAM tabular outputs for saving as Excel spreadsheet
#' @details  NEED TO MERGE THIS WITH EJAMejscreenapi::xls_formatting_api() 
#' @param overall table to save in one tab, from EJAM analysis of indicators overall (one row)
#' @param eachsite table to save in another tab, from EJAM analysis site by site (one row per site)
#' @param longnames vector of indicator names to display in Excel table
#' @param summary_plot optional plot passed from EJAM shiny app to save in 'Plot' sheet of Excel table
#' @param analysis_title optional title passed from Shiny app to 'Notes' sheet
#' @param buffer_desc optional description of buffer used in analysis, passed to 'Notes' sheet
#' @param heatmap_cuts vector of values to separate heatmap colors, between 0-100
#' @param heatmap_colors vector of color names for heatmap bins, same length as 
#'   heatmap_cuts, where first color is for those >= 1st cutpoint, but <2d,
#'   second color is for those >=2d cutpoint but <3d, etc. 
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
xls_formatting2 <- function(overall, eachsite, longnames, 
                            summary_plot = NULL, analysis_title = NULL,
                            buffer_desc = NULL,
                            heatmap_cuts = c(80, 90, 95), heatmap_colors = c("yellow", "orange", "red"),
                            graycolnums=NULL, narrowcolnums=NULL, graycolor='gray', narrow6=6, ...) {

  
  if (length(heatmap_cuts) != length(heatmap_colors))
    stop("heatmap_cuts and heatmap_colors must be same length")
  
  ## if no names provided, use existing column names
  if(is.null(longnames)){
    longnames <- names(overall)
  }
  
  ## not currently used
  # if (!is.null(graycolnums))   {graycolnums <- which(grepl('avg', names(overall)) | grepl('state', names(overall)))}   # only works for the short names
  # if (!is.null(narrowcolnums)) {narrowcolnums <- graycolnums}
  
  
  headers_overall <- names(overall)
  headers_eachsite <- names(eachsite)
  
  ## replace Inf with NA to remove #NUM! errors in Excel
  overall <- overall %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), function(x) ifelse(!is.finite(x), NA, x)))
  eachsite <- eachsite %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), function(x) ifelse(!is.finite(x), NA, x)))
  
  #filter out sitecount - TEMP
  filter_out_cols <- c("sitecount_unique","sitecount_avg","sitecount_max")
  filter_out_temp_overall <- !(headers_overall %in% filter_out_cols)
  filter_out_temp_bysite <- !(headers_eachsite %in% filter_out_cols)
  
  overall  <- overall[,..filter_out_temp_overall]
  eachsite <- eachsite[,..filter_out_temp_bysite]
  
 
  #filter out sitecount from longnames - TEMP
  longnames <- longnames[filter_out_temp_bysite]
 
  ## fix NA longname added from doaggregate updates
  longnames[is.na(longnames)] <- 'statename'
   
  ## replace missing column headers with friendly names 
  longnames[longnames == ""] <- EJAMejscreenapi::map_headernames$names_friendly[match(names(overall)[longnames == ""], 
                                                                                      EJAMejscreenapi::map_headernames$newnames_ejscreenapi)]
  ## add patch to switch 'statename' and 'Radius (miles)' order
  longnames[length(longnames)] <- 'Radius (Miles)'
  longnames[length(longnames) - 1] <- 'State Name'
  
  ## remove URL columns for eachsite table
  longnames_no_url <- longnames[!(longnames %in% c('EJScreen Report','EJScreen Map','ACS Report','ECHO report', 'State Name'))] 
  
  names(overall) <- ifelse(!is.na(longnames_no_url), longnames_no_url, names(overall))
  names(eachsite)  <- ifelse(!is.na(longnames), longnames, names(eachsite))
  
  
  if (length(heatmap_cuts) != length(heatmap_colors))
    stop("heatmap_cuts and heatmap_colors must be same length")
  
  ## if no names provided, use existing column names
  if(is.null(longnames)){
    longnames <- names(overall)
  }
  
  ## not currently used
  # if (!is.null(graycolnums))   {graycolnums <- which(grepl('avg', names(overall)) | grepl('state', names(overall)))}   # only works for the short names
  # if (!is.null(narrowcolnums)) {narrowcolnums <- graycolnums}
  
  
  headers_overall <- names(overall)
  headers_eachsite <- names(eachsite)
  
  ## replace Inf with NA to remove #NUM! errors in Excel
  overall <- overall %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), function(x) ifelse(!is.finite(x), NA, x)))
  eachsite <- eachsite %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric), function(x) ifelse(!is.finite(x), NA, x)))
  
  #filter out sitecount - TEMP
  filter_out_cols <- c("sitecount_unique","sitecount_avg","sitecount_max")
  filter_out_temp_overall <- !(headers_overall %in% filter_out_cols)
  filter_out_temp_bysite <- !(headers_eachsite %in% filter_out_cols)
  
  overall  <- overall[,..filter_out_temp_overall]
  eachsite <- eachsite[,..filter_out_temp_bysite]
  
  #filter out sitecount from longnames - TEMP
  longnames <- longnames[filter_out_temp_bysite]
 
  ## fix NA longname added from doaggregate updates
  longnames[is.na(longnames)] <- 'statename'

  ## replace missing column headers with friendly names 
  longnames[longnames == ""] <- EJAMejscreenapi::map_headernames$names_friendly[match(names(overall)[longnames == ""], 
                                                                                      EJAMejscreenapi::map_headernames$newnames_ejscreenapi)]
  ## add patch to switch 'statename' and 'Radius (miles)' order
  longnames[length(longnames)] <- 'Radius (Miles)'
  longnames[length(longnames) - 1] <- 'State Name'
  
  ## remove URL columns for overall table
  longnames_no_url <- longnames[!(longnames %in% c('EJScreen Report','EJScreen Map','ACS Report','ECHO report', 'State Name'))] 
 
  names(overall) <- ifelse(!is.na(longnames_no_url), longnames_no_url, names(overall))
  names(eachsite)  <- ifelse(!is.na(longnames), longnames, names(eachsite))
 
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, 'Overall')
  openxlsx::addWorksheet(wb, 'Each Site')
  
  openxlsx::addWorksheet(wb, sheetName = "plot", gridLines = FALSE)
  
  ## add v1_summary_plot() to 'plot' sheet of Excel download
  ## will be moved to eventual merged 'xls_formatting' function
  ggplot2::ggsave(filename = paste0(tempdir(), '/', 'summary_plot.png'), plot = summary_plot,
                  height = 7, width = 9, units = 'in')
  openxlsx::insertImage(wb, sheet = 'plot', 
                        file = paste0(tempdir(), '/', 'summary_plot.png'),
                        width = 9, height = 7)
  # showGridLines(wb, sheet = "plot", showGridLines = FALSE)

    
  openxlsx::addWorksheet(wb, sheetName = "notes", gridLines = FALSE)
  
  ## add analysis overview to 'notes' tab
  notes_df <- data.frame(
    'Analysis Title' = analysis_title,
    'Number of Points Analyzed' = NROW(eachsite),
    ## to be generalized later when shapefile buffers are enabled
    'Radius of Circular Buffer (miles)' = buffer_desc,
    check.names = FALSE
  ) 
  notes_df <- t(notes_df)
  
  openxlsx::writeData(wb = wb, sheet = 'notes', x = notes_df, rowNames = TRUE, colNames = FALSE)
  
  
  headstyle_basic <- openxlsx::createStyle(
    wrapText = TRUE, halign = "CENTER", valign = 'center',
    #fgFill = "#4F81BD",  
    textDecoration = "Bold" #,
    # border = "Bottom", fontColour = "white"
  )
  
  ## format URL columns as hyperlinks
  if('EJScreen Report' %in% names(eachsite)){
    eachsite$`EJScreen Report` <- gsub('<a href=\"', '',gsub('\", target=\"_blank\".*','',eachsite$`EJScreen Report`)) 
    class(eachsite$`EJScreen Report`) <- "hyperlink"   
  }
 
  if('EJScreen Map' %in% names(eachsite)){
    eachsite$`EJScreen Map` <- gsub('<a href=\"', '',gsub('\", target=\"_blank\".*','',eachsite$`EJScreen Map`)) 
    class(eachsite$`EJScreen Map`) <- "hyperlink"
  }
  
  if('ACS Report' %in% names(eachsite)){
    eachsite$`ACS Report` <- gsub('<a href=\"', '',gsub('\", target=\"_blank\".*','',eachsite$`ACS Report`)) 
    class(eachsite$`ACS Report`) <- "hyperlink"
    
  }
  
  if('ECHO report' %in% names(eachsite)){
    
    eachsite$`ECHO report` <-ifelse(eachsite$`ECHO report` == 'N/A', 'N/A',  
                                    gsub('<a href=\"', '',gsub('\", target=\"_blank\".*','',eachsite$`ECHO report`)) )
    class(eachsite$`ECHO report`) <- ifelse(eachsite$`ECHO report` == 'N/A', "character", "hyperlink") 
  }

  ## get variable types for all columns
  vartypes_overall <- varname2vartype_ejam(headers_overall, EJAMejscreenapi::map_headernames)
  vartypes_eachsite <- varname2vartype_ejam(headers_eachsite, EJAMejscreenapi::map_headernames)
  
  ## define percentile columns
  pctile_colnums_overall <- which(vartypes_overall == 'percentile')
  pctile_colnums_eachsite <- which(vartypes_eachsite == 'percentile')

  ## apply heatmap colors for percentile columns
  ## note: there seems to a small bug that also colors traffic indicator values...
  for (i in 1:length(heatmap_cuts)) {
    mystyle <- openxlsx::createStyle(bgFill = heatmap_colors[i])
    openxlsx::conditionalFormatting(wb, 'Overall', cols = pctile_colnums_overall,
                                    rows = 2:(NROW(overall) + 1), rule = paste0(">=", heatmap_cuts[i]),
                                    style = mystyle)
    openxlsx::conditionalFormatting(wb, 'Each Site', cols = pctile_colnums_eachsite,
                                    rows = 2:(NROW(eachsite) + 1), rule = paste0(">=", heatmap_cuts[i]),
                                    style = mystyle)
  }
  
  openxlsx::writeData(wb, 
                      sheet = 'Overall', x = overall, 
                      xy = c(1,1), colNames = TRUE, 
                      withFilter = FALSE, 
                      keepNA = FALSE, # NA converted to blank or to #N/A
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
  
  
  # add fill colors to column headers 
  header_colors_overall <- varname2color_ejam(headers_overall, EJAMejscreenapi::map_headernames)
  header_colors_eachsite <- varname2color_ejam(headers_eachsite, EJAMejscreenapi::map_headernames)
  
  header_colors_overall[is.na(header_colors_overall)] <- 'gray'
  header_colors_eachsite[is.na(header_colors_eachsite)] <- 'gray'
 
  new_colors <- c(unique(header_colors_overall), unique(header_colors_eachsite))
  
  for(i in new_colors){
    style_cur <- openxlsx::createStyle(fgFill = i)
    openxlsx::addStyle(wb, 'Overall',   cols = which(header_colors_overall == i), rows = 1, style = style_cur, stack = TRUE)
    openxlsx::addStyle(wb, 'Each Site',   cols = which(header_colors_eachsite == i), rows = 1, style = style_cur, stack = TRUE)
  }
  
  ## apply number format to percentile columns 
  pctile_var_style <- openxlsx::createStyle(numFmt = '0')
  openxlsx::addStyle(wb, sheet = 'Overall', rows = 2, cols = pctile_colnums_overall, style=pctile_var_style)
  openxlsx::addStyle(wb, sheet = 'Each Site', rows = 2:(NROW(eachsite) + 1), cols = pctile_colnums_eachsite, style=pctile_var_style, gridExpand = TRUE)

  ## apply number format to raw indicator columns
  raw_colnums_overall <- which(vartypes_overall == 'raw data for indicator')
  raw_colnums_eachsite <-  which(vartypes_eachsite == 'raw data for indicator') 
  raw_var_style <- openxlsx::createStyle(numFmt = '0.00')
  openxlsx::addStyle(wb, sheet = 'Overall', rows = 2, cols = raw_colnums_overall, style=raw_var_style)
  openxlsx::addStyle(wb, sheet = 'Each Site', rows = 2:(NROW(eachsite) + 1), cols = raw_colnums_eachsite, style=raw_var_style, gridExpand = TRUE)
  
  ## apply number format to count columns (including Total Population)
  count_colnums_overall <- c(which(headers_overall == 'pop'), which(vartypes_overall == 'count demog'))
  count_colnums_eachsite <- c(which(headers_overall == 'pop'), which(vartypes_eachsite == 'count demog'))
  count_var_style <- openxlsx::createStyle(numFmt = "#,###,###" )
  openxlsx::addStyle(wb, sheet = 'Overall', rows = 2, cols = count_colnums_overall, style=count_var_style)
  openxlsx::addStyle(wb, sheet = 'Each Site', rows = 2:(NROW(eachsite) + 1), cols = count_colnums_eachsite, style=count_var_style, gridExpand = TRUE)
  
  ## apply general number format to all other columns
  other_colnums_overall <- setdiff(1:length(vartypes_overall), c(pctile_colnums_overall, raw_colnums_overall, count_colnums_overall))
  other_colnums_eachsite <- setdiff(1:length(vartypes_eachsite), c(pctile_colnums_eachsite, raw_colnums_eachsite, count_colnums_eachsite))
  other_var_style <- openxlsx::createStyle(numFmt = '##,##0.00')
  openxlsx::addStyle(wb, sheet = 'Overall', rows = 2, cols = other_colnums_overall, style=other_var_style)
  openxlsx::addStyle(wb, sheet = 'Each Site', rows = 2:(NROW(eachsite) + 1), cols = count_colnums_eachsite, style=other_var_style, gridExpand = TRUE)
  
  return(wb)
  
}

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
      'raw data for indicator' , '#FFA500', # "orange"
      'average',                 '#90EE90', # "lightgreen"
      'ratio',                   '#FFD700', # "gold"
      'count demog',             '#FFBBFF', # "plum1"
      'misc',                    '#BEBEBE'  # "gray"
    ), 
    ncol=2, byrow=TRUE
  )
  colnames(coloring) <- c('vartype', 'color')
  coloring[match(vartype, coloring[, 'vartype'], nomatch = NA) , 'color']
  
}

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
  cur_matches <- varnameinfo[match(varname, varnameinfo[ , 'newnames_ejscreenapi'], nomatch = NA) , 'jsondoc_vartype']
  
  ## hardcode Total Population column to 'misc'/gray since it has two rows in map_headernames table
  cur_matches[grep('pop', varname)] <- 'misc'
  #cur_matches[grep('Total Population', varname)] <- 'misc'
  
  cur_matches[grep('^avg', varname)] <- 'average'
  cur_matches[grep('^ratio.to.', varname)] <- 'ratio'
  cur_matches[cur_matches == 'n'] <- NA
  cur_matches[cur_matches == ""] <- NA
  return(cur_matches)
  
}
